"""
Tests for version stability during generation.

These tests verify that:
1. Only stable (complete) versions are used as the base for new generations
2. Cancelling a generation cleans up properly
3. The latest stable version is correctly identified
4. Version numbers are assigned correctly even with incomplete versions
"""
from django.test import TestCase, TransactionTestCase
from django.contrib.auth import get_user_model
from rest_framework.test import APIClient
from rest_framework import status

from ..models import (
    AppVersion,
    AppVersionGenerationStatus,
    InternalApp,
    Organization,
    UserOrganization,
    VersionFile,
)
from ..services.version_service import VersionService

User = get_user_model()


class VersionServiceTestCase(TestCase):
    """Test the VersionService helper functions."""
    
    def setUp(self):
        """Set up test data."""
        self.user = User.objects.create_user(
            username='testuser',
            email='test@example.com',
            password='testpass123',
        )
        self.org = Organization.objects.create(
            name='Test Org',
            slug='test-org',
        )
        UserOrganization.objects.create(
            user=self.user,
            organization=self.org,
            role='admin',
        )
        self.app = InternalApp.objects.create(
            organization=self.org,
            name='Test App',
            created_by=self.user,
        )
    
    def test_get_latest_stable_version_no_versions(self):
        """Should return None when no versions exist."""
        result = VersionService.get_latest_stable_version(self.app)
        self.assertIsNone(result)
    
    def test_get_latest_stable_version_only_generating(self):
        """Should return None when only generating versions exist."""
        AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            spec_json={'test': True},
            generation_status=AppVersionGenerationStatus.GENERATING,
            created_by=self.user,
        )
        
        result = VersionService.get_latest_stable_version(self.app)
        self.assertIsNone(result)
    
    def test_get_latest_stable_version_skips_generating(self):
        """Should skip generating versions and return the latest complete one."""
        # Create complete version
        complete_v1 = AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            spec_json={'version': 1},
            generation_status=AppVersionGenerationStatus.COMPLETE,
            is_active=True,  # Active versions only
            created_by=self.user,
        )
        
        # Create generating version (newer)
        AppVersion.objects.create(
            internal_app=self.app,
            version_number=2,
            spec_json={'version': 2},
            generation_status=AppVersionGenerationStatus.GENERATING,
            created_by=self.user,
        )
        
        result = VersionService.get_latest_stable_version(self.app)
        self.assertEqual(result, complete_v1)
        self.assertEqual(result.version_number, 1)
    
    def test_get_latest_stable_version_skips_error(self):
        """Should skip error versions and return the latest complete one."""
        # Create complete version
        complete_v1 = AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            spec_json={'version': 1},
            generation_status=AppVersionGenerationStatus.COMPLETE,
            is_active=True,  # Active versions only
            created_by=self.user,
        )
        
        # Create error version (newer)
        AppVersion.objects.create(
            internal_app=self.app,
            version_number=2,
            spec_json={'version': 2},
            generation_status=AppVersionGenerationStatus.ERROR,
            generation_error='Test error',
            created_by=self.user,
        )
        
        result = VersionService.get_latest_stable_version(self.app)
        self.assertEqual(result, complete_v1)
    
    def test_get_latest_version_includes_all(self):
        """get_latest_version should include generating versions for number calculation."""
        AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            spec_json={'version': 1},
            generation_status=AppVersionGenerationStatus.COMPLETE,
            created_by=self.user,
        )
        
        generating = AppVersion.objects.create(
            internal_app=self.app,
            version_number=2,
            spec_json={'version': 2},
            generation_status=AppVersionGenerationStatus.GENERATING,
            created_by=self.user,
        )
        
        result = VersionService.get_latest_version(self.app)
        self.assertEqual(result, generating)
        self.assertEqual(result.version_number, 2)
    
    def test_get_next_version_number_accounts_for_generating(self):
        """Next version number should be correct even with generating versions."""
        AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            spec_json={'version': 1},
            generation_status=AppVersionGenerationStatus.COMPLETE,
            created_by=self.user,
        )
        
        AppVersion.objects.create(
            internal_app=self.app,
            version_number=2,
            spec_json={'version': 2},
            generation_status=AppVersionGenerationStatus.GENERATING,
            created_by=self.user,
        )
        
        next_num = VersionService.get_next_version_number(self.app)
        self.assertEqual(next_num, 3)  # Should be 3, not 2


class CancelGenerationTestCase(TestCase):
    """Test the cancel generation functionality."""
    
    def setUp(self):
        """Set up test data."""
        self.user = User.objects.create_user(
            username='testuser',
            email='test@example.com',
            password='testpass123',
        )
        self.org = Organization.objects.create(
            name='Test Org',
            slug='test-org',
        )
        UserOrganization.objects.create(
            user=self.user,
            organization=self.org,
            role='admin',
        )
        self.app = InternalApp.objects.create(
            organization=self.org,
            name='Test App',
            created_by=self.user,
        )
    
    def test_cancel_generating_version_no_files_deletes(self):
        """Cancelling a generating version with no files should delete it."""
        version = AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            spec_json={'test': True},
            generation_status=AppVersionGenerationStatus.GENERATING,
            created_by=self.user,
        )
        version_id = str(version.id)
        
        result = VersionService.cancel_generating_version(version_id)
        
        self.assertTrue(result['success'])
        self.assertEqual(result['action'], 'deleted')
        
        # Version should be deleted
        self.assertFalse(AppVersion.objects.filter(pk=version_id).exists())
    
    def test_cancel_generating_version_with_files_marks_error(self):
        """Cancelling a generating version with files should mark it as error."""
        version = AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            spec_json={'test': True},
            generation_status=AppVersionGenerationStatus.GENERATING,
            created_by=self.user,
        )
        
        # Add a file
        VersionFile.objects.create(
            app_version=version,
            path='src/App.tsx',
            content='export default function App() { return <div>Test</div> }',
        )
        
        result = VersionService.cancel_generating_version(str(version.id))
        
        self.assertTrue(result['success'])
        self.assertEqual(result['action'], 'marked_error')
        
        # Version should still exist but be marked as error
        version.refresh_from_db()
        self.assertEqual(version.generation_status, AppVersionGenerationStatus.ERROR)
        self.assertEqual(version.generation_error, 'Generation cancelled by user')
    
    def test_cancel_complete_version_no_op(self):
        """Cancelling a complete version should be a no-op."""
        version = AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            spec_json={'test': True},
            generation_status=AppVersionGenerationStatus.COMPLETE,
            created_by=self.user,
        )
        
        result = VersionService.cancel_generating_version(str(version.id))
        
        self.assertTrue(result['success'])
        self.assertEqual(result['action'], 'already_complete')
        
        # Version should be unchanged
        version.refresh_from_db()
        self.assertEqual(version.generation_status, AppVersionGenerationStatus.COMPLETE)
    
    def test_cancel_nonexistent_version(self):
        """Cancelling a non-existent version should return error."""
        import uuid
        result = VersionService.cancel_generating_version(str(uuid.uuid4()))
        
        self.assertFalse(result['success'])
        self.assertEqual(result['error'], 'Version not found')


class CancelEndpointTestCase(TransactionTestCase):
    """Test the cancel generation API endpoint."""
    
    def setUp(self):
        """Set up test data."""
        self.client = APIClient()
        self.user = User.objects.create_user(
            username='testuser',
            email='test@example.com',
            password='testpass123',
        )
        self.org = Organization.objects.create(
            name='Test Org',
            slug='test-org',
        )
        UserOrganization.objects.create(
            user=self.user,
            organization=self.org,
            role='admin',
        )
        self.app = InternalApp.objects.create(
            organization=self.org,
            name='Test App',
            created_by=self.user,
        )
        self.client.force_authenticate(user=self.user)
    
    def test_cancel_endpoint_deletes_empty_version(self):
        """API endpoint should delete a generating version with no files."""
        version = AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            spec_json={'test': True},
            generation_status=AppVersionGenerationStatus.GENERATING,
            created_by=self.user,
        )
        
        response = self.client.post(f'/api/v1/versions/{version.id}/cancel/')
        
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertTrue(response.data['success'])
        self.assertEqual(response.data['action'], 'deleted')
        
        # Version should be deleted
        self.assertFalse(AppVersion.objects.filter(pk=version.id).exists())
    
    def test_cancel_endpoint_returns_stable_version(self):
        """Cancel endpoint should return info about the latest stable version."""
        # Create a complete version
        stable = AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            spec_json={'stable': True},
            generation_status=AppVersionGenerationStatus.COMPLETE,
            is_active=True,  # Active versions only
            created_by=self.user,
        )
        
        # Create a generating version
        generating = AppVersion.objects.create(
            internal_app=self.app,
            version_number=2,
            spec_json={'generating': True},
            generation_status=AppVersionGenerationStatus.GENERATING,
            created_by=self.user,
        )
        
        response = self.client.post(f'/api/v1/versions/{generating.id}/cancel/')
        
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(response.data['latest_stable_version_id'], str(stable.id))
        self.assertEqual(response.data['latest_stable_version_number'], 1)
    
    def test_cancel_endpoint_requires_auth(self):
        """Cancel endpoint should require authentication."""
        version = AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            spec_json={'test': True},
            generation_status=AppVersionGenerationStatus.GENERATING,
            created_by=self.user,
        )
        
        # Use unauthenticated client
        unauth_client = APIClient()
        response = unauth_client.post(f'/api/v1/versions/{version.id}/cancel/')
        
        self.assertEqual(response.status_code, status.HTTP_401_UNAUTHORIZED)


class StableVersionUsageTestCase(TestCase):
    """Test that stable versions are used correctly in various operations."""
    
    def setUp(self):
        """Set up test data."""
        self.user = User.objects.create_user(
            username='testuser',
            email='test@example.com',
            password='testpass123',
        )
        self.org = Organization.objects.create(
            name='Test Org',
            slug='test-org',
        )
        UserOrganization.objects.create(
            user=self.user,
            organization=self.org,
            role='admin',
        )
        self.app = InternalApp.objects.create(
            organization=self.org,
            name='Test App',
            created_by=self.user,
        )
    
    def test_new_generation_uses_stable_version_spec(self):
        """New generations should use the stable version's spec, not generating."""
        # Create stable version
        stable = AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            spec_json={'pages': [{'id': 'page1', 'title': 'Stable Page'}]},
            generation_status=AppVersionGenerationStatus.COMPLETE,
            is_active=True,  # Active versions only
            created_by=self.user,
        )
        
        # Create generating version with different spec
        AppVersion.objects.create(
            internal_app=self.app,
            version_number=2,
            spec_json={'pages': [{'id': 'page2', 'title': 'Generating Page'}]},
            generation_status=AppVersionGenerationStatus.GENERATING,
            created_by=self.user,
        )
        
        # Get stable version - should be the first one
        latest_stable = VersionService.get_latest_stable_version(self.app)
        self.assertEqual(latest_stable, stable)
        self.assertEqual(latest_stable.spec_json['pages'][0]['title'], 'Stable Page')
    
    def test_version_number_sequential_despite_cancelled(self):
        """Version numbers should remain sequential even after cancellation."""
        # Create versions 1 and 2 (complete)
        AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            spec_json={'v': 1},
            generation_status=AppVersionGenerationStatus.COMPLETE,
            created_by=self.user,
        )
        
        v2 = AppVersion.objects.create(
            internal_app=self.app,
            version_number=2,
            spec_json={'v': 2},
            generation_status=AppVersionGenerationStatus.GENERATING,
            created_by=self.user,
        )
        
        # Cancel v2
        VersionService.cancel_generating_version(str(v2.id))
        
        # Next version should be 3 (v2 was deleted, but we still track version numbers)
        # Actually, since v2 was deleted, the max is 1, so next should be 2
        next_num = VersionService.get_next_version_number(self.app)
        self.assertEqual(next_num, 2)  # v2 was deleted, so next is 2
    
    def test_stable_version_after_multiple_cancellations(self):
        """Should correctly identify stable version after multiple cancellations."""
        # Create stable v1
        stable = AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            spec_json={'v': 1},
            generation_status=AppVersionGenerationStatus.COMPLETE,
            is_active=True,  # Active versions only
            created_by=self.user,
        )
        
        # Create and cancel multiple versions
        for i in range(2, 5):
            v = AppVersion.objects.create(
                internal_app=self.app,
                version_number=i,
                spec_json={'v': i},
                generation_status=AppVersionGenerationStatus.GENERATING,
                created_by=self.user,
            )
            VersionService.cancel_generating_version(str(v.id))
        
        # Stable should still be v1
        result = VersionService.get_latest_stable_version(self.app)
        self.assertEqual(result, stable)

