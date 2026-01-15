"""
Integration tests for Organization Logo API endpoints.

Tests the full API workflow for logo upload, retrieval, and deletion.

Usage:
    cd /Users/mac/Engineering/internal-apps/internal-apps-backend
    source venv/bin/activate
    python tests/test_organization_logo_api.py
"""
import os
import sys
import uuid
from io import BytesIO
from PIL import Image
from vector_app.models import UserOrganizationRole
from vector_app.models import UserOrganizationRole



# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Setup Django settings
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'internal_apps.settings')

import django
django.setup()

from django.test import RequestFactory
from django.contrib.auth import get_user_model
from rest_framework.test import force_authenticate

from vector_app.models import Organization, UserOrganization
from vector_app.views.organization_views import OrganizationViewSet
from vector_app.services.image_upload_service import ImageUploadService
from vector_app.services.cloud_storage_service import CloudStorageService

User = get_user_model()


class TestOrganizationLogoAPI:
    """Integration tests for organization logo API."""
    
    def __init__(self):
        self.passed = 0
        self.failed = 0
        self.errors = []
        self.factory = RequestFactory()
        self.user = None
        self.org = None
    
    def setup(self):
        """Create test user and organization."""
        # Create or get test user
        unique_id = uuid.uuid4().hex[:8]
        email = f'test_{unique_id}@example.com'
        self.user = User.objects.create_user(
            username=f'testuser_{unique_id}',
            email=email,
            password='testpass123',
            first_name='Test',
            last_name='User'
        )
        
        # Create test organization
        self.org = Organization.objects.create(
            name=f'Test Org {uuid.uuid4().hex[:8]}',
            slug=f'test-org-{uuid.uuid4().hex[:8]}'
        )
        
        # Make user an admin of the org
        UserOrganization.objects.create(
            user=self.user,
            organization=self.org,
            role=UserOrganizationRole.ADMIN
        )
        
        print(f"  Created test user: {self.user.email}")
        print(f"  Created test org: {self.org.name}")
    
    def teardown(self):
        """Clean up test data."""
        # Delete org logo if it exists
        if self.org:
            if self.org.logo_storage_key:
                ImageUploadService.delete_image(self.org.logo_storage_key)
            if self.org.logo:
                self.org.logo.delete(save=False)
            
            self.org.delete()
        
        if self.user:
            self.user.delete()
        
        print("  Cleaned up test data")
    
    def create_test_image(self, format='PNG', size=(100, 100), color='red'):
        """Create a test image file."""
        image = Image.new('RGB', size, color)
        buffer = BytesIO()
        image.save(buffer, format=format)
        buffer.seek(0)
        return buffer
    
    def test(self, name, condition, message=""):
        """Run a single test."""
        try:
            if condition:
                self.passed += 1
                print(f"  ✅ {name}")
            else:
                self.failed += 1
                error_msg = f"  ❌ {name}: {message}"
                print(error_msg)
                self.errors.append(error_msg)
        except Exception as e:
            self.failed += 1
            error_msg = f"  ❌ {name}: Exception - {str(e)}"
            print(error_msg)
            self.errors.append(error_msg)
    
    def run_all_tests(self):
        """Run all integration tests."""
        print("\n" + "="*60)
        print("Organization Logo API Integration Tests")
        print("="*60)
        
        # Check storage mode
        is_cloud = CloudStorageService.is_configured()
        print(f"\nStorage Mode: {'Cloud (Cloudflare R2)' if is_cloud else 'Local (MEDIA_ROOT)'}")
        
        print("\n--- Setup ---")
        self.setup()
        
        try:
            print("\n--- Test: Upload Logo ---")
            self.test_upload_logo()
            
            print("\n--- Test: Get Organization with Logo ---")
            self.test_get_org_with_logo()
            
            print("\n--- Test: Delete Logo ---")
            self.test_delete_logo()
            
            print("\n--- Test: Upload Again After Delete ---")
            self.test_upload_after_delete()
            
        finally:
            print("\n--- Teardown ---")
            self.teardown()
        
        # Summary
        print("\n" + "="*60)
        print(f"Results: {self.passed} passed, {self.failed} failed")
        print("="*60)
        
        if self.errors:
            print("\nErrors:")
            for error in self.errors:
                print(error)
        
        return self.failed == 0
    
    def test_upload_logo(self):
        """Test uploading a logo via API."""
        from django.core.files.uploadedfile import InMemoryUploadedFile
        
        # Create test image
        image_buffer = self.create_test_image(format='PNG', color='blue')
        image_file = InMemoryUploadedFile(
            file=image_buffer,
            field_name='logo',
            name='test_logo.png',
            content_type='image/png',
            size=len(image_buffer.getvalue()),
            charset=None
        )
        
        # Create request
        request = self.factory.post(
            f'/api/v1/orgs/{self.org.id}/logo/',
            {'logo': image_file},
            format='multipart'
        )
        force_authenticate(request, user=self.user)
        
        # Get view
        view = OrganizationViewSet.as_view({'post': 'logo'})
        response = view(request, pk=str(self.org.id))
        
        self.test(
            "Logo upload returns 200",
            response.status_code == 200,
            f"Status: {response.status_code}, Data: {response.data}"
        )
        
        self.test(
            "Response contains organization data",
            'organization' in response.data,
            f"Response: {response.data}"
        )
        
        self.test(
            "Response contains logo_url",
            response.data.get('organization', {}).get('logo_url') is not None,
            f"Organization: {response.data.get('organization')}"
        )
        
        self.test(
            "Response indicates storage_type",
            'storage_type' in response.data,
            f"Response: {response.data}"
        )
        
        # Refresh org from DB
        self.org.refresh_from_db()
        
        self.test(
            "Org has logo_storage_key set",
            self.org.logo_storage_key is not None,
            f"logo_storage_key: {self.org.logo_storage_key}"
        )
        
        storage_key = self.org.logo_storage_key
        is_cloud = CloudStorageService.is_configured()
        
        if is_cloud:
            self.test(
                "Storage key has r2:// prefix for cloud",
                storage_key.startswith('r2://'),
                f"storage_key: {storage_key}"
            )
        else:
            self.test(
                "Storage key has local:// prefix for local storage",
                storage_key.startswith('local://'),
                f"storage_key: {storage_key}"
            )
    
    def test_get_org_with_logo(self):
        """Test retrieving organization with logo URL."""
        # Create request
        request = self.factory.get(f'/api/v1/orgs/{self.org.id}/')
        force_authenticate(request, user=self.user)
        
        # Get view
        view = OrganizationViewSet.as_view({'get': 'retrieve'})
        response = view(request, pk=str(self.org.id))
        
        self.test(
            "Get org returns 200",
            response.status_code == 200,
            f"Status: {response.status_code}"
        )
        
        logo_url = response.data.get('logo_url')
        
        self.test(
            "Org has logo_url in response",
            logo_url is not None,
            f"Response: {response.data}"
        )
        
        self.test(
            "Logo URL is a valid HTTP URL",
            logo_url and logo_url.startswith('http'),
            f"logo_url: {logo_url}"
        )
        
        print(f"  ℹ️  Logo URL: {logo_url[:80]}..." if logo_url and len(logo_url) > 80 else f"  ℹ️  Logo URL: {logo_url}")
    
    def test_delete_logo(self):
        """Test deleting logo via API."""
        # Store current storage key for verification
        old_storage_key = self.org.logo_storage_key
        
        # Create request
        request = self.factory.delete(f'/api/v1/orgs/{self.org.id}/logo/')
        force_authenticate(request, user=self.user)
        
        # Get view
        view = OrganizationViewSet.as_view({'delete': 'delete_logo'})
        response = view(request, pk=str(self.org.id))
        
        self.test(
            "Delete logo returns 200",
            response.status_code == 200,
            f"Status: {response.status_code}"
        )
        
        self.test(
            "Response indicates success",
            'message' in response.data and 'deleted' in response.data['message'].lower(),
            f"Response: {response.data}"
        )
        
        # Refresh org from DB
        self.org.refresh_from_db()
        
        self.test(
            "Org logo_storage_key is cleared",
            self.org.logo_storage_key is None,
            f"logo_storage_key: {self.org.logo_storage_key}"
        )
        
        self.test(
            "Org logo field is cleared",
            not self.org.logo,
            f"logo: {self.org.logo}"
        )
        
        # Verify the actual file was deleted (for local storage)
        if old_storage_key and old_storage_key.startswith('local://'):
            from django.conf import settings
            relative_path = old_storage_key[8:]
            full_path = os.path.join(settings.MEDIA_ROOT, relative_path)
            self.test(
                "Local file was actually deleted",
                not os.path.exists(full_path),
                f"File still exists: {full_path}"
            )
    
    def test_upload_after_delete(self):
        """Test uploading a new logo after deleting the old one."""
        from django.core.files.uploadedfile import InMemoryUploadedFile
        
        # Create new test image
        image_buffer = self.create_test_image(format='PNG', color='green')
        image_file = InMemoryUploadedFile(
            file=image_buffer,
            field_name='logo',
            name='new_logo.png',
            content_type='image/png',
            size=len(image_buffer.getvalue()),
            charset=None
        )
        
        # Create request
        request = self.factory.post(
            f'/api/v1/orgs/{self.org.id}/logo/',
            {'logo': image_file},
            format='multipart'
        )
        force_authenticate(request, user=self.user)
        
        # Get view
        view = OrganizationViewSet.as_view({'post': 'logo'})
        response = view(request, pk=str(self.org.id))
        
        self.test(
            "Re-upload returns 200",
            response.status_code == 200,
            f"Status: {response.status_code}"
        )
        
        # Refresh org from DB
        self.org.refresh_from_db()
        
        self.test(
            "New logo_storage_key is set",
            self.org.logo_storage_key is not None,
            f"logo_storage_key: {self.org.logo_storage_key}"
        )
        
        # Get URL
        url = self.org.get_logo_url()
        
        self.test(
            "get_logo_url returns valid URL",
            url and url.startswith('http'),
            f"URL: {url}"
        )


def main():
    """Run all integration tests."""
    test_suite = TestOrganizationLogoAPI()
    success = test_suite.run_all_tests()
    
    print("\n" + "="*60)
    if success:
        print("🎉 All integration tests passed!")
    else:
        print("❌ Some tests failed. See errors above.")
    print("="*60 + "\n")
    
    return 0 if success else 1


if __name__ == '__main__':
    sys.exit(main())

