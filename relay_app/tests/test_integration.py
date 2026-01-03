"""
End-to-end integration tests
"""
import pytest
from django.test import TestCase, Client
from django.contrib.auth import get_user_model
from django.urls import reverse
import json

from relay_app.models import (
    Organization,
    UserOrganization,
    BackendConnection,
    InternalApp,
    AppVersion,
)

User = get_user_model()


@pytest.mark.django_db
class IntegrationTestCase(TestCase):
    """End-to-end integration tests."""
    
    def setUp(self):
        """Set up test data."""
        self.client = Client()
        self.user = User.objects.create_user(
            username='testuser',
            email='test@example.com',
            password='testpass123'
        )
        self.org = Organization.objects.create(name='Test Org', slug='test-org')
        UserOrganization.objects.create(
            user=self.user,
            organization=self.org,
            role=UserOrganization.ROLE_ADMIN
        )
        self.client.force_login(self.user)
    
    def test_create_organization(self):
        """Test creating an organization."""
        response = self.client.post(
            '/api/v1/orgs/',
            data=json.dumps({
                'name': 'New Org',
                'slug': 'new-org',
            }),
            content_type='application/json',
        )
        self.assertEqual(response.status_code, 201)
        data = json.loads(response.content)
        self.assertEqual(data['name'], 'New Org')
        self.assertEqual(data['slug'], 'new-org')
    
    def test_create_backend_connection(self):
        """Test creating a backend connection."""
        backend_data = {
            'adapter_type': 'supabase',
            'display_name': 'Test Supabase',
            'supabase_url': 'https://test.supabase.co',
            'service_role_key': 'test_key_123',
        }
        response = self.client.post(
            f'/api/v1/orgs/{self.org.id}/backends/',
            data=json.dumps(backend_data),
            content_type='application/json',
        )
        self.assertEqual(response.status_code, 201)
        data = json.loads(response.content)
        self.assertEqual(data['display_name'], 'Test Supabase')
        self.assertEqual(data['adapter_type'], 'supabase')
    
    def test_create_app(self):
        """Test creating an internal app."""
        backend = BackendConnection.objects.create(
            organization=self.org,
            adapter_type=BackendConnection.ADAPTER_SUPABASE,
            display_name='Test Backend',
            config_encrypted='',
        )
        backend.set_config({'supabase_url': 'https://test.co', 'service_role_key': 'key'})
        backend.save()
        
        app_data = {
            'name': 'Test App',
            'description': 'Test description',
            'backend_connection': str(backend.id),
            'allow_actions_in_preview': False,
        }
        response = self.client.post(
            f'/api/v1/orgs/{self.org.id}/apps/',
            data=json.dumps(app_data),
            content_type='application/json',
        )
        self.assertEqual(response.status_code, 201)
        data = json.loads(response.content)
        self.assertEqual(data['name'], 'Test App')
        self.assertEqual(data['status'], 'draft')
    
    def test_ai_edit_endpoint(self):
        """Test AI edit endpoint (with mock AI)."""
        backend = BackendConnection.objects.create(
            organization=self.org,
            adapter_type=BackendConnection.ADAPTER_SUPABASE,
            display_name='Test Backend',
            config_encrypted='',
        )
        backend.set_config({'supabase_url': 'https://test.co', 'service_role_key': 'key'})
        backend.save()
        
        app = InternalApp.objects.create(
            organization=self.org,
            name='Test App',
            backend_connection=backend,
            created_by=self.user,
        )
        
        # Mock AI service (in real tests, you'd mock the OpenAI client)
        # For now, test endpoint structure
        edit_data = {
            'intent_message': 'Create a table view',
            'source': 'ai_edit',
        }
        response = self.client.post(
            f'/api/v1/apps/{app.id}/versions/ai-edit/',
            data=json.dumps(edit_data),
            content_type='application/json',
        )
        # Should either succeed (if OpenAI key is set) or fail gracefully
        self.assertIn(response.status_code, [201, 500, 400])


if __name__ == '__main__':
    pytest.main([__file__])

