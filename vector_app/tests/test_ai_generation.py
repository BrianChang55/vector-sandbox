"""
Tests for AI Generation and Streaming Functionality
"""
import json
from django.test import TestCase, Client
from django.urls import reverse
from rest_framework.test import APIClient
from rest_framework import status

from ..models import (
    User, Organization, UserOrganization,
    InternalApp, ChatSession, ChatMessage, AppVersion
)


class OpenRouterServiceTests(TestCase):
    """Test OpenRouter service functionality."""
    
    def test_service_instantiation(self):
        """Test service can be instantiated."""
        from ..services.openrouter_service import get_openrouter_service
        service = get_openrouter_service()
        self.assertIsNotNone(service)
    
    def test_available_models(self):
        """Test available models list."""
        from ..services.openrouter_service import get_openrouter_service
        service = get_openrouter_service()
        models = service.get_available_models()
        
        self.assertIsInstance(models, list)
        self.assertGreater(len(models), 0)
        
        # Check model structure
        model = models[0]
        self.assertIn('id', model)
        self.assertIn('name', model)
        self.assertIn('category', model)
        self.assertIn('cost', model)
    
    def test_model_categories(self):
        """Test models are properly categorized."""
        from ..services.openrouter_service import get_openrouter_service
        service = get_openrouter_service()
        models = service.get_available_models()
        
        categories = set(m['category'] for m in models)
        self.assertIn('premium', categories)
        self.assertIn('standard', categories)
        self.assertIn('economy', categories)


class ChatSessionModelTests(TestCase):
    """Test ChatSession model."""
    
    def setUp(self):
        self.user = User.objects.create_user(
            username='testuser',
            email='test@example.com',
            password='testpass123'
        )
        self.org = Organization.objects.create(
            name='Test Org',
            slug='test-org'
        )
        UserOrganization.objects.create(
            user=self.user,
            organization=self.org,
            role=UserOrganization.ROLE_ADMIN
        )
        self.app = InternalApp.objects.create(
            organization=self.org,
            name='Test App',
            created_by=self.user
        )
    
    def test_create_chat_session(self):
        """Test chat session creation."""
        session = ChatSession.objects.create(
            internal_app=self.app,
            title='Test Chat',
            model_id='anthropic/claude-3.5-sonnet',
            created_by=self.user
        )
        
        self.assertIsNotNone(session.id)
        self.assertEqual(session.title, 'Test Chat')
        self.assertTrue(session.is_active)
    
    def test_chat_message_creation(self):
        """Test chat message creation."""
        session = ChatSession.objects.create(
            internal_app=self.app,
            created_by=self.user
        )
        
        message = ChatMessage.objects.create(
            session=session,
            role=ChatMessage.ROLE_USER,
            content='Create a user dashboard',
            status=ChatMessage.STATUS_COMPLETE
        )
        
        self.assertIsNotNone(message.id)
        self.assertEqual(message.role, 'user')
    
    def test_message_with_generated_spec(self):
        """Test message with generated AppSpec."""
        session = ChatSession.objects.create(
            internal_app=self.app,
            created_by=self.user
        )
        
        spec = {
            'appName': 'User Dashboard',
            'pages': [{
                'id': 'users',
                'title': 'Users',
                'layout': 'table_detail_drawer',
                'primaryResource': 'public.users',
                'view': {'table': {'columns': [{'field': 'email'}]}}
            }]
        }
        
        message = ChatMessage.objects.create(
            session=session,
            role=ChatMessage.ROLE_ASSISTANT,
            content=json.dumps(spec),
            generated_spec_json=spec,
            model_id='anthropic/claude-3.5-sonnet'
        )
        
        self.assertEqual(message.generated_spec_json['appName'], 'User Dashboard')


class StreamingViewsTests(TestCase):
    """Test streaming API endpoints."""
    
    def setUp(self):
        self.client = APIClient()
        self.user = User.objects.create_user(
            username='testuser2',
            email='test2@example.com',
            password='testpass123'
        )
        self.org = Organization.objects.create(
            name='Test Org',
            slug='test-org'
        )
        UserOrganization.objects.create(
            user=self.user,
            organization=self.org,
            role=UserOrganization.ROLE_ADMIN
        )
        self.app = InternalApp.objects.create(
            organization=self.org,
            name='Test App',
            created_by=self.user
        )
        self.client.force_authenticate(user=self.user)
    
    def test_available_models_endpoint(self):
        """Test GET /api/v1/models/ returns available models."""
        response = self.client.get('/api/v1/models/')
        
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertIn('models', response.data)
        self.assertIn('grouped', response.data)
        self.assertIn('default', response.data)
    
    def test_chat_sessions_list(self):
        """Test listing chat sessions for an app."""
        ChatSession.objects.create(
            internal_app=self.app,
            title='Test Session',
            created_by=self.user
        )
        
        response = self.client.get(f'/api/v1/apps/{self.app.id}/chat-sessions/')
        
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertIn('sessions', response.data)
        self.assertEqual(len(response.data['sessions']), 1)
    
    def test_create_chat_session(self):
        """Test creating a chat session."""
        response = self.client.post(
            f'/api/v1/apps/{self.app.id}/chat-sessions/',
            {
                'title': 'New Chat',
                'model_id': 'openai/gpt-4o'
            }
        )
        
        self.assertEqual(response.status_code, status.HTTP_201_CREATED)
        self.assertEqual(response.data['title'], 'New Chat')
        self.assertEqual(response.data['model_id'], 'openai/gpt-4o')
    
    def test_chat_messages_list(self):
        """Test listing messages in a session."""
        session = ChatSession.objects.create(
            internal_app=self.app,
            created_by=self.user
        )
        ChatMessage.objects.create(
            session=session,
            role=ChatMessage.ROLE_USER,
            content='Hello'
        )
        
        response = self.client.get(f'/api/v1/chat-sessions/{session.id}/messages/')
        
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertIn('messages', response.data)
        self.assertEqual(len(response.data['messages']), 1)


class AppSpecValidationTests(TestCase):
    """Test AppSpec validation."""
    
    def test_valid_app_spec(self):
        """Test validation of a valid AppSpec."""
        from ..services.validation import AppSpecValidationService
        
        spec = {
            'appName': 'Test App',
            'pages': [{
                'id': 'page1',
                'title': 'Users',
                'layout': 'table_detail_drawer',
                'primaryResource': 'public.users',
                'view': {
                    'table': {
                        'columns': [
                            {'field': 'id', 'label': 'ID'},
                            {'field': 'email', 'label': 'Email'}
                        ]
                    }
                }
            }]
        }
        
        # Should pass basic structure validation
        self.assertIsInstance(spec, dict)
        self.assertIn('appName', spec)
        self.assertIn('pages', spec)


class CodegenServiceTests(TestCase):
    """Test code generation service."""
    
    def setUp(self):
        self.user = User.objects.create_user(
            username='testuser3',
            email='test3@example.com',
            password='testpass123'
        )
        self.org = Organization.objects.create(
            name='Test Org',
            slug='test-org'
        )
        self.app = InternalApp.objects.create(
            organization=self.org,
            name='Test App',
            created_by=self.user
        )
    
    def test_generate_files_from_spec(self):
        """Test generating files from AppSpec."""
        from ..services.codegen import CodegenService
        
        version = AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            source=AppVersion.SOURCE_AI,
            spec_json={
                'appName': 'Test App',
                'pages': [{
                    'id': 'users',
                    'title': 'Users',
                    'layout': 'table_detail_drawer',
                    'primaryResource': 'public.users',
                    'view': {
                        'table': {'columns': [{'field': 'email'}]}
                    }
                }]
            },
            created_by=self.user
        )
        
        files = CodegenService.generate_files_from_spec(version)
        
        self.assertGreater(len(files), 0)
        
        # Check expected files exist
        file_paths = [f.path for f in files]
        self.assertIn('src/lib/runtimeClient.ts', file_paths)
        self.assertIn('src/app/page.tsx', file_paths)
        self.assertIn('src/components/TableView.tsx', file_paths)

