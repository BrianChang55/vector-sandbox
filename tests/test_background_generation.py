"""
Comprehensive Tests for Background Generation System

Tests the job-based code generation with Celery workers:
- Job creation and queuing
- SSE streaming from database
- Reconnection support
- Cancellation
- Error handling
"""
import json
import time
from unittest.mock import patch, MagicMock
from django.test import TestCase, TransactionTestCase
from django.urls import reverse
from rest_framework.test import APIClient
from rest_framework import status
from rest_framework_simplejwt.tokens import RefreshToken

from accounts.models import Organization, User, UserOrganization
from accounts.types import UserOrganizationRole
from vector_app.models import (
    AppVersion,
    AppVersionGenerationStatus,
    AppVersionSource,
    ChatMessage,
    ChatSession,
    CodeGenerationJob,
    CodeGenerationJobStatus,
    InternalApp,
)


def get_auth_header(user):
    """Get JWT auth header for a user."""
    refresh = RefreshToken.for_user(user)
    return {'HTTP_AUTHORIZATION': f'Bearer {refresh.access_token}'}


class JobCreationTestCase(TestCase):
    """Test job creation via the agentic generate endpoint."""
    
    def setUp(self):
        """Set up test user, org, and app."""
        self.client = APIClient()
        
        # Create user (email is USERNAME_FIELD)
        self.user = User.objects.create_user(
            username='testuser',
            email='test@example.com',
            password='testpass123',
        )
        
        # Create organization
        self.org = Organization.objects.create(
            name='Test Org',
            slug='test-org',
        )
        
        # Link user to org
        UserOrganization.objects.create(
            user=self.user,
            organization=self.org,
            role=UserOrganizationRole.ADMIN,
        )
        
        # Create app
        self.app = InternalApp.objects.create(
            name='Test App',
            organization=self.org,
            created_by=self.user,
        )
        
        # Get auth header for JWT (needed for View-based endpoints)
        self.auth_header = get_auth_header(self.user)
    
    @patch('vector_app.tasks.run_agentic_generation.delay')
    def test_post_creates_job_and_queues_task(self, mock_delay):
        """POST /apps/{id}/generate/agentic/ creates a job and queues Celery task."""
        url = reverse('generate-agentic', args=[self.app.id])
        
        response = self.client.post(
            url,
            json.dumps({'message': 'Create a todo app', 'model': 'anthropic/claude-sonnet-4.5'}),
            content_type='application/json',
            **self.auth_header
        )
        
        self.assertEqual(response.status_code, status.HTTP_201_CREATED)
        data = response.json()
        self.assertIn('job_id', data)
        self.assertIn('stream_url', data)
        self.assertEqual(data['status'], 'queued')
        
        # Verify job was created
        job_id = data['job_id']
        job = CodeGenerationJob.objects.get(pk=job_id)
        self.assertEqual(job.status, CodeGenerationJobStatus.QUEUED)
        self.assertEqual(job.user_message, 'Create a todo app')
        self.assertEqual(job.internal_app, self.app)
        
        # Verify Celery task was queued
        mock_delay.assert_called_once_with(job_id)
    
    @patch('vector_app.tasks.run_agentic_generation.delay')
    def test_post_with_session_id(self, mock_delay):
        """POST with session_id links to existing session."""
        session = ChatSession.objects.create(
            internal_app=self.app,
            title='Test Session',
            created_by=self.user,
        )
        
        url = reverse('generate-agentic', args=[self.app.id])
        response = self.client.post(
            url,
            json.dumps({'message': 'Add a feature', 'session_id': str(session.id)}),
            content_type='application/json',
            **self.auth_header
        )
        
        self.assertEqual(response.status_code, status.HTTP_201_CREATED)
        
        data = response.json()
        job = CodeGenerationJob.objects.get(pk=data['job_id'])
        self.assertEqual(job.session, session)
    
    def test_post_without_message_returns_error(self):
        """POST without message returns 400."""
        url = reverse('generate-agentic', args=[self.app.id])
        response = self.client.post(
            url,
            json.dumps({}),
            content_type='application/json',
            **self.auth_header
        )
        
        self.assertEqual(response.status_code, status.HTTP_400_BAD_REQUEST)
    
    def test_post_unauthorized_returns_error(self):
        """POST without auth returns 401."""
        url = reverse('generate-agentic', args=[self.app.id])
        response = self.client.post(
            url,
            json.dumps({'message': 'Test'}),
            content_type='application/json',
        )
        
        self.assertEqual(response.status_code, status.HTTP_401_UNAUTHORIZED)


class JobStatusTestCase(TestCase):
    """Test job status endpoint."""
    
    def setUp(self):
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
            role=UserOrganizationRole.ADMIN,
        )
        
        self.app = InternalApp.objects.create(
            name='Test App',
            organization=self.org,
            created_by=self.user,
        )
        
        self.job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test message',
            status=CodeGenerationJobStatus.STREAMING,
            events_json=[
                {'type': 'agent_start', 'data': {}, 'timestamp': time.time()},
                {'type': 'phase_change', 'data': {'phase': 'planning'}, 'timestamp': time.time()},
            ],
            created_by=self.user,
        )
        
        self.client.force_authenticate(user=self.user)
    
    def test_get_job_status(self):
        """GET /jobs/{id}/ returns job status."""
        url = reverse('job-status', args=[self.job.id])
        response = self.client.get(url)
        
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(response.data['job_id'], str(self.job.id))
        self.assertEqual(response.data['status'], 'streaming')
        self.assertEqual(response.data['event_count'], 2)
        self.assertTrue(response.data['is_active'])
    
    def test_get_job_not_found(self):
        """GET /jobs/{invalid_id}/ returns 404."""
        url = reverse('job-status', args=['00000000-0000-0000-0000-000000000000'])
        response = self.client.get(url)
        
        self.assertEqual(response.status_code, status.HTTP_404_NOT_FOUND)
    
    def test_get_job_access_denied(self):
        """GET /jobs/{id}/ returns 403 for other org's job."""
        other_user = User.objects.create_user(
            username='otheruser',
            email='other@example.com',
            password='testpass123',
        )
        self.client.force_authenticate(user=other_user)
        
        url = reverse('job-status', args=[self.job.id])
        response = self.client.get(url)
        
        self.assertEqual(response.status_code, status.HTTP_403_FORBIDDEN)


class LatestJobTestCase(TestCase):
    """Test latest job endpoint."""
    
    def setUp(self):
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
            role=UserOrganizationRole.ADMIN,
        )
        
        self.app = InternalApp.objects.create(
            name='Test App',
            organization=self.org,
            created_by=self.user,
        )
        
        self.client.force_authenticate(user=self.user)
    
    def test_no_jobs_returns_empty(self):
        """GET /apps/{id}/latest-job/ returns no job when none exist."""
        url = reverse('latest-job', args=[self.app.id])
        response = self.client.get(url)
        
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertFalse(response.data['has_active_job'])
        self.assertIsNone(response.data['job_id'])
    
    def test_active_job_returned(self):
        """GET /apps/{id}/latest-job/ returns active job."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.STREAMING,
            created_by=self.user,
        )
        
        url = reverse('latest-job', args=[self.app.id])
        response = self.client.get(url)
        
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertTrue(response.data['has_active_job'])
        self.assertEqual(response.data['job_id'], str(job.id))
    
    def test_completed_job_returned_when_no_active(self):
        """GET returns completed job when no active jobs."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.COMPLETE,
            created_by=self.user,
        )
        
        url = reverse('latest-job', args=[self.app.id])
        response = self.client.get(url)
        
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertFalse(response.data['has_active_job'])
        self.assertEqual(response.data['job_id'], str(job.id))


class JobCancelTestCase(TestCase):
    """Test job cancellation."""
    
    def setUp(self):
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
            role=UserOrganizationRole.ADMIN,
        )
        
        self.app = InternalApp.objects.create(
            name='Test App',
            organization=self.org,
            created_by=self.user,
        )
        
        self.client.force_authenticate(user=self.user)
    
    def test_cancel_active_job(self):
        """POST /jobs/{id}/cancel/ cancels an active job."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.STREAMING,
            created_by=self.user,
        )
        
        url = reverse('job-cancel', args=[job.id])
        response = self.client.post(url)
        
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(response.data['status'], 'cancelled')
        
        job.refresh_from_db()
        self.assertEqual(job.status, CodeGenerationJobStatus.CANCELLED)
    
    def test_cancel_with_version(self):
        """Cancelling job also marks version as errored."""
        version = AppVersion.objects.create(
            internal_app=self.app,
            version_number=1,
            source=AppVersionSource.AI,
            spec_json={'generated': True},  # Required field
            generation_status=AppVersionGenerationStatus.GENERATING,
            created_by=self.user,
        )
        
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            version=version,
            user_message='Test',
            status=CodeGenerationJobStatus.STREAMING,
            created_by=self.user,
        )
        
        url = reverse('job-cancel', args=[job.id])
        response = self.client.post(url)
        
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        
        version.refresh_from_db()
        self.assertEqual(version.generation_status, AppVersionGenerationStatus.ERROR)
        self.assertEqual(version.generation_error, 'Cancelled by user')
    
    def test_cancel_completed_job_no_effect(self):
        """Cancelling completed job returns current status."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.COMPLETE,
            created_by=self.user,
        )
        
        url = reverse('job-cancel', args=[job.id])
        response = self.client.post(url)
        
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(response.data['status'], 'complete')
        self.assertIn('message', response.data)


class JobStreamTestCase(TestCase):
    """Test SSE streaming from job events."""
    
    def setUp(self):
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
            role=UserOrganizationRole.ADMIN,
        )
        
        self.app = InternalApp.objects.create(
            name='Test App',
            organization=self.org,
            created_by=self.user,
        )
        
        # Get auth header for JWT (needed for View-based endpoints)
        self.auth_header = get_auth_header(self.user)
    
    def test_stream_completed_job(self):
        """Streaming a completed job returns all events."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.COMPLETE,
            events_json=[
                {'type': 'agent_start', 'data': {'goal': 'Test'}, 'timestamp': time.time(), 'index': 0},
                {'type': 'plan_created', 'data': {'plan': {}}, 'timestamp': time.time(), 'index': 1},
                {'type': 'done', 'data': {'success': True}, 'timestamp': time.time(), 'index': 2},
            ],
            created_by=self.user,
        )
        
        url = reverse('job-stream', args=[job.id])
        response = self.client.get(url, **self.auth_header)
        
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(response['Content-Type'], 'text/event-stream')
        
        # Collect SSE events from response
        content = b''.join(response.streaming_content).decode('utf-8')
        
        self.assertIn('event: agent_start', content)
        self.assertIn('event: plan_created', content)
        self.assertIn('event: done', content)
    
    def test_stream_with_last_index(self):
        """Streaming with last_index only returns new events."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.COMPLETE,
            events_json=[
                {'type': 'agent_start', 'data': {}, 'timestamp': time.time(), 'index': 0},
                {'type': 'plan_created', 'data': {}, 'timestamp': time.time(), 'index': 1},
                {'type': 'done', 'data': {'success': True}, 'timestamp': time.time(), 'index': 2},
            ],
            created_by=self.user,
        )
        
        url = reverse('job-stream', args=[job.id]) + '?last_index=2'
        response = self.client.get(url, **self.auth_header)
        
        content = b''.join(response.streaming_content).decode('utf-8')
        
        # Should only have done event (index 2)
        self.assertNotIn('event: agent_start', content)
        self.assertNotIn('event: plan_created', content)
        self.assertIn('event: done', content)


class CeleryTaskTestCase(TransactionTestCase):
    """Test Celery task execution."""
    
    def setUp(self):
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
            role=UserOrganizationRole.ADMIN,
        )
        
        self.app = InternalApp.objects.create(
            name='Test App',
            organization=self.org,
            created_by=self.user,
        )
    
    @patch('vector_app.services.agentic_service.get_agentic_service')
    def test_task_runs_generation(self, mock_get_service):
        """Task runs agentic generation and stores events."""
        from vector_app.tasks import run_agentic_generation
        from vector_app.services.types import AgentEvent
        
        # Mock the agentic service to yield events
        mock_service = MagicMock()
        mock_service.generate_app.return_value = iter([
            AgentEvent(type='agent_start', data={'goal': 'Test'}),
            AgentEvent(type='plan_created', data={'plan': {'steps': []}}),
            AgentEvent(type='done', data={'success': True}),
        ])
        mock_get_service.return_value = mock_service
        
        # Create job
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Create a test app',
            status=CodeGenerationJobStatus.QUEUED,
            created_by=self.user,
        )
        
        # Run task synchronously
        run_agentic_generation(str(job.id))
        
        # Verify job is complete
        job.refresh_from_db()
        self.assertEqual(job.status, CodeGenerationJobStatus.COMPLETE)
        self.assertIsNotNone(job.completed_at)
        
        # Verify events were stored
        self.assertGreaterEqual(len(job.events_json), 3)
        event_types = [e['type'] for e in job.events_json]
        self.assertIn('agent_start', event_types)
        self.assertIn('plan_created', event_types)
        self.assertIn('done', event_types)
    
    @patch('vector_app.services.agentic_service.get_agentic_service')
    def test_task_handles_cancellation(self, mock_get_service):
        """Task checks for cancellation and stops."""
        from vector_app.tasks import run_agentic_generation
        from vector_app.services.types import AgentEvent
        
        call_count = 0
        
        def mock_generate(*args, **kwargs):
            nonlocal call_count
            for event in [
                AgentEvent(type='agent_start', data={}),
                AgentEvent(type='plan_created', data={'plan': {'steps': []}}),
            ]:
                call_count += 1
                # Cancel after first event
                if call_count == 1:
                    job = CodeGenerationJob.objects.get(pk=kwargs.get('app').generation_jobs.first().id)
                    job.status = CodeGenerationJobStatus.CANCELLED
                    job.save()
                yield event
        
        mock_service = MagicMock()
        mock_service.generate_app = mock_generate
        mock_get_service.return_value = mock_service
        
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.QUEUED,
            created_by=self.user,
        )
        
        run_agentic_generation(str(job.id))
        
        job.refresh_from_db()
        # Job should stay cancelled
        self.assertEqual(job.status, CodeGenerationJobStatus.CANCELLED)
    
    @patch('vector_app.services.agentic_service.get_agentic_service')
    def test_task_handles_error(self, mock_get_service):
        """Task handles errors and marks job as failed."""
        from vector_app.tasks import run_agentic_generation
        
        mock_service = MagicMock()
        mock_service.generate_app.side_effect = Exception('LLM API error')
        mock_get_service.return_value = mock_service
        
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.QUEUED,
            created_by=self.user,
        )
        
        run_agentic_generation(str(job.id))
        
        job.refresh_from_db()
        self.assertEqual(job.status, CodeGenerationJobStatus.FAILED)
        self.assertIsNotNone(job.error_message)
        self.assertIn('LLM API error', job.error_message)
        
        # Error event should be in events
        event_types = [e['type'] for e in job.events_json]
        self.assertIn('agent_error', event_types)


class EventAppendTestCase(TestCase):
    """Test event appending to job."""
    
    def setUp(self):
        self.user = User.objects.create_user(
            username='testuser',
            email='test@example.com',
            password='testpass123',
        )
        
        self.org = Organization.objects.create(
            name='Test Org',
            slug='test-org',
        )
        
        self.app = InternalApp.objects.create(
            name='Test App',
            organization=self.org,
            created_by=self.user,
        )
    
    def test_append_event(self):
        """append_event adds event with correct structure."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.STREAMING,
            created_by=self.user,
        )
        
        job.append_event('test_event', {'key': 'value'})
        
        self.assertEqual(len(job.events_json), 1)
        event = job.events_json[0]
        self.assertEqual(event['type'], 'test_event')
        self.assertEqual(event['data'], {'key': 'value'})
        self.assertIn('timestamp', event)
        self.assertEqual(event['index'], 0)
        self.assertEqual(job.chunk_count, 1)
    
    def test_append_multiple_events(self):
        """append_event maintains correct indices."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.STREAMING,
            created_by=self.user,
        )
        
        job.append_event('event_1', {})
        job.append_event('event_2', {})
        job.append_event('event_3', {})
        
        self.assertEqual(len(job.events_json), 3)
        self.assertEqual(job.events_json[0]['index'], 0)
        self.assertEqual(job.events_json[1]['index'], 1)
        self.assertEqual(job.events_json[2]['index'], 2)
        self.assertEqual(job.chunk_count, 3)

