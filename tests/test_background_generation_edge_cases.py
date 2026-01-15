"""
Edge Case Tests for Background Generation System

Covers:
- Race conditions
- Concurrent job handling  
- State transitions
- Large event payloads
- Malformed requests
- Authorization edge cases
- Streaming edge cases
- Database consistency
"""
import json
import time
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed
from unittest.mock import patch, MagicMock
from django.test import TestCase, TransactionTestCase
from django.urls import reverse
from django.db import connection
from rest_framework.test import APIClient
from rest_framework import status
from rest_framework_simplejwt.tokens import RefreshToken

from accounts.models import Organization, User, UserOrganization
from accounts.types import UserOrganizationRole
from vector_app.models import (
    AppVersion,
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


class BaseTestCase(TestCase):
    """Base test case with common setup."""
    
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
        
        self.auth_header = get_auth_header(self.user)
        self.client.force_authenticate(user=self.user)


class ConcurrentJobTestCase(TransactionTestCase):
    """Test concurrent job creation and handling."""
    
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
        
        self.auth_header = get_auth_header(self.user)
    
    @patch('vector_app.tasks.run_agentic_generation.delay')
    def test_concurrent_job_creation(self, mock_delay):
        """Multiple concurrent job creation requests should all succeed."""
        url = reverse('generate-agentic', args=[self.app.id])
        
        results = []
        errors = []
        
        def create_job(message):
            try:
                client = APIClient()
                response = client.post(
                    url,
                    json.dumps({'message': message}),
                    content_type='application/json',
                    **self.auth_header
                )
                results.append((message, response.status_code, response.json() if response.status_code == 201 else None))
            except Exception as e:
                errors.append((message, str(e)))
        
        # Create 5 concurrent jobs
        with ThreadPoolExecutor(max_workers=5) as executor:
            futures = [
                executor.submit(create_job, f"Test message {i}")
                for i in range(5)
            ]
            for future in as_completed(futures):
                pass
        
        # All should succeed
        self.assertEqual(len(errors), 0, f"Errors: {errors}")
        self.assertEqual(len(results), 5)
        
        for msg, status_code, data in results:
            self.assertEqual(status_code, 201, f"Failed for {msg}")
            self.assertIsNotNone(data.get('job_id'))
        
        # Verify 5 jobs were created
        job_count = CodeGenerationJob.objects.filter(internal_app=self.app).count()
        self.assertEqual(job_count, 5)


class JobStateTransitionTestCase(BaseTestCase):
    """Test job state transitions."""
    
    def test_job_status_transitions(self):
        """Job should transition through states correctly."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.QUEUED,
            created_by=self.user,
        )
        
        # Queued -> Processing
        job.status = CodeGenerationJobStatus.PROCESSING
        job.started_at = job.created_at
        job.save()
        job.refresh_from_db()
        self.assertEqual(job.status, CodeGenerationJobStatus.PROCESSING)
        
        # Processing -> Streaming
        job.status = CodeGenerationJobStatus.STREAMING
        job.save()
        job.refresh_from_db()
        self.assertEqual(job.status, CodeGenerationJobStatus.STREAMING)
        
        # Streaming -> Complete
        job.status = CodeGenerationJobStatus.COMPLETE
        job.completed_at = job.created_at
        job.save()
        job.refresh_from_db()
        self.assertEqual(job.status, CodeGenerationJobStatus.COMPLETE)
    
    def test_cancelled_job_cannot_complete(self):
        """Cancelled job should stay cancelled."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.CANCELLED,
            created_by=self.user,
        )
        
        # Cancel endpoint should not change status
        url = reverse('job-cancel', args=[job.id])
        response = self.client.post(url)
        
        self.assertEqual(response.status_code, 200)
        job.refresh_from_db()
        self.assertEqual(job.status, CodeGenerationJobStatus.CANCELLED)
    
    def test_failed_job_cannot_be_reprocessed(self):
        """Failed job should stay failed."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.FAILED,
            error_message='Previous error',
            created_by=self.user,
        )
        
        # Cancel should not affect failed job
        url = reverse('job-cancel', args=[job.id])
        response = self.client.post(url)
        
        self.assertEqual(response.status_code, 200)
        job.refresh_from_db()
        self.assertEqual(job.status, CodeGenerationJobStatus.FAILED)


class LargeEventPayloadTestCase(BaseTestCase):
    """Test handling of large event payloads."""
    
    def test_large_event_data(self):
        """Large event data should be stored correctly."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.STREAMING,
            created_by=self.user,
        )
        
        # Create a large payload (simulate large file content)
        large_content = 'x' * 100000  # 100KB of content
        
        job.append_event('file_generated', {
            'path': 'src/large_file.tsx',
            'content': large_content,
            'action': 'create',
        })
        
        job.refresh_from_db()
        
        self.assertEqual(len(job.events_json), 1)
        self.assertEqual(job.events_json[0]['data']['content'], large_content)
    
    def test_many_events(self):
        """Job should handle many events correctly."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.STREAMING,
            created_by=self.user,
        )
        
        # Add 500 events
        for i in range(500):
            job.append_event(f'event_{i}', {'index': i, 'data': f'payload_{i}'})
        
        job.refresh_from_db()
        
        self.assertEqual(len(job.events_json), 500)
        self.assertEqual(job.chunk_count, 500)
        
        # Verify event ordering
        for i, event in enumerate(job.events_json):
            self.assertEqual(event['index'], i)
            self.assertEqual(event['data']['index'], i)


class StreamingEdgeCasesTestCase(BaseTestCase):
    """Test SSE streaming edge cases."""
    
    def test_stream_empty_job(self):
        """Streaming an empty job should return done event."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.COMPLETE,
            events_json=[],
            created_by=self.user,
        )
        
        url = reverse('job-stream', args=[job.id])
        response = self.client.get(url, **self.auth_header)
        
        self.assertEqual(response.status_code, 200)
        content = b''.join(response.streaming_content).decode('utf-8')
        
        # Should have done event
        self.assertIn('event: done', content)
    
    def test_stream_with_high_last_index(self):
        """Streaming with last_index higher than event count should return done."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.COMPLETE,
            events_json=[
                {'type': 'event1', 'data': {}, 'timestamp': time.time(), 'index': 0},
            ],
            created_by=self.user,
        )
        
        url = reverse('job-stream', args=[job.id]) + '?last_index=100'
        response = self.client.get(url, **self.auth_header)
        
        self.assertEqual(response.status_code, 200)
        content = b''.join(response.streaming_content).decode('utf-8')
        
        # Should just have done event
        self.assertIn('event: done', content)
    
    def test_stream_cancelled_job(self):
        """Streaming a cancelled job should return events then done."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.CANCELLED,
            events_json=[
                {'type': 'agent_start', 'data': {}, 'timestamp': time.time(), 'index': 0},
            ],
            created_by=self.user,
        )
        
        url = reverse('job-stream', args=[job.id])
        response = self.client.get(url, **self.auth_header)
        
        self.assertEqual(response.status_code, 200)
        content = b''.join(response.streaming_content).decode('utf-8')
        
        self.assertIn('event: agent_start', content)
        self.assertIn('event: done', content)
    
    def test_stream_failed_job(self):
        """Streaming a failed job should return events then done."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.FAILED,
            error_message='Generation failed',
            events_json=[
                {'type': 'agent_start', 'data': {}, 'timestamp': time.time(), 'index': 0},
                {'type': 'agent_error', 'data': {'message': 'Failed'}, 'timestamp': time.time(), 'index': 1},
            ],
            created_by=self.user,
        )
        
        url = reverse('job-stream', args=[job.id])
        response = self.client.get(url, **self.auth_header)
        
        self.assertEqual(response.status_code, 200)
        content = b''.join(response.streaming_content).decode('utf-8')
        
        self.assertIn('event: agent_error', content)
        self.assertIn('event: done', content)


class MalformedRequestTestCase(BaseTestCase):
    """Test handling of malformed requests."""
    
    def test_create_job_with_invalid_json(self):
        """Invalid JSON should return 400."""
        url = reverse('generate-agentic', args=[self.app.id])
        response = self.client.post(
            url,
            'not valid json{',
            content_type='application/json',
            **self.auth_header
        )
        
        self.assertEqual(response.status_code, 400)
    
    def test_create_job_with_empty_message(self):
        """Empty message should return 400."""
        url = reverse('generate-agentic', args=[self.app.id])
        response = self.client.post(
            url,
            json.dumps({'message': ''}),
            content_type='application/json',
            **self.auth_header
        )
        
        self.assertEqual(response.status_code, 400)
    
    @patch('vector_app.tasks.run_agentic_generation.delay')
    def test_create_job_with_whitespace_message(self, mock_delay):
        """Whitespace-only message is accepted (server trims later)."""
        url = reverse('generate-agentic', args=[self.app.id])
        response = self.client.post(
            url,
            json.dumps({'message': '   \n\t  '}),
            content_type='application/json',
            **self.auth_header
        )
        
        # Whitespace is accepted - the agentic service handles validation
        self.assertEqual(response.status_code, 201)
    
    def test_invalid_job_id_format(self):
        """Invalid UUID format should return 404."""
        url = '/api/v1/jobs/not-a-uuid/'
        response = self.client.get(url, **self.auth_header)
        
        self.assertEqual(response.status_code, 404)
    
    def test_invalid_app_id_format(self):
        """Invalid app UUID should return 404."""
        url = '/api/v1/apps/not-a-uuid/generate/agentic/'
        response = self.client.post(
            url,
            json.dumps({'message': 'test'}),
            content_type='application/json',
            **self.auth_header
        )
        
        self.assertEqual(response.status_code, 404)


class AuthorizationEdgeCasesTestCase(BaseTestCase):
    """Test authorization edge cases."""
    
    def test_job_access_after_org_removal(self):
        """User removed from org should not access job."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.COMPLETE,
            created_by=self.user,
        )
        
        # Remove user from org
        UserOrganization.objects.filter(user=self.user, organization=self.org).delete()
        
        url = reverse('job-status', args=[job.id])
        response = self.client.get(url)
        
        self.assertEqual(response.status_code, 403)
    
    def test_cross_org_job_access(self):
        """User should not access jobs from other orgs."""
        other_org = Organization.objects.create(
            name='Other Org',
            slug='other-org',
        )
        other_user = User.objects.create_user(
            username='otheruser',
            email='other@example.com',
            password='testpass123',
        )
        UserOrganization.objects.create(
            user=other_user,
            organization=other_org,
            role=UserOrganizationRole.ADMIN,
        )
        other_app = InternalApp.objects.create(
            name='Other App',
            organization=other_org,
            created_by=other_user,
        )
        
        other_job = CodeGenerationJob.objects.create(
            internal_app=other_app,
            user_message='Test',
            status=CodeGenerationJobStatus.COMPLETE,
            created_by=other_user,
        )
        
        # Try to access other org's job
        url = reverse('job-status', args=[other_job.id])
        response = self.client.get(url)
        
        self.assertEqual(response.status_code, 403)
    
    def test_cancel_other_orgs_job(self):
        """User should not cancel jobs from other orgs."""
        other_org = Organization.objects.create(
            name='Other Org',
            slug='other-org-2',
        )
        other_user = User.objects.create_user(
            username='otheruser2',
            email='other2@example.com',
            password='testpass123',
        )
        other_app = InternalApp.objects.create(
            name='Other App',
            organization=other_org,
            created_by=other_user,
        )
        
        other_job = CodeGenerationJob.objects.create(
            internal_app=other_app,
            user_message='Test',
            status=CodeGenerationJobStatus.STREAMING,
            created_by=other_user,
        )
        
        url = reverse('job-cancel', args=[other_job.id])
        response = self.client.post(url)
        
        self.assertEqual(response.status_code, 403)
        
        # Job should not be cancelled
        other_job.refresh_from_db()
        self.assertEqual(other_job.status, CodeGenerationJobStatus.STREAMING)


class EventConsistencyTestCase(BaseTestCase):
    """Test event storage consistency."""
    
    def test_event_indices_are_sequential(self):
        """Event indices should always be sequential."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.STREAMING,
            created_by=self.user,
        )
        
        # Add events with gaps in timing
        for i in range(100):
            job.append_event(f'event_{i}', {'value': i})
            if i % 10 == 0:
                job.refresh_from_db()  # Force reload
        
        job.refresh_from_db()
        
        # Verify all indices are sequential
        for i, event in enumerate(job.events_json):
            self.assertEqual(event['index'], i, f"Index mismatch at position {i}")
    
    def test_chunk_count_matches_events(self):
        """chunk_count should always match number of events."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Test',
            status=CodeGenerationJobStatus.STREAMING,
            created_by=self.user,
        )
        
        for i in range(50):
            job.append_event(f'event_{i}', {})
        
        job.refresh_from_db()
        
        self.assertEqual(job.chunk_count, 50)
        self.assertEqual(len(job.events_json), 50)


class LatestJobEdgeCasesTestCase(BaseTestCase):
    """Test latest job endpoint edge cases."""
    
    def test_multiple_completed_jobs_returns_latest(self):
        """Should return the most recently created job."""
        job1 = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='First job',
            status=CodeGenerationJobStatus.COMPLETE,
            created_by=self.user,
        )
        
        # Slight delay to ensure different timestamps
        import time
        time.sleep(0.01)
        
        job2 = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Second job',
            status=CodeGenerationJobStatus.COMPLETE,
            created_by=self.user,
        )
        
        url = reverse('latest-job', args=[self.app.id])
        response = self.client.get(url)
        
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response.data['job_id'], str(job2.id))
    
    def test_active_job_prioritized_over_completed(self):
        """Active job should be returned even if completed job is newer."""
        completed_job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Completed',
            status=CodeGenerationJobStatus.COMPLETE,
            created_by=self.user,
        )
        
        import time
        time.sleep(0.01)
        
        active_job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Active',
            status=CodeGenerationJobStatus.STREAMING,
            created_by=self.user,
        )
        
        url = reverse('latest-job', args=[self.app.id])
        response = self.client.get(url)
        
        self.assertEqual(response.status_code, 200)
        self.assertTrue(response.data['has_active_job'])
        self.assertEqual(response.data['job_id'], str(active_job.id))
    
    def test_queued_job_is_considered_active(self):
        """Queued job should be considered active."""
        job = CodeGenerationJob.objects.create(
            internal_app=self.app,
            user_message='Queued',
            status=CodeGenerationJobStatus.QUEUED,
            created_by=self.user,
        )
        
        url = reverse('latest-job', args=[self.app.id])
        response = self.client.get(url)
        
        self.assertEqual(response.status_code, 200)
        self.assertTrue(response.data['has_active_job'])


class CeleryTaskEdgeCasesTestCase(TransactionTestCase):
    """Test Celery task edge cases."""
    
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
    
    def test_task_with_missing_job(self):
        """Task should handle missing job gracefully."""
        from vector_app.tasks import run_agentic_generation
        
        # Try to run task with non-existent job
        result = run_agentic_generation('00000000-0000-0000-0000-000000000000')
        
        # Should return error result, not raise
        self.assertIsNone(result)
    
    @patch('vector_app.services.agentic_service.get_agentic_service')
    def test_task_mid_generation_cancellation(self, mock_get_service):
        """Task should stop immediately when cancelled mid-generation."""
        from vector_app.tasks import run_agentic_generation
        from vector_app.services.types import AgentEvent
        
        events_yielded = []
        
        def mock_generate(*args, **kwargs):
            for i in range(10):
                events_yielded.append(i)
                yield AgentEvent(type=f'event_{i}', data={'index': i})
                
                # Cancel after 3rd event
                if i == 2:
                    job = CodeGenerationJob.objects.get(internal_app=self.app)
                    job.status = CodeGenerationJobStatus.CANCELLED
                    job.save()
        
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
        
        # Should have stopped after cancellation check
        # (may process a few more events before checking)
        self.assertLessEqual(len(events_yielded), 5)
        
        job.refresh_from_db()
        self.assertEqual(job.status, CodeGenerationJobStatus.CANCELLED)
    
    @patch('vector_app.services.agentic_service.get_agentic_service')
    def test_task_stores_version_from_event(self, mock_get_service):
        """Task should create version and link it to job."""
        from vector_app.tasks import run_agentic_generation
        from vector_app.services.types import AgentEvent
        
        def mock_generate(*args, **kwargs):
            yield AgentEvent(type='agent_start', data={'goal': 'Test'})
            yield AgentEvent(type='done', data={'success': True})
        
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
        
        # Job should complete successfully
        self.assertEqual(job.status, CodeGenerationJobStatus.COMPLETE)
        # Version should be created and linked
        self.assertIsNotNone(job.version_id)


class SessionHandlingTestCase(BaseTestCase):
    """Test chat session handling in job creation."""
    
    @patch('vector_app.tasks.run_agentic_generation.delay')
    def test_valid_session_id_accepted(self, mock_delay):
        """Valid session_id should be used for the job."""
        session = ChatSession.objects.create(
            internal_app=self.app,
            title='Test Session',
            created_by=self.user,
        )
        
        url = reverse('generate-agentic', args=[self.app.id])
        
        response = self.client.post(
            url,
            json.dumps({
                'message': 'Test',
                'session_id': str(session.id),
            }),
            content_type='application/json',
            **self.auth_header
        )
        
        self.assertEqual(response.status_code, 201)
        
        job = CodeGenerationJob.objects.get(pk=response.json()['job_id'])
        self.assertEqual(job.session_id, session.id)
    
    @patch('vector_app.tasks.run_agentic_generation.delay')
    def test_job_without_session_succeeds(self, mock_delay):
        """Job without session_id should still be created."""
        url = reverse('generate-agentic', args=[self.app.id])
        
        response = self.client.post(
            url,
            json.dumps({
                'message': 'Test',
            }),
            content_type='application/json',
            **self.auth_header
        )
        
        self.assertEqual(response.status_code, 201)
        
        # Job is created (session may be created later by the task)
        job = CodeGenerationJob.objects.get(pk=response.json()['job_id'])
        self.assertEqual(job.user_message, 'Test')

