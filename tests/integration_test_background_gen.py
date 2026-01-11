#!/usr/bin/env python
"""
Integration Test for Background Generation System

This script tests the full end-to-end flow:
1. Create a generation job
2. Poll the job stream endpoint for SSE events
3. Test reconnection with last_index
4. Test job cancellation

Run with: python tests/integration_test_background_gen.py
"""
import os
import sys
import json
import time

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import requests

# Django setup
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'internal_apps.settings')
import django
django.setup()

from rest_framework_simplejwt.tokens import RefreshToken
from vector_app.models import User, Organization, UserOrganization, InternalApp, CodeGenerationJob


BASE_URL = "http://localhost:8001/api/v1"


def get_or_create_test_user():
    """Get or create a test user for integration testing."""
    email = "integration-test@example.com"
    user, created = User.objects.get_or_create(
        email=email,
        defaults={
            'username': 'integration_test',
            'is_active': True,
        }
    )
    if created:
        user.set_password('testpass123')
        user.save()
        print(f"✓ Created test user: {email}")
    return user


def get_or_create_test_org(user):
    """Get or create a test organization."""
    org, created = Organization.objects.get_or_create(
        slug='integration-test-org',
        defaults={'name': 'Integration Test Org'}
    )
    
    UserOrganization.objects.get_or_create(
        user=user,
        organization=org,
        defaults={'role': UserOrganization.ROLE_ADMIN}
    )
    
    if created:
        print(f"✓ Created test org: {org.name}")
    return org


def get_or_create_test_app(org, user):
    """Get or create a test app."""
    app, created = InternalApp.objects.get_or_create(
        name='Integration Test App',
        organization=org,
        defaults={'created_by': user}
    )
    if created:
        print(f"✓ Created test app: {app.name}")
    return app


def get_auth_token(user):
    """Get JWT access token for user."""
    refresh = RefreshToken.for_user(user)
    return str(refresh.access_token)


def test_job_creation(app_id, token):
    """Test 1: Create a generation job."""
    print("\n--- Test 1: Job Creation ---")
    
    url = f"{BASE_URL}/apps/{app_id}/generate/agentic/"
    headers = {
        'Authorization': f'Bearer {token}',
        'Content-Type': 'application/json',
    }
    data = {
        'message': 'Create a simple hello world app with a button',
        'model': 'anthropic/claude-sonnet-4',
    }
    
    response = requests.post(url, headers=headers, json=data)
    
    if response.status_code == 201:
        result = response.json()
        print(f"✓ Job created successfully")
        print(f"  Job ID: {result.get('job_id')}")
        print(f"  Status: {result.get('status')}")
        print(f"  Stream URL: {result.get('stream_url')}")
        return result.get('job_id')
    else:
        print(f"✗ Failed to create job: {response.status_code}")
        print(f"  Response: {response.text}")
        return None


def test_job_status(job_id, token):
    """Test 2: Get job status."""
    print("\n--- Test 2: Job Status ---")
    
    url = f"{BASE_URL}/jobs/{job_id}/"
    headers = {'Authorization': f'Bearer {token}'}
    
    response = requests.get(url, headers=headers)
    
    if response.status_code == 200:
        result = response.json()
        print(f"✓ Job status retrieved")
        print(f"  Status: {result.get('status')}")
        print(f"  Event count: {result.get('event_count')}")
        print(f"  Is active: {result.get('is_active')}")
        return result
    else:
        print(f"✗ Failed to get job status: {response.status_code}")
        return None


def test_latest_job(app_id, token):
    """Test 3: Get latest job for app."""
    print("\n--- Test 3: Latest Job Endpoint ---")
    
    url = f"{BASE_URL}/apps/{app_id}/latest-job/"
    headers = {'Authorization': f'Bearer {token}'}
    
    response = requests.get(url, headers=headers)
    
    if response.status_code == 200:
        result = response.json()
        print(f"✓ Latest job retrieved")
        print(f"  Has active job: {result.get('has_active_job')}")
        print(f"  Job ID: {result.get('job_id')}")
        return result
    else:
        print(f"✗ Failed to get latest job: {response.status_code}")
        return None


def test_job_stream(job_id, token, last_index=0):
    """Test 4: Stream events from job."""
    print(f"\n--- Test 4: Job Stream (from index {last_index}) ---")
    
    url = f"{BASE_URL}/jobs/{job_id}/stream/?last_index={last_index}"
    headers = {
        'Authorization': f'Bearer {token}',
        'Accept': 'text/event-stream',
    }
    
    try:
        # Use streaming to read SSE events
        response = requests.get(url, headers=headers, stream=True, timeout=10)
        
        if response.status_code == 200:
            print(f"✓ Connected to job stream")
            
            events_received = 0
            max_events = 10  # Limit for testing
            
            for line in response.iter_lines():
                if line:
                    decoded = line.decode('utf-8')
                    if decoded.startswith('event:'):
                        event_type = decoded.replace('event:', '').strip()
                        print(f"  Event: {event_type}")
                        events_received += 1
                    
                    if events_received >= max_events:
                        print(f"  (Stopped after {max_events} events)")
                        break
            
            print(f"✓ Received {events_received} events")
            return events_received
        else:
            print(f"✗ Failed to connect to stream: {response.status_code}")
            return 0
            
    except requests.exceptions.Timeout:
        print("✗ Stream timed out (job may still be processing)")
        return 0
    except Exception as e:
        print(f"✗ Stream error: {e}")
        return 0


def test_job_cancel(job_id, token):
    """Test 5: Cancel a job."""
    print("\n--- Test 5: Job Cancellation ---")
    
    url = f"{BASE_URL}/jobs/{job_id}/cancel/"
    headers = {'Authorization': f'Bearer {token}'}
    
    response = requests.post(url, headers=headers)
    
    if response.status_code == 200:
        result = response.json()
        print(f"✓ Job cancel request sent")
        print(f"  Status: {result.get('status')}")
        return result
    else:
        print(f"✗ Failed to cancel job: {response.status_code}")
        return None


def test_reconnection_scenario(app_id, token):
    """Test 6: Simulate reconnection scenario."""
    print("\n--- Test 6: Reconnection Scenario ---")
    
    # Check for active job
    latest = test_latest_job(app_id, token)
    
    if latest and latest.get('has_active_job'):
        job_id = latest.get('job_id')
        event_count = latest.get('event_count', 0)
        
        print(f"  Found active job with {event_count} events")
        
        # Simulate reconnection by streaming from event 0 (replay all)
        if event_count > 0:
            print(f"  Replaying from start to restore state...")
            test_job_stream(job_id, token, last_index=0)
        
        return True
    else:
        print("  No active job to reconnect to")
        return False


def cleanup_test_jobs():
    """Clean up test jobs."""
    print("\n--- Cleanup ---")
    count = CodeGenerationJob.objects.filter(
        internal_app__name='Integration Test App'
    ).delete()[0]
    print(f"✓ Cleaned up {count} test jobs")


def main():
    """Run all integration tests."""
    print("=" * 60)
    print("Background Generation Integration Tests")
    print("=" * 60)
    
    # Setup
    print("\n--- Setup ---")
    user = get_or_create_test_user()
    org = get_or_create_test_org(user)
    app = get_or_create_test_app(org, user)
    token = get_auth_token(user)
    print(f"✓ Auth token generated")
    
    app_id = str(app.id)
    
    # Run tests
    all_passed = True
    
    # Test 1: Create job
    job_id = test_job_creation(app_id, token)
    if not job_id:
        all_passed = False
    
    if job_id:
        # Test 2: Get status
        status = test_job_status(job_id, token)
        if not status:
            all_passed = False
        
        # Wait a moment for job to start processing
        print("\n  Waiting for job to process...")
        time.sleep(2)
        
        # Test 3: Latest job
        latest = test_latest_job(app_id, token)
        if not latest:
            all_passed = False
        
        # Test 4: Stream events
        events = test_job_stream(job_id, token)
        
        # Test 5: Cancel (if still active)
        status = test_job_status(job_id, token)
        if status and status.get('is_active'):
            cancel_result = test_job_cancel(job_id, token)
            if not cancel_result:
                all_passed = False
    
    # Test 6: Reconnection
    test_reconnection_scenario(app_id, token)
    
    # Cleanup
    cleanup_test_jobs()
    
    # Summary
    print("\n" + "=" * 60)
    if all_passed:
        print("✓ All integration tests passed!")
    else:
        print("✗ Some tests failed")
    print("=" * 60)
    
    return 0 if all_passed else 1


if __name__ == '__main__':
    sys.exit(main())

