"""
Celery Configuration for Internal Apps Backend

This module configures Celery for async task processing.
Used primarily for:
- Background processing tasks
- Long-running operations

Broker: Redis (local development) or Redis Cloud (production)
"""

import os
from celery import Celery
from django.conf import settings

# Set the default Django settings module
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'internal_apps.settings')

# Create Celery app
app = Celery('internal_apps')

# Configure Celery using Django settings with CELERY_ prefix
app.config_from_object('django.conf:settings', namespace='CELERY')

# Auto-discover tasks in all registered Django apps
app.autodiscover_tasks()


@app.task(bind=True, ignore_result=True)
def debug_task(self):
    """Debug task for testing Celery setup."""
    print(f'Request: {self.request!r}')

