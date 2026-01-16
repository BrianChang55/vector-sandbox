from django.core.management.base import BaseCommand
from django.utils import autoreload


def run_celery():
    from internal_apps.celery import app as celery_app

    celery_app.worker_main([
        '-A', 'internal_apps',
        'worker',
        '--loglevel=info',
        '--pool=solo',
    ])


class Command(BaseCommand):
    help = 'Run Celery worker with auto-reload for development'

    def handle(self, *args, **options):
        self.stdout.write('Starting Celery worker with autoreload...')
        autoreload.run_with_reloader(run_celery)
