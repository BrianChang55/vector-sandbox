"""
Management command to backfill version snapshots for existing versions.

This command creates VersionStateSnapshot records for any AppVersion
that doesn't have one, enabling the version revert system to work
with historical versions.

Usage:
    python manage.py backfill_snapshots
    python manage.py backfill_snapshots --app-id <uuid>
    python manage.py backfill_snapshots --dry-run
    python manage.py backfill_snapshots --limit 100
"""
import logging
from django.core.management.base import BaseCommand, CommandError
from django.db import transaction

from vector_app.models import AppVersion, VersionStateSnapshot, InternalApp
from vector_app.services.snapshot_service import SnapshotService
from vector_app.models import AppVersionGenerationStatus
from vector_app.models import AppVersionGenerationStatus



logger = logging.getLogger(__name__)


class Command(BaseCommand):
    help = 'Backfill version snapshots for existing versions that lack them'

    def add_arguments(self, parser):
        parser.add_argument(
            '--app-id',
            type=str,
            help='Backfill only for a specific app (UUID)',
        )
        parser.add_argument(
            '--dry-run',
            action='store_true',
            help='Show what would be done without making changes',
        )
        parser.add_argument(
            '--limit',
            type=int,
            default=None,
            help='Limit the number of versions to backfill',
        )
        parser.add_argument(
            '--verbose',
            action='store_true',
            help='Show detailed progress for each version',
        )

    def handle(self, *args, **options):
        app_id = options.get('app_id')
        dry_run = options.get('dry_run', False)
        limit = options.get('limit')
        verbose = options.get('verbose', False)

        self.stdout.write(self.style.NOTICE('Starting version snapshot backfill...'))
        
        if dry_run:
            self.stdout.write(self.style.WARNING('DRY RUN - No changes will be made'))

        # Build queryset
        versions_qs = AppVersion.objects.filter(
            generation_status=AppVersionGenerationStatus.COMPLETE
        ).select_related('internal_app')

        if app_id:
            try:
                app = InternalApp.objects.get(pk=app_id)
                versions_qs = versions_qs.filter(internal_app=app)
                self.stdout.write(f'Filtering to app: {app.name} ({app.id})')
            except InternalApp.DoesNotExist:
                raise CommandError(f'App with ID {app_id} not found')

        # Get versions that don't have snapshots
        existing_snapshot_ids = VersionStateSnapshot.objects.values_list(
            'app_version_id', flat=True
        )
        versions_without_snapshots = versions_qs.exclude(
            id__in=existing_snapshot_ids
        ).order_by('created_at')

        total_count = versions_without_snapshots.count()
        self.stdout.write(f'Found {total_count} versions without snapshots')

        if limit:
            versions_without_snapshots = versions_without_snapshots[:limit]
            self.stdout.write(f'Limited to {limit} versions')

        # Process versions
        created_count = 0
        error_count = 0
        skipped_count = 0

        for version in versions_without_snapshots:
            try:
                if dry_run:
                    if verbose:
                        self.stdout.write(
                            f'  Would create snapshot for v{version.version_number} '
                            f'of {version.internal_app.name}'
                        )
                    created_count += 1
                else:
                    snapshot = SnapshotService.create_version_snapshot(version)
                    created_count += 1
                    
                    if verbose:
                        self.stdout.write(
                            f'  Created snapshot for v{version.version_number} '
                            f'of {version.internal_app.name}: '
                            f'{snapshot.total_tables} tables, {snapshot.file_count} files'
                        )

            except Exception as e:
                error_count += 1
                self.stdout.write(
                    self.style.ERROR(
                        f'  Error creating snapshot for v{version.version_number} '
                        f'of {version.internal_app.name}: {e}'
                    )
                )

        # Summary
        self.stdout.write('')
        self.stdout.write(self.style.SUCCESS('Backfill complete!'))
        self.stdout.write(f'  Snapshots created: {created_count}')
        self.stdout.write(f'  Errors: {error_count}')
        
        if dry_run:
            self.stdout.write(self.style.WARNING(
                '\nThis was a dry run. Run without --dry-run to apply changes.'
            ))

        # Also show stats
        total_with_snapshots = VersionStateSnapshot.objects.count()
        total_versions = AppVersion.objects.filter(
            generation_status=AppVersionGenerationStatus.COMPLETE
        ).count()
        
        self.stdout.write('')
        self.stdout.write(f'Overall status:')
        self.stdout.write(f'  Total complete versions: {total_versions}')
        self.stdout.write(f'  Versions with snapshots: {total_with_snapshots}')
        self.stdout.write(f'  Coverage: {(total_with_snapshots / max(total_versions, 1)) * 100:.1f}%')

