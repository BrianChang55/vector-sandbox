"""
Version Service for managing AppVersion operations.

Provides helpers for getting stable versions, cancelling generations,
and version management utilities.
"""
import logging
from typing import Optional
from django.db import transaction

from ..models import AppVersion, AppVersionGenerationStatus, InternalApp

logger = logging.getLogger(__name__)


class VersionService:
    """
    Service for managing app versions.
    
    Provides utilities for:
    - Getting stable (complete) versions
    - Cancelling in-progress generations
    - Version number management
    """
    
    @staticmethod
    def get_latest_stable_version(app: InternalApp) -> Optional[AppVersion]:
        """
        Get the latest version that is stable (generation complete and active).
        
        This excludes versions that are:
        - Still generating (generation_status='generating')
        - Failed with errors (generation_status='error')
        - Not yet activated (is_active=False)
        
        Args:
            app: The internal app to get the version for
            
        Returns:
            The latest stable AppVersion, or None if no stable version exists
        """
        return AppVersion.objects.filter(
            internal_app=app,
            generation_status=AppVersionGenerationStatus.COMPLETE,
            is_active=True
        ).order_by('-version_number').first()
    
    @staticmethod
    def get_latest_version(app: InternalApp) -> Optional[AppVersion]:
        """
        Get the latest version regardless of generation status.
        
        This is used for version number calculations only.
        
        Args:
            app: The internal app to get the version for
            
        Returns:
            The latest AppVersion by version number, or None if no version exists
        """
        return AppVersion.objects.filter(
            internal_app=app
        ).order_by('-version_number').first()
    
    @staticmethod
    def get_next_version_number(app: InternalApp) -> int:
        """
        Get the next version number for an app.
        
        Args:
            app: The internal app
            
        Returns:
            The next version number (1 if no versions exist)
        """
        latest = VersionService.get_latest_version(app)
        return (latest.version_number + 1) if latest else 1
    
    @staticmethod
    def cancel_generating_version(version_id: str, user=None) -> dict:
        """
        Cancel an in-progress generation and clean up the version.
        
        If the version is in 'generating' status:
        - If it has files, mark it as 'error' with a cancellation message
        - If it has no files, delete the version entirely
        
        Args:
            version_id: The ID of the version to cancel
            user: The user requesting cancellation (for auth check)
            
        Returns:
            Dict with 'success', 'action' ('deleted', 'marked_error', 'already_complete'),
            and optionally 'error' message
        """
        try:
            version = AppVersion.objects.select_related('internal_app').get(pk=version_id)
        except AppVersion.DoesNotExist:
            return {
                'success': False,
                'error': 'Version not found',
                'action': None
            }
        
        # If already complete, nothing to do
        if version.generation_status == AppVersionGenerationStatus.COMPLETE:
            return {
                'success': True,
                'action': 'already_complete',
                'version_id': str(version.id)
            }
        
        # If generating or pending, cancel it
        if version.generation_status in [
            AppVersionGenerationStatus.GENERATING,
            AppVersionGenerationStatus.PENDING
        ]:
            with transaction.atomic():
                file_count = version.files.count()
                
                if file_count == 0:
                    # No files generated yet - safe to delete the version
                    version_number = version.version_number
                    version.delete()
                    logger.info(f"Deleted cancelled version {version_id} (v{version_number}) - no files")
                    return {
                        'success': True,
                        'action': 'deleted',
                        'version_number': version_number
                    }
                else:
                    # Has partial files - mark as error so user can see partial progress
                    version.generation_status = AppVersionGenerationStatus.ERROR
                    version.generation_error = 'Generation cancelled by user'
                    version.save(update_fields=['generation_status', 'generation_error', 'updated_at'])
                    logger.info(f"Marked version {version_id} as cancelled/error - had {file_count} files")
                    return {
                        'success': True,
                        'action': 'marked_error',
                        'version_id': str(version.id),
                        'file_count': file_count
                    }
        
        # Already in error state
        if version.generation_status == AppVersionGenerationStatus.ERROR:
            return {
                'success': True,
                'action': 'already_error',
                'version_id': str(version.id)
            }
        
        return {
            'success': False,
            'error': f'Unknown generation status: {version.generation_status}',
            'action': None
        }
    
    @staticmethod
    def cleanup_stale_generating_versions(app: InternalApp, max_age_minutes: int = 30) -> int:
        """
        Clean up versions that have been stuck in 'generating' status.
        
        This is a maintenance utility for handling orphaned generations
        (e.g., from server restarts or disconnected clients).
        
        Args:
            app: The internal app
            max_age_minutes: Consider versions stale if generating for longer than this
            
        Returns:
            Number of versions cleaned up
        """
        from django.utils import timezone
        from datetime import timedelta
        
        cutoff = timezone.now() - timedelta(minutes=max_age_minutes)
        
        stale_versions = AppVersion.objects.filter(
            internal_app=app,
            generation_status=AppVersionGenerationStatus.GENERATING,
            created_at__lt=cutoff
        )
        
        cleaned_count = 0
        for version in stale_versions:
            result = VersionService.cancel_generating_version(str(version.id))
            if result['success']:
                cleaned_count += 1
                logger.info(f"Cleaned up stale generating version: {version.id}")
        
        return cleaned_count

