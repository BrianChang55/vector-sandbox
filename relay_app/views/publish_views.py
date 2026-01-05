"""
Publish API views
"""
import logging
from rest_framework.decorators import api_view, permission_classes
from rest_framework.response import Response
from rest_framework import status
from rest_framework.permissions import IsAuthenticated
from django.shortcuts import get_object_or_404
from django.db import transaction

from ..models import InternalApp, AppVersion, ResourceRegistryEntry
from ..services.version_service import VersionService

logger = logging.getLogger(__name__)


@api_view(['POST'])
@permission_classes([IsAuthenticated])
def publish_app(request, pk=None):
    """Publish app by creating a version with scope snapshot (POST /api/v1/apps/:app_id/publish)."""
    app = get_object_or_404(InternalApp, pk=pk)
    
    # Verify access
    if not request.user.user_organizations.filter(organization=app.organization).exists():
        return Response(
            {'error': 'Access denied'},
            status=status.HTTP_403_FORBIDDEN
        )
    
    # Get latest STABLE version (complete generation) to publish
    latest_stable = VersionService.get_latest_stable_version(app)
    if not latest_stable:
        return Response(
            {'error': 'No stable version to publish. Create a complete version first.'},
            status=status.HTTP_400_BAD_REQUEST
        )
    
    # Create scope snapshot (registry state)
    registry_entries = ResourceRegistryEntry.objects.filter(
        backend_connection=app.backend_connection,
        enabled=True
    )
    
    scope_snapshot = []
    for entry in registry_entries:
        scope_snapshot.append({
            'resource_id': entry.resource_id,
            'resource_name': entry.resource_name,
            'exposed_fields': entry.exposed_fields_json,
            'allowed_actions': entry.allowed_actions_json,
        })
    
    # Get next version number using version service
    next_version_number = VersionService.get_next_version_number(app)
    
    with transaction.atomic():
        # Create publish version from the stable version
        publish_version = AppVersion.objects.create(
            internal_app=app,
            version_number=next_version_number,
            parent_version=latest_stable,
            source=AppVersion.SOURCE_PUBLISH,
            intent_message=latest_stable.intent_message,
            spec_json=latest_stable.spec_json,
            scope_snapshot_json=scope_snapshot,
            created_by=request.user,
            is_active=False,  # Start inactive until files are copied
        )
        
        # Copy all files from stable version
        from ..models import VersionFile
        for source_file in latest_stable.files.all():
            VersionFile.objects.create(
                app_version=publish_version,
                path=source_file.path,
                content=source_file.content,
                content_hash=source_file.content_hash or VersionFile.compute_hash(source_file.content),
            )
        
        # Update app status to published
        app.status = InternalApp.STATUS_PUBLISHED
        app.save()
        
        # Mark as active after successful file copy
        publish_version.is_active = True
        publish_version.save(update_fields=['is_active', 'updated_at'])
    
    from ..serializers import AppVersionSerializer
    return Response(
        AppVersionSerializer(publish_version).data,
        status=status.HTTP_201_CREATED
    )
