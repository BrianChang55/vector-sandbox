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
    
    # Get latest version
    latest_version = AppVersion.objects.filter(internal_app=app).order_by('-version_number').first()
    if not latest_version:
        return Response(
            {'error': 'No version to publish. Create a version first.'},
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
    
    # Get next version number
    next_version_number = latest_version.version_number + 1
    
    with transaction.atomic():
        # Create publish version
        publish_version = AppVersion.objects.create(
            internal_app=app,
            version_number=next_version_number,
            parent_version=latest_version,
            source=AppVersion.SOURCE_PUBLISH,
            spec_json=latest_version.spec_json,
            scope_snapshot_json=scope_snapshot,
            created_by=request.user,
        )
        
        # Copy all files from latest version
        from ..models import VersionFile
        for source_file in latest_version.files.all():
            VersionFile.objects.create(
                app_version=publish_version,
                path=source_file.path,
                content=source_file.content,
                content_hash=source_file.content_hash,
            )
        
        # Update app status to published
        app.status = InternalApp.STATUS_PUBLISHED
        app.save()
    
    from ..serializers import AppVersionSerializer
    return Response(
        AppVersionSerializer(publish_version).data,
        status=status.HTTP_201_CREATED
    )
