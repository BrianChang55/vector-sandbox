"""
Publish API views
"""

import logging
from rest_framework.decorators import api_view, permission_classes as drf_permission_classes
from rest_framework.response import Response
from rest_framework import status
from rest_framework.permissions import IsAuthenticated
from django.shortcuts import get_object_or_404
from django.db import transaction

from ..models import InternalApp, AppVersion, ResourceRegistryEntry, Organization
from ..services.version_service import VersionService
from ..serializers import InternalAppSerializer, AppVersionSerializer
from ..permissions import require_admin

logger = logging.getLogger(__name__)


@api_view(["POST"])
@drf_permission_classes([IsAuthenticated])
def publish_app(request, pk=None):
    """
    Publish app by creating a version with scope snapshot (admin only).

    POST /api/v1/apps/:app_id/publish/
    """
    app = get_object_or_404(InternalApp, pk=pk)

    # Verify user is admin (publishing is an admin operation)
    membership, error = require_admin(request, app.organization)
    if error:
        return error

    # Get latest STABLE version (complete generation) to publish
    latest_stable = VersionService.get_latest_stable_version(app)
    if not latest_stable:
        return Response(
            {"error": "No stable version to publish. Create a complete version first."},
            status=status.HTTP_400_BAD_REQUEST,
        )

    # Check validation status - only allow publishing validated versions
    # Allow 'passed' or 'skipped' (for legacy versions without validation)
    if latest_stable.validation_status == AppVersion.VALIDATION_FAILED:
        return Response(
            {
                "error": "Cannot publish a version with validation errors. Please fix the errors first.",
                "validation_status": latest_stable.validation_status,
                "validation_errors": latest_stable.validation_errors_json or [],
            },
            status=status.HTTP_400_BAD_REQUEST,
        )

    # Log if publishing an unvalidated version
    if latest_stable.validation_status == AppVersion.VALIDATION_PENDING:
        logger.warning(
            f"Publishing version {latest_stable.id} with pending validation status. "
            f"Consider waiting for validation to complete."
        )

    # Create scope snapshot (registry state)
    registry_entries = ResourceRegistryEntry.objects.filter(
        backend_connection=app.backend_connection, enabled=True
    )

    scope_snapshot = []
    for entry in registry_entries:
        scope_snapshot.append(
            {
                "resource_id": entry.resource_id,
                "resource_name": entry.resource_name,
                "exposed_fields": entry.exposed_fields_json,
                "allowed_actions": entry.allowed_actions_json,
            }
        )

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

        # Ensure app has a slug (auto-generated in model.save() if not set)
        if not app.slug:
            app.slug = app.generate_slug()

        # Set this version as the active published version
        app.published_version = publish_version
        app.status = InternalApp.STATUS_PUBLISHED
        app.save()

    # Build published URL for response
    published_url = f"/{app.organization.slug}/{app.slug}"

    version_data = AppVersionSerializer(publish_version).data
    version_data["published_url"] = published_url

    return Response(version_data, status=status.HTTP_201_CREATED)


@api_view(["GET"])
@drf_permission_classes([IsAuthenticated])
def get_published_app(request, org_slug=None, app_slug=None):
    """
    Fetch the active published version of an app by org/app slug.

    GET /api/v1/published/:org_slug/:app_slug/

    Returns:
        - app: InternalApp data
        - version: The published AppVersion with files
        - files: List of version files for Sandpack rendering
    """
    # Get organization by slug
    org = get_object_or_404(Organization, slug=org_slug)

    # Verify user has access to this organization
    if not request.user.user_organizations.filter(organization=org).exists():
        return Response(
            {"error": "Access denied. You are not a member of this organization."},
            status=status.HTTP_403_FORBIDDEN,
        )

    # Get app by slug within the organization
    app = get_object_or_404(InternalApp, organization=org, slug=app_slug)

    # Check if app has a published version
    if not app.published_version:
        return Response({"error": "This app has not been published yet."}, status=status.HTTP_404_NOT_FOUND)

    # Get the published version with files
    published_version = app.published_version

    # Serialize the app
    app_data = InternalAppSerializer(app).data

    # Serialize the version (includes files)
    version_data = AppVersionSerializer(published_version).data

    # Get files for Sandpack rendering
    files = []
    for file in published_version.files.all():
        files.append(
            {
                "id": str(file.id),
                "path": file.path,
                "content": file.content,
                "content_hash": file.content_hash,
            }
        )

    return Response(
        {
            "app": app_data,
            "version": version_data,
            "files": files,
        }
    )
