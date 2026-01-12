"""
Resource Registry API views
"""

import logging
from rest_framework import viewsets, status
from rest_framework.decorators import api_view, permission_classes as drf_permission_classes
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.shortcuts import get_object_or_404

from ..models import BackendConnection, ResourceRegistryEntry, Organization
from ..serializers import ResourceRegistryEntrySerializer
from ..services.resource_discovery import ResourceDiscoveryService
from ..permissions import IsOrgEditorOrAbove, require_editor_or_above, require_admin

logger = logging.getLogger(__name__)


class ResourceRegistryEntryViewSet(viewsets.ModelViewSet):
    """
    ViewSet for ResourceRegistryEntry model.
    GET /api/v1/backends/:backend_id/resources - List resources (any member)
    GET /api/v1/resources/:id - Get resource (any member)
    PATCH /api/v1/resources/:id - Update resource (editor+)
    DELETE /api/v1/resources/:id - Delete resource (editor+)
    """

    permission_classes = [IsAuthenticated, IsOrgEditorOrAbove]
    serializer_class = ResourceRegistryEntrySerializer

    def get_queryset(self):
        """
        Filter resources to backend, with org access verification.
        """
        backend_id = self.kwargs.get("backend_connection_pk")

        if backend_id:
            # Verify user has access to the backend's organization
            try:
                backend = BackendConnection.objects.select_related("organization").get(id=backend_id)
                if not self.request.user.user_organizations.filter(
                    organization=backend.organization
                ).exists():
                    return ResourceRegistryEntry.objects.none()
            except BackendConnection.DoesNotExist:
                return ResourceRegistryEntry.objects.none()

            return ResourceRegistryEntry.objects.filter(backend_connection_id=backend_id)

        # For direct access (e.g., /resources/:id/), return all accessible resources
        user_org_ids = self.request.user.user_organizations.values_list("organization_id", flat=True)
        return ResourceRegistryEntry.objects.filter(backend_connection__organization_id__in=user_org_ids)


@api_view(["POST"])
@drf_permission_classes([IsAuthenticated])
def discover_resources(request, pk=None):
    """
    Discover resources for a backend (admin only).

    POST /api/v1/backends/:backend_id/discover/
    """
    backend = get_object_or_404(BackendConnection, pk=pk)

    # Verify user is admin (resource discovery is an admin operation)
    membership, error = require_admin(request, backend.organization)
    if error:
        return error

    try:
        discovery_service = ResourceDiscoveryService()
        entries = discovery_service.discover_resources(backend)

        return Response(
            {
                "success": True,
                "message": f"Discovered {len(entries)} resources",
                "count": len(entries),
            }
        )

    except Exception as e:
        logger.error(f"Error discovering resources: {e}")
        return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)
