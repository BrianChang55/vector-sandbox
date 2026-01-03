"""
Resource Registry API views
"""
import logging
from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.shortcuts import get_object_or_404

from ..models import BackendConnection, ResourceRegistryEntry, Organization
from ..serializers import ResourceRegistryEntrySerializer
from ..services.resource_discovery import ResourceDiscoveryService

logger = logging.getLogger(__name__)


class ResourceRegistryEntryViewSet(viewsets.ModelViewSet):
    """
    ViewSet for ResourceRegistryEntry model.
    GET /api/v1/backends/:backend_id/resources - List resources
    PATCH /api/v1/resources/:id - Update resource (enable/disable, expose fields, allowlist actions)
    """
    permission_classes = [IsAuthenticated]
    serializer_class = ResourceRegistryEntrySerializer
    
    def get_queryset(self):
        """Filter resources to backend."""
        backend_id = self.kwargs.get('backend_connection_pk')
        return ResourceRegistryEntry.objects.filter(backend_connection_id=backend_id)


"""
Resource Registry API views
"""
import logging
from rest_framework import viewsets, status
from rest_framework.decorators import api_view, permission_classes
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.shortcuts import get_object_or_404

from ..models import BackendConnection, ResourceRegistryEntry, Organization
from ..serializers import ResourceRegistryEntrySerializer
from ..services.resource_discovery import ResourceDiscoveryService

logger = logging.getLogger(__name__)


class ResourceRegistryEntryViewSet(viewsets.ModelViewSet):
    """
    ViewSet for ResourceRegistryEntry model.
    GET /api/v1/backends/:backend_id/resources - List resources
    PATCH /api/v1/resources/:id - Update resource (enable/disable, expose fields, allowlist actions)
    """
    permission_classes = [IsAuthenticated]
    serializer_class = ResourceRegistryEntrySerializer
    
    def get_queryset(self):
        """Filter resources to backend."""
        backend_id = self.kwargs.get('backend_connection_pk')
        return ResourceRegistryEntry.objects.filter(backend_connection_id=backend_id)


@api_view(['POST'])
@permission_classes([IsAuthenticated])
def discover_resources(request, pk=None):
    """Discover resources for a backend (POST /api/v1/backends/:backend_id/discover)."""
    backend = get_object_or_404(BackendConnection, pk=pk)
    
    # Verify user has access
    if not request.user.user_organizations.filter(organization=backend.organization).exists():
        return Response(
            {'error': 'Access denied'},
            status=status.HTTP_403_FORBIDDEN
        )
    
    try:
        # TODO: For V1, run synchronously. In production, use Celery task
        discovery_service = ResourceDiscoveryService()
        entries = discovery_service.discover_resources(backend)
        
        return Response({
            'success': True,
            'message': f'Discovered {len(entries)} resources',
            'count': len(entries),
        })
        
    except Exception as e:
        logger.error(f"Error discovering resources: {e}")
        return Response(
            {'error': str(e)},
            status=status.HTTP_500_INTERNAL_SERVER_ERROR
        )
        """Trigger resource discovery for a backend."""
        backend = get_object_or_404(BackendConnection, pk=pk)
        
        # Verify user has access
        if not request.user.user_organizations.filter(organization=backend.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        try:
            # TODO: For V1, run synchronously. In production, use Celery task
            discovery_service = ResourceDiscoveryService()
            entries = discovery_service.discover_resources(backend)
            
            return Response({
                'success': True,
                'message': f'Discovered {len(entries)} resources',
                'count': len(entries),
            })
            
        except Exception as e:
            logger.error(f"Error discovering resources: {e}")
            return Response(
                {'error': str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )

