"""
Internal App API views
"""
import logging
from rest_framework import viewsets, status
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.shortcuts import get_object_or_404

from ..models import InternalApp, Organization
from ..serializers import InternalAppSerializer, InternalAppCreateSerializer

logger = logging.getLogger(__name__)


class InternalAppViewSet(viewsets.ModelViewSet):
    """
    ViewSet for InternalApp model.
    GET /api/v1/orgs/:org_id/apps - List apps
    POST /api/v1/orgs/:org_id/apps - Create app
    GET /api/v1/apps/:id - Get app
    PATCH /api/v1/apps/:id - Update app
    """
    permission_classes = [IsAuthenticated]
    serializer_class = InternalAppSerializer
    
    def get_queryset(self):
        """Filter apps to organization or return all for direct app access."""
        org_id = self.kwargs.get('organization_pk')
        queryset = InternalApp.objects.select_related('backend_connection', 'created_by')
        
        # When accessing via /orgs/:org_id/apps/, filter by organization
        # When accessing via /apps/:pk/, return all (lookup by pk will filter)
        if org_id:
            queryset = queryset.filter(organization_id=org_id)
        
        return queryset
    
    def get_serializer_class(self):
        """Use create serializer for POST."""
        if self.action == 'create':
            return InternalAppCreateSerializer
        return InternalAppSerializer
    
    def get_serializer_context(self):
        """Add organization to serializer context."""
        context = super().get_serializer_context()
        org_id = self.kwargs.get('organization_pk')
        if org_id:
            context['organization'] = get_object_or_404(Organization, id=org_id)
        return context
    
    def perform_create(self, serializer):
        """Verify user is member of organization."""
        org_id = self.kwargs.get('organization_pk')
        organization = get_object_or_404(Organization, id=org_id)
        
        # Verify user is member
        if not self.request.user.user_organizations.filter(organization=organization).exists():
            raise PermissionError('You are not a member of this organization')
        
        serializer.save()
    
    def create(self, request, *args, **kwargs):
        """Create app and return full serialized response."""
        serializer = self.get_serializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        self.perform_create(serializer)
        
        # Return the full app details using the read serializer
        instance = serializer.instance
        read_serializer = InternalAppSerializer(instance, context=self.get_serializer_context())
        
        headers = self.get_success_headers(read_serializer.data)
        return Response(read_serializer.data, status=status.HTTP_201_CREATED, headers=headers)

