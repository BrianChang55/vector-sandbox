"""
Organization API views
"""
import logging
from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.shortcuts import get_object_or_404

from ..models import Organization, UserOrganization
from ..serializers import OrganizationSerializer, OrganizationCreateSerializer, UserOrganizationSerializer

logger = logging.getLogger(__name__)


class OrganizationViewSet(viewsets.ModelViewSet):
    """
    ViewSet for Organization model.
    GET /api/v1/orgs - List organizations
    POST /api/v1/orgs - Create organization
    GET /api/v1/orgs/:id - Get organization
    POST /api/v1/orgs/:id/switch - Switch current organization
    """
    permission_classes = [IsAuthenticated]
    serializer_class = OrganizationSerializer
    
    def get_queryset(self):
        """Filter organizations to those the user belongs to."""
        user = self.request.user
        user_orgs = UserOrganization.objects.filter(user=user).select_related('organization')
        org_ids = [uo.organization_id for uo in user_orgs]
        return Organization.objects.filter(id__in=org_ids)
    
    def get_serializer_class(self):
        """Use create serializer for POST."""
        if self.action == 'create':
            return OrganizationCreateSerializer
        return OrganizationSerializer
    
    def perform_create(self, serializer):
        """Set organization in context."""
        serializer.save()
    
    @action(detail=True, methods=['post'])
    def switch(self, request, pk=None):
        """Switch current organization (store in session)."""
        org = self.get_object()
        
        # Verify user is member
        if not UserOrganization.objects.filter(user=request.user, organization=org).exists():
            return Response(
                {'error': 'You are not a member of this organization'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Store in session
        request.session['current_organization_id'] = str(org.id)
        request.session.save()
        
        return Response({
            'organization': OrganizationSerializer(org).data,
            'message': 'Organization switched',
        })


class UserOrganizationViewSet(viewsets.ReadOnlyModelViewSet):
    """
    ViewSet for viewing organization memberships.
    GET /api/v1/orgs/:org_id/members - List members
    """
    permission_classes = [IsAuthenticated]
    serializer_class = UserOrganizationSerializer
    
    def get_queryset(self):
        """Filter to organization memberships."""
        org_id = self.kwargs.get('organization_pk')
        return UserOrganization.objects.filter(organization_id=org_id).select_related('user', 'organization')

