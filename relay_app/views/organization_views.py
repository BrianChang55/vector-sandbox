"""
Organization API views
"""
import logging
from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from rest_framework.parsers import MultiPartParser, FormParser, JSONParser
from django.shortcuts import get_object_or_404

from ..models import Organization, UserOrganization
from ..serializers import OrganizationSerializer, OrganizationCreateSerializer, UserOrganizationSerializer
from ..serializers.organization import OrganizationUpdateSerializer, OrganizationLogoUploadSerializer

logger = logging.getLogger(__name__)


class OrganizationViewSet(viewsets.ModelViewSet):
    """
    ViewSet for Organization model.
    GET /api/v1/orgs - List organizations
    POST /api/v1/orgs - Create organization
    GET /api/v1/orgs/:id - Get organization
    PATCH /api/v1/orgs/:id - Update organization
    POST /api/v1/orgs/:id/switch - Switch current organization
    POST /api/v1/orgs/:id/logo - Upload organization logo
    DELETE /api/v1/orgs/:id/logo - Remove organization logo
    """
    permission_classes = [IsAuthenticated]
    serializer_class = OrganizationSerializer
    parser_classes = [JSONParser, MultiPartParser, FormParser]
    
    def get_queryset(self):
        """Filter organizations to those the user belongs to."""
        user = self.request.user
        user_orgs = UserOrganization.objects.filter(user=user).select_related('organization')
        org_ids = [uo.organization_id for uo in user_orgs]
        return Organization.objects.filter(id__in=org_ids)
    
    def get_serializer_class(self):
        """Use appropriate serializer based on action."""
        if self.action == 'create':
            return OrganizationCreateSerializer
        if self.action in ['update', 'partial_update']:
            return OrganizationUpdateSerializer
        return OrganizationSerializer
    
    def perform_create(self, serializer):
        """Set organization in context."""
        serializer.save()
    
    def update(self, request, *args, **kwargs):
        """Update organization (only admins)."""
        org = self.get_object()
        
        # Check if user is admin
        user_org = UserOrganization.objects.filter(
            user=request.user, 
            organization=org,
            role=UserOrganization.ROLE_ADMIN
        ).first()
        
        if not user_org:
            return Response(
                {'error': 'Only admins can update organization settings'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        return super().update(request, *args, **kwargs)
    
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
            'organization': OrganizationSerializer(org, context={'request': request}).data,
            'message': 'Organization switched',
        })
    
    @action(detail=True, methods=['post'], parser_classes=[MultiPartParser, FormParser])
    def logo(self, request, pk=None):
        """Upload organization logo."""
        org = self.get_object()
        
        # Check if user is admin
        user_org = UserOrganization.objects.filter(
            user=request.user, 
            organization=org,
            role=UserOrganization.ROLE_ADMIN
        ).first()
        
        if not user_org:
            return Response(
                {'error': 'Only admins can upload organization logo'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        serializer = OrganizationLogoUploadSerializer(data=request.data)
        if not serializer.is_valid():
            return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
        
        # Delete old logo if exists
        if org.logo:
            org.logo.delete(save=False)
        
        # Save new logo
        org.logo = serializer.validated_data['logo']
        org.save()
        
        logger.info(f"Logo uploaded for organization {org.id}")
        
        return Response({
            'message': 'Logo uploaded successfully',
            'organization': OrganizationSerializer(org, context={'request': request}).data,
        })
    
    @logo.mapping.delete
    def delete_logo(self, request, pk=None):
        """Delete organization logo."""
        org = self.get_object()
        
        # Check if user is admin
        user_org = UserOrganization.objects.filter(
            user=request.user, 
            organization=org,
            role=UserOrganization.ROLE_ADMIN
        ).first()
        
        if not user_org:
            return Response(
                {'error': 'Only admins can delete organization logo'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        if org.logo:
            org.logo.delete(save=True)
            logger.info(f"Logo deleted for organization {org.id}")
        
        return Response({
            'message': 'Logo deleted successfully',
            'organization': OrganizationSerializer(org, context={'request': request}).data,
        })
    
    def destroy(self, request, *args, **kwargs):
        """Delete organization (only admins, requires confirmation)."""
        org = self.get_object()
        
        # Check if user is admin
        user_org = UserOrganization.objects.filter(
            user=request.user, 
            organization=org,
            role=UserOrganization.ROLE_ADMIN
        ).first()
        
        if not user_org:
            return Response(
                {'error': 'Only admins can delete the organization'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Require confirmation by matching the organization name
        confirmation_name = request.data.get('confirmation_name', '')
        if confirmation_name != org.name:
            return Response(
                {'error': 'Organization name does not match. Please type the exact name to confirm deletion.'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        org_name = org.name
        org_id = str(org.id)
        
        # Delete the organization (cascades to related objects)
        org.delete()
        
        logger.info(f"Organization {org_id} ({org_name}) deleted by user {request.user.id}")
        
        return Response({
            'message': f'Organization "{org_name}" has been permanently deleted.',
        }, status=status.HTTP_200_OK)


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
