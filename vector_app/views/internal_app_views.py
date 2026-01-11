"""
Internal App API views
"""
import logging
from rest_framework import viewsets, status
from rest_framework.decorators import api_view, permission_classes
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from rest_framework.views import APIView
from django.shortcuts import get_object_or_404
from rest_framework.exceptions import PermissionDenied

from ..models import InternalApp, Organization, AppFavorite, UserOrganization
from ..serializers import InternalAppSerializer, InternalAppCreateSerializer, AppFavoriteSerializer
from ..permissions import IsOrgEditorOrAbove, require_editor_or_above, require_admin

logger = logging.getLogger(__name__)


class InternalAppViewSet(viewsets.ModelViewSet):
    """
    ViewSet for InternalApp model.
    GET /api/v1/orgs/:org_id/apps - List apps (any member)
    POST /api/v1/orgs/:org_id/apps - Create app (editor+)
    GET /api/v1/apps/:id - Get app (any member)
    PATCH /api/v1/apps/:id - Update app (editor+)
    DELETE /api/v1/apps/:id - Delete app (admin only)
    """
    permission_classes = [IsAuthenticated, IsOrgEditorOrAbove]
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
        """Verify user is editor or above in organization."""
        org_id = self.kwargs.get('organization_pk')
        organization = get_object_or_404(Organization, id=org_id)
        
        # Verify user is editor or above (permission class should catch this, but double-check)
        membership, error = require_editor_or_above(self.request, organization)
        if error:
            raise PermissionDenied('You must be an editor or admin to create apps')
        
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

    def destroy(self, request, *args, **kwargs):
        """Delete an app (admin only)."""
        app = self.get_object()

        # Verify user is admin of the app's organization
        membership, error = require_admin(request, app.organization)
        if error:
            raise PermissionDenied('Only admins can delete apps')

        app.delete()
        return Response(status=status.HTTP_204_NO_CONTENT)


class AppFavoritesView(APIView):
    """
    View for managing app favorites within an organization.
    
    GET /api/v1/orgs/:org_id/favorites/ - List user's favorites
    POST /api/v1/orgs/:org_id/favorites/ - Add/toggle favorite
    DELETE /api/v1/orgs/:org_id/favorites/:app_id/ - Remove favorite
    """
    permission_classes = [IsAuthenticated]
    
    def get(self, request, org_id):
        """List all favorites for the current user in this organization."""
        organization = get_object_or_404(Organization, id=org_id)
        
        # Verify user is member
        if not request.user.user_organizations.filter(organization=organization).exists():
            raise PermissionDenied('You are not a member of this organization')
        
        favorites = AppFavorite.objects.filter(
            user=request.user,
            organization=organization
        ).select_related('app')
        
        # Return just the app IDs for efficiency
        app_ids = [str(fav.app_id) for fav in favorites]
        return Response({'favorites': app_ids})
    
    def post(self, request, org_id):
        """Add an app to favorites (or toggle if exists)."""
        organization = get_object_or_404(Organization, id=org_id)
        
        # Verify user is member
        if not request.user.user_organizations.filter(organization=organization).exists():
            raise PermissionDenied('You are not a member of this organization')
        
        app_id = request.data.get('app_id')
        if not app_id:
            return Response({'error': 'app_id is required'}, status=status.HTTP_400_BAD_REQUEST)
        
        app = get_object_or_404(InternalApp, id=app_id, organization=organization)
        
        # Toggle: if exists, delete; if not, create
        favorite, created = AppFavorite.objects.get_or_create(
            user=request.user,
            organization=organization,
            app=app
        )
        
        if not created:
            # Already existed, so delete (toggle off)
            favorite.delete()
            return Response({'favorited': False, 'app_id': str(app_id)})
        
        return Response({'favorited': True, 'app_id': str(app_id)}, status=status.HTTP_201_CREATED)


class AppFavoriteDetailView(APIView):
    """
    View for removing a specific favorite.
    
    DELETE /api/v1/orgs/:org_id/favorites/:app_id/ - Remove favorite
    """
    permission_classes = [IsAuthenticated]
    
    def delete(self, request, org_id, app_id):
        """Remove an app from favorites."""
        organization = get_object_or_404(Organization, id=org_id)
        
        # Verify user is member
        if not request.user.user_organizations.filter(organization=organization).exists():
            raise PermissionDenied('You are not a member of this organization')
        
        deleted_count, _ = AppFavorite.objects.filter(
            user=request.user,
            organization=organization,
            app_id=app_id
        ).delete()
        
        if deleted_count == 0:
            return Response({'error': 'Favorite not found'}, status=status.HTTP_404_NOT_FOUND)
        
        return Response(status=status.HTTP_204_NO_CONTENT)

