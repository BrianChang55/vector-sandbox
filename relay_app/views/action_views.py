"""
Action allowlist API views
"""
import logging
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from rest_framework import status
from django.shortcuts import get_object_or_404

from ..models import ResourceRegistryEntry

logger = logging.getLogger(__name__)


class ActionAllowlistView(APIView):
    """
    View for managing action allowlists.
    POST /api/v1/actions/allowlist - Add action to allowlist
    DELETE /api/v1/actions/:action_id - Remove action from allowlist
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request):
        """Add action to allowlist."""
        registry_id = request.data.get('registry_id')
        action_def = request.data.get('action_def')
        
        if not registry_id or not action_def:
            return Response(
                {'error': 'registry_id and action_def are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        registry_entry = get_object_or_404(ResourceRegistryEntry, pk=registry_id)
        
        # Verify access
        if not request.user.user_organizations.filter(organization=registry_entry.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Add action to allowlist
        allowed_actions = registry_entry.allowed_actions_json or []
        allowed_actions.append(action_def)
        registry_entry.allowed_actions_json = allowed_actions
        registry_entry.save()
        
        return Response({
            'success': True,
            'message': 'Action added to allowlist',
        })
    
    def delete(self, request, action_id=None):
        """Remove action from allowlist."""
        registry_id = request.data.get('registry_id') or request.query_params.get('registry_id')
        
        if not registry_id or not action_id:
            return Response(
                {'error': 'registry_id and action_id are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        registry_entry = get_object_or_404(ResourceRegistryEntry, pk=registry_id)
        
        # Verify access
        if not request.user.user_organizations.filter(organization=registry_entry.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Remove action from allowlist
        allowed_actions = registry_entry.allowed_actions_json or []
        allowed_actions = [a for a in allowed_actions if a.get('action_id') != action_id]
        registry_entry.allowed_actions_json = allowed_actions
        registry_entry.save()
        
        return Response({
            'success': True,
            'message': 'Action removed from allowlist',
        })

