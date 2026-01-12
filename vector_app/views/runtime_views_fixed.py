"""
Runtime Proxy API views (FIXED VERSION)

Runtime endpoints for preview bundle to query data and execute actions.
All queries/actions use user JWT to enforce RLS.
Uses scope_snapshot from version for published apps.
"""
import logging
import jsonschema
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from rest_framework import status
from django.shortcuts import get_object_or_404

from ..models import InternalApp, AppVersion, ResourceRegistryEntry, ProjectUserBackendAuth, ActionExecutionLog
from ..adapters.supabase import SupabaseAdapter
from ..adapters.base import UserContext

logger = logging.getLogger(__name__)


class RuntimeQueryView(APIView):
    """
    Runtime query endpoint.
    POST /api/v1/runtime/query
    
    Uses scope_snapshot from version if available (for published apps).
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request):
        """Execute a query against a resource."""
        app_id = request.data.get('appId')
        version_id = request.data.get('versionId')
        resource_id = request.data.get('resourceId')
        query_spec = request.data.get('querySpec', {})
        
        # Validate inputs
        if not all([app_id, version_id, resource_id]):
            return Response(
                {'error': 'appId, versionId, and resourceId are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get app and version
        app = get_object_or_404(InternalApp, pk=app_id)
        version = get_object_or_404(AppVersion, pk=version_id, internal_app=app)
        
        # Verify user has access
        if not request.user.user_organizations.filter(organization=app.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Get scope snapshot from version (if published) or use current registry
        if version.scope_snapshot_json:
            # Use scope snapshot (published version)
            scope_resources = {r['resource_id']: r for r in version.scope_snapshot_json}
            resource_data = scope_resources.get(resource_id)
            
            if not resource_data:
                return Response(
                    {'error': f'Resource {resource_id} not found in version scope'},
                    status=status.HTTP_400_BAD_REQUEST
                )
            
            exposed_fields = set(resource_data.get('exposed_fields', []))
            if not resource_data.get('exposed_fields'):
                return Response(
                    {'error': f'Resource {resource_id} has no exposed fields in version scope'},
                    status=status.HTTP_400_BAD_REQUEST
                )
        else:
            # Use current registry (draft version)
            registry_entry = get_object_or_404(
                ResourceRegistryEntry,
                resource_id=resource_id,
                backend_connection=app.backend_connection,
                enabled=True
            )
            exposed_fields = set(registry_entry.exposed_fields_json or [])
        
        # Verify fields in query are exposed
        select_fields = query_spec.get('select', ['*'])
        if select_fields != ['*']:
            invalid_fields = set(select_fields) - exposed_fields
            if invalid_fields:
                return Response(
                    {'error': f'Fields not exposed: {list(invalid_fields)}'},
                    status=status.HTTP_400_BAD_REQUEST
                )
        
        # Get user JWT
        user_auth = ProjectUserBackendAuth.objects.filter(
            user=request.user,
            backend_connection=app.backend_connection
        ).first()
        
        if not user_auth:
            return Response(
                {'error': 'User authentication not configured for this backend'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Execute query via adapter
        try:
            adapter = SupabaseAdapter()
            config = app.backend_connection.get_config()
            
            user_ctx = UserContext(
                backend_url=config.get('supabase_url', ''),
                user_jwt=user_auth.get_jwt(),
                anon_key=config.get('anon_key'),
            )
            
            result = adapter.query(resource_id, query_spec, user_ctx)
            
            return Response(result)
            
        except Exception as e:
            logger.error(f"Error in runtime query: {e}")
            return Response(
                {'error': str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )


class RuntimeActionView(APIView):
    """
    Runtime action endpoint.
    POST /api/v1/runtime/action
    
    Uses scope_snapshot from version if available (for published apps).
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request):
        """Execute an allowlisted action."""
        app_id = request.data.get('appId')
        version_id = request.data.get('versionId')
        action_id = request.data.get('actionId')
        args = request.data.get('args', {})
        
        # Validate inputs
        if not all([app_id, version_id, action_id]):
            return Response(
                {'error': 'appId, versionId, and actionId are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get app and version
        app = get_object_or_404(InternalApp, pk=app_id)
        version = get_object_or_404(AppVersion, pk=version_id, internal_app=app)
        
        # Verify user has access
        if not request.user.user_organizations.filter(organization=app.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Check if actions allowed in preview
        if not app.allow_actions_in_preview:
            return Response(
                {'error': 'Actions are not allowed in preview mode'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Get scope snapshot from version (if published) or use current registry
        if version.scope_snapshot_json:
            # Use scope snapshot (published version)
            scope_resources = {r['resource_id']: r for r in version.scope_snapshot_json}
            # Find action in scope snapshot
            action_def = None
            for resource_data in scope_resources.values():
                allowed_actions = resource_data.get('allowed_actions', [])
                action_def = next((a for a in allowed_actions if a.get('action_id') == action_id), None)
                if action_def:
                    break
        else:
            # Use current registry (draft version)
            registry_entries = ResourceRegistryEntry.objects.filter(
                backend_connection=app.backend_connection,
                enabled=True
            )
            action_def = None
            for entry in registry_entries:
                allowed_actions = entry.allowed_actions_json or []
                action_def = next((a for a in allowed_actions if a.get('action_id') == action_id), None)
                if action_def:
                    break
        
        if not action_def:
            return Response(
                {'error': f'Action {action_id} is not allowlisted'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Validate args against input_schema
        input_schema = action_def.get('input_schema', {})
        try:
            jsonschema.validate(instance=args, schema=input_schema)
        except jsonschema.ValidationError as e:
            return Response(
                {'error': f'Invalid action arguments: {e.message}'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get user JWT
        user_auth = ProjectUserBackendAuth.objects.filter(
            user=request.user,
            backend_connection=app.backend_connection
        ).first()
        
        if not user_auth:
            return Response(
                {'error': 'User authentication not configured for this backend'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Execute action via adapter
        try:
            adapter = SupabaseAdapter()
            config = app.backend_connection.get_config()
            
            user_ctx = UserContext(
                backend_url=config.get('supabase_url', ''),
                user_jwt=user_auth.get_jwt(),
                anon_key=config.get('anon_key'),
            )
            
            result = adapter.execute_action(action_def, args, user_ctx)
            
            # Log execution
            ActionExecutionLog.objects.create(
                internal_app=app,
                app_version=version,
                user=request.user,
                backend_connection=app.backend_connection,
                action_id=action_id,
                resource_id=action_def.get('resource_id', ''),
                args_json=args,
                status=ActionExecutionLog.STATUS_SUCCESS,
            )
            
            return Response(result)
            
        except Exception as e:
            logger.error(f"Error executing action: {e}")
            
            # Log failed execution
            ActionExecutionLog.objects.create(
                internal_app=app,
                app_version=version,
                user=request.user,
                backend_connection=app.backend_connection,
                action_id=action_id,
                resource_id=action_def.get('resource_id', ''),
                args_json=args,
                status=ActionExecutionLog.STATUS_ERROR,
                error_message=str(e),
            )
            
            return Response(
                {'error': str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )

