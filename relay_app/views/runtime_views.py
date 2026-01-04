"""
Runtime Proxy API views

Runtime endpoints for preview bundle to query data and execute actions.
All queries/actions use user JWT to enforce RLS (Supabase) or direct connection (PostgreSQL/MySQL).
"""
import logging
import jsonschema
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from rest_framework import status
from django.shortcuts import get_object_or_404

from ..models import (
    InternalApp, 
    AppVersion, 
    ResourceRegistryEntry, 
    ProjectUserBackendAuth, 
    ActionExecutionLog,
    BackendConnection,
)
from ..adapters.supabase import SupabaseAdapter
from ..adapters.postgresql import PostgreSQLAdapter
from ..adapters.mysql import MySQLAdapter
from ..adapters.base import UserContext, AdapterContext

logger = logging.getLogger(__name__)


def get_adapter(adapter_type: str):
    """Get the appropriate adapter based on type."""
    adapters = {
        BackendConnection.ADAPTER_SUPABASE: SupabaseAdapter,
        BackendConnection.ADAPTER_POSTGRESQL: PostgreSQLAdapter,
        BackendConnection.ADAPTER_MYSQL: MySQLAdapter,
    }
    adapter_class = adapters.get(adapter_type)
    return adapter_class() if adapter_class else None


def get_user_context(backend: BackendConnection, user_auth: ProjectUserBackendAuth = None) -> UserContext:
    """
    Create user context from backend configuration.
    
    For Supabase: Uses user JWT (RLS enforced)
    For PostgreSQL/MySQL: Uses the stored connection credentials
    """
    config = backend.get_config()
    
    if backend.adapter_type == BackendConnection.ADAPTER_SUPABASE:
        user_jwt = user_auth.get_jwt() if user_auth else None
        return UserContext(
            backend_url=config.get('supabase_url', ''),
            user_jwt=user_jwt,
            anon_key=config.get('anon_key'),
        )
    elif backend.adapter_type in [BackendConnection.ADAPTER_POSTGRESQL, BackendConnection.ADAPTER_MYSQL]:
        # For direct database connections, use the stored credentials
        # In production, you might want per-user DB credentials
        return UserContext(
            host=config.get('host', ''),
            port=config.get('port'),
            database=config.get('database', ''),
            username=config.get('username', ''),
            password=config.get('password', ''),
            ssl_mode=config.get('ssl_mode', 'disable'),
        )
    
    return UserContext()


class RuntimeQueryView(APIView):
    """
    Runtime query endpoint.
    POST /api/v1/runtime/query
    
    Supports all database adapters: Supabase, PostgreSQL, MySQL
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request):
        """Execute a query against a resource."""
        app_id = request.data.get('appId')
        version_id = request.data.get('versionId')
        resource_id = request.data.get('resourceId')
        query_spec = request.data.get('querySpec', {})
        
        # Validate inputs
        if not all([app_id, resource_id]):
            return Response(
                {'error': 'appId and resourceId are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get app
        app = get_object_or_404(InternalApp, pk=app_id)
        
        # Get version if specified
        version = None
        if version_id:
            version = get_object_or_404(AppVersion, pk=version_id, internal_app=app)
        
        # Verify user has access
        if not request.user.user_organizations.filter(organization=app.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Verify app has a backend connection
        if not app.backend_connection:
            return Response(
                {'error': 'App does not have a backend connection configured'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get scope snapshot from version (if published) or use current registry
        if version and version.scope_snapshot_json:
            # Use scope snapshot (published version)
            scope_resources = {r['resource_id']: r for r in version.scope_snapshot_json}
            resource_data = scope_resources.get(resource_id)
            
            if not resource_data:
                return Response(
                    {'error': f'Resource {resource_id} not found in version scope'},
                    status=status.HTTP_400_BAD_REQUEST
                )
            
            exposed_fields = set(resource_data.get('exposed_fields', []))
        else:
            # Use current registry (draft version)
            registry_entry = ResourceRegistryEntry.objects.filter(
                resource_id=resource_id,
                backend_connection=app.backend_connection,
                enabled=True
            ).first()
            
            if not registry_entry:
                return Response(
                    {'error': f'Resource {resource_id} is not enabled or does not exist'},
                    status=status.HTTP_400_BAD_REQUEST
                )
            
            exposed_fields = set(registry_entry.exposed_fields_json or [])
        
        # Verify fields in query are exposed (if exposed_fields is set)
        if exposed_fields:
            select_fields = query_spec.get('select', [])
            if select_fields and select_fields != ['*']:
                invalid_fields = set(select_fields) - exposed_fields
                if invalid_fields:
                    return Response(
                        {'error': f'Fields not exposed: {list(invalid_fields)}'},
                        status=status.HTTP_400_BAD_REQUEST
                    )
        
        # Get adapter
        adapter = get_adapter(app.backend_connection.adapter_type)
        if not adapter:
            return Response(
                {'error': f'Unknown adapter type: {app.backend_connection.adapter_type}'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get user auth for Supabase (required for RLS)
        user_auth = None
        if app.backend_connection.adapter_type == BackendConnection.ADAPTER_SUPABASE:
            user_auth = ProjectUserBackendAuth.objects.filter(
                user=request.user,
                backend_connection=app.backend_connection
            ).first()
            
            if not user_auth:
                return Response(
                    {'error': 'User authentication not configured for this Supabase backend'},
                    status=status.HTTP_400_BAD_REQUEST
                )
        
        # Execute query via adapter
        try:
            user_ctx = get_user_context(app.backend_connection, user_auth)
            result = adapter.query(resource_id, query_spec, user_ctx)
            
            return Response(result)
            
        except Exception as e:
            logger.error(f"Error in runtime query: {e}", exc_info=True)
            return Response(
                {'error': str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )


class RuntimeActionView(APIView):
    """
    Runtime action endpoint.
    POST /api/v1/runtime/action
    
    Supports all database adapters: Supabase, PostgreSQL, MySQL
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request):
        """Execute an allowlisted action."""
        app_id = request.data.get('appId')
        version_id = request.data.get('versionId')
        action_id = request.data.get('actionId')
        args = request.data.get('args', {})
        
        # Validate inputs
        if not all([app_id, action_id]):
            return Response(
                {'error': 'appId and actionId are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get app
        app = get_object_or_404(InternalApp, pk=app_id)
        
        # Get version if specified
        version = None
        if version_id:
            version = get_object_or_404(AppVersion, pk=version_id, internal_app=app)
        
        # Verify user has access
        if not request.user.user_organizations.filter(organization=app.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Verify app has a backend connection
        if not app.backend_connection:
            return Response(
                {'error': 'App does not have a backend connection configured'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Check if actions allowed in preview
        if not app.allow_actions_in_preview:
            return Response(
                {'error': 'Actions are not allowed in preview mode'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Get scope snapshot from version (if published) or use current registry
        if version and version.scope_snapshot_json:
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
        if input_schema:
            try:
                jsonschema.validate(instance=args, schema=input_schema)
            except jsonschema.ValidationError as e:
                return Response(
                    {'error': f'Invalid action arguments: {e.message}'},
                    status=status.HTTP_400_BAD_REQUEST
                )
        
        # Get adapter
        adapter = get_adapter(app.backend_connection.adapter_type)
        if not adapter:
            return Response(
                {'error': f'Unknown adapter type: {app.backend_connection.adapter_type}'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get user auth for Supabase (required for RLS)
        user_auth = None
        if app.backend_connection.adapter_type == BackendConnection.ADAPTER_SUPABASE:
            user_auth = ProjectUserBackendAuth.objects.filter(
                user=request.user,
                backend_connection=app.backend_connection
            ).first()
            
            if not user_auth:
                return Response(
                    {'error': 'User authentication not configured for this Supabase backend'},
                    status=status.HTTP_400_BAD_REQUEST
                )
        
        # Execute action via adapter
        try:
            user_ctx = get_user_context(app.backend_connection, user_auth)
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
                result_json=result,
                status=ActionExecutionLog.STATUS_SUCCESS,
            )
            
            return Response(result)
            
        except Exception as e:
            logger.error(f"Error executing action: {e}", exc_info=True)
            
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
