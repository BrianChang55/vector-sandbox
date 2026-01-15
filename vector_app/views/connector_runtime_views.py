"""
Runtime Connector Proxy API.

Single endpoint for generated apps to interact with external connectors.
Handles authentication via app context and routes to appropriate operations.

Organization-level integration model:
- Uses organization's connected credentials for all tool executions
- All org members share the same integration connections

Authorization:
- All requests must be authenticated
- User must be a member of the app's organization
"""
from __future__ import annotations

import logging
import time
from typing import Tuple, Optional
from rest_framework import status
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated

from ..models import (
    ConnectorCache,
    ConnectorExecutionLog,
    ConnectorExecutionStatus,
    InternalApp,
    MergeIntegrationProvider,
    OrganizationConnectorLink,
    User,
    UserOrganization,
)
from ..services.merge_service import merge_service, MergeAPIError
from ..permissions import require_org_membership

logger = logging.getLogger(__name__)


def check_app_access(request, app: InternalApp) -> Tuple[bool, Optional[str]]:
    """
    Check if the request has access to the app.
    
    All requests must be authenticated and user must be a member of the app's organization.
    
    Returns:
        (has_access, error_message) - if has_access is False, error_message explains why
    """
    user = request.user
    
    # Require authentication (should be handled by IsAuthenticated, but double-check)
    if not user or not user.is_authenticated:
        return False, "Authentication required"
    
    # Verify user belongs to the app's organization using permissions helper
    membership, _ = require_org_membership(request, app.organization)
    
    if not membership:
        return False, "You don't have access to this app"
    
    return True, None


class RuntimeConnectorProxyView(APIView):
    """
    Unified runtime proxy for connector operations.
    
    This endpoint is called by generated apps at runtime to execute
    tools on external connectors (Jira, Slack, etc.) using the
    organization's connected credentials.
    
    Authorization:
    - All requests must be authenticated
    - User must be a member of the app's organization
    
    Request format:
    {
        "appId": "uuid",
        "connectorId": "jira",           // or "_meta" for meta operations
        "toolId": "create_issue",        // or "list", "tools" for meta ops
        "params": { ... }                // Tool-specific parameters
    }
    
    Meta operations (connectorId = "_meta"):
    - toolId = "list": List available connectors
    - toolId = "tools": Get tools for a connector (params.connectorId required)
    """
    
    # Use IsAuthenticated - app access verification is done in check_app_access
    permission_classes = [IsAuthenticated]
    
    def post(self, request):
        """Handle all connector operations."""
        data = request.data
        
        # Extract required fields
        app_id = data.get('appId')
        connector_id = data.get('connectorId')
        tool_id = data.get('toolId')
        params = data.get('params', {})
        
        # Validate required fields
        if not app_id:
            return Response(
                {'error': 'appId is required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        if not connector_id:
            return Response(
                {'error': 'connectorId is required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        if not tool_id:
            return Response(
                {'error': 'toolId is required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get the app
        try:
            app = InternalApp.objects.select_related('organization').get(id=app_id)
        except InternalApp.DoesNotExist:
            return Response(
                {'error': 'App not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        # Authorization check
        has_access, error_message = check_app_access(request, app)
        if not has_access:
            return Response(
                {'error': error_message},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Get integration provider
        try:
            provider = MergeIntegrationProvider.objects.get(
                organization=app.organization,
                is_active=True,
            )
        except MergeIntegrationProvider.DoesNotExist:
            return Response(
                {'error': 'No integration provider configured for this organization'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        # Handle meta operations
        if connector_id == '_meta':
            return self._handle_meta_operation(provider, tool_id, params)
        
        # Get user context for logging
        user = self._get_execution_user(request, app)
        
        # Execute the tool
        return self._execute_tool(
            app=app,
            provider=provider,
            user=user,
            connector_id=connector_id,
            tool_id=tool_id,
            params=params,
        )
    
    def _get_execution_user(self, request, app: InternalApp) -> User:
        """
        Determine which user context to use for logging.
        
        In authenticated requests, use the request user.
        In preview/published mode, fall back to app creator.
        """
        if request.user and request.user.is_authenticated:
            return request.user
        
        # Fall back to app creator for preview mode
        return app.created_by
    
    def _handle_meta_operation(
        self, 
        provider: MergeIntegrationProvider, 
        tool_id: str, 
        params: dict
    ) -> Response:
        """Handle meta operations like listing connectors and tools."""
        
        if tool_id == 'list':
            # List available (connected) connectors from cache
            connectors = ConnectorCache.objects.filter(
                provider=provider,
                is_enabled=True,
            ).order_by('connector_name')
            
            # Get connection status
            connected_ids = set(
                OrganizationConnectorLink.objects.filter(
                    provider=provider,
                    is_connected=True,
                ).values_list('connector__connector_id', flat=True)
            )
            
            result = []
            for connector in connectors:
                result.append({
                    'id': connector.connector_id,
                    'name': connector.connector_name,
                    'category': connector.category,
                    'logo_url': connector.logo_url,
                    'tool_count': len(connector.tools_json or []),
                    'is_connected': connector.connector_id in connected_ids,
                })
            
            return Response({'connectors': result})
        
        elif tool_id == 'tools':
            # Get tools for a specific connector from cache
            connector_id = params.get('connectorId')
            if not connector_id:
                return Response(
                    {'error': 'connectorId is required in params'},
                    status=status.HTTP_400_BAD_REQUEST
                )
            
            try:
                connector = ConnectorCache.objects.get(
                    provider=provider,
                    connector_id=connector_id,
                    is_enabled=True,
                )
            except ConnectorCache.DoesNotExist:
                return Response(
                    {'error': f'Connector "{connector_id}" not found'},
                    status=status.HTTP_404_NOT_FOUND
                )
            
            return Response({'tools': connector.tools_json or []})
        
        elif tool_id == 'mcp_tools':
            # List available tools via live MCP tools/list endpoint
            if not provider.merge_registered_user_id:
                return Response(
                    {'error': 'Organization is not registered with Merge'},
                    status=status.HTTP_400_BAD_REQUEST
                )
            
            try:
                tools = merge_service.mcp_list_tools(provider)
                return Response({
                    'tools': tools,
                    'count': len(tools),
                })
            except MergeAPIError as e:
                logger.error(f"Failed to list MCP tools: {e}")
                return Response(
                    {'error': str(e)},
                    status=status.HTTP_400_BAD_REQUEST
                )
        
        elif tool_id == 'mcp_call':
            # Execute an MCP tool via tools/call endpoint
            # This is the main entry point for generated apps to call integration tools
            if not provider.merge_registered_user_id:
                return Response(
                    {'error': 'Organization is not registered with Merge'},
                    status=status.HTTP_400_BAD_REQUEST
                )
            
            tool_name = params.get('tool_name')
            arguments = params.get('arguments', {})
            
            if not tool_name:
                return Response(
                    {'error': 'tool_name is required in params'},
                    status=status.HTTP_400_BAD_REQUEST
                )
            
            try:
                result = merge_service.mcp_call_tool(provider, tool_name, arguments)
                
                if result.success:
                    # Check if the content contains an error message
                    # MCP sometimes returns success=true but with error in content
                    content = result.content
                    error_in_content = None
                    
                    if isinstance(content, list) and len(content) > 0:
                        first_item = content[0]
                        if isinstance(first_item, dict) and first_item.get('type') == 'text':
                            text = first_item.get('text', '')
                            # Strip outer quotes if present (MCP sometimes double-escapes)
                            if (text.startswith('"') and text.endswith('"')) or (text.startswith("'") and text.endswith("'")):
                                text = text[1:-1]
                            # Unescape escaped quotes
                            text = text.replace('\\"', '"')
                            # Check if the text contains an error
                            if 'error' in text.lower():
                                try:
                                    import json
                                    import re
                                    # First try: pattern for 'error': "..."
                                    match = re.search(r"'error':\s*\"([^\"]+)\"", text)
                                    if match:
                                        error_in_content = match.group(1)
                                    else:
                                        # Second try: pattern for "error": "..."
                                        match = re.search(r'"error":\s*"([^"]+)"', text)
                                        if match:
                                            error_in_content = match.group(1)
                                        else:
                                            # Third try: parse as JSON
                                            parsed = json.loads(text.replace("'", '"'))
                                            if isinstance(parsed, dict) and 'error' in parsed:
                                                error_in_content = parsed['error']
                                except (json.JSONDecodeError, ValueError, AttributeError):
                                    pass
                    
                    if error_in_content:
                        logger.warning(f"MCP tool '{tool_name}' returned error in content: {error_in_content}")
                        return Response({
                            'success': False,
                            'error': error_in_content,
                        }, status=status.HTTP_400_BAD_REQUEST)
                    
                    return Response({
                        'success': True,
                        'data': content,
                    })
                else:
                    return Response({
                        'success': False,
                        'error': result.error_message or 'Tool execution failed',
                    }, status=status.HTTP_400_BAD_REQUEST)
                    
            except MergeAPIError as e:
                logger.error(f"Failed to call MCP tool '{tool_name}': {e}")
                return Response(
                    {'success': False, 'error': str(e)},
                    status=status.HTTP_400_BAD_REQUEST
                )
        
        else:
            return Response(
                {'error': f'Unknown meta operation: {tool_id}'},
                status=status.HTTP_400_BAD_REQUEST
            )
    
    def _execute_tool(
        self,
        app: InternalApp,
        provider: MergeIntegrationProvider,
        user: User,
        connector_id: str,
        tool_id: str,
        params: dict,
    ) -> Response:
        """Execute a tool on a connector using org-level credentials."""
        start_time = time.time()
        
        # Verify organization is registered with Merge
        if not provider.merge_registered_user_id:
            return Response(
                {
                    'error': 'Organization has not set up integrations. '
                             'Please configure integrations in the organization settings.'
                },
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Get connector for logging and verification
        connector = None
        try:
            connector = ConnectorCache.objects.get(
                provider=provider,
                connector_id=connector_id,
            )
        except ConnectorCache.DoesNotExist:
            return Response(
                {'error': f'Connector "{connector_id}" not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        # Verify the organization has connected this connector
        try:
            link = OrganizationConnectorLink.objects.get(
                provider=provider,
                connector=connector,
            )
            if not link.is_connected:
                return Response(
                    {
                        'error': f'Organization has not connected to {connector.connector_name}. '
                                 'Please connect this integration in the organization settings.'
                    },
                    status=status.HTTP_403_FORBIDDEN
                )
        except OrganizationConnectorLink.DoesNotExist:
            return Response(
                {
                    'error': f'Organization has not connected to {connector.connector_name}. '
                             'Please connect this integration in the organization settings.'
                },
                status=status.HTTP_403_FORBIDDEN
            )
        
        try:
            # Execute via Merge service using org credentials
            result = merge_service.execute_tool(
                provider=provider,
                connector_id=connector_id,
                tool_id=tool_id,
                params=params,
            )
            
            duration_ms = int((time.time() - start_time) * 1000)
            
            # Log execution
            ConnectorExecutionLog.objects.create(
                internal_app=app,
                user=user,
                connector=connector,
                connector_slug=connector_id,
                tool_id=tool_id,
                params_json=params,
                result_json=result.data if result.success else None,
                status=ConnectorExecutionStatus.SUCCESS if result.success else ConnectorExecutionStatus.ERROR,
                error_message=result.error if not result.success else None,
                duration_ms=duration_ms,
            )
            
            if result.success:
                return Response({
                    'success': True,
                    'data': result.data,
                })
            else:
                return Response({
                    'success': False,
                    'error': result.error,
                }, status=status.HTTP_400_BAD_REQUEST)
            
        except MergeAPIError as e:
            duration_ms = int((time.time() - start_time) * 1000)
            
            # Log failed execution
            ConnectorExecutionLog.objects.create(
                internal_app=app,
                user=user,
                connector=connector,
                connector_slug=connector_id,
                tool_id=tool_id,
                params_json=params,
                status=ConnectorExecutionStatus.ERROR,
                error_message=str(e),
                duration_ms=duration_ms,
            )
            
            logger.error(f"Connector execution failed: {e}")
            return Response(
                {'success': False, 'error': str(e)},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        except Exception as e:
            duration_ms = int((time.time() - start_time) * 1000)
            
            # Log unexpected error
            ConnectorExecutionLog.objects.create(
                internal_app=app,
                user=user,
                connector=connector,
                connector_slug=connector_id,
                tool_id=tool_id,
                params_json=params,
                status=ConnectorExecutionStatus.ERROR,
                error_message=str(e),
                duration_ms=duration_ms,
            )
            
            logger.exception(f"Unexpected error in connector execution: {e}")
            return Response(
                {'success': False, 'error': 'An unexpected error occurred'},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )
