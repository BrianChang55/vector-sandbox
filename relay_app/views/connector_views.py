"""
Connector & Integration API Views.

User-facing API for managing integrations and connectors.
Internally uses Merge Agent Handler API via merge_service.py.

Organization-level integration model:
- Each organization is registered as a single "user" with Merge
- Connectors are connected once per organization
- All org members share the connected integrations

Note: Merge API credentials are read from environment variables (settings.py),
not from user input. Providers are auto-created when needed.
"""
import logging
from rest_framework import viewsets, status
from rest_framework.decorators import api_view, permission_classes
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.shortcuts import get_object_or_404

from ..models import (
    Organization,
    UserOrganization,
    MergeIntegrationProvider,
    ConnectorCache,
    OrganizationConnectorLink,
)
from ..services.merge_service import merge_service, MergeAPIError, is_merge_configured
from ..serializers.connector_serializers import (
    IntegrationProviderSerializer,
    IntegrationProviderCreateSerializer,
)

logger = logging.getLogger(__name__)


def require_admin_for_integrations(request, organization):
    """
    Check if user is admin of the organization.
    Integrations management requires admin role.
    
    Returns True if allowed, False otherwise.
    """
    membership = UserOrganization.objects.filter(
        user=request.user,
        organization=organization
    ).first()
    
    if not membership:
        return False
    
    return membership.can_manage_integrations()


def get_or_create_provider(organization: Organization) -> MergeIntegrationProvider:
    """
    Get or create an integration provider for an organization.
    
    Since credentials come from environment variables, we can auto-create
    providers when needed.
    """
    provider, created = MergeIntegrationProvider.objects.get_or_create(
        organization=organization,
        defaults={'display_name': 'Integrations'}
    )
    if created:
        logger.info(f"Auto-created integration provider for org: {organization.name}")
    return provider


class IntegrationProviderViewSet(viewsets.ModelViewSet):
    """
    ViewSet for managing Integration Providers.
    
    GET /api/v1/orgs/:org_id/integrations/ - List providers (auto-creates if configured)
    POST /api/v1/orgs/:org_id/integrations/ - Create provider
    GET /api/v1/integrations/:id/ - Get provider
    PATCH /api/v1/integrations/:id/ - Update provider
    DELETE /api/v1/integrations/:id/ - Delete provider
    
    Note: Since Merge credentials come from environment variables, providers
    can be auto-created when first accessed.
    """
    permission_classes = [IsAuthenticated]
    serializer_class = IntegrationProviderSerializer
    
    def get_queryset(self):
        """Filter providers to organization or return all user-accessible providers."""
        org_id = self.kwargs.get('organization_pk')
        if org_id:
            return MergeIntegrationProvider.objects.filter(organization_id=org_id)
        
        # For direct access, return providers from user's orgs
        user_org_ids = self.request.user.user_organizations.values_list('organization_id', flat=True)
        return MergeIntegrationProvider.objects.filter(organization_id__in=user_org_ids)
    
    def list(self, request, *args, **kwargs):
        """
        List integration providers for an organization.
        
        If Merge is configured and no provider exists, auto-create one.
        """
        org_id = self.kwargs.get('organization_pk')
        
        if org_id and is_merge_configured():
            organization = get_object_or_404(Organization, id=org_id)
            
            # Verify user is member
            if request.user.user_organizations.filter(organization=organization).exists():
                # Auto-create provider if it doesn't exist
                get_or_create_provider(organization)
        
        return super().list(request, *args, **kwargs)
    
    def get_serializer_class(self):
        """Use create serializer for POST."""
        if self.action == 'create':
            return IntegrationProviderCreateSerializer
        return IntegrationProviderSerializer
    
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


@api_view(['GET'])
@permission_classes([IsAuthenticated])
def integration_status(request, org_id):
    """
    Check integration configuration status for an organization.
    
    GET /api/v1/orgs/:org_id/integrations/status/
    
    Returns:
    {
        "is_configured": true,  // Merge API credentials in env
        "provider_exists": true,  // Provider record exists
        "is_registered": false,  // Org registered with Merge
        "connector_count": 0,
        "connected_count": 0
    }
    """
    organization = get_object_or_404(Organization, id=org_id)
    
    # Verify user has access
    if not request.user.user_organizations.filter(organization=organization).exists():
        return Response(
            {'error': 'Access denied'},
            status=status.HTTP_403_FORBIDDEN
        )
    
    configured = is_merge_configured()
    
    try:
        provider = MergeIntegrationProvider.objects.get(organization=organization)
        provider_exists = True
        is_registered = bool(provider.merge_registered_user_id)
        connector_count = provider.connectors.filter(is_enabled=True).count()
        connected_count = OrganizationConnectorLink.objects.filter(
            provider=provider,
            is_connected=True,
        ).count()
    except MergeIntegrationProvider.DoesNotExist:
        provider_exists = False
        is_registered = False
        connector_count = 0
        connected_count = 0
    
    return Response({
        'is_configured': configured,
        'provider_exists': provider_exists,
        'is_registered': is_registered,
        'connector_count': connector_count,
        'connected_count': connected_count,
    })


@api_view(['POST'])
@permission_classes([IsAuthenticated])
def sync_connectors(request, pk):
    """
    Sync available connectors from the integration provider.
    
    POST /api/v1/integrations/:id/sync/
    
    This fetches all available connectors and their tools from Merge,
    and updates the local cache.
    
    Admin only.
    """
    provider = get_object_or_404(MergeIntegrationProvider, pk=pk)
    
    # Verify user is admin (integration management requires admin)
    if not require_admin_for_integrations(request, provider.organization):
        return Response(
            {'error': 'Only admins can manage integrations'},
            status=status.HTTP_403_FORBIDDEN
        )
    
    # Check if Merge is configured
    if not is_merge_configured():
        return Response(
            {'error': 'Integrations are not available. Merge API credentials are not configured.'},
            status=status.HTTP_503_SERVICE_UNAVAILABLE
        )
    
    try:
        # Fetch connectors from Merge (credentials from settings)
        connectors_data = merge_service.sync_all_connectors()
        
        # Update cache
        synced_count = 0
        for connector_data in connectors_data:
            connector, created = ConnectorCache.objects.update_or_create(
                provider=provider,
                connector_id=connector_data['connector_id'],
                defaults={
                    'connector_name': connector_data['connector_name'],
                    'category': connector_data['category'],
                    'categories_json': connector_data.get('categories', [connector_data['category']]),
                    'logo_url': connector_data.get('logo_url'),
                    'source_url': connector_data.get('source_url'),
                    'icon_url': connector_data.get('logo_url'),  # Legacy fallback
                    'description': connector_data.get('description', ''),
                    'tools_json': connector_data['tools'],
                    'is_enabled': True,
                }
            )
            synced_count += 1
        
        return Response({
            'success': True,
            'message': f'Synced {synced_count} connectors',
            'connector_count': synced_count,
        })
        
    except MergeAPIError as e:
        logger.error(f"Failed to sync connectors: {e}")
        return Response(
            {'error': str(e)},
            status=status.HTTP_400_BAD_REQUEST
        )
    except ValueError as e:
        # Credentials not configured
        logger.error(f"Merge API not configured: {e}")
        return Response(
            {'error': str(e)},
            status=status.HTTP_503_SERVICE_UNAVAILABLE
        )


@api_view(['GET'])
@permission_classes([IsAuthenticated])
def list_connectors(request, pk):
    """
    List available connectors for an integration provider.
    
    GET /api/v1/integrations/:id/connectors/
    
    Returns cached connectors with their tools and organization connection status.
    Auto-syncs from Merge if no connectors are cached.
    """
    provider = get_object_or_404(MergeIntegrationProvider, pk=pk)
    
    # Verify user has access
    if not request.user.user_organizations.filter(organization=provider.organization).exists():
        return Response(
            {'error': 'Access denied'},
            status=status.HTTP_403_FORBIDDEN
        )
    
    # Get cached connectors
    connectors = ConnectorCache.objects.filter(
        provider=provider,
        is_enabled=True,
    ).order_by('connector_name')
    
    # Auto-sync if no connectors cached and Merge is configured
    if not connectors.exists() and is_merge_configured():
        try:
            logger.info(f"Auto-syncing connectors for provider {pk}")
            connectors_data = merge_service.sync_all_connectors()
            
            for connector_data in connectors_data:
                ConnectorCache.objects.update_or_create(
                    provider=provider,
                    connector_id=connector_data['connector_id'],
                    defaults={
                        'connector_name': connector_data['connector_name'],
                        'category': connector_data['category'],
                        'categories_json': connector_data.get('categories', [connector_data['category']]),
                        'logo_url': connector_data.get('logo_url'),
                        'source_url': connector_data.get('source_url'),
                        'icon_url': connector_data.get('logo_url'),  # Legacy fallback
                        'description': connector_data.get('description', ''),
                        'tools_json': connector_data['tools'],
                        'is_enabled': True,
                    }
                )
            
            # Re-fetch connectors after sync
            connectors = ConnectorCache.objects.filter(
                provider=provider,
                is_enabled=True,
            ).order_by('connector_name')
            
        except (MergeAPIError, ValueError) as e:
            logger.warning(f"Failed to auto-sync connectors: {e}")
    
    # Get authenticated connectors from Merge API (source of truth)
    # Only connectors in this list are truly connected
    authenticated_connector_ids = set()
    if provider.merge_registered_user_id and is_merge_configured():
        try:
            authenticated_connector_ids = set(merge_service.get_organization_connections(provider))
            logger.debug(f"Authenticated connectors from Merge: {authenticated_connector_ids}")
        except MergeAPIError as e:
            logger.warning(f"Failed to fetch authenticated connectors from Merge: {e}")
    
    # Get local metadata (connected_by, connected_at) for display
    org_connections = {}
    for link in OrganizationConnectorLink.objects.filter(provider=provider).select_related('connector', 'connected_by'):
        org_connections[link.connector.connector_id] = {
            'connected_at': link.connected_at.isoformat() if link.connected_at else None,
            'connected_by': link.connected_by.email if link.connected_by else None,
        }
    
    # Build response - is_connected comes from Merge API, not local DB
    result = []
    for connector in connectors:
        connector_id = connector.connector_id
        is_connected = connector_id in authenticated_connector_ids
        local_meta = org_connections.get(connector_id, {})
        
        result.append({
            'id': str(connector.id),
            'connector_id': connector_id,
            'name': connector.connector_name,
            'category': connector.category,
            'categories': connector.categories_json or [connector.category],
            'logo_url': connector.logo_url or connector.icon_url,
            'icon_url': connector.logo_url or connector.icon_url,  # Backwards compat
            'source_url': connector.source_url,
            'description': connector.description,
            'is_connected': is_connected,  # From Merge API, not local DB
            'connected_at': local_meta.get('connected_at') if is_connected else None,
            'connected_by': local_meta.get('connected_by') if is_connected else None,
            'tool_count': len(connector.tools_json or []),
            'tools': connector.tools_json or [],
        })
    
    return Response({'connectors': result})


@api_view(['POST'])
@permission_classes([IsAuthenticated])
def generate_link_token(request, pk):
    """
    Generate an OAuth link token for connecting the organization to an integration.
    
    POST /api/v1/integrations/:id/link-token/
    
    Request body:
    {
        "connector_id": "jira"  // Required - specific connector to link
    }
    
    Returns:
    {
        "link_token": "...",
        "expires_in": 3600
    }
    
    Admin only.
    """
    provider = get_object_or_404(MergeIntegrationProvider, pk=pk)
    
    # Verify user is admin (integration management requires admin)
    if not require_admin_for_integrations(request, provider.organization):
        return Response(
            {'error': 'Only admins can manage integrations'},
            status=status.HTTP_403_FORBIDDEN
        )
    
    connector_id = request.data.get('connector_id')
    
    if not connector_id:
        return Response(
            {'error': 'connector_id is required. Please select a specific integration to connect.'},
            status=status.HTTP_400_BAD_REQUEST
        )
    
    # Check if Merge is configured
    if not is_merge_configured():
        return Response(
            {'error': 'Integrations are not available. Merge API credentials are not configured.'},
            status=status.HTTP_503_SERVICE_UNAVAILABLE
        )
    
    try:
        # Generate link token (will auto-register org if needed)
        link_token = merge_service.generate_link_token(
            provider=provider,
            connector_id=connector_id,
        )
        
        return Response({
            'link_token': link_token,
            'expires_in': 3600,  # 1 hour typical expiry
        })
        
    except ValueError as e:
        return Response(
            {'error': str(e)},
            status=status.HTTP_400_BAD_REQUEST
        )
    except MergeAPIError as e:
        logger.error(f"Failed to generate link token: {e}")
        return Response(
            {'error': str(e)},
            status=status.HTTP_400_BAD_REQUEST
        )


@api_view(['GET'])
@permission_classes([IsAuthenticated])
def org_connector_status(request, pk):
    """
    Get the organization's connection status for all connectors.
    
    GET /api/v1/integrations/:id/connections/
    
    Connection status is determined by Merge API's authenticated_connectors list,
    which is the source of truth for what's actually connected.
    
    Returns:
    {
        "connections": [
            {
                "connector_id": "jira",
                "connector_name": "Jira",
                "is_connected": true,
                "connected_at": "2024-01-15T10:30:00Z",
                "connected_by": "admin@company.com"
            },
            ...
        ]
    }
    """
    provider = get_object_or_404(MergeIntegrationProvider, pk=pk)
    
    # Verify user has access
    if not request.user.user_organizations.filter(organization=provider.organization).exists():
        return Response(
            {'error': 'Access denied'},
            status=status.HTTP_403_FORBIDDEN
        )
    
    # Get authenticated connectors from Merge API (source of truth)
    authenticated_connector_ids = set()
    if provider.merge_registered_user_id and is_merge_configured():
        try:
            authenticated_connector_ids = set(merge_service.get_organization_connections(provider))
            logger.debug(f"Authenticated connectors from Merge: {authenticated_connector_ids}")
            
            # Sync local records with Merge state
            for connector_id in authenticated_connector_ids:
                try:
                    connector = ConnectorCache.objects.get(
                        provider=provider,
                        connector_id=connector_id,
                    )
                    link, created = OrganizationConnectorLink.objects.get_or_create(
                        provider=provider,
                        connector=connector,
                    )
                    if not link.is_connected:
                        link.mark_connected()
                except ConnectorCache.DoesNotExist:
                    pass
            
            # Mark disconnected connectors locally
            OrganizationConnectorLink.objects.filter(
                provider=provider,
                is_connected=True,
            ).exclude(
                connector__connector_id__in=authenticated_connector_ids
            ).update(is_connected=False, connected_at=None, connected_by=None)
            
        except MergeAPIError as e:
            logger.warning(f"Failed to fetch connections from Merge: {e}")
    
    # Get local metadata for display
    org_connections = {}
    for link in OrganizationConnectorLink.objects.filter(provider=provider).select_related('connector', 'connected_by'):
        org_connections[link.connector.connector_id] = {
            'connected_at': link.connected_at.isoformat() if link.connected_at else None,
            'connected_by': link.connected_by.email if link.connected_by else None,
        }
    
    # Build response - is_connected comes from Merge API
    connections = []
    for connector in ConnectorCache.objects.filter(provider=provider, is_enabled=True):
        connector_id = connector.connector_id
        is_connected = connector_id in authenticated_connector_ids
        local_meta = org_connections.get(connector_id, {})
        
        connections.append({
            'connector_id': connector_id,
            'connector_name': connector.connector_name,
            'is_connected': is_connected,  # From Merge API
            'connected_at': local_meta.get('connected_at') if is_connected else None,
            'connected_by': local_meta.get('connected_by') if is_connected else None,
        })
    
    return Response({'connections': connections})


@api_view(['POST'])
@permission_classes([IsAuthenticated])
def handle_link_callback(request, pk):
    """
    Handle callback after user completes OAuth flow.
    
    POST /api/v1/integrations/:id/link-callback/
    
    This is called by the frontend after an org member completes the Link flow.
    It verifies with the Merge API that the connector is actually authenticated
    before marking it as connected locally.
    
    Request body:
    {
        "connector_id": "jira"
    }
    
    The connection is only marked as successful if the connector appears in
    the authenticated_connectors list from Merge API.
    
    Admin only.
    """
    provider = get_object_or_404(MergeIntegrationProvider, pk=pk)
    
    # Verify user is admin (integration management requires admin)
    if not require_admin_for_integrations(request, provider.organization):
        return Response(
            {'error': 'Only admins can manage integrations'},
            status=status.HTTP_403_FORBIDDEN
        )
    
    connector_id = request.data.get('connector_id')
    if not connector_id:
        return Response(
            {'error': 'connector_id is required'},
            status=status.HTTP_400_BAD_REQUEST
        )
    
    try:
        connector = ConnectorCache.objects.get(
            provider=provider,
            connector_id=connector_id,
        )
        
        # Verify with Merge API that the connector is actually authenticated
        # This is the source of truth - only mark as connected if Merge confirms it
        is_authenticated = False
        try:
            authenticated_connectors = merge_service.get_organization_connections(provider)
            is_authenticated = connector_id in authenticated_connectors
            logger.info(
                f"Link callback for {connector_id}: authenticated={is_authenticated}, "
                f"authenticated_connectors={authenticated_connectors}"
            )
        except MergeAPIError as e:
            logger.warning(f"Failed to verify connection with Merge: {e}")
            # If we can't verify, don't mark as connected
            return Response({
                'success': False,
                'connector_id': connector_id,
                'is_connected': False,
                'error': 'Unable to verify connection status with integration provider',
            }, status=status.HTTP_503_SERVICE_UNAVAILABLE)
        
        if not is_authenticated:
            # OAuth flow was not completed or failed
            return Response({
                'success': False,
                'connector_id': connector_id,
                'is_connected': False,
                'message': 'Connection not completed. Please try again.',
            })
        
        # Only mark as connected if Merge confirms the authentication
        link, created = OrganizationConnectorLink.objects.get_or_create(
            provider=provider,
            connector=connector,
        )
        link.mark_connected(user=request.user)
        
        return Response({
            'success': True,
            'connector_id': connector_id,
            'is_connected': True,
            'connected_at': link.connected_at.isoformat(),
            'connected_by': request.user.email,
        })
        
    except ConnectorCache.DoesNotExist:
        return Response(
            {'error': f'Connector "{connector_id}" not found'},
            status=status.HTTP_404_NOT_FOUND
        )


@api_view(['GET'])
@permission_classes([IsAuthenticated])
def get_tool_pack_info(request, pk):
    """
    Get information about the configured tool pack.
    
    GET /api/v1/integrations/:id/tool-pack/
    
    Returns tool pack details including available connectors.
    """
    provider = get_object_or_404(MergeIntegrationProvider, pk=pk)
    
    # Verify user has access
    if not request.user.user_organizations.filter(organization=provider.organization).exists():
        return Response(
            {'error': 'Access denied'},
            status=status.HTTP_403_FORBIDDEN
        )
    
    if not is_merge_configured():
        return Response(
            {'error': 'Integrations are not configured'},
            status=status.HTTP_503_SERVICE_UNAVAILABLE
        )
    
    try:
        tool_pack = merge_service.get_tool_pack_info()
        return Response(tool_pack)
    except MergeAPIError as e:
        return Response(
            {'error': str(e)},
            status=status.HTTP_400_BAD_REQUEST
        )


@api_view(['GET'])
@permission_classes([IsAuthenticated])
def get_mcp_config(request, pk):
    """
    Get MCP (Model Context Protocol) configuration for the organization.
    
    GET /api/v1/integrations/:id/mcp/
    
    MCP provides a standardized way for AI models to access tools.
    Returns the list of available tools based on authenticated connectors.
    """
    provider = get_object_or_404(MergeIntegrationProvider, pk=pk)
    
    # Verify user has access
    if not request.user.user_organizations.filter(organization=provider.organization).exists():
        return Response(
            {'error': 'Access denied'},
            status=status.HTTP_403_FORBIDDEN
        )
    
    if not is_merge_configured():
        return Response(
            {'error': 'Integrations are not configured'},
            status=status.HTTP_503_SERVICE_UNAVAILABLE
        )
    
    mcp_config = merge_service.get_mcp_config(provider)
    return Response(mcp_config)


@api_view(['GET'])
@permission_classes([IsAuthenticated])
def list_mcp_tools(request, pk):
    """
    List available MCP tools for the organization.
    
    GET /api/v1/integrations/:id/mcp/tools/
    
    Returns tools available based on the organization's authenticated connectors.
    Uses JSON-RPC tools/list method internally.
    """
    provider = get_object_or_404(MergeIntegrationProvider, pk=pk)
    
    # Verify user has access
    if not request.user.user_organizations.filter(organization=provider.organization).exists():
        return Response(
            {'error': 'Access denied'},
            status=status.HTTP_403_FORBIDDEN
        )
    
    if not is_merge_configured():
        return Response(
            {'error': 'Integrations are not configured'},
            status=status.HTTP_503_SERVICE_UNAVAILABLE
        )
    
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


@api_view(['POST'])
@permission_classes([IsAuthenticated])
def call_mcp_tool(request, pk):
    """
    Execute an MCP tool.
    
    POST /api/v1/integrations/:id/mcp/call/
    
    Request body:
    {
        "tool_name": "jira_create_issue",
        "arguments": { ... }
    }
    
    Returns:
    {
        "success": true,
        "content": { ... }
    }
    """
    provider = get_object_or_404(MergeIntegrationProvider, pk=pk)
    
    # Verify user has access
    if not request.user.user_organizations.filter(organization=provider.organization).exists():
        return Response(
            {'error': 'Access denied'},
            status=status.HTTP_403_FORBIDDEN
        )
    
    if not is_merge_configured():
        return Response(
            {'error': 'Integrations are not configured'},
            status=status.HTTP_503_SERVICE_UNAVAILABLE
        )
    
    if not provider.merge_registered_user_id:
        return Response(
            {'error': 'Organization is not registered with Merge'},
            status=status.HTTP_400_BAD_REQUEST
        )
    
    tool_name = request.data.get('tool_name')
    arguments = request.data.get('arguments', {})
    
    if not tool_name:
        return Response(
            {'error': 'tool_name is required'},
            status=status.HTTP_400_BAD_REQUEST
        )
    
    try:
        result = merge_service.mcp_call_tool(provider, tool_name, arguments)
        
        if result.success:
            return Response({
                'success': True,
                'content': result.content,
            })
        else:
            return Response({
                'success': False,
                'error': result.error_message or 'Tool execution failed',
            }, status=status.HTTP_400_BAD_REQUEST)
            
    except MergeAPIError as e:
        logger.error(f"Failed to call MCP tool: {e}")
        return Response(
            {'success': False, 'error': str(e)},
            status=status.HTTP_400_BAD_REQUEST
        )
