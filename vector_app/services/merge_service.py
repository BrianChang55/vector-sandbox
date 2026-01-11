"""
Merge Agent Handler API Service.

Internal client for communicating with the Merge Agent Handler API.
This service is not exposed directly to users - it's abstracted via
connector_views.py which uses user-friendly naming.

Credentials are read from environment variables (settings.py):
- MERGE_TOOL_PACK_ID: The tool pack ID from Merge Agent Handler dashboard
- MERGE_ACCESS_KEY: The production access key for API authentication

API Documentation: https://docs.ah.merge.dev/api-reference/overview
"""
import logging
import time
import uuid
from typing import Any, Dict, List, Optional, TYPE_CHECKING
from dataclasses import dataclass

import httpx
from django.conf import settings

if TYPE_CHECKING:
    from vector_app.models import (
        MergeIntegrationProvider,
        ConnectorCache,
    )

logger = logging.getLogger(__name__)


def get_merge_credentials() -> tuple[str, str]:
    """
    Get Merge API credentials from settings.
    
    Returns:
        Tuple of (tool_pack_id, access_key)
        
    Raises:
        ValueError: If credentials are not configured
    """
    tool_pack_id = getattr(settings, 'MERGE_TOOL_PACK_ID', '')
    access_key = getattr(settings, 'MERGE_ACCESS_KEY', '')
    
    if not tool_pack_id or not access_key:
        raise ValueError(
            "Merge API credentials not configured. "
            "Set MERGE_TOOL_PACK_ID and MERGE_ACCESS_KEY in environment variables."
        )
    
    return tool_pack_id, access_key


def is_merge_configured() -> bool:
    """Check if Merge API credentials are configured."""
    tool_pack_id = getattr(settings, 'MERGE_TOOL_PACK_ID', '')
    access_key = getattr(settings, 'MERGE_ACCESS_KEY', '')
    return bool(tool_pack_id and access_key)


@dataclass
class MergeAPIError(Exception):
    """Exception raised when Merge API returns an error."""
    status_code: int
    message: str
    details: Optional[Dict[str, Any]] = None
    
    def __str__(self):
        return f"Merge API Error ({self.status_code}): {self.message}"


@dataclass
class ConnectorInfo:
    """Information about a connector from Merge Tool Pack API."""
    id: str  # slug (e.g., "linear", "jira")
    name: str  # Display name (e.g., "Linear", "Jira")
    categories: List[str]  # Category tags (e.g., ["Project Management"])
    logo_url: Optional[str] = None  # Logo image URL
    source_url: Optional[str] = None  # Source website URL (e.g., "https://linear.app")
    description: Optional[str] = None
    tools: Optional[List[Dict[str, Any]]] = None  # List of available tools
    
    @property
    def category(self) -> str:
        """Return first category for backwards compatibility."""
        return self.categories[0] if self.categories else 'other'


@dataclass
class ToolInfo:
    """Information about a tool available on a connector."""
    id: str
    name: str
    description: str
    parameters: Dict[str, Any]


@dataclass
class ToolExecutionResult:
    """Result of executing a tool."""
    success: bool
    data: Any
    error: Optional[str] = None


@dataclass
class MCPToolResult:
    """Result from MCP tools/call."""
    success: bool
    content: Any
    is_error: bool = False
    error_message: Optional[str] = None


class MergeAgentHandlerService:
    """
    Internal client for Merge Agent Handler API.
    
    This service handles all communication with Merge's API including:
    - Registering users (POST /registered-users)
    - Generating OAuth link tokens (POST /registered-users/{id}/link-token)
    - Listing tools via MCP (POST /tool-packs/{id}/registered-users/{id}/mcp with tools/list)
    - Executing tools via MCP (POST /tool-packs/{id}/registered-users/{id}/mcp with tools/call)
    
    API Reference: https://docs.ah.merge.dev/api-reference/overview
    
    Credentials are read from settings (environment variables), not from
    the MergeIntegrationProvider model. The provider is only used for
    storing organization-specific data like the registered_user_id.
    """
    
    BASE_URL = "https://ah-api.merge.dev/api/v1"
    DEFAULT_TIMEOUT = 30.0
    
    def __init__(self):
        self.timeout = getattr(settings, 'MERGE_API_TIMEOUT', self.DEFAULT_TIMEOUT)
        self._jsonrpc_id = 0
    
    def _next_jsonrpc_id(self) -> int:
        """Generate next JSON-RPC request ID."""
        self._jsonrpc_id += 1
        return self._jsonrpc_id
    
    def _get_credentials(self) -> tuple[str, str]:
        """
        Get API credentials from settings.
        
        Returns:
            Tuple of (tool_pack_id, access_key)
        """
        return get_merge_credentials()
    
    def _get_headers(self) -> Dict[str, str]:
        """Build authorization headers for Merge API requests."""
        _, access_key = self._get_credentials()
        return {
            "Authorization": f"Bearer {access_key}",
            "Content-Type": "application/json",
        }
    
    def _get_tool_pack_id(self) -> str:
        """Get the tool pack ID from settings."""
        tool_pack_id, _ = self._get_credentials()
        return tool_pack_id
    
    def _make_request(
        self,
        method: str,
        endpoint: str,
        data: Optional[Dict] = None,
        params: Optional[Dict] = None,
    ) -> Dict[str, Any]:
        """
        Make an HTTP request to the Merge API.
        
        Args:
            method: HTTP method (GET, POST, etc.)
            endpoint: API endpoint path (without base URL)
            data: JSON body for POST/PUT requests
            params: Query parameters
            
        Returns:
            Parsed JSON response
            
        Raises:
            MergeAPIError: If the API returns an error
        """
        url = f"{self.BASE_URL}{endpoint}"
        headers = self._get_headers()
        
        logger.debug(f"Merge API request: {method} {url}")
        if data:
            logger.debug(f"Request body: {data}")
        
        try:
            with httpx.Client(timeout=self.timeout, follow_redirects=True) as client:
                response = client.request(
                    method=method,
                    url=url,
                    headers=headers,
                    json=data,
                    params=params,
                )
                
                # Try to parse JSON response
                try:
                    response_data = response.json() if response.content else {}
                except Exception:
                    # Response is not JSON - could be HTML error page or empty
                    logger.error(
                        f"Merge API returned non-JSON response: {response.status_code} - "
                        f"{response.text[:200] if response.text else 'empty'}"
                    )
                    raise MergeAPIError(
                        status_code=response.status_code,
                        message=f"Invalid response from Merge API (status {response.status_code})",
                        details={'raw_response': response.text[:500] if response.text else None},
                    )
                
                if response.status_code >= 400:
                    raise MergeAPIError(
                        status_code=response.status_code,
                        message=response_data.get('error', response_data.get('message', 'Unknown error')),
                        details=response_data,
                    )
                
                return response_data
                
        except httpx.RequestError as e:
            logger.error(f"Merge API request failed: {e}")
            raise MergeAPIError(
                status_code=0,
                message=f"Network error: {str(e)}",
            )
    
    # =========================================================================
    # Organization Registration
    # API: POST /api/v1/registered-users
    # Docs: https://docs.ah.merge.dev/api-reference/registered-users/create-registered-user
    # =========================================================================
    
    def register_organization(
        self, 
        provider: 'MergeIntegrationProvider',
    ) -> str:
        """
        Register an organization with Merge Agent Handler.
        
        The organization is treated as a single "user" in Merge's system.
        All organization members share the connected integrations via
        the shared_credential_group.
        
        API: POST /api/v1/registered-users
        
        Args:
            provider: The integration provider (contains org reference)
            
        Returns:
            Merge's registered user ID (stored on provider)
        """
        org = provider.organization
        
        # Build request per API spec:
        # https://docs.ah.merge.dev/api-reference/registered-users/create-registered-user
        response = self._make_request(
            method="POST",
            endpoint="/registered-users",
            data={
                "origin_user_id": f"org_{org.id}",
                "origin_user_name": org.name,
                "shared_credential_group": {
                    "origin_company_id": str(org.id),
                    "origin_company_name": org.name,
                },
                "user_type": "SYSTEM",  # SYSTEM for organization-level access
            },
        )
        
        registered_user_id = response.get('id', '')
        
        if not registered_user_id:
            raise MergeAPIError(
                status_code=0,
                message="No 'id' returned from registered-users endpoint",
                details=response,
            )
        
        # Store the registered user ID on the provider
        provider.merge_registered_user_id = registered_user_id
        provider.save(update_fields=['merge_registered_user_id', 'updated_at'])
        
        logger.info(f"Registered organization '{org.name}' with Merge. User ID: {registered_user_id}")
        
        return registered_user_id
    
    def ensure_organization_registered(
        self,
        provider: 'MergeIntegrationProvider',
    ) -> str:
        """
        Ensure the organization is registered with Merge.
        
        If not already registered, registers and stores the ID.
        
        Args:
            provider: The integration provider
            
        Returns:
            The organization's registered user ID
        """
        if provider.merge_registered_user_id:
            return provider.merge_registered_user_id
        
        return self.register_organization(provider)
    
    def get_registered_user(
        self,
        registered_user_id: str,
    ) -> Dict[str, Any]:
        """
        Get details of a registered user including authenticated_connectors.
        
        API: GET /api/v1/registered-users/{id}
        
        Args:
            registered_user_id: The Merge registered user ID
            
        Returns:
            Registered user details including authenticated_connectors list
        """
        response = self._make_request(
            method="GET",
            endpoint=f"/registered-users/{registered_user_id}",
        )
        return response
    
    # =========================================================================
    # OAuth Link Flow
    # API: POST /api/v1/registered-users/{registered_user_id}/link-token
    # Docs: https://docs.ah.merge.dev/get-started/setup-link
    # =========================================================================
    
    def generate_link_token(
        self,
        provider: 'MergeIntegrationProvider',
        connector_id: str,
    ) -> str:
        """
        Generate an OAuth link token for the organization.
        
        This token is used by the frontend to initiate the OAuth flow
        for connecting the organization to a third-party service.
        All org members share the resulting connection via shared_credential_group.
        
        API: POST /api/v1/registered-users/{registered_user_id}/link-token
        
        Args:
            provider: The integration provider (must be registered)
            connector_id: The connector slug to link (e.g., "jira", "slack")
            
        Returns:
            Link token string for the frontend
            
        Raises:
            ValueError: If connector_id is not provided
            MergeAPIError: If the API request fails
        """
        if not connector_id:
            raise ValueError("connector_id is required to generate a link token")
        
        # Ensure org is registered
        registered_user_id = self.ensure_organization_registered(provider)
        
        response = self._make_request(
            method="POST",
            endpoint=f"/registered-users/{registered_user_id}/link-token",
            data={'connector': connector_id},  # Note: 'connector' not 'connector_id'
        )
        
        link_token = response.get('link_token', response.get('token', ''))
        
        if not link_token:
            raise MergeAPIError(
                status_code=0,
                message="No link_token returned from API",
                details=response,
            )
        
        logger.info(f"Generated link token for connector '{connector_id}' (org: {provider.organization.name})")
        
        return link_token
    
    def get_organization_connections(
        self,
        provider: 'MergeIntegrationProvider',
    ) -> List[str]:
        """
        Get the list of connectors the organization has connected.
        
        Uses the authenticated_connectors field from the registered user.
        
        Args:
            provider: The integration provider
            
        Returns:
            List of connector slugs that are authenticated
        """
        if not provider.merge_registered_user_id:
            return []
        
        try:
            user_data = self.get_registered_user(provider.merge_registered_user_id)
            return user_data.get('authenticated_connectors', [])
        except MergeAPIError as e:
            logger.warning(f"Failed to get organization connections: {e}")
            return []
    
    # =========================================================================
    # MCP (Model Context Protocol) Integration
    # API: POST /api/v1/tool-packs/{tool_pack_id}/registered-users/{registered_user_id}/mcp
    # Docs: https://docs.ah.merge.dev/api-reference/mcp/mcp-endpoint--list-tools-%26-call-tool
    # =========================================================================
    
    def _mcp_request(
        self,
        provider: 'MergeIntegrationProvider',
        method: str,
        params: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Make an MCP JSON-RPC request.
        
        API: POST /api/v1/tool-packs/{tool_pack_id}/registered-users/{registered_user_id}/mcp
        
        Args:
            provider: The integration provider
            method: JSON-RPC method (e.g., "tools/list", "tools/call")
            params: Method parameters
            
        Returns:
            JSON-RPC result
            
        Raises:
            MergeAPIError: If the request fails or returns an error
        """
        if not provider.merge_registered_user_id:
            raise MergeAPIError(
                status_code=0,
                message="Organization is not registered with Merge",
            )
        
        tool_pack_id = self._get_tool_pack_id()
        registered_user_id = provider.merge_registered_user_id
        
        # Build JSON-RPC 2.0 request
        jsonrpc_request = {
            "jsonrpc": "2.0",
            "id": self._next_jsonrpc_id(),
            "method": method,
        }
        
        if params:
            jsonrpc_request["params"] = params
        
        response = self._make_request(
            method="POST",
            endpoint=f"/tool-packs/{tool_pack_id}/registered-users/{registered_user_id}/mcp",
            data=jsonrpc_request,
        )
        
        # Check for JSON-RPC error
        if "error" in response:
            error = response["error"]
            raise MergeAPIError(
                status_code=error.get("code", -1),
                message=error.get("message", "Unknown MCP error"),
                details=error.get("data"),
            )
        
        return response.get("result", response)
    
    def mcp_list_tools(
        self,
        provider: 'MergeIntegrationProvider',
    ) -> List[Dict[str, Any]]:
        """
        List all available tools via MCP tools/list.
        
        Returns tools that the organization can use based on their
        authenticated connectors.
        
        API: POST /mcp with method "tools/list"
        
        Args:
            provider: The integration provider
            
        Returns:
            List of available tools with their schemas
        """
        result = self._mcp_request(provider, "tools/list")
        
        # MCP tools/list returns {tools: [...]}
        return result.get("tools", [])
    
    def mcp_call_tool(
        self,
        provider: 'MergeIntegrationProvider',
        tool_name: str,
        arguments: Dict[str, Any],
    ) -> MCPToolResult:
        """
        Execute a tool via MCP tools/call.
        
        API: POST /mcp with method "tools/call"
        
        Args:
            provider: The integration provider
            tool_name: Full tool name (e.g., "jira_create_issue")
            arguments: Tool arguments
            
        Returns:
            MCPToolResult with the execution result
        """
        start_time = time.time()
        
        try:
            result = self._mcp_request(
                provider,
                "tools/call",
                params={
                    "name": tool_name,
                    "arguments": arguments,
                },
            )
            
            duration_ms = int((time.time() - start_time) * 1000)
            logger.info(
                f"MCP tool executed: {tool_name} "
                f"(org: {provider.organization.name}, duration: {duration_ms}ms)"
            )
            
            # MCP tools/call returns {content: [...], isError: bool}
            is_error = result.get("isError", False)
            content = result.get("content", result)
            
            return MCPToolResult(
                success=not is_error,
                content=content,
                is_error=is_error,
            )
            
        except MergeAPIError as e:
            duration_ms = int((time.time() - start_time) * 1000)
            logger.error(
                f"MCP tool execution failed: {tool_name} "
                f"(org: {provider.organization.name}, error: {e.message}, duration: {duration_ms}ms)"
            )
            
            return MCPToolResult(
                success=False,
                content=None,
                is_error=True,
                error_message=e.message,
            )
    
    # =========================================================================
    # Tool Execution (Legacy wrapper - uses MCP internally)
    # =========================================================================
    
    def execute_tool(
        self,
        provider: 'MergeIntegrationProvider',
        connector_id: str,
        tool_id: str,
        params: Dict[str, Any],
    ) -> ToolExecutionResult:
        """
        Execute a tool using the organization's connected credentials.
        
        This is a convenience wrapper that internally uses MCP tools/call.
        The tool_name is constructed as "{connector_id}_{tool_id}".
        
        Args:
            provider: The integration provider (must be registered and connected)
            connector_id: ID of the connector (e.g., "jira")
            tool_id: ID of the tool to execute (e.g., "create_issue")
            params: Parameters for the tool
            
        Returns:
            ToolExecutionResult with the result or error
        """
        if not provider.merge_registered_user_id:
            return ToolExecutionResult(
                success=False,
                data=None,
                error="Organization is not registered with the integration provider",
            )
        
        # Construct MCP tool name
        # Format may vary - try common patterns
        tool_name = f"{connector_id}_{tool_id}"
        
        result = self.mcp_call_tool(provider, tool_name, params)
        
        return ToolExecutionResult(
            success=result.success,
            data=result.content if result.success else None,
            error=result.error_message if not result.success else None,
        )
    
    # =========================================================================
    # Connector/Tool Discovery
    # API: GET /api/v1/tool-packs/{id}
    # Docs: https://docs.ah.merge.dev/api-reference/tool-packs/list-tool-packs
    # =========================================================================
    
    def list_connectors(self) -> List[ConnectorInfo]:
        """
        List all available connectors from the tool pack.
        
        Uses the Tool Pack API to get connectors with their tools, logos, and metadata.
        API: GET /api/v1/tool-packs/{tool_pack_id}
        
        Returns:
            List of available connectors with full metadata
        """
        tool_pack_id = self._get_tool_pack_id()
        
        # Fetch the tool pack which includes connectors
        response = self._make_request(
            method="GET",
            endpoint=f"/tool-packs/{tool_pack_id}",
        )
        
        connectors = []
        connector_list = response.get('connectors', [])
        
        for item in connector_list:
            if isinstance(item, dict):
                # Parse categories - can be a list or single value
                categories = item.get('categories', [])
                if isinstance(categories, str):
                    categories = [categories]
                if not categories:
                    categories = ['Other']
                
                # Parse tools
                tools = []
                for tool in item.get('tools', []):
                    tools.append({
                        'id': tool.get('name', ''),
                        'name': tool.get('name', ''),
                        'description': tool.get('description', ''),
                    })
                
                connectors.append(ConnectorInfo(
                    id=item.get('slug', item.get('id', '')),
                    name=item.get('name', ''),
                    categories=categories,
                    logo_url=item.get('logo_url', item.get('icon_url', item.get('square_image'))),
                    source_url=item.get('source_url', item.get('website_url')),
                    description=item.get('description', ''),
                    tools=tools,
                ))
        
        logger.info(f"Fetched {len(connectors)} connectors from tool pack {tool_pack_id}")
        return connectors
    
    def get_connector_tools(
        self, 
        connector_id: str,
    ) -> List[ToolInfo]:
        """
        Get available tools for a specific connector.
        
        First tries to get from cached connectors, then falls back to API.
        
        Args:
            connector_id: ID/slug of the connector (e.g., "jira")
            
        Returns:
            List of available tools
        """
        # Get all connectors and find the matching one
        connectors = self.list_connectors()
        for connector in connectors:
            if connector.id == connector_id and connector.tools:
                return [
                    ToolInfo(
                        id=t.get('id', t.get('name', '')),
                        name=t.get('name', ''),
                        description=t.get('description', ''),
                        parameters=t.get('parameters', t.get('input_schema', {})),
                    )
                    for t in connector.tools
                ]
        
        return []
    
    def sync_all_connectors(self) -> List[Dict[str, Any]]:
        """
        Sync all connectors and their tools from the Tool Pack API.
        
        This fetches all available connectors with their metadata and tools,
        which can then be cached in ConnectorCache.
        
        API: GET /api/v1/tool-packs/{tool_pack_id}
        
        Returns:
            List of connector dicts with tools and full metadata included
        """
        connectors = self.list_connectors()
        result = []
        
        for connector in connectors:
            result.append({
                'connector_id': connector.id,
                'connector_name': connector.name,
                'category': connector.category,  # Primary category
                'categories': connector.categories,  # All categories as tags
                'logo_url': connector.logo_url,
                'source_url': connector.source_url,
                'description': connector.description,
                'tools': connector.tools or [],
            })
        
        logger.info(f"Synced {len(result)} connectors from tool pack")
        return result
    
    def sync_connectors_from_mcp(
        self,
        provider: 'MergeIntegrationProvider',
    ) -> List[Dict[str, Any]]:
        """
        Sync connectors and tools from MCP tools/list endpoint.
        
        This returns the actual tools available to the organization
        based on their authenticated connectors.
        
        Args:
            provider: The integration provider
            
        Returns:
            List of tools with connector info
        """
        tools = self.mcp_list_tools(provider)
        
        # Group tools by connector
        connectors_map: Dict[str, Dict[str, Any]] = {}
        
        for tool in tools:
            # Parse connector from tool name (e.g., "jira_create_issue" -> "jira")
            tool_name = tool.get('name', '')
            parts = tool_name.split('_', 1)
            connector_id = parts[0] if parts else 'unknown'
            
            if connector_id not in connectors_map:
                connectors_map[connector_id] = {
                    'connector_id': connector_id,
                    'connector_name': connector_id.title(),
                    'category': 'other',
                    'tools': [],
                }
            
            connectors_map[connector_id]['tools'].append({
                'id': tool_name,
                'name': tool.get('name', tool_name),
                'description': tool.get('description', ''),
                'parameters': tool.get('inputSchema', {}),
            })
        
        return list(connectors_map.values())
    
    # =========================================================================
    # Tool Pack Info
    # =========================================================================
    
    def get_tool_pack_info(self) -> Dict[str, Any]:
        """
        Get information about the configured tool pack.
            
        Returns:
            Tool pack details including name, available connectors, etc.
        """
        tool_pack_id = self._get_tool_pack_id()
        
        response = self._make_request(
            method="GET",
            endpoint=f"/tool-packs/{tool_pack_id}",
        )
        
        return response
    
    def search_tools(
        self,
        query: str,
    ) -> List[Dict[str, Any]]:
        """
        Search for tools across all connectors.
        
        Args:
            query: Search query
            
        Returns:
            List of matching tools with connector info
        """
        tool_pack_id = self._get_tool_pack_id()
        
        response = self._make_request(
            method="GET",
            endpoint=f"/tool-search",
            params={
                "query": query,
                "tool_pack_id": tool_pack_id,
            },
        )
        
        return response.get('tools', response.get('results', []))
    
    # =========================================================================
    # Legacy MCP Config (for backwards compatibility)
    # =========================================================================
    
    def get_mcp_config(
        self,
        provider: 'MergeIntegrationProvider',
    ) -> Dict[str, Any]:
        """
        Get MCP configuration and available tools for an organization.
        
        This uses MCP tools/list to get the actual available tools.
        
        Args:
            provider: The integration provider
            
        Returns:
            MCP configuration including available tools
        """
        if not provider.merge_registered_user_id:
            return {
                'enabled': False,
                'error': 'Organization not registered',
            }
        
        try:
            tools = self.mcp_list_tools(provider)
            
            return {
                'enabled': True,
                'tools': tools,
                'tool_count': len(tools),
            }
        except MergeAPIError as e:
            logger.warning(f"Failed to get MCP config: {e}")
            return {
                'enabled': False,
                'error': str(e),
            }


# Singleton instance for convenience
merge_service = MergeAgentHandlerService()
