"""
MCP Tools Context Generator for LLM Consumption.

Provides functions to fetch live MCP tools from Merge and format them
for use in LLM prompts during agentic code generation.

Key differences from connectors_context.py:
- Fetches LIVE tools via mcp_list_tools() instead of cached tools_json
- Includes full inputSchema for each tool (critical for code generation)
- Generates TypeScript type definitions for tool parameters
- Creates ready-to-use code snippets
"""
import logging
from dataclasses import dataclass, field
from typing import Any, Dict, List

from vector_app.models import MergeIntegrationProvider, InternalApp
from vector_app.services.merge_service import merge_service, MergeAPIError, is_merge_configured, get_connected_connectors


logger = logging.getLogger(__name__)


@dataclass
class MCPToolDefinition:
    """Definition of a single MCP tool with full schema."""
    name: str                    # e.g., "hubspot_list_deals"
    connector_id: str            # e.g., "hubspot" (derived from name prefix)
    description: str
    input_schema: Dict[str, Any]  # Full JSON Schema for parameters
    typescript_interface: str = ""  # Generated TS interface for this tool's params


@dataclass
class MCPToolsContext:
    """Complete MCP tools context for LLM consumption."""
    tools: List[MCPToolDefinition] = field(default_factory=list)
    connectors_summary: str = ""      # Brief list for planning
    full_context: str = ""            # Rich description for code generation
    typescript_definitions: str = ""   # All TypeScript interfaces
    has_tools: bool = False
    provider_id: str = ""             # Provider ID for API calls
    authenticated_connectors: List[str] = field(default_factory=list)  # Already connected integrations


def build_mcp_tools_context(app: 'InternalApp') -> MCPToolsContext:
    """
    Build MCP tools context for LLM consumption.
    
    This function:
    1. Gets the organization's integration provider
    2. Fetches which connectors are already authenticated
    3. For authenticated connectors, fetches actual tools from tool pack API
    4. Formats tools with descriptions for code generation
    5. Generates TypeScript type definitions
    
    Note: We use the tool pack API (list_connectors) to get actual connector tools,
    not MCP tools/list which only returns authenticate_* tools.
    
    Args:
        app: The InternalApp to get MCP tools for
        
    Returns:
        MCPToolsContext with tools, summaries, and TypeScript definitions
    """
    # Check if Merge is configured
    if not is_merge_configured():
        return _build_no_merge_context()
    
    # Get the organization's integration provider
    try:
        provider = MergeIntegrationProvider.objects.get(
            organization=app.organization,
            is_active=True,
        )
    except MergeIntegrationProvider.DoesNotExist:
        return _build_no_provider_context()
    
    # Check if organization is registered
    if not provider.merge_registered_user_id:
        return _build_not_registered_context()
    
    # Fetch authenticated connectors from database
    authenticated_connectors = _get_authenticated_connectors(provider)
    
    if not authenticated_connectors:
        return _build_no_authenticated_context(str(provider.id))
    
    # Fetch connector tools from tool pack API (not MCP tools/list which only has authenticate_* tools)
    try:
        all_connectors = merge_service.list_connectors()
    except MergeAPIError as e:
        logger.warning("Failed to fetch connectors: %s", e)
        return _build_error_context(str(e))
    except Exception as e:
        logger.exception("Unexpected error fetching connectors: %s", e)
        return _build_error_context("Failed to fetch available connectors")
    
    if not all_connectors:
        return _build_no_tools_context(str(provider.id))
    
    # Build tools list from authenticated connectors only
    tools = _build_tools_from_connectors(all_connectors, authenticated_connectors)
    
    if not tools:
        return _build_no_tools_context(str(provider.id))
    
    # Generate TypeScript definitions
    typescript_defs = _generate_typescript_definitions(tools)
    
    # Build the full context string for LLM
    full_context = _build_full_context(tools, typescript_defs, str(provider.id), authenticated_connectors)
    
    # Build summary for planning phase
    connectors_summary = _build_connectors_summary(tools, authenticated_connectors)
    
    return MCPToolsContext(
        tools=tools,
        connectors_summary=connectors_summary,
        full_context=full_context,
        typescript_definitions=typescript_defs,
        has_tools=len(tools) > 0,
        provider_id=str(provider.id),
        authenticated_connectors=authenticated_connectors,
    )


def _build_tools_from_connectors(
    all_connectors: list,
    authenticated_connectors: List[str],
) -> List[MCPToolDefinition]:
    """
    Build MCPToolDefinition list from connector info for authenticated connectors.
    
    For each authenticated connector, extracts its tools using the exact tool names
    from the Merge API (e.g., "retrieve_balance", "list_customers").
    """
    tools = []
    
    for connector in all_connectors:
        connector_id = connector.id.lower()
        
        # Only include tools for authenticated connectors
        if connector_id not in authenticated_connectors:
            continue
        
        for tool_info in connector.tools:
            tool_id = tool_info.get('id', tool_info.get('name', ''))
            if not tool_id:
                continue
            
            # MCP tool names use the format: {connector}__{tool_id} (double underscore)
            # e.g., "stripe__retrieve_balance", "github__get_issue"
            tool_name = f"{connector_id}__{tool_id}"
            description = tool_info.get('description', f'{tool_id} operation for {connector.name}')
            
            # Build input schema from tool parameters if available
            params = tool_info.get('parameters', tool_info.get('input_schema', {}))
            if not params:
                params = {"type": "object", "properties": {}, "required": []}
            
            tools.append(MCPToolDefinition(
                name=tool_name,
                connector_id=connector_id,
                description=description,
                input_schema=params,
                typescript_interface=_generate_tool_interface(tool_name, params),
            ))
    
    return tools


def _get_authenticated_connectors(provider: MergeIntegrationProvider) -> List[str]:
    """
    Get list of already authenticated connector IDs.
    
    This now uses the shared helper which tries Merge API first (source of truth),
    then falls back to local database.
    
    Returns:
        List of connector IDs that are already authenticated (e.g., ['stripe', 'hubspot'])
    """
    return get_connected_connectors(provider)


def _parse_mcp_tools(raw_tools: List[Dict[str, Any]]) -> List[MCPToolDefinition]:
    """
    Parse raw MCP tools response into structured definitions.
    
    MCP tools/list returns tools in format:
    {
        "name": "hubspot_list_deals",
        "description": "List deals from Hubspot",
        "inputSchema": {
            "type": "object",
            "properties": {...},
            "required": [...]
        }
    }
    """
    tools = []
    
    for raw_tool in raw_tools:
        name = raw_tool.get('name', '')
        if not name:
            continue
        
        # Extract connector ID from tool name (e.g., "hubspot_list_deals" -> "hubspot")
        connector_id = name.split('_')[0] if '_' in name else 'unknown'
        
        description = raw_tool.get('description', '')
        input_schema = raw_tool.get('inputSchema', {})
        
        # Generate TypeScript interface for this tool
        ts_interface = _generate_tool_interface(name, input_schema)
        
        tools.append(MCPToolDefinition(
            name=name,
            connector_id=connector_id,
            description=description,
            input_schema=input_schema,
            typescript_interface=ts_interface,
        ))
    
    return tools


def _generate_tool_interface(tool_name: str, schema: Dict[str, Any]) -> str:
    """Generate TypeScript interface for a tool's parameters."""
    interface_name = _to_pascal_case(tool_name) + "Params"
    
    properties = schema.get('properties', {})
    required = set(schema.get('required', []))
    
    if not properties:
        return f"type {interface_name} = Record<string, any>;"
    
    lines = [f"interface {interface_name} {{"]
    
    for prop_name, prop_info in properties.items():
        is_required = prop_name in required
        ts_type = _json_schema_to_typescript(prop_info)
        optional_marker = "" if is_required else "?"
        
        # Add description as comment if available
        description = prop_info.get('description', '')
        if description:
            lines.append(f"  /** {description} */")
        
        lines.append(f"  {prop_name}{optional_marker}: {ts_type};")
    
    lines.append("}")
    
    return "\n".join(lines)


def _json_schema_to_typescript(schema: Dict[str, Any]) -> str:
    """Convert JSON Schema type to TypeScript type."""
    schema_type = schema.get('type', 'any')
    
    if schema_type == 'string':
        # Check for enum
        enum_values = schema.get('enum')
        if enum_values:
            return " | ".join(f'"{v}"' for v in enum_values)
        return 'string'
    elif schema_type == 'integer' or schema_type == 'number':
        return 'number'
    elif schema_type == 'boolean':
        return 'boolean'
    elif schema_type == 'array':
        items = schema.get('items', {})
        item_type = _json_schema_to_typescript(items)
        return f'{item_type}[]'
    elif schema_type == 'object':
        # Check for additional properties
        additional_props = schema.get('additionalProperties')
        if additional_props:
            value_type = _json_schema_to_typescript(additional_props)
            return f'Record<string, {value_type}>'
        return 'Record<string, any>'
    elif schema_type == 'null':
        return 'null'
    elif isinstance(schema_type, list):
        # Union type
        types = [_json_schema_to_typescript({'type': t}) for t in schema_type]
        return " | ".join(types)
    else:
        return 'any'


def _to_pascal_case(snake_str: str) -> str:
    """Convert snake_case to PascalCase."""
    components = snake_str.split('_')
    return ''.join(x.title() for x in components)


def _generate_typescript_definitions(tools: List[MCPToolDefinition]) -> str:
    """Generate all TypeScript definitions for the tools."""
    if not tools:
        return ""
    
    lines = [
        "// Auto-generated TypeScript definitions for MCP tools",
        "// These types help ensure correct parameter usage",
        "",
    ]
    
    # Group by connector
    by_connector: Dict[str, List[MCPToolDefinition]] = {}
    for tool in tools:
        if tool.connector_id not in by_connector:
            by_connector[tool.connector_id] = []
        by_connector[tool.connector_id].append(tool)
    
    for connector_id, connector_tools in sorted(by_connector.items()):
        lines.append(f"// === {connector_id.upper()} Tools ===")
        lines.append("")
        
        for tool in connector_tools:
            if tool.typescript_interface:
                lines.append(tool.typescript_interface)
                lines.append("")
    
    return "\n".join(lines)


def _build_full_context(
    tools: List[MCPToolDefinition], 
    typescript_defs: str,
    provider_id: str,
    authenticated_connectors: List[str] = None,
) -> str:
    """Build the complete context string for LLM consumption."""
    authenticated_connectors = authenticated_connectors or []
    
    lines = [
        "## Available MCP Integration Tools",
        "",
        "Your app can call external integration tools via the MCP (Model Context Protocol) API.",
        "These tools allow interaction with connected services like Hubspot, Jira, Slack, etc.",
        "",
    ]
    
    # Add authentication flow explanation
    lines.append("### ⚡ MCP Authentication Flow")
    lines.append("")
    lines.append("**IMPORTANT:** Before using integration tools, you must check if the connector is authenticated.")
    lines.append("MCP tools require authentication before functional tools are available.")
    lines.append("")
    lines.append("**Authentication Pattern:**")
    lines.append("```typescript")
    lines.append("// Step 1: Try to call the tool (use connector__tool format)")
    lines.append("const result = await mcpTools.call('stripe__retrieve_balance', {});")
    lines.append("")
    lines.append("// Step 2: If tool not found, initiate authentication")
    lines.append("if (!result.success && result.error?.includes('not found')) {")
    lines.append("  // Call authenticate_* to get a link token")
    lines.append("  const authResult = await mcpTools.call('authenticate_stripe', {});")
    lines.append("  if (authResult.success) {")
    lines.append("    // Show user a 'Connect Stripe' button that opens Merge Link with the token")
    lines.append("    const linkToken = authResult.data?.[0]?.text;")
    lines.append("    // Parse: JSON.parse(linkToken).link_token")
    lines.append("  }")
    lines.append("}")
    lines.append("```")
    lines.append("")
    
    # Show which connectors have org-level connections (for reference)
    if authenticated_connectors:
        lines.append("**Organization Connections:** The following connectors have been connected at the organization level:")
        lines.append("")
        for connector_id in sorted(authenticated_connectors):
            connector_name = connector_id.replace('_', ' ').title()
            lines.append(f"- {connector_name}")
        lines.append("")
        lines.append("These connectors may require MCP authentication to activate their tools in the app.")
        lines.append("")
    
    # Group tools by connector for better organization
    by_connector: Dict[str, List[MCPToolDefinition]] = {}
    for tool in tools:
        if tool.connector_id not in by_connector:
            by_connector[tool.connector_id] = []
        by_connector[tool.connector_id].append(tool)
    
    # List available tools by connector
    lines.append("### Available Tools by Integration")
    lines.append("")
    lines.append("*These tools become available after MCP authentication. Call `authenticate_<connector>` first if tools return 'not found'.*")
    lines.append("")
    
    for connector_id, connector_tools in sorted(by_connector.items()):
        connector_name = connector_id.replace('_', ' ').title()
        auth_tool = f"authenticate_{connector_id}"
        lines.append(f"**{connector_name}** ({len(connector_tools)} tools)")
        lines.append(f"  *Auth tool: `{auth_tool}`*")
        lines.append("")
        
        for tool in connector_tools[:15]:  # Limit to 15 tools per connector for prompt size
            lines.append(f"- `{tool.name}`: {tool.description[:100]}{'...' if len(tool.description) > 100 else ''}")
            
            # Add key parameters
            props = tool.input_schema.get('properties', {})
            required = tool.input_schema.get('required', [])
            if props:
                param_strs = []
                for prop_name in list(props.keys())[:5]:  # Show first 5 params
                    is_required = prop_name in required
                    prop_type = props[prop_name].get('type', 'any')
                    req_marker = " (required)" if is_required else ""
                    param_strs.append(f"`{prop_name}: {prop_type}{req_marker}`")
                if param_strs:
                    lines.append(f"  Parameters: {', '.join(param_strs)}")
        
        if len(connector_tools) > 15:
            lines.append(f"  ... and {len(connector_tools) - 15} more tools")
        lines.append("")
    
    # Add TypeScript definitions
    if typescript_defs:
        lines.append("### TypeScript Definitions")
        lines.append("")
        lines.append("```typescript")
        lines.append(typescript_defs)
        lines.append("```")
        lines.append("")
    
    # Add usage instructions
    lines.append("### Using MCP Tools in Code")
    lines.append("")
    lines.append("Use the `mcpTools` client from `./lib/mcpTools.ts` to call integration tools:")
    lines.append("")
    lines.append("```typescript")
    lines.append("import { mcpTools } from './lib/mcpTools';")
    lines.append("")
    lines.append("// Call any MCP tool by name")
    lines.append("const result = await mcpTools.call('tool_name', {")
    lines.append("  param1: 'value1',")
    lines.append("  param2: 'value2',")
    lines.append("});")
    lines.append("")
    lines.append("// Handle the response")
    lines.append("if (result.success) {")
    lines.append("  console.log('Data:', result.data);")
    lines.append("} else {")
    lines.append("  console.error('Error:', result.error);")
    lines.append("}")
    lines.append("```")
    lines.append("")
    
    # Add mcpTools.ts client template
    lines.append("### MCP Tools Client Library")
    lines.append("")
    lines.append("Generate this file at `src/lib/mcpTools.ts`:")
    lines.append("")
    lines.append("```typescript")
    lines.append(_get_mcp_tools_client_template(provider_id))
    lines.append("```")
    lines.append("")
    
    # Add common patterns
    lines.append("### Common Patterns")
    lines.append("")
    lines.append("```typescript")
    lines.append("// Fetching data with loading state")
    lines.append("const [data, setData] = useState([]);")
    lines.append("const [loading, setLoading] = useState(true);")
    lines.append("const [error, setError] = useState<string | null>(null);")
    lines.append("")
    lines.append("useEffect(() => {")
    lines.append("  async function fetchData() {")
    lines.append("    const result = await mcpTools.call('connector_list_items', { limit: 50 });")
    lines.append("    if (result.success) {")
    lines.append("      setData(result.data?.items || []);")
    lines.append("    } else {")
    lines.append("      setError(result.error || 'Failed to fetch data');")
    lines.append("    }")
    lines.append("    setLoading(false);")
    lines.append("  }")
    lines.append("  fetchData();")
    lines.append("}, []);")
    lines.append("```")
    lines.append("")
    
    return "\n".join(lines)


def _get_mcp_tools_client_template(provider_id: str) -> str:
    """Get the mcpTools.ts client library template."""
    return f'''/**
 * MCP Tools Client - Auto-generated
 * Provides typed access to external integration tools via MCP protocol
 */

interface MCPToolResult<T = any> {{
  success: boolean;
  data?: T;
  error?: string;
}}

const getConfig = () => {{
  const config = (window as any).__VECTOR_CONFIG__;
  return {{
    apiBaseUrl: config?.apiBaseUrl || '',
    appId: config?.appId || '',
    providerId: '{provider_id}',
  }};
}};

export const mcpTools = {{
  /**
   * Call an MCP tool by name
   * @param toolName - The full tool name (e.g., "hubspot_list_deals")
   * @param params - Tool-specific parameters
   */
  async call<T = any>(
    toolName: string,
    params: Record<string, any> = {{}}
  ): Promise<MCPToolResult<T>> {{
    const config = getConfig();
    
    try {{
      const response = await fetch(`${{config.apiBaseUrl}}/runtime/connectors/`, {{
        method: 'POST',
        headers: {{ 'Content-Type': 'application/json' }},
        body: JSON.stringify({{
          appId: config.appId,
          connectorId: '_meta',
          toolId: 'mcp_call',
          params: {{
            tool_name: toolName,
            arguments: params,
          }},
        }}),
      }});
      
      if (!response.ok) {{
        const errorData = await response.json().catch(() => ({{}}));
        return {{
          success: false,
          error: errorData.error || `HTTP ${{response.status}}`,
        }};
      }}
      
      return await response.json();
    }} catch (error) {{
      return {{
        success: false,
        error: error instanceof Error ? error.message : 'Network error',
      }};
    }}
  }},

  /**
   * List all available MCP tools
   */
  async listTools(): Promise<any[]> {{
    const config = getConfig();
    
    try {{
      const response = await fetch(`${{config.apiBaseUrl}}/runtime/connectors/`, {{
        method: 'POST',
        headers: {{ 'Content-Type': 'application/json' }},
        body: JSON.stringify({{
          appId: config.appId,
          connectorId: '_meta',
          toolId: 'mcp_tools',
        }}),
      }});
      
      const data = await response.json();
      return data.tools || [];
    }} catch (error) {{
      console.error('Failed to list MCP tools:', error);
      return [];
    }}
  }},
}};'''


def _build_connectors_summary(tools: List[MCPToolDefinition], authenticated_connectors: List[str] = None) -> str:
    """Build a brief summary for planning phase."""
    authenticated_connectors = authenticated_connectors or []
    
    if not tools:
        return "No MCP integration tools available."
    
    # Group by connector
    by_connector: Dict[str, int] = {}
    for tool in tools:
        by_connector[tool.connector_id] = by_connector.get(tool.connector_id, 0) + 1
    
    summaries = []
    for connector_id, count in sorted(by_connector.items()):
        connector_name = connector_id.replace('_', ' ').title()
        auth_status = "✅ AUTHENTICATED" if connector_id in authenticated_connectors else "Not connected"
        summaries.append(f"- {connector_name}: {count} tools [{auth_status}]")
    
    result = "Available MCP integrations:\n" + "\n".join(summaries)
    
    if authenticated_connectors:
        result += f"\n\n⚡ Ready-to-use (no auth needed): {', '.join(authenticated_connectors)}"
    
    return result


def get_mcp_tools_summary(app: 'InternalApp') -> str:
    """
    Get a brief summary of available MCP tools (for planning prompts).
    
    This is a lighter-weight version that just returns the summary string.
    """
    context = build_mcp_tools_context(app)
    return context.connectors_summary


# ============================================================================
# Context builders for various states
# ============================================================================

def _build_no_merge_context() -> MCPToolsContext:
    """Build context when Merge is not configured."""
    return MCPToolsContext(
        full_context="""## MCP Integration Tools

MCP integration tools are not available. The Merge API credentials are not configured.

To enable integrations like Hubspot, Jira, Slack, etc., configure MERGE_TOOL_PACK_ID 
and MERGE_ACCESS_KEY in the environment variables.
""",
        connectors_summary="MCP integrations not configured.",
    )


def _build_no_provider_context() -> MCPToolsContext:
    """Build context when no provider is configured for the org."""
    return MCPToolsContext(
        full_context="""## MCP Integration Tools

No integration provider is configured for this organization.

To enable MCP integration tools, configure an integration provider in the 
organization settings.
""",
        connectors_summary="No integration provider configured.",
    )


def _build_not_registered_context() -> MCPToolsContext:
    """Build context when org is not registered with Merge."""
    return MCPToolsContext(
        full_context="""## MCP Integration Tools

The organization has not completed the integration setup.

An organization member needs to connect at least one integration (Hubspot, Jira, etc.) 
in the Settings > Integrations page before MCP tools become available.
""",
        connectors_summary="Integration setup not complete.",
    )


def _build_no_tools_context(provider_id: str) -> MCPToolsContext:
    """Build context when no tools are available."""
    return MCPToolsContext(
        full_context="""## MCP Integration Tools

An integration provider is configured, but no tools are currently available.

This could mean:
- No integrations have been connected yet
- The connected integrations don't have any available tools
- There was an issue fetching tools from the integration provider

Connect integrations in Settings > Integrations to enable MCP tools.
""",
        connectors_summary="No MCP tools available.",
        provider_id=provider_id,
    )


def _build_no_authenticated_context(provider_id: str) -> MCPToolsContext:
    """Build context when no connectors are authenticated yet."""
    return MCPToolsContext(
        full_context="""## MCP Integration Tools

An integration provider is configured, but no integrations have been connected yet.

To use MCP integration tools (like Hubspot, Stripe, Jira, etc.), an organization 
member needs to connect at least one integration in the Settings > Integrations page.

Once connected, you'll have access to tools for reading and writing data to those 
external services directly from your generated application.
""",
        connectors_summary="No integrations connected yet. Connect integrations in Settings > Integrations.",
        provider_id=provider_id,
    )


def _build_error_context(error_message: str) -> MCPToolsContext:
    """Build context when there was an error fetching tools."""
    return MCPToolsContext(
        full_context=f"""## MCP Integration Tools

Failed to fetch available integration tools: {error_message}

You can still build the app without MCP tools. If the user requires integration 
functionality, suggest they check the Settings > Integrations page.
""",
        connectors_summary=f"Error fetching MCP tools: {error_message}",
    )

