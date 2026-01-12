from typing import List
from vector_app.models import InternalApp, MergeIntegrationProvider
from vector_app.services.mcp_context import (
    MCPToolsContext,
    MCPToolDefinition,
    _build_no_merge_context,
    _build_no_provider_context,
    _build_not_registered_context,
    _generate_typescript_definitions,
    _build_full_context,
    _build_connectors_summary,
    _generate_tool_interface,
)
from vector_app.services.merge_service import is_merge_configured


def build_context_from_matched_tools(
    app: "InternalApp",
    tool_match_result,
) -> MCPToolsContext:
    """
    Build MCP tools context from already-matched tools.

    This uses the ToolMatchResult from tool_matcher to build a context
    containing only the relevant tools for the user's request.

    Args:
        app: The InternalApp to get MCP tools for
        tool_match_result: ToolMatchResult from tool_matcher.match_tools()

    Returns:
        MCPToolsContext with only the matched tools
    """

    # Same error handling as build_mcp_tools_context
    if not is_merge_configured():
        return _build_no_merge_context()

    try:
        provider = MergeIntegrationProvider.objects.get(
            organization=app.organization,
            is_active=True,
        )
    except MergeIntegrationProvider.DoesNotExist:
        return _build_no_provider_context()

    if not provider.merge_registered_user_id:
        return _build_not_registered_context()

    if not tool_match_result or not tool_match_result.matched_tools:
        return MCPToolsContext(
            has_tools=False,
            full_context="No matching MCP tools found for this operation.",
            provider_id=str(provider.id),
        )

    # Convert MatchedTool objects to MCPToolDefinition for compatibility
    tools = _convert_matched_tools_to_definitions(tool_match_result.matched_tools)

    # Generate TypeScript definitions (same helper as build_mcp_tools_context)
    typescript_defs = _generate_typescript_definitions(tools)

    # Build the full context string (same helper as build_mcp_tools_context)
    full_context = _build_full_context(
        tools,
        typescript_defs,
        str(provider.id),
        tool_match_result.connected_connectors,
    )

    # Build summary (same helper as build_mcp_tools_context)
    connectors_summary = _build_connectors_summary(tools, tool_match_result.connected_connectors)

    return MCPToolsContext(
        tools=tools,
        connectors_summary=connectors_summary,
        full_context=full_context,
        typescript_definitions=typescript_defs,
        has_tools=len(tools) > 0,
        provider_id=str(provider.id),
        authenticated_connectors=tool_match_result.connected_connectors,
    )


def _convert_matched_tools_to_definitions(matched_tools) -> List[MCPToolDefinition]:
    """Convert MatchedTool objects to MCPToolDefinition for compatibility."""
    tools = []
    for matched in matched_tools:
        tool_def = MCPToolDefinition(
            name=matched.tool_id,
            connector_id=matched.connector_id,
            description=matched.description,
            input_schema=matched.input_schema,
            typescript_interface=_generate_tool_interface(matched.tool_id, matched.input_schema),
        )
        tools.append(tool_def)
    return tools
