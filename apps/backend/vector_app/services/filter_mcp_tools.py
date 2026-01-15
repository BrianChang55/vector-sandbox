"""
Filter MCP Tools Service

Handles action classification and tool matching for MCP tools.
Extracts user intent actions and matches them to available MCP tools.
"""

import logging
from dataclasses import dataclass
from typing import Any, Dict, Iterator, List, Optional, Union

from vector_app.action_classification.action_classifier import get_action_classifier
from vector_app.action_classification.build_mcp_context import build_context_from_matched_tools
from vector_app.action_classification.tool_matcher import get_tool_matcher, ToolMatchResult
from vector_app.action_classification.types import ActionResult, MatchedTool

from vector_app.models import InternalApp, ConnectorToolAction, MergeIntegrationProvider
from vector_app.services.context_analyzer import AppContext
from vector_app.services.intent_classifier import IntentResult
from vector_app.services.merge_service import get_connected_connectors

logger = logging.getLogger(__name__)


def _log_matched_tools_details(
    tool_match_result: ToolMatchResult,
    action_result: ActionResult,
    mcp_context_str: Optional[str] = None,
) -> None:
    """
    Helper to log detailed information about matched MCP tools.
    
    Args:
        tool_match_result: The result from tool matching
        action_result: The classified actions
        mcp_context_str: The full context string for LLM prompt (optional)
    """
    action_types_str = ", ".join(a.action.value for a in action_result.actions)
    
    # Log summary
    logger.info(
        "[MCP Tools] Matched %d tools for actions: %s",
        len(tool_match_result.matched_tools),
        action_types_str
    )
    logger.info(
        "[MCP Tools] Connected connectors: %s",
        ', '.join(tool_match_result.connected_connectors)
    )
    
    # Log each matched tool with full details
    for idx, tool in enumerate(tool_match_result.matched_tools, 1):
        logger.debug(
            "[MCP Tools] Tool %d/%d: %s.%s (action_type=%s)",
            idx,
            len(tool_match_result.matched_tools),
            tool.connector_name,
            tool.tool_name,
            tool.action_type
        )
        logger.debug("[MCP Tools]   Description: %s", tool.description)
        logger.debug("[MCP Tools]   Tool ID: %s", tool.tool_id)
    
    # Log the actual context that goes into the LLM prompt
    if mcp_context_str:
        logger.debug("[MCP Tools] Context size for LLM prompt: %d characters", len(mcp_context_str))


def get_tools_for_connectors(
    provider: MergeIntegrationProvider,
    connector_ids: List[str],
    limit_per_connector: int = 15,
) -> List[MatchedTool]:
    """
    Get all tools for specific connectors.
    
    Used to include tools from connectors already used in an app's code,
    regardless of the current action classification.
    
    Args:
        provider: The integration provider
        connector_ids: List of connector IDs to get tools for
        limit_per_connector: Maximum tools per connector
        
    Returns:
        List of MatchedTool objects
    """
    if not connector_ids:
        return []
    
    tools = ConnectorToolAction.objects.filter(
        connector_cache__provider=provider,
        connector_cache__connector_id__in=connector_ids,
    ).select_related("connector_cache")[:limit_per_connector * len(connector_ids)]
    
    matched_tools = []
    for tool in tools:
        matched_tools.append(MatchedTool(
            tool_id=tool.tool_id,
            tool_name=tool.tool_name,
            description=tool.description,
            action_type=tool.action_type,
            connector_id=tool.connector_cache.connector_id,
            connector_name=tool.connector_cache.connector_name,
            input_schema=tool.input_schema,
        ))
    
    return matched_tools


def merge_matched_tools(
    primary_tools: List[MatchedTool],
    secondary_tools: List[MatchedTool],
) -> List[MatchedTool]:
    """
    Merge two lists of matched tools, avoiding duplicates.
    
    Primary tools take precedence (higher relevance from action matching).
    
    Args:
        primary_tools: Tools from action classification (higher priority)
        secondary_tools: Tools from used connectors (lower priority)
        
    Returns:
        Merged list with duplicates removed
    """
    # Track seen tools by tool_id + connector_id
    seen = set()
    merged = []
    
    for tool in primary_tools:
        key = (tool.tool_id, tool.connector_id)
        if key not in seen:
            seen.add(key)
            merged.append(tool)
    
    for tool in secondary_tools:
        key = (tool.tool_id, tool.connector_id)
        if key not in seen:
            seen.add(key)
            merged.append(tool)
    
    return merged


@dataclass
class FilterMCPToolsEvent:
    """Event emitted during MCP tools filtering."""

    event_type: str
    data: Dict[str, Any]


@dataclass
class FilterMCPToolsResult:
    """Final result of filtering MCP tools."""

    action_result: ActionResult
    matched_tools_context: Optional[str] = None
    tool_match_result: Optional[ToolMatchResult] = None


def filter_mcp_tools(
    user_message: str,
    app_context: "AppContext",
    intent: "IntentResult",
    app: Optional["InternalApp"] = None,
    include_used_connectors: bool = False,
) -> Iterator[Union[FilterMCPToolsEvent, FilterMCPToolsResult]]:
    """
    Classify user actions and match them to available MCP tools.

    This generator yields events in real-time as they happen, then yields
    the final result as the last item.

    Usage:
        for item in filter_mcp_tools(...):
            if isinstance(item, FilterMCPToolsEvent):
                yield AgentEvent(item.event_type, item.data)
            else:
                # item is FilterMCPToolsResult
                matched_tools_context = item.matched_tools_context

    Args:
        user_message: The user's request message
        app_context: Context about the current app state
        intent: The classified intent result
        app: The InternalApp instance (optional, needed for tool matching)
        include_used_connectors: If True, also include tools from connectors
            already used in the app's existing code (for non-generate intents)

    Yields:
        FilterMCPToolsEvent for real-time events, then FilterMCPToolsResult as final item
    """
    # Get provider and connected connectors once (reused for classification and matching)
    provider = None
    connected_connectors = []
    if app and app.organization:
        provider = app.organization.integration_providers.first()
        if provider:
            connected_connectors = get_connected_connectors(provider)
    
    # ===== ACTION CLASSIFICATION ===== using LLM
    action_classifier = get_action_classifier()
    action_result = action_classifier.classify(
        user_message, app_context, intent, connected_connectors or None
    )

    # Log all classified actions
    if action_result.actions:
        actions_summary = ", ".join(
            f"{a.action.value}({a.confidence:.0%})" for a in action_result.actions
        )
        logger.info("Actions classified: %s", actions_summary)
    else:
        logger.warning("No actions classified")

    yield FilterMCPToolsEvent(
        "thinking",
        {
            "content": f"Actions: {action_result.description}",
            "type": "decision",
        },
    )

    # Emit event for each classified action
    for action_item in action_result.actions:
        yield FilterMCPToolsEvent(
            "action_classified",
            {
                "action": action_item.action.value,
                "confidence": action_item.confidence,
                "target": action_item.target,
                "description": action_item.description,
            },
        )

    # ===== TOOL MATCHING =====
    # Find available MCP tools that match the classified actions
    matched_tools_context = None
    tool_match_result = None

    if provider and connected_connectors:
        tool_matcher = get_tool_matcher()
        tool_match_result = tool_matcher.match_tools(action_result, provider, connected_connectors)

        # ===== MERGE USED CONNECTORS =====
        # For non-generate intents, also include tools from connectors already used in the app
        logger.info(
            "[MCP Tools] Include used connectors: %s",
            include_used_connectors
        )
        logger.info(
            "[MCP Tools] Used connectors: %s",
            app_context.used_connectors
        )
        if include_used_connectors and app_context.used_connectors:
            # Filter to only include connectors that are actually connected
            used_and_connected = [
                c for c in app_context.used_connectors
                if c in connected_connectors
            ]
            
            if used_and_connected:
                logger.info(
                    "[MCP Tools] Including tools from %d used connector(s): %s",
                    len(used_and_connected),
                    ", ".join(used_and_connected)
                )
                
                # Get tools for used connectors
                used_connector_tools = get_tools_for_connectors(provider, used_and_connected)
                
                if used_connector_tools:
                    # Merge with action-matched tools (action-matched have higher priority)
                    merged_tools = merge_matched_tools(
                        tool_match_result.matched_tools,
                        used_connector_tools
                    )
                    
                    # Update the tool_match_result with merged tools
                    tool_match_result.matched_tools = merged_tools
                    
                    # Ensure used connectors are in the connected_connectors list
                    for connector_id in used_and_connected:
                        if connector_id not in tool_match_result.connected_connectors:
                            tool_match_result.connected_connectors.append(connector_id)
                    
                    logger.info(
                        "[MCP Tools] After merge: %d total tools",
                        len(merged_tools)
                    )
                    
                    yield FilterMCPToolsEvent(
                        "thinking",
                        {
                            "content": f"Including {len(used_connector_tools)} tools from existing connectors: {', '.join(used_and_connected)}",
                            "type": "observation",
                        },
                    )

        if tool_match_result.matched_tools:
            action_types_str = ", ".join(a.action.value for a in action_result.actions)
            
            yield FilterMCPToolsEvent(
                "thinking",
                {
                    "content": f"Found {len(tool_match_result.matched_tools)} available MCP tools for {action_types_str} operations",
                    "type": "observation",
                },
            )

            # Build context string from matched tools
            mcp_context = build_context_from_matched_tools(app, tool_match_result)
            if mcp_context.has_tools:
                matched_tools_context = mcp_context.full_context
            
            # Log detailed information about matched tools
            _log_matched_tools_details(tool_match_result, action_result, matched_tools_context)

            yield FilterMCPToolsEvent(
                "tools_matched",
                {
                    "action_types": [a.action.value for a in action_result.actions],
                    "matched_count": len(tool_match_result.matched_tools),
                    "connected_connectors": tool_match_result.connected_connectors,
                    "tools": [
                        t.to_dict() for t in tool_match_result.matched_tools[:10]
                    ],  # Limit for UI
                },
            )
        else:
            logger.info("No matching MCP tools found for actions")

    # Yield final result
    yield FilterMCPToolsResult(
        action_result=action_result,
        matched_tools_context=matched_tools_context,
        tool_match_result=tool_match_result,
    )
