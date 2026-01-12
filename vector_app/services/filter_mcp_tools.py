"""
Filter MCP Tools Service

Handles action classification and tool matching for MCP tools.
Extracts user intent actions and matches them to available MCP tools.
"""

import logging
from dataclasses import dataclass
from typing import Any, Dict, Generator, Optional, Union

from vector_app.action_classification.action_classifier import get_action_classifier
from vector_app.action_classification.build_mcp_context import build_context_from_matched_tools
from vector_app.action_classification.tool_matcher import get_tool_matcher, ToolMatchResult
from vector_app.action_classification.types import ActionResult

from vector_app.models import InternalApp
from vector_app.services.context_analyzer import AppContext
from vector_app.services.intent_classifier import IntentResult

logger = logging.getLogger(__name__)


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
) -> Generator[Union[FilterMCPToolsEvent, FilterMCPToolsResult], None, None]:
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

    Yields:
        FilterMCPToolsEvent for real-time events, then FilterMCPToolsResult as final item
    """
    # ===== ACTION CLASSIFICATION ===== using LLM
    action_classifier = get_action_classifier()
    action_result = action_classifier.classify(user_message, app_context, intent)

    # Log all classified actions
    if action_result.actions:
        actions_summary = ", ".join(
            f"{a.action.value}({a.confidence:.0%})" for a in action_result.actions
        )
        logger.info(f"Actions classified: {actions_summary}")
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

    if app and app.organization:
        provider = app.organization.integration_providers.first()
        if provider:
            tool_matcher = get_tool_matcher()
            tool_match_result = tool_matcher.match_tools(action_result, provider)

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
