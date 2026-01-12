"""
Tool Matcher Service

Matches classified actions to available MCP tools based on:
1. Action type (QUERY, CREATE, UPDATE, DELETE, SEND)
2. Connected connectors for the organization
3. Target service extracted from user message

This bridges the gap between user intent and available tool capabilities.
"""

import logging
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional

from vector_app.action_classification.types import ActionResult, ActionItem, MatchedTool, ActionType
from vector_app.models import (
    ConnectorToolAction,
    MergeIntegrationProvider,
)

logger = logging.getLogger(__name__)


def categorize_tool_action(tool_id: str, description: str = "") -> str:
    """
    Determine action type from tool ID and description.

    Uses prefix-based matching on tool_id first, then falls back to
    description analysis.

    Args:
        tool_id: The tool identifier (e.g., "create_issue", "list_users")
        description: Optional description text to analyze as fallback

    Returns:
        Action type string: 'query', 'create', 'update', 'delete', 'send', or 'other'
    """
    tool_lower = tool_id.lower()

    # Query actions (read operations)
    if any(
        tool_lower.startswith(p)
        for p in ["get_", "list_", "search_", "fetch_", "find_", "retrieve_", "query_"]
    ):
        return ActionType.QUERY

    # Create actions
    if any(tool_lower.startswith(p) for p in ["create_", "add_", "insert_", "post_", "new_"]):
        return ActionType.CREATE

    # Update actions
    if any(tool_lower.startswith(p) for p in ["update_", "edit_", "modify_", "patch_", "set_"]):
        return ActionType.UPDATE

    # Delete actions
    if any(tool_lower.startswith(p) for p in ["delete_", "remove_", "archive_", "trash_"]):
        return ActionType.DELETE

    # Send/notification actions
    if any(tool_lower.startswith(p) for p in ["send_", "notify_", "message_", "post_message"]):
        return ActionType.SEND

    # Fallback: check description for hints
    if description:
        desc_lower = description.lower()
        if any(
            word in desc_lower for word in ["retrieve", "get", "list", "fetch", "search", "find", "query"]
        ):
            return ActionType.QUERY
        if any(word in desc_lower for word in ["create", "add", "new", "insert"]):
            return ActionType.CREATE
        if any(word in desc_lower for word in ["update", "edit", "modify", "change"]):
            return ActionType.UPDATE
        if any(word in desc_lower for word in ["delete", "remove", "archive", "trash"]):
            return ActionType.DELETE
        if any(word in desc_lower for word in ["send", "notify", "message", "email", "post message"]):
            return ActionType.SEND

    return ActionType.OTHER


@dataclass
class ToolMatchResult:
    """Result of tool matching."""

    action: ActionResult
    matched_tools: List[MatchedTool] = field(default_factory=list)
    connected_connectors: List[str] = field(default_factory=list)
    total_available_tools: int = 0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "action": self.action.to_dict(),
            "matched_tools": [t.to_dict() for t in self.matched_tools],
            "connected_connectors": self.connected_connectors,
            "total_available_tools": self.total_available_tools,
            "matched_count": len(self.matched_tools),
        }


class ToolMatcher:
    """
    Matches action classifications to available MCP tools.

    Queries the ConnectorToolAction table filtered by:
    - Action type from classification
    - Connected connectors for the organization
    """

    def match_tools(
        self,
        action: ActionResult,
        provider: MergeIntegrationProvider,
        connected_connectors: List[str],
        limit: int = 20,
    ) -> ToolMatchResult:
        """
        Find available tools matching the action classification.
        
        Queries ConnectorToolAction filtered by:
        - Action types from the classified actions
        - Connected connectors for this organization

        Args:
            action: The classified action result with multiple actions
            provider: The integration provider for the organization
            connected_connectors: List of connected connector IDs for the organization
            limit: Maximum number of tools to return per action type

        Returns:
            ToolMatchResult with matched tools from all action types
        """
        # Return early if no actions
        if not action.actions:
            return ToolMatchResult(
                action=action,
                matched_tools=[],
                connected_connectors=[],
                total_available_tools=0,
            )

        connected_connector_ids = connected_connectors
        if not connected_connector_ids:
            return ToolMatchResult(
                action=action,
                matched_tools=[],
                connected_connectors=[],
                total_available_tools=0,
            )

        # Build query filters
        action_types = [action_item.action.value for action_item in action.actions]
        tools_query = ConnectorToolAction.objects.filter(
            connector_cache__provider=provider,
            connector_cache__connector_id__in=connected_connector_ids,
            action_type__in=action_types,
        ).select_related("connector_cache")
        
        total_count = tools_query.count()
        tools = tools_query[:limit * len(action_types)]
        
        # Build matched tools with relevance scoring
        action_lookup = {action_item.action.value: action_item for action_item in action.actions}
        matched_tools = []
        
        for tool in tools:
            action_item = action_lookup.get(tool.action_type)
            if action_item:
                matched_tools.append(MatchedTool(
                    tool_id=tool.tool_id,
                    tool_name=tool.tool_name,
                    description=tool.description,
                    action_type=tool.action_type,
                    connector_id=tool.connector_cache.connector_id,
                    connector_name=tool.connector_cache.connector_name,
                    input_schema=tool.input_schema,
                    relevance_score=self._calculate_relevance(tool, action_item),
                ))
        
        # Sort by relevance (highest first)
        matched_tools.sort(key=lambda t: t.relevance_score, reverse=True)

        return ToolMatchResult(
            action=action,
            matched_tools=matched_tools,
            connected_connectors=connected_connector_ids,
            total_available_tools=total_count,
        )

    def _calculate_relevance(
        self,
        tool: ConnectorToolAction,
        action_item: ActionItem,
    ) -> float:
        """
        Calculate relevance score for a tool based on the action item.

        Higher score = more relevant.
        """
        score = 1.0

        # Boost based on action confidence
        score += action_item.confidence * 0.5

        # Boost if tool name contains relevant keywords from target
        target_lower = action_item.target.lower()
        tool_id_lower = tool.tool_id.lower()
        tool_desc_lower = tool.description.lower()

        # Check for target keywords in tool
        target_words = target_lower.split()
        for word in target_words:
            if len(word) > 3:  # Skip short words
                if word in tool_id_lower:
                    score += 0.3
                if word in tool_desc_lower:
                    score += 0.1

        return min(score, 2.0)  # Cap at 2.0


# Singleton instance
_tool_matcher: Optional[ToolMatcher] = None


def get_tool_matcher() -> ToolMatcher:
    """Get singleton tool matcher instance."""
    global _tool_matcher
    if _tool_matcher is None:
        _tool_matcher = ToolMatcher()
    return _tool_matcher
