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

from vector_app.action_classification.types import ActionResult, MatchedTool, ActionType
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
        limit: int = 20, # TODO: limit yes, but we need to do an embedding / semantic matching to get the most relevant tools and order them by relevance
    ) -> ToolMatchResult:
        """
        Find available tools matching the action classification.
        
        Queries ConnectorToolAction filtered by:
        - Action types from the classified actions
        - Target connector for each action (must match a connected connector)
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

        # Match tools for each action, filtering by BOTH action type AND target connector
        matched_tools = []
        seen_tool_keys = set()  # Track (tool_id, connector_id) to dedupe
        total_count = 0
        used_connectors = set()
        
        for action_item in action.actions:
            # Find which connected connector(s) match this action's target
            target_connectors = self._resolve_target_to_connectors(
                action_item.target,
                connected_connector_ids,
            )
            
            if not target_connectors:
                # Target is 'unknown' or doesn't match any connected connector
                # Skip this action - it won't contribute tools
                logger.debug(
                    "Skipping action %s with target '%s' - no matching connected connector",
                    action_item.action.value,
                    action_item.target,
                )
                continue
            
            # Query tools for this specific action + target connector combination
            tools_query = ConnectorToolAction.objects.filter(
                connector_cache__provider=provider,
                connector_cache__connector_id__in=target_connectors,
                action_type=action_item.action.value,
            ).select_related("connector_cache")
            
            action_tool_count = tools_query.count()
            total_count += action_tool_count
            tools = tools_query[:limit]
            
            logger.debug(
                "Action %s on target '%s' matched %d tools from connector(s): %s",
                action_item.action.value,
                action_item.target,
                action_tool_count,
                ", ".join(target_connectors),
            )
            
            for tool in tools:
                matched_tool = self._create_matched_tool_if_unique(
                    tool=tool,
                    seen_tool_keys=seen_tool_keys,
                )
                if matched_tool:
                    used_connectors.add(tool.connector_cache.connector_id)
                    matched_tools.append(matched_tool)

        return ToolMatchResult(
            action=action,
            matched_tools=matched_tools,
            connected_connectors=list(used_connectors),
            total_available_tools=total_count,
        )
    
    def _resolve_target_to_connectors(
        self,
        target: str,
        connected_connectors: List[str],
    ) -> List[str]:
        """
        Resolve an action target to matching connected connector(s).
        
        Uses fuzzy matching to find connectors that match the target.
        Returns empty list if target is 'unknown' or doesn't match any connector.
        
        Args:
            target: The target from action classification (e.g., 'github', 'Slack', 'unknown')
            connected_connectors: List of connected connector IDs
            
        Returns:
            List of matching connector IDs (usually 1, but could be multiple for ambiguous targets)
        """
        if not target or target.lower() in ('unknown', 'external data', 'external api'): # TODO: handle the structured output so we can do ez validation
            return []
        
        target_lower = target.lower()
        matched = []
        
        for connector_id in connected_connectors:
            connector_lower = connector_id.lower()
            
            # Exact match
            if target_lower == connector_lower:
                return [connector_id]  # Exact match, return immediately
            
            # Partial match (target is substring of connector or vice versa)
            if target_lower in connector_lower or connector_lower in target_lower:
                matched.append(connector_id)
        
        return matched

    def _create_matched_tool_if_unique(
        self,
        tool: ConnectorToolAction,
        seen_tool_keys: set,
    ) -> Optional[MatchedTool]:
        """
        Create a MatchedTool if it hasn't been seen before.
        
        Deduplicates by (tool_id, connector_id) to prevent the same tool
        from the same connector being added multiple times.
        
        Args:
            tool: The ConnectorToolAction from the database
            seen_tool_keys: Set of (tool_id, connector_id) tuples already seen
            
        Returns:
            MatchedTool if unique, None if duplicate
        """
        tool_key = (tool.tool_id, tool.connector_cache.connector_id)
        if tool_key in seen_tool_keys:
            return None
        seen_tool_keys.add(tool_key)
        
        return MatchedTool(
            tool_id=tool.tool_id,
            tool_name=tool.tool_name,
            description=tool.description,
            action_type=tool.action_type,
            connector_id=tool.connector_cache.connector_id,
            connector_name=tool.connector_cache.connector_name,
            input_schema=tool.input_schema,
        )


# Singleton instance
_tool_matcher: Optional[ToolMatcher] = None


def get_tool_matcher() -> ToolMatcher:
    """Get singleton tool matcher instance."""
    global _tool_matcher
    if _tool_matcher is None:
        _tool_matcher = ToolMatcher()
    return _tool_matcher
