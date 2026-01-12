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

from vector_app.action_classification.types import ActionResult, MatchedTool
from vector_app.models import (
    ConnectorToolAction,
    OrganizationConnectorLink,
    MergeIntegrationProvider,
)
from vector_app.services.merge_service import merge_service, is_merge_configured, MergeAPIError

logger = logging.getLogger(__name__)


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
    - Optional: target service name
    """

    # Map common service names to connector IDs
    SERVICE_TO_CONNECTOR = {
        "github": ["github"],
        "jira": ["jira"],
        "slack": ["slack"],
        "notion": ["notion"],
        "linear": ["linear"],
        "asana": ["asana"],
        "trello": ["trello"],
        "salesforce": ["salesforce"],
        "hubspot": ["hubspot"],
        "zendesk": ["zendesk"],
        "intercom": ["intercom"],
        "stripe": ["stripe"],
        "google": ["google_calendar", "google_drive", "gmail"],
        "gmail": ["gmail"],
        "calendar": ["google_calendar"],
        "drive": ["google_drive"],
        "dropbox": ["dropbox"],
        "airtable": ["airtable"],
    }

    def match_tools(
        self,
        action: ActionResult,
        provider: MergeIntegrationProvider,
        limit: int = 20,
    ) -> ToolMatchResult:
        """
        Find available tools matching the action classification.

        Args:
            action: The classified action result
            provider: The integration provider for the organization
            limit: Maximum number of tools to return

        Returns:
            ToolMatchResult with matched tools
        """
        logger.info("=" * 60)
        logger.info("TOOL MATCHING - START")
        logger.info("=" * 60)
        logger.info("Action type: %s", action.action.value)
        logger.info("Target: %s", action.target)
        logger.info("Provider: %s", provider.id)

        # Step 1: Get connected connectors for this organization
        connected_connector_ids = self._get_connected_connectors(provider)

        logger.info("Connected connectors: %s", connected_connector_ids)

        if not connected_connector_ids:
            logger.info("No connected connectors - returning empty result")
            return ToolMatchResult(
                action=action,
                matched_tools=[],
                connected_connectors=[],
                total_available_tools=0,
            )

        # Step 2: Query tools by action type from connected connectors
        tools_query = ConnectorToolAction.objects.filter(
            connector_cache__provider=provider,
            connector_cache__connector_id__in=connected_connector_ids,
            action_type=action.action.value,
        ).select_related("connector_cache")

        # Step 3: If we have a specific target service, prioritize those connectors
        target_connector_ids = self._get_target_connectors(action.target)

        if target_connector_ids:
            logger.info("Target service connectors: %s", target_connector_ids)
            # Filter to target connectors that are connected
            matching_target_connectors = set(target_connector_ids) & set(connected_connector_ids)

            if matching_target_connectors:
                logger.info("Filtering to matching target connectors: %s", matching_target_connectors)
                tools_query = tools_query.filter(connector_cache__connector_id__in=matching_target_connectors)

        # Step 4: Execute query and build results
        tools = tools_query[:limit]
        total_count = tools_query.count()

        logger.info("Found %d matching tools (returning %d)", total_count, len(tools))

        matched_tools = []
        for tool in tools:
            matched_tool = MatchedTool(
                tool_id=tool.tool_id,
                tool_name=tool.tool_name,
                description=tool.description,
                action_type=tool.action_type,
                connector_id=tool.connector_cache.connector_id,
                connector_name=tool.connector_cache.connector_name,
                input_schema=tool.input_schema,
                relevance_score=self._calculate_relevance(tool, action),
            )
            matched_tools.append(matched_tool)

            logger.info("  - %s:%s (%s)", tool.connector_cache.connector_id, tool.tool_id, tool.action_type)

        # Sort by relevance
        matched_tools.sort(key=lambda t: t.relevance_score, reverse=True)

        result = ToolMatchResult(
            action=action,
            matched_tools=matched_tools,
            connected_connectors=connected_connector_ids,
            total_available_tools=total_count,
        )

        logger.info("-" * 40)
        logger.info("TOOL MATCHING - RESULT:")
        logger.info("  Matched tools: %d", len(matched_tools))
        logger.info("  Connected connectors: %d", len(connected_connector_ids))
        logger.info("  Total available: %d", total_count)
        logger.info("=" * 60)

        return result

    def _get_connected_connectors(
        self,
        provider: MergeIntegrationProvider,
    ) -> List[str]:
        """Get list of connected connector IDs for the organization."""
        # First try to get from Merge API (source of truth)
        if provider.merge_registered_user_id and is_merge_configured():
            try:
                return list(merge_service.get_organization_connections(provider))
            except MergeAPIError as e:
                logger.warning("Failed to get connections from Merge API: %s", e)

        # Fallback to local database
        links = OrganizationConnectorLink.objects.filter(
            provider=provider,
            is_connected=True,
        ).select_related("connector")

        return [link.connector.connector_id for link in links]

    def _get_target_connectors(self, target: str) -> List[str]:
        """Map target service name to connector IDs."""
        target_lower = target.lower()

        for service_name, connector_ids in self.SERVICE_TO_CONNECTOR.items():
            if service_name in target_lower:
                return connector_ids

        return []

    def _calculate_relevance(
        self,
        tool: ConnectorToolAction,
        action: ActionResult,
    ) -> float:
        """
        Calculate relevance score for a tool based on the action.

        Higher score = more relevant.
        """
        score = 1.0

        # Boost if tool name contains relevant keywords from target
        target_lower = action.target.lower()
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
