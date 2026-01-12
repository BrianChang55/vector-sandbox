from enum import StrEnum
from dataclasses import dataclass, field
from typing import Any, Dict


class ActionType(StrEnum):
    """Action type categories for MCP tools."""

    QUERY = "QUERY"  # get_*, list_*, search_*, fetch_*, find_*
    CREATE = "CREATE"  # create_*, add_*, insert_*, post_*
    UPDATE = "UPDATE"  # update_*, edit_*, modify_*, patch_*
    DELETE = "DELETE"  # delete_*, remove_*, archive_*
    SEND = "SEND"  # send_*, notify_*, message_*
    OTHER = "OTHER"  # Fallback for uncategorized tools


@dataclass
class ActionResult:
    """Result of action classification."""

    action: ActionType
    confidence: float
    target: str  # What MCP the action applies to (e.g., "github", "jira", "slack", "notion", "linear", "asana", "trello", "salesforce", "hubspot", "zendesk", "intercom", "stripe", "google", "gmail", "calendar", "drive", "dropbox", "airtable", "database", "api")
    description: str  # Human-readable description
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "action": self.action.value,
            "confidence": self.confidence,
            "target": self.target,
            "description": self.description,
            "metadata": self.metadata,
        }


@dataclass
class MatchedTool:
    """A tool that matches the action classification."""

    tool_id: str
    tool_name: str
    description: str
    action_type: str
    connector_id: str
    connector_name: str
    input_schema: Dict[str, Any] = field(default_factory=dict)
    relevance_score: float = 1.0  # How relevant this tool is to the request

    def to_dict(self) -> Dict[str, Any]:
        return {
            "tool_id": self.tool_id,
            "tool_name": self.tool_name,
            "description": self.description,
            "action_type": self.action_type,
            "connector_id": self.connector_id,
            "connector_name": self.connector_name,
            "input_schema": self.input_schema,
            "relevance_score": self.relevance_score,
        }
