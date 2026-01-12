from enum import StrEnum
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional


# Keywords that indicate the app needs to READ/FETCH external data
# e.g., "show my pull requests", "display issues", "list users"
QUERY_KEYWORDS = [
    "show",
    "display",
    "list",
    "fetch",
    "get",
    "retrieve",
    "find",
    "search",
    "filter",
    "sort",
    "view",
    "see",
    "query",
    "pull",
    "load",
    "read",
    "look up",
    "browse",
    "explore",
    "dashboard",
    "report",
    "analytics",
    "monitor",
    "track",
    "ranks",
    "ranking",
    "order by",
    "aggregate",
]

# Keywords that indicate the app needs to CREATE external records
# e.g., "create jira tickets", "add new issues", "submit forms"
CREATE_KEYWORDS = [
    "creates",
    "add",
    "insert",
    "submit",
    "post",
    "new record",
    "new issue",
    "new ticket",
    "new task",
    "new item",
    "file",
    "open ticket",
    "open issue",
    "log",
    "register",
]

# Keywords that indicate the app needs to UPDATE external records
# e.g., "update status", "modify tickets", "change priority"
UPDATE_KEYWORDS = [
    "updates",
    "modify",
    "change status",
    "edit record",
    "mark as",
    "set status",
    "assign",
    "reassign",
    "close",
    "resolve",
    "transition",
    "move to",
    "change priority",
]

# Keywords that indicate the app needs to DELETE external records
# e.g., "delete old tickets", "remove completed tasks"
DELETE_KEYWORDS = ["deletes", "remove record", "archive", "trash", "clean up", "purge", "clear old"]

# Keywords that indicate the app needs to SEND notifications/messages
# e.g., "send slack message", "notify team", "email updates"
SEND_KEYWORDS = [
    "send",
    "notify",
    "message",
    "email",
    "alert",
    "post message",
    "broadcast",
    "publish",
    "share",
    "slack",
    "notification",
    "announce",
    "ping",
    "remind",
    "trigger webhook",
]


class ActionType(StrEnum):
    """Action type categories for MCP tools."""

    QUERY = "QUERY"  # get_*, list_*, search_*, fetch_*, find_*
    CREATE = "CREATE"  # create_*, add_*, insert_*, post_*
    UPDATE = "UPDATE"  # update_*, edit_*, modify_*, patch_*
    DELETE = "DELETE"  # delete_*, remove_*, archive_*
    SEND = "SEND"  # send_*, notify_*, message_*
    OTHER = "OTHER"  # Fallback for uncategorized tools


@dataclass
class ActionItem:
    """Single action with its details."""

    action: ActionType
    confidence: float
    target: str
    description: str


@dataclass
class ActionResult:
    """Result of action classification (can include multiple actions).
    
    The primary data is in the 'actions' list. The single fields (action, confidence, target)
    are kept for backward compatibility but should not be used - use the actions list instead.
    """

    actions: List[ActionItem] = field(default_factory=list)  # All identified actions (filtered, ranked by confidence)
    description: str = ""  # Overall description
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    # Deprecated fields for backward compatibility - do not use, use actions list instead
    action: Optional[ActionType] = None
    confidence: Optional[float] = None
    target: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        result = {
            "actions": [
                {
                    "action": item.action.value,
                    "confidence": item.confidence,
                    "target": item.target,
                    "description": item.description,
                }
                for item in self.actions
            ],
            "description": self.description,
            "metadata": self.metadata,
        }
        
        # Include deprecated fields if set
        if self.action is not None:
            result["action"] = self.action.value
        if self.confidence is not None:
            result["confidence"] = self.confidence
        if self.target is not None:
            result["target"] = self.target
            
        return result


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
