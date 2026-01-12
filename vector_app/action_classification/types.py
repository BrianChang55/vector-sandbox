from enum import StrEnum
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional


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
