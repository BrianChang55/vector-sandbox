"""
Action Classifier Service

Classifies user requests to determine what MCP tool operations
the user's app will need to perform on external systems.

This is different from intent classification:
- Intent: How we build the app (generate, edit, refactor)
- Action: What external operations the app needs (query, create, update, send)

For example:
- "Build a dashboard that queries GitHub PRs" → Action: QUERY
- "Create an app that sends Slack notifications" → Action: SEND
- "Make a form that creates Jira tickets" → Action: CREATE
"""

import logging
from typing import List, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from vector_app.action_classification.types import ActionType, ActionResult
    from vector_app.services.intent_classifier import IntentResult

logger = logging.getLogger(__name__)


class ActionClassifier:
    """
    Classifies user messages to determine what MCP tool operations
    the user's app will need to perform on external systems.

    This identifies the data operations required, not what we do to the app.
    """

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

    def classify(
        self,
        user_message: str,
        context: Optional["AppContext"] = None,
        intent: Optional["IntentResult"] = None,
    ) -> "ActionResult":
        """
        Classify the user's action based on their message.

        Args:
            user_message: The user's request
            context: Optional app context
            intent: Optional intent result for additional context

        Returns:
            ActionResult with classified action and metadata
        """
        # Import at runtime to avoid circular imports
        from vector_app.action_classification.types import ActionType, ActionResult

        logger.info("=" * 60)
        logger.info("ACTION CLASSIFICATION - Determining MCP tool operations needed")
        logger.info("=" * 60)
        logger.info("Input message: %s%s", user_message[:200], "..." if len(user_message) > 200 else "")
        logger.info("Has context: %s", context is not None)
        logger.info("Has intent: %s", intent is not None)
        if intent:
            logger.info("  Intent type: %s", intent.intent.value)
            logger.info("  Intent confidence: %.0f%%", intent.confidence * 100)

        message_lower = user_message.lower()

        # Score each action type (aligned with ActionType enum)
        logger.info("-" * 40)
        logger.info("KEYWORD SCORING:")
        scores = {
            ActionType.QUERY: self._count_matches(message_lower, self.QUERY_KEYWORDS),
            ActionType.CREATE: self._count_matches(message_lower, self.CREATE_KEYWORDS),
            ActionType.UPDATE: self._count_matches(message_lower, self.UPDATE_KEYWORDS),
            ActionType.DELETE: self._count_matches(message_lower, self.DELETE_KEYWORDS),
            ActionType.SEND: self._count_matches(message_lower, self.SEND_KEYWORDS),
        }

        # Log all scores
        for action_type, score in sorted(scores.items(), key=lambda x: x[1], reverse=True):
            logger.info("  %s: %d matches", action_type.value, score)

        # Find best action
        best_action = max(scores, key=scores.get)
        best_score = scores[best_action]

        logger.info("-" * 40)
        logger.info("Best match: %s (score: %d)", best_action.value, best_score)

        # Handle no matches - default to QUERY since most apps need to read data
        if best_score == 0:
            best_action = ActionType.QUERY
            confidence = 0.5
            logger.info("No keyword matches - defaulting to QUERY (most apps need to read data)")
        else:
            # Calculate confidence
            total = sum(scores.values())
            confidence = min(0.95, (best_score / total) * 0.7 + 0.3) if total > 0 else 0.5
            logger.info("Confidence calculation: %d/%d = %.0f%%", best_score, total, confidence * 100)

        # Extract target from message
        target = self._extract_target(user_message)
        logger.info("Extracted target: %s", target)

        # Generate description
        description = self._generate_description(best_action, target)

        result = ActionResult(
            action=best_action,
            confidence=confidence,
            target=target,
            description=description,
            metadata={
                "scores": {k.value: v for k, v in scores.items()},
                "intent": intent.intent.value if intent else None,
            },
        )

        logger.info("-" * 40)
        logger.info("MCP OPERATION NEEDED:")
        logger.info("  Action Type: %s", result.action.value)
        logger.info("  Confidence: %.0f%%", result.confidence * 100)
        logger.info("  Target Service/Data: %s", result.target)
        logger.info("  Description: %s", result.description)
        logger.info("=" * 60)

        return result

    def _count_matches(self, message: str, keywords: List[str]) -> int:
        """Count keyword matches in message."""
        return sum(1 for kw in keywords if kw in message)

    def _extract_target(self, message: str) -> str:
        """Extract the external service/data source the app will interact with.

        This is a placeholder implementation - could be enhanced with NLP.
        """
        # Look for common external services/integrations
        service_patterns = [
            ("github", "GitHub"),
            ("jira", "Jira"),
            ("slack", "Slack"),
            ("notion", "Notion"),
            ("linear", "Linear"),
            ("asana", "Asana"),
            ("trello", "Trello"),
            ("salesforce", "Salesforce"),
            ("hubspot", "HubSpot"),
            ("zendesk", "Zendesk"),
            ("intercom", "Intercom"),
            ("stripe", "Stripe"),
            ("google", "Google"),
            ("gmail", "Gmail"),
            ("calendar", "Calendar"),
            ("drive", "Drive"),
            ("dropbox", "Dropbox"),
            ("airtable", "Airtable"),
            ("database", "database"),
            ("api", "external API"),
        ]

        message_lower = message.lower()
        for keyword, target in service_patterns:
            if keyword in message_lower:
                return target

        # Look for data types
        data_patterns = [
            ("pull request", "pull requests"),
            ("issue", "issues"),
            ("ticket", "tickets"),
            ("task", "tasks"),
            ("user", "users"),
            ("customer", "customers"),
            ("order", "orders"),
            ("message", "messages"),
            ("email", "emails"),
            ("file", "files"),
            ("document", "documents"),
        ]

        for keyword, target in data_patterns:
            if keyword in message_lower:
                return target

        return "external data"  # Default target

    def _generate_description(self, action: "ActionType", target: str) -> str:
        """Generate a human-readable description of what MCP operations the app needs."""
        # Import at runtime to avoid circular imports
        from vector_app.action_classification.types import ActionType

        descriptions = {
            ActionType.QUERY: "App will query/fetch data from %s" % target,
            ActionType.CREATE: "App will create records in %s" % target,
            ActionType.UPDATE: "App will update records in %s" % target,
            ActionType.DELETE: "App will delete records from %s" % target,
            ActionType.SEND: "App will send messages/notifications via %s" % target,
            ActionType.OTHER: "App will interact with %s" % target,
        }
        return descriptions.get(action, "App will interact with %s" % target)


# Singleton instance
_action_classifier: Optional[ActionClassifier] = None


def get_action_classifier() -> ActionClassifier:
    """Get singleton action classifier instance."""
    global _action_classifier
    if _action_classifier is None:
        _action_classifier = ActionClassifier()
    return _action_classifier
