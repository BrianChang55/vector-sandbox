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
import json
from typing import List, Optional, TYPE_CHECKING

import httpx

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
        Classify the user's action based on their message using LLM.

        Args:
            user_message: The user's request
            context: Optional app context
            intent: Optional intent result for additional context

        Returns:
            ActionResult with classified action and metadata
        """
        # Import at runtime to avoid circular imports
        from vector_app.action_classification.types import ActionType, ActionResult, ActionItem

        logger.info("=" * 60)
        logger.info("ACTION CLASSIFICATION - Determining MCP tool operations needed")
        logger.info("=" * 60)
        logger.info("Input message: %s%s", user_message[:200], "..." if len(user_message) > 200 else "")
        logger.info("Has context: %s", context is not None)
        logger.info("Has intent: %s", intent is not None)
        if intent:
            logger.info("  Intent type: %s", intent.intent.value)
            logger.info("  Intent confidence: %.0f%%", intent.confidence * 100)

        # Build prompt for LLM classification
        system_prompt = """You are an expert at classifying user requests for internal app building.

Your task is to determine what type of MCP (Model Context Protocol) tool operations the user's app will need to perform on external systems.

IMPORTANT: A single request can require MULTIPLE action types. For example:
- "Fetch GitHub issues and send Slack notifications" = [QUERY, SEND]
- "Create a Jira ticket and update the status" = [CREATE, UPDATE]
- "Show all users" = [QUERY]

Action Types:
- QUERY: The app needs to READ/FETCH data (e.g., "show GitHub PRs", "display Jira issues", "list users")
- CREATE: The app needs to CREATE new records (e.g., "create Jira tickets", "add calendar events", "submit forms")
- UPDATE: The app needs to MODIFY existing records (e.g., "update issue status", "edit documents", "modify settings")
- DELETE: The app needs to REMOVE records (e.g., "delete old files", "archive tickets", "remove users")
- SEND: The app needs to SEND messages/notifications (e.g., "send Slack messages", "email notifications", "SMS alerts")
- OTHER: Fallback for operations that don't fit the above categories

Also identify the target service/system for each action (e.g., "github", "jira", "slack", "database", "api", "email").

Return a JSON object with:
{
  "actions": [
    {
      "action": "QUERY|CREATE|UPDATE|DELETE|SEND|OTHER",
      "confidence": 0.0-1.0,
      "target": "service name or 'unknown'",
      "description": "brief description of this specific operation"
    }
    // ... more actions if the request requires multiple operations
  ],
  "reasoning": "why you chose these classifications"
}

Always return at least 1 action. Return multiple actions only when the request clearly requires different operation types."""

        user_prompt = f"""User's request: {user_message}"""
        
        if intent:
            user_prompt += f"\n\nIntent context: The user wants to {intent.intent.value} the app (confidence: {intent.confidence:.0%})"

        logger.info("-" * 40)
        logger.info("LLM CLASSIFICATION:")
        
        try:
            action_items, description, reasoning = self._classify_with_llm(
                system_prompt, user_prompt, ActionType, ActionItem
            )
        except Exception as e:
            logger.error("LLM classification failed: %s", e)
            logger.info("Falling back to keyword-based classification")
            action_items, description, reasoning = self._classify_with_keywords(
                user_message, ActionType, ActionItem
            )

        result = ActionResult(
            actions=action_items,
            description=description,
            metadata={
                "reasoning": reasoning,
                "intent": intent.intent.value if intent else None,
                "classification_method": "llm" if reasoning != "Fallback keyword matching" else "keyword_fallback",
            },
        )

        logger.info("-" * 40)
        logger.info("MCP OPERATIONS NEEDED:")
        logger.info("  Total Actions: %d (filtered, confidence >= 0.4)", len(result.actions))
        logger.info("  Description: %s", result.description)
        if result.actions:
            logger.info("  All Actions (ranked by confidence):")
            for i, action in enumerate(result.actions, 1):
                logger.info("    %d. %s (%.0f%%, target: %s) - %s", 
                           i, action.action.value, action.confidence * 100, 
                           action.target, action.description)
        else:
            logger.warning("  No actions classified!")
        logger.info("=" * 60)

        return result

    def _classify_with_llm(self, system_prompt: str, user_prompt: str, ActionType, ActionItem):
        """
        Classify actions using LLM.

        Args:
            system_prompt: System prompt for the LLM
            user_prompt: User prompt for the LLM
            ActionType: ActionType enum class
            ActionItem: ActionItem class

        Returns:
            Tuple of (action_items, description, reasoning)
        """
        from vector_app.services.openrouter_service import get_openrouter_service

        # Use OpenRouter service for classification
        openrouter = get_openrouter_service()
        
        response = httpx.post(
            openrouter.OPENROUTER_API_URL,
            headers=openrouter._build_headers(),
            json={
                "model": "openai/gpt-5-mini",  # Fast and accurate
                "messages": [
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": user_prompt},
                ],
                "response_format": {"type": "json_object"},
                "temperature": 0.2,  # Low temperature for consistent classification
            },
            timeout=30.0,
        )
        response.raise_for_status()
        
        result_data = response.json()
        content = result_data["choices"][0]["message"]["content"]
        classification = json.loads(content)
        
        logger.info("LLM Response: %s", json.dumps(classification, indent=2))
        
        # Parse LLM response - now supports multiple actions
        actions_data = classification.get("actions", [])
        reasoning = classification.get("reasoning", "")
        
        if not actions_data:
            # Fallback for old format or empty response
            logger.warning("No actions array in response, using fallback")
            actions_data = [{
                "action": "QUERY",
                "confidence": 0.5,
                "target": "unknown",
                "description": "Read data"
            }]
        
        # Parse all actions
        action_items = []
        for action_data in actions_data:
            action_str = action_data.get("action", "QUERY").upper()
            try:
                action_type = ActionType[action_str]
            except KeyError:
                logger.warning("Invalid action type from LLM: %s, defaulting to QUERY", action_str)
                action_type = ActionType.QUERY
            
            action_items.append(ActionItem(
                action=action_type,
                confidence=float(action_data.get("confidence", 0.7)),
                target=action_data.get("target", "unknown"),
                description=action_data.get("description", f"{action_type.value} operation")
            ))
        
        # Sort by confidence (highest first) and filter out low-confidence actions
        action_items.sort(key=lambda x: x.confidence, reverse=True)
        action_items = [item for item in action_items if item.confidence >= 0.4]
        
        # Ensure we have at least one action
        if not action_items:
            logger.warning("All actions filtered out due to low confidence, using default QUERY")
            action_items = [ActionItem(
                action=ActionType.QUERY,
                confidence=0.5,
                target="unknown",
                description="Read data"
            )]
        
        # Build overall description
        if len(action_items) == 1:
            description = action_items[0].description
        else:
            description = f"Multiple operations: {', '.join(item.action.value for item in action_items)}"
        
        logger.info("-" * 40)
        logger.info("Classification result:")
        logger.info("  Number of actions (after filtering <0.4): %d", len(action_items))
        for i, item in enumerate(action_items, 1):
            logger.info("  Action %d: %s (%.0f%%, target: %s) - %s", 
                       i, item.action.value, item.confidence * 100, item.target, item.description)
        logger.info("  Reasoning: %s", reasoning)

        return action_items, description, reasoning

    def _classify_with_keywords(self, user_message: str, ActionType, ActionItem):
        """
        Classify actions using keyword matching (fallback method).

        Args:
            user_message: The user's message
            ActionType: ActionType enum class
            ActionItem: ActionItem class

        Returns:
            Tuple of (action_items, description, reasoning)
        """
        # Fallback to simple keyword matching if LLM fails
        message_lower = user_message.lower()
        scores = {
            ActionType.QUERY: self._count_matches(message_lower, self.QUERY_KEYWORDS),
            ActionType.CREATE: self._count_matches(message_lower, self.CREATE_KEYWORDS),
            ActionType.UPDATE: self._count_matches(message_lower, self.UPDATE_KEYWORDS),
            ActionType.DELETE: self._count_matches(message_lower, self.DELETE_KEYWORDS),
            ActionType.SEND: self._count_matches(message_lower, self.SEND_KEYWORDS),
        }
        
        total = sum(scores.values())
        target = self._extract_target(user_message)
        
        # Create action items for all actions with scores, calculate confidence for each
        action_items = []
        for action_type, score in scores.items():
            if score > 0 and total > 0:
                confidence = min(0.95, (score / total) * 0.7 + 0.3)
                action_items.append(ActionItem(
                    action=action_type,
                    confidence=confidence,
                    target=target,
                    description=self._generate_description(action_type, target)
                ))
        
        # Sort by confidence and filter
        action_items.sort(key=lambda x: x.confidence, reverse=True)
        action_items = [item for item in action_items if item.confidence >= 0.4]
        
        # Fallback if no actions meet threshold
        if not action_items:
            action_items = [ActionItem(
                action=ActionType.QUERY,
                confidence=0.5,
                target=target,
                description=self._generate_description(ActionType.QUERY, target)
            )]
        
        # Build overall description
        if len(action_items) == 1:
            description = action_items[0].description
        else:
            description = f"Multiple operations: {', '.join(item.action.value for item in action_items)}"
        
        reasoning = "Fallback keyword matching"

        return action_items, description, reasoning

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
