"""
Prompts for action classification using LLM.
"""

from typing import Optional, Tuple
from vector_app.services.intent_classifier import IntentResult


def build_action_classification_prompts(
    user_message: str,
    intent: Optional[IntentResult] = None,
) -> Tuple[str, str]:
    """
    Build system and user prompts for action classification.

    Args:
        user_message: The user's request
        intent: Optional intent result for additional context

    Returns:
        Tuple of (system_prompt, user_prompt)
    """
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

    return system_prompt, user_prompt

