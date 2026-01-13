"""
Prompts for action classification using LLM.
"""

from typing import List, Optional, Tuple, TYPE_CHECKING

if TYPE_CHECKING:
    from vector_app.services.context_analyzer import AppContext

from vector_app.services.intent_classifier import IntentResult


def build_action_classification_prompts(
    user_message: str,
    intent: Optional[IntentResult] = None,
    available_connectors: Optional[List[str]] = None,
    context: Optional["AppContext"] = None,
) -> Tuple[str, str]:
    """
    Build system and user prompts for action classification.

    Args:
        user_message: The user's request
        intent: Optional intent result for additional context
        available_connectors: Optional list of available service/connector names
        context: Optional app context with conversation history and used connectors

    Returns:
        Tuple of (system_prompt, user_prompt)
    """
    system_prompt = """You are an expert at classifying user requests for internal app building.

Your task is to determine what type of MCP (Model Context Protocol) tool operations the user's app will need to perform on external systems.

IMPORTANT: 
1. A single request can require MULTIPLE action types. For example:
   - "Fetch GitHub issues and send Slack notifications" = [QUERY on github, SEND on slack]
   - "Create a Jira ticket and update the status" = [CREATE on jira, UPDATE on jira]

2. The SAME action type can apply to MULTIPLE services. For example:
   - "Show my GitHub PRs and Linear tickets" = [QUERY on github, QUERY on linear]
   - "Create a Jira issue and a Slack channel" = [CREATE on jira, CREATE on slack]

Action Types:
- QUERY: The app needs to READ/FETCH data (e.g., "show GitHub PRs", "display Jira issues", "list users")
- CREATE: The app needs to CREATE new records (e.g., "create Jira tickets", "add calendar events", "submit forms")
- UPDATE: The app needs to MODIFY existing records (e.g., "update issue status", "edit documents", "modify settings")
- DELETE: The app needs to REMOVE records (e.g., "delete old files", "archive tickets", "remove users")
- SEND: The app needs to SEND messages/notifications (e.g., "send Slack messages", "email notifications", "SMS alerts")
- OTHER: Fallback for operations that don't fit the above categories

Target Services:
- CAREFULLY identify ALL services/systems mentioned in the request
- If the same action applies to multiple services, create a SEPARATE action object for each service
- ONLY use service names from the provided list of available connectors
- Match service names as closely as possible to the available connectors (case-insensitive matching is fine)
- If a mentioned service is not in the available list, use the closest match or "unknown"

Return a JSON object with:
{
  "actions": [
    {
      "action": "QUERY|CREATE|UPDATE|DELETE|SEND|OTHER",
      "confidence": 0.0-1.0,
      "target": "specific service name or 'unknown'",
      "description": "brief description of this specific operation on this service"
    }
    // Create separate action objects for:
    // - Different action types (QUERY vs CREATE)
    // - Same action type on different services (QUERY on github, QUERY on linear)
  ],
  "reasoning": "why you chose these classifications and which services were identified"
}

Always return at least 1 action. Return multiple actions when the request requires different operation types OR the same operation on multiple services."""

    user_prompt = f"""User's request: {user_message}"""
    
    # Add app context information for better classification
    if context:
        if context.has_existing_app:
            user_prompt += f"\n\nApp context: Existing app with {context.file_count} files"
            if context.existing_tables:
                table_names = ", ".join([t.name for t in context.existing_tables[:3]])
                more = f" (and {len(context.existing_tables) - 3} more)" if len(context.existing_tables) > 3 else ""
                user_prompt += f", {len(context.existing_tables)} data table(s): {table_names}{more}"
        else:
            user_prompt += "\n\nApp context: New app (no existing code)"
        
        # Include used connectors - these are ALREADY integrated into the app
        if context.used_connectors:
            user_prompt += f"\n\nConnectors already used by this app: {', '.join(context.used_connectors)}"
            user_prompt += "\nIMPORTANT: These connectors are already integrated. If the user's request relates to these services, they likely want to continue using them or add related operations."
        
        # Include recent conversation history for context
        if context.message_history:
            history_count = len(context.message_history)
            user_prompt += f"\n\nConversation history ({history_count} previous message(s)):"
            # Include last 3 messages for context
            for i, msg in enumerate(context.message_history[-3:], 1):
                msg_num = history_count - 3 + i if history_count > 3 else i
                preview = msg[:150] + "..." if len(msg) > 150 else msg
                user_prompt += f"\n  {msg_num}. {preview}"
            user_prompt += "\nNOTE: Use this history to understand the context and continuity of the user's requests."
    
    if available_connectors:
        connectors_list = ", ".join(available_connectors)
        user_prompt += f"\n\nAvailable connectors/services: {connectors_list}"
    
    if intent:
        user_prompt += f"\n\nIntent context: The user wants to {intent.intent.value} the app (confidence: {intent.confidence:.0%})"

    return system_prompt, user_prompt

