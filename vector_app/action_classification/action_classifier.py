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
from typing import List, Optional, Tuple

import httpx

from vector_app.action_classification.types import (
    ActionType,
    ActionResult,
    ActionItem,
    QUERY_KEYWORDS,
    CREATE_KEYWORDS,
    UPDATE_KEYWORDS,
    DELETE_KEYWORDS,
    SEND_KEYWORDS,
)
from vector_app.action_classification.prompts import build_action_classification_prompts
from vector_app.services.intent_classifier import IntentResult
from vector_app.services.openrouter_service import get_openrouter_service

logger = logging.getLogger(__name__)


def _log_classification_input(
    user_message: str,
    context: Optional["AppContext"],
    intent: Optional["IntentResult"],
    available_connectors: Optional[List[str]],
) -> None:
    """Log classification input details at debug level."""
    logger.debug("=" * 60)
    logger.debug("ACTION CLASSIFICATION - Determining MCP tool operations needed")
    logger.debug("=" * 60)
    logger.debug("Input message: %s%s", user_message[:200], "..." if len(user_message) > 200 else "")
    logger.debug("Has context: %s", context is not None)
    logger.debug("Has intent: %s", intent is not None)
    if intent:
        logger.debug("  Intent type: %s", intent.intent.value)
        logger.debug("  Intent confidence: %.0f%%", intent.confidence * 100)
    if available_connectors:
        logger.debug("Available connectors: %s", ", ".join(available_connectors))
    logger.debug("-" * 40)
    logger.debug("LLM CLASSIFICATION:")


def _log_classification_result(result: "ActionResult") -> None:
    """Log classification result details at debug level."""
    logger.debug("-" * 40)
    logger.debug("MCP OPERATIONS NEEDED:")
    logger.debug("  Total Actions: %d (filtered, confidence >= 0.4)", len(result.actions))
    logger.debug("  Description: %s", result.description)
    if result.actions:
        logger.debug("  All Actions (ranked by confidence):")
        for i, action in enumerate(result.actions, 1):
            logger.debug("    %d. %s (%.0f%%, target: %s) - %s", 
                        i, action.action.value, action.confidence * 100, 
                        action.target, action.description)
    else:
        logger.warning("  No actions classified!")
    logger.debug("=" * 60)


def _log_processed_actions(
    action_items: List["ActionItem"],
    reasoning: str,
) -> None:
    """Log processed action items at debug level."""
    logger.debug("-" * 40)
    logger.debug("Classification result:")
    logger.debug("  Number of actions (after filtering <0.4): %d", len(action_items))
    for i, item in enumerate(action_items, 1):
        logger.debug("  Action %d: %s (%.0f%%, target: %s) - %s", 
                    i, item.action.value, item.confidence * 100, item.target, item.description)
    logger.debug("  Reasoning: %s", reasoning)


class ActionClassifier:
    """
    Classifies user messages to determine what MCP tool operations
    the user's app will need to perform on external systems.

    This identifies the data operations required, not what we do to the app.
    """

    def classify(
        self,
        user_message: str,
        context: Optional["AppContext"] = None,
        intent: Optional["IntentResult"] = None,
        available_connectors: Optional[List[str]] = None,
    ) -> "ActionResult":
        """
        Classify the user's action based on their message using LLM.

        Args:
            user_message: The user's request
            context: Optional app context
            intent: Optional intent result for additional context
            available_connectors: Optional list of available service/connector names

        Returns:
            ActionResult with classified action and metadata
        """
        _log_classification_input(user_message, context, intent, available_connectors)

        system_prompt, user_prompt = build_action_classification_prompts(
            user_message, intent, available_connectors
        )

        try:
            action_items, description, reasoning = self._classify_with_llm(
                system_prompt, user_prompt
            )
        except Exception as e:
            logger.error("LLM classification failed, falling back to keyword-based classification: %s", e)
            action_items, description, reasoning = self._classify_with_keywords(
                user_message
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

        _log_classification_result(result)

        return result

    def _process_and_filter_actions(
        self,
        action_items: List[ActionItem],
        reasoning: str,
        default_target: str = "unknown",
        default_description: str = "Read data"
    ) -> Tuple[List[ActionItem], str]:
        """
        Process, filter, and log action items.

        Args:
            action_items: List of action items to process
            reasoning: Classification reasoning for logging
            default_target: Target for default action if all filtered out
            default_description: Description for default action if all filtered out

        Returns:
            Tuple of (filtered_action_items, description)
        """
        action_items.sort(key=lambda x: x.confidence, reverse=True)
        action_items = [item for item in action_items if item.confidence >= 0.4]

        if not action_items:
            logger.warning("All actions filtered out due to low confidence, using default QUERY")
            action_items = [ActionItem(
                action=ActionType.QUERY,
                confidence=0.5,
                target=default_target,
                description=default_description
            )]

        if len(action_items) == 1:
            description = action_items[0].description
        else:
            description = f"Multiple operations: {', '.join(item.action.value for item in action_items)}"

        _log_processed_actions(action_items, reasoning)

        return action_items, description

    def _classify_with_llm(
        self,
        system_prompt: str,
        user_prompt: str,
    ) -> Tuple[List[ActionItem], str, str]:
        """
        Classify actions using LLM.

        Args:
            system_prompt: System prompt for the LLM
            user_prompt: User prompt for the LLM

        Returns:
            Tuple of (action_items, description, reasoning)
        """
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
        
        logger.debug("LLM Response: %s", json.dumps(classification, indent=2))
        
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
                parsed_action = ActionType[action_str]
            except KeyError:
                logger.warning("Invalid action type from LLM: %s, defaulting to QUERY", action_str)
                parsed_action = ActionType.QUERY
            
            action_items.append(ActionItem(
                action=parsed_action,
                confidence=float(action_data.get("confidence", 0.7)),
                target=action_data.get("target", "unknown"),
                description=action_data.get("description", f"{parsed_action.value} operation")
            ))
        
        # Process and filter actions
        action_items, description = self._process_and_filter_actions(
            action_items, reasoning
        )

        return action_items, description, reasoning

    def _classify_with_keywords(
        self,
        user_message: str,
    ) -> Tuple[List[ActionItem], str, str]:
        """
        Classify actions using keyword matching (fallback method).

        Args:
            user_message: The user's message

        Returns:
            Tuple of (action_items, description, reasoning)
        """
        # Fallback to simple keyword matching if LLM fails
        message_lower = user_message.lower()
        scores = {
            ActionType.QUERY: self._count_matches(message_lower, QUERY_KEYWORDS),
            ActionType.CREATE: self._count_matches(message_lower, CREATE_KEYWORDS),
            ActionType.UPDATE: self._count_matches(message_lower, UPDATE_KEYWORDS),
            ActionType.DELETE: self._count_matches(message_lower, DELETE_KEYWORDS),
            ActionType.SEND: self._count_matches(message_lower, SEND_KEYWORDS),
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
        
        reasoning = "Fallback keyword matching"
        
        # Process and filter actions
        action_items, description = self._process_and_filter_actions(
            action_items,
            reasoning,
            default_target=target,
            default_description=self._generate_description(ActionType.QUERY, target)
        )

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
