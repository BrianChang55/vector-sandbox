"""
Questioning Service

Orchestrates the multi-turn questioning flow for requirement gathering
before app generation. Uses LLM to generate clarifying questions,
check when sufficient information has been gathered, and synthesize
final requirements from the conversation.

Key methods:
- check_exit_condition: Determine if questioning phase should end
- generate_question: Create the next clarifying question
- synthesize_requirements: Convert conversation into structured requirements
"""

import json
import logging
import re
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, TYPE_CHECKING

from vector_app.ai.client import get_llm_client
from vector_app.ai.models import AIModel
from vector_app.ai.types import LLMSettings
from vector_app.prompts.questioning import (
    QUESTION_GENERATION_SYSTEM_PROMPT,
    SUFFICIENCY_CHECK_SYSTEM_PROMPT,
    SYNTHESIS_SYSTEM_PROMPT,
    build_question_generation_prompt,
    build_sufficiency_check_prompt,
    build_synthesis_prompt,
)

__all__ = [
    "QuestioningResult",
    "QuestioningService",
    "get_questioning_service",
]

logger = logging.getLogger(__name__)

# Keywords that indicate user wants to skip questioning
SKIP_KEYWORDS = [
    "pass",
    "skip",
    "just build",
    "build it",
    "that's enough",
    "thats enough",
    "go ahead",
]


@dataclass
class QuestioningResult:
    """Result of a questioning phase operation.

    Attributes:
        is_complete: Whether the questioning phase is done
        should_skip: Whether the user requested to skip questioning
        next_question: The next question to ask (None if complete)
        synthesized_requirements: Final requirements document (None if not complete)
        reasoning: Explanation of the decision
    """

    is_complete: bool
    should_skip: bool = False
    next_question: Optional[str] = None
    synthesized_requirements: Optional[Dict[str, Any]] = None
    reasoning: str = ""


class QuestioningService:
    """
    Orchestrates the multi-turn questioning flow for requirement gathering.

    Uses LLM to:
    - Check if user wants to skip or if we have enough information
    - Generate clarifying questions based on conversation context
    - Synthesize structured requirements from the Q&A conversation

    Follows the same singleton pattern as IntentClassifier.
    """

    def check_exit_condition(
        self,
        user_message: str,
        chat_history: List[Dict],
        initial_request: str,
        model: AIModel = AIModel.CLAUDE_HAIKU_4_5,
    ) -> tuple[bool, bool, str]:
        """Check if questioning phase should end.

        First checks for skip keywords, then uses LLM to evaluate
        if sufficient information has been gathered.

        Args:
            user_message: The user's latest message
            chat_history: List of previous messages [{role, content}, ...]
            initial_request: The original user request that triggered questioning
            model: LLM model to use (defaults to Haiku for speed)

        Returns:
            Tuple of (is_complete, should_skip, reasoning)
            - is_complete: True if questioning should end
            - should_skip: True if user explicitly requested to skip
            - reasoning: Explanation of the decision
        """
        message_lower = user_message.lower().strip()

        # Check for skip keywords first
        for keyword in SKIP_KEYWORDS:
            if keyword in message_lower:
                logger.info("User requested to skip questioning with keyword: %s", keyword)
                return (True, True, f"User requested to skip questioning: '{keyword}'")

        # Use LLM to check sufficiency
        formatted_history = self._format_chat_history(chat_history)

        # Build gathered info summary for sufficiency check
        gathered_info = self._summarize_gathered_info(chat_history)

        prompt = build_sufficiency_check_prompt(
            initial_request=initial_request,
            chat_history=formatted_history,
            gathered_info=gathered_info,
        )

        try:
            result = get_llm_client().run(
                system_prompt=SUFFICIENCY_CHECK_SYSTEM_PROMPT,
                user_prompt=prompt,
                llm_settings=LLMSettings(
                    model=model,
                    temperature=0.1,
                    max_tokens=500,
                    timeout=30.0,
                ),
            )

            data = result.validated(self._parse_json_response, default=None)
            if data and isinstance(data, dict):
                has_enough = data.get("has_enough", False)
                reasoning = data.get("reasoning", "LLM determined sufficiency")
                logger.info(
                    "Sufficiency check result: has_enough=%s, reasoning=%s",
                    has_enough,
                    reasoning,
                )
                return (has_enough, False, reasoning)

            logger.warning("Failed to parse sufficiency check response")
            return (False, False, "Could not determine sufficiency")

        except Exception as e:
            logger.warning("Sufficiency check failed: %s", e)
            return (False, False, "Could not determine sufficiency due to error")

    def generate_question(
        self,
        chat_history: List[Dict],
        initial_request: str,
        question_count: int,
        model: AIModel = AIModel.CLAUDE_SONNET_4_5,
    ) -> Optional[str]:
        """Generate the next clarifying question.

        Args:
            chat_history: List of previous messages [{role, content}, ...]
            initial_request: The original user request that triggered questioning
            question_count: Number of questions already asked
            model: LLM model to use (defaults to Sonnet for quality)

        Returns:
            The next question to ask, or None if generation failed
        """
        formatted_history = self._format_chat_history(chat_history)

        prompt = build_question_generation_prompt(
            initial_request=initial_request,
            chat_history=formatted_history,
            question_count=question_count,
        )

        try:
            result = get_llm_client().run(
                system_prompt=QUESTION_GENERATION_SYSTEM_PROMPT,
                user_prompt=prompt,
                llm_settings=LLMSettings(
                    model=model,
                    temperature=0.7,
                    max_tokens=500,
                    timeout=30.0,
                ),
            )

            question = result.content.strip() if result.content else None
            if question:
                logger.debug("Generated question: %s", question[:100])
                return question

            logger.warning("Question generation returned empty response")
            return None

        except Exception as e:
            logger.warning("Question generation failed: %s", e)
            return None

    def synthesize_requirements(
        self,
        chat_history: List[Dict],
        initial_request: str,
        model: AIModel = AIModel.CLAUDE_SONNET_4_5,
    ) -> Optional[Dict[str, Any]]:
        """Synthesize structured requirements from the conversation.

        Args:
            chat_history: List of previous messages [{role, content}, ...]
            initial_request: The original user request that triggered questioning
            model: LLM model to use (defaults to Sonnet for quality)

        Returns:
            Structured requirements dict, or None if synthesis failed
        """
        formatted_history = self._format_chat_history(chat_history)

        prompt = build_synthesis_prompt(
            initial_request=initial_request,
            chat_history=formatted_history,
        )

        try:
            result = get_llm_client().run(
                system_prompt=SYNTHESIS_SYSTEM_PROMPT,
                user_prompt=prompt,
                llm_settings=LLMSettings(
                    model=model,
                    temperature=0.3,
                    max_tokens=2000,
                    timeout=60.0,
                ),
            )

            data = result.validated(self._parse_json_response, default=None)
            if data and isinstance(data, dict):
                logger.info("Successfully synthesized requirements: %s", list(data.keys()))
                return data

            logger.warning("Failed to parse synthesis response")
            return None

        except Exception as e:
            logger.warning("Requirements synthesis failed: %s", e)
            return None

    def _format_chat_history(self, chat_history: List[Dict]) -> str:
        """Format chat history for prompt injection.

        Args:
            chat_history: List of messages [{role, content}, ...]

        Returns:
            Formatted string with each message on its own line
        """
        if not chat_history:
            return "No conversation yet."

        lines = []
        for msg in chat_history:
            role = msg.get("role", "user").capitalize()
            content = msg.get("content", "")
            lines.append(f"{role}: {content}")

        return "\n".join(lines)

    def _summarize_gathered_info(self, chat_history: List[Dict]) -> str:
        """Create a brief summary of gathered information.

        Args:
            chat_history: List of messages [{role, content}, ...]

        Returns:
            Summary string of what we know so far
        """
        if not chat_history:
            return "No information gathered yet."

        # Extract user responses (these contain the actual information)
        user_responses = [
            msg.get("content", "")
            for msg in chat_history
            if msg.get("role") == "user"
        ]

        if not user_responses:
            return "No user responses yet."

        return f"User has provided {len(user_responses)} response(s) with information about their requirements."

    def _parse_json_response(self, content: str) -> Optional[Dict]:
        """Extract a JSON object from an LLM response string.

        Handles markdown code blocks and extracts JSON objects.
        Follows the same pattern as IntentClassifier._load_json_response.

        Args:
            content: Raw LLM response content

        Returns:
            Parsed dict or None if parsing failed
        """
        content = content.strip()

        # Strip markdown code blocks if present
        if content.startswith("```"):
            content = re.sub(r"^```(?:json)?\s*", "", content)
            content = re.sub(r"\s*```$", "", content)

        # Find JSON object in content
        json_match = re.search(r"\{.*\}", content, re.DOTALL)
        if not json_match:
            logger.warning("No JSON found in response: %s", content[:100])
            return None

        try:
            return json.loads(json_match.group())
        except json.JSONDecodeError as e:
            logger.warning("Failed to parse JSON response: %s", e)
            return None


# Singleton instance
_questioning_service: Optional[QuestioningService] = None


def get_questioning_service() -> QuestioningService:
    """Get singleton questioning service instance."""
    global _questioning_service
    if _questioning_service is None:
        _questioning_service = QuestioningService()
    return _questioning_service
