"""
Questioning Service - Pure Distillation Layer

Orchestrates the multi-turn questioning flow for requirement gathering
before app generation.

ARCHITECTURAL PRINCIPLE: This service is a pure distillation layer.
- It extracts what the user said, it does NOT decide anything
- It does NOT determine sufficiency or when to stop
- It does NOT infer requirements or fill gaps
- The main agent receives extracted facts and makes all decisions

Key methods:
- detect_skip_request: Check if user wants to skip questioning
- generate_question: Create the next clarifying question
- extract_facts: Extract facts from conversation (no inference)
"""

import json
import logging
import re
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

from vector_app.ai.client import get_llm_client
from vector_app.ai.models import AIModel
from vector_app.ai.types import LLMSettings
from vector_app.prompts.questioning import (
    EXTRACTION_SYSTEM_PROMPT,
    QUESTION_GENERATION_SYSTEM_PROMPT,
    build_extraction_prompt,
    build_question_generation_prompt,
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
    """Result of a questioning service operation.

    NOTE: This service DISTILLS only. It does not decide sufficiency.
    The main agent receives these facts and decides next steps.

    Attributes:
        next_question: The next question to ask (None if not generating)
        extracted_facts: Facts extracted from conversation (no inference)
        skip_requested: True if user explicitly requested to skip
        skip_keyword: The keyword that triggered skip (empty if not)
    """

    next_question: Optional[str] = None
    extracted_facts: Optional[Dict[str, Any]] = None
    skip_requested: bool = False
    skip_keyword: str = ""


class QuestioningService:
    """
    Pure distillation layer for multi-turn requirement gathering.

    ARCHITECTURAL PRINCIPLE:
    This service extracts and organizes facts from user responses.
    It does NOT decide sufficiency, infer requirements, or fill gaps.
    The main agent receives extracted facts and makes all decisions.

    Methods:
    - detect_skip_request: Reports if user said "skip" (not a decision)
    - generate_question: Creates next question (always generates)
    - extract_facts: Extracts what user said (no inference)

    Follows the same singleton pattern as IntentClassifier.
    """

    def detect_skip_request(self, user_message: str) -> tuple[bool, str]:
        """Detect if user wants to skip questioning.

        Returns:
            Tuple of (should_skip, detected_keyword)
            - should_skip: True if skip keyword detected
            - detected_keyword: The keyword that triggered skip (empty if not)

        NOTE: This does NOT decide sufficiency. Only reports if user said "skip".
        """
        message_lower = user_message.lower().strip()
        for keyword in SKIP_KEYWORDS:
            if keyword in message_lower:
                logger.info("Skip keyword detected: %s", keyword)
                return (True, keyword)
        return (False, "")

    def generate_question(
        self,
        chat_history: List[Dict],
        initial_request: str,
        question_count: int,
        model: AIModel = AIModel.CLAUDE_SONNET_4_5,
    ) -> Optional[str]:
        """Generate the next clarifying question.

        NOTE: This always generates a question. The main agent decides
        when to stop questioning — this service does not make that decision.

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

            question = result.validated(default="").strip()
            if question:
                logger.debug("Generated question: %s", question[:100])
                return question

            logger.warning("Question generation returned empty response")
            return None

        except Exception as e:
            logger.warning("Question generation failed: %s", e)
            return None

    def extract_facts(
        self,
        chat_history: List[Dict],
        initial_request: str,
        model: AIModel = AIModel.CLAUDE_SONNET_4_5,
    ) -> Optional[Dict[str, Any]]:
        """Extract facts from the conversation — NO INFERENCE.

        Returns dict with:
        - goals: List of explicitly stated goals
        - explicit_requirements: List of stated requirements
        - ui_mentions: List of UI/UX specifics mentioned
        - unknowns: List of things not answered or unclear

        NOTE: This extracts ONLY what the user said. No inference,
        no gap-filling, no "reasonable defaults". The main agent
        uses these facts to decide next steps.

        Args:
            chat_history: List of previous messages [{role, content}, ...]
            initial_request: The original user request that triggered questioning
            model: LLM model to use (defaults to Sonnet for quality)

        Returns:
            Extracted facts dict, or None if extraction failed
        """
        formatted_history = self._format_chat_history(chat_history)

        prompt = build_extraction_prompt(
            initial_request=initial_request,
            chat_history=formatted_history,
        )

        try:
            result = get_llm_client().run(
                system_prompt=EXTRACTION_SYSTEM_PROMPT,
                user_prompt=prompt,
                llm_settings=LLMSettings(
                    model=model,
                    temperature=0.1,
                    max_tokens=2000,
                    timeout=60.0,
                ),
                json_mode=True,
            )

            data = result.validated(json.loads, default=None)
            if data and isinstance(data, dict):
                logger.info("Extracted facts: %s", list(data.keys()))
                return data

            logger.warning("Failed to parse extraction response")
            return None

        except Exception as e:
            logger.warning("Fact extraction failed: %s", e)
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
