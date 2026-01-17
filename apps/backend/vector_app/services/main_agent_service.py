"""
Main Agent Service - Central Orchestrator

Makes all control-flow decisions for the questioning phase.
Uses QuestioningService for distillation (extracting facts, generating questions).

ARCHITECTURAL PRINCIPLE: This service DECIDES, subservices DISTILL.
- QuestioningService extracts facts from conversation
- MainAgentService decides: ask more? done?

EXTRACTION BOUNDARY PRINCIPLE:
- Extraction happens EXACTLY ONCE: when questioning ends (skip or complete)
- During questioning: NO extraction, NO semantic analysis of answers
- After extraction: high-entropy context (chat_history, initial_request) is destroyed
- Downstream stages (planning/building) only receive extracted facts

State machine: idle → questioning → extracted → ready
"""

import json
import logging
from dataclasses import dataclass
from enum import StrEnum
from typing import Any, Dict, List, Optional, TYPE_CHECKING

from vector_app.ai.client import get_llm_client
from vector_app.ai.models import AIModel
from vector_app.ai.types import LLMSettings
from vector_app.prompts.main_agent import (
    CONTINUATION_DECISION_SYSTEM_PROMPT,
    build_continuation_decision_prompt,
)
from vector_app.services.questioning_service import (
    get_questioning_service,
)

if TYPE_CHECKING:
    from vector_app.models import QuestioningSession

__all__ = [
    "AgentState",
    "AgentDecision",
    "ExtractionResult",
    "MainAgentService",
    "get_main_agent_service",
]

logger = logging.getLogger(__name__)

# Policy constants for questioning phase
MAX_QUESTIONS = 50
MIN_QUESTIONS_BEFORE_COMPLETION = 1


class AgentState(StrEnum):
    """State machine states for the questioning flow."""
    IDLE = "idle"
    QUESTIONING = "questioning"
    EXTRACTED = "extracted"
    READY = "ready"


@dataclass
class ExtractionResult:
    """Result of crossing the extraction boundary.

    This dataclass represents the ONLY output from the questioning phase.
    Once created, the source chat_history and initial_request are destroyed.

    Downstream stages (planning, building) receive ONLY this structure.
    They cannot access raw conversation data.
    """
    facts: Dict[str, Any]
    reasoning: str
    question_count: int

    # Markers that high-entropy context was destroyed
    chat_history_destroyed: bool = True
    initial_request_destroyed: bool = True


@dataclass
class AgentDecision:
    """Result of an agent decision.

    Attributes:
        action: What to do next (ask_question, skip, proceed)
        next_state: State to transition to
        question: The question to ask (if action is ask_question)
        extraction: Extraction result (ONLY set when crossing extraction boundary)
        reasoning: Why this decision was made

    INVARIANT: extraction is ONLY set when crossing the extraction boundary.
    Once set, chat_history and initial_request are destroyed and inaccessible.
    """
    action: str  # "ask_question" | "skip" | "proceed"
    next_state: AgentState
    question: Optional[str] = None
    extraction: Optional[ExtractionResult] = None
    reasoning: str = ""


class MainAgentService:
    """
    Central orchestrator for the questioning flow.

    Makes control-flow decisions:
    - Should I ask another question?
    - Is the last answer vague or clear?
    - Did the user request to skip?
    - Are there blocking unknowns?

    These are CONTROL-FLOW decisions, not CONTENT decisions.
    The QuestioningService provides the raw facts; this service decides what to do.

    EXTRACTION BOUNDARY: Facts are extracted exactly ONCE when questioning ends.
    During questioning, no semantic analysis or extraction occurs.
    """

    def __init__(self):
        self._questioning = get_questioning_service()

    def process_user_message(
        self,
        session: "QuestioningSession",
        user_message: str,
        chat_history: List[Dict],
    ) -> AgentDecision:
        """Process a user message and decide what to do next.

        This is the main entry point. Given a user message:
        1. Check if user requested to skip
        2. If not skipping, decide if we need more questions
        3. Return the appropriate action

        IMPORTANT: This method may destroy chat_history and initial_request
        if the extraction boundary is crossed. Callers must not rely on
        these values after receiving an extraction result.

        Args:
            session: The QuestioningSession being processed
            user_message: The latest user message
            chat_history: Full conversation history

        Returns:
            AgentDecision with action, next_state, and optional data
        """
        # Check for skip request first
        skip_requested, skip_keyword = self._questioning.detect_skip_request(user_message)
        if skip_requested:
            logger.info("User requested skip with keyword: '%s'", skip_keyword)
            return self._cross_extraction_boundary(
                session=session,
                chat_history=chat_history,
                action="skip",
                reasoning=f"User requested skip with keyword: '{skip_keyword}'",
            )

        # Decide if we need more questions or have enough
        logger.info("Deciding next step...")
        return self._decide_next_step(session, chat_history)

    def _decide_next_step(
        self,
        session: "QuestioningSession",
        chat_history: List[Dict],
    ) -> AgentDecision:
        """Decide whether to ask more questions or proceed.

        PRINCIPLE: No extraction happens here. This is purely a control-flow
        decision based on the raw conversation state.

        Uses LLM to make procedural decision based on:
        - Raw chat history (not extracted facts)
        - Number of questions already asked
        - Whether recent answers were clear or vague
        """
        # Procedural decision: should we continue questioning?
        should_continue = self._should_continue_questioning(
            session=session,
            chat_history=chat_history,
        )

        logger.debug("Should continue questioning: %s", should_continue)

        if should_continue:
            # Generate next question (no extraction involved)
            question = self._questioning.generate_question(
                chat_history=chat_history,
                initial_request=session.initial_request,
                question_count=session.question_count,
            )

            if question:
                return AgentDecision(
                    action="ask_question",
                    next_state=AgentState.QUESTIONING,
                    question=question,
                    reasoning="Blocking unknowns remain - more clarification needed",
                )

        # Questioning complete — cross the extraction boundary
        return self._cross_extraction_boundary(
            session=session,
            chat_history=chat_history,
            action="proceed",
            reasoning="No blocking unknowns remain - proceeding to build",
        )

    def _cross_extraction_boundary(
        self,
        session: "QuestioningSession",
        chat_history: List[Dict],
        action: str,
        reasoning: str,
    ) -> AgentDecision:
        """Cross the extraction boundary - extract facts and destroy high-entropy context.

        This is the ONLY place where extraction happens.
        After this call, chat_history and initial_request are conceptually destroyed.

        Args:
            session: The QuestioningSession being processed
            chat_history: Full conversation history (will be destroyed after extraction)
            action: The action that triggered extraction ("skip" or "proceed")
            reasoning: Why we're crossing the boundary

        Returns:
            AgentDecision with extraction result
        """
        logger.info(
            "Crossing extraction boundary: action=%s, question_count=%d",
            action,
            session.question_count,
        )

        # Extract facts - this is the ONE AND ONLY extraction call
        facts = self._questioning.extract_facts(
            chat_history=chat_history,
            initial_request=session.initial_request,
        )

        # Create extraction result - marks that high-entropy context is destroyed
        extraction = ExtractionResult(
            facts=facts,
            reasoning=reasoning,
            question_count=session.question_count,
            chat_history_destroyed=True,
            initial_request_destroyed=True,
        )

        return AgentDecision(
            action=action,
            next_state=AgentState.READY,
            extraction=extraction,
            reasoning=reasoning,
        )

    def _parse_json_response(self, content: str) -> Optional[Dict]:
        """Parse JSON response, handling markdown code fences.

        LLMs sometimes wrap JSON in ```json ... ``` blocks.
        Strip those before parsing.
        """
        stripped = content.strip()

        # Remove markdown code fences
        if stripped.startswith("```"):
            # Remove opening fence (```json or ```)
            lines = stripped.split("\n")
            if lines[0].startswith("```"):
                lines = lines[1:]
            # Remove closing fence
            if lines and lines[-1].strip() == "```":
                lines = lines[:-1]
            stripped = "\n".join(lines).strip()

        try:
            return json.loads(stripped)
        except json.JSONDecodeError as e:
            logger.warning("Failed to parse JSON: %s", e)
            return None

    def _should_continue_questioning(
        self,
        session: "QuestioningSession",
        chat_history: List[Dict],
    ) -> bool:
        """Procedural decision: should we ask more questions?

        PRINCIPLE: This is a purely PROCEDURAL decision based on:
        - Raw chat history (conversation turns)
        - Number of questions asked
        - Policy constants

        FORBIDDEN inputs:
        - Extracted facts
        - Inferred structure
        - Semantic judgments about "sufficiency"

        The LLM analyzes the RAW conversation to determine if there are
        unresolved or unclear aspects that would BLOCK building.
        """
        # Hard limit: don't exceed max questions
        if session.question_count >= MAX_QUESTIONS:
            logger.debug("Hit max questions limit (%d), stopping", MAX_QUESTIONS)
            return False

        # Format chat history for prompt (raw, no extraction)
        formatted_history = self._format_chat_history(chat_history)

        prompt = build_continuation_decision_prompt(
            initial_request=session.initial_request,
            chat_history=formatted_history,
        )

        try:
            result = get_llm_client().run(
                system_prompt=CONTINUATION_DECISION_SYSTEM_PROMPT,
                user_prompt=prompt,
                llm_settings=LLMSettings(
                    model=AIModel.CLAUDE_SONNET_4_5,  # Fast model for procedural decisions
                    temperature=0.1,
                    max_tokens=500,
                    timeout=15.0,
                ),
                json_mode=True,
            )

            data = result.validated(self._parse_json_response, default=None)
            if data and isinstance(data, dict):
                should_continue = data.get("should_continue", False)
                reasoning = data.get("reasoning", "")
                unclear_aspects = data.get("unclear_aspects", [])
                logger.debug(
                    "Continuation decision: continue=%s, unclear_aspects=%s, reason=%s",
                    should_continue,
                    unclear_aspects,
                    reasoning,
                )
                return should_continue

            return False

        except Exception as e:
            logger.warning("Continuation check failed: %s, defaulting to proceed", e)
            return False

    def _format_chat_history(self, chat_history: List[Dict]) -> str:
        """Format chat history for prompt.

        Returns raw conversation turns, no semantic analysis.
        """
        if not chat_history:
            return "No conversation yet."

        lines = []
        for msg in chat_history:
            role = msg.get("role", "user").capitalize()
            content = msg.get("content", "")
            lines.append(f"{role}: {content}")

        return "\n".join(lines)


# Singleton instance
_main_agent_service: Optional[MainAgentService] = None


def get_main_agent_service() -> MainAgentService:
    """Get singleton main agent service instance."""
    global _main_agent_service
    if _main_agent_service is None:
        _main_agent_service = MainAgentService()
    return _main_agent_service
