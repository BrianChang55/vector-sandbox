"""
Main Agent Service - Central Orchestrator

Makes all control-flow decisions for the questioning phase.
Uses QuestioningService for distillation (extracting facts, generating questions).

ARCHITECTURAL PRINCIPLE: This service DECIDES, subservices DISTILL.
- QuestioningService extracts facts from conversation
- MainAgentService decides: ask more? enough? skip?

State machine: idle → (questioning OR skip) → extracted → ready
"""

import logging
from dataclasses import dataclass
from enum import StrEnum
from typing import Any, Dict, List, Optional, TYPE_CHECKING

from vector_app.ai.client import get_llm_client
from vector_app.ai.models import AIModel
from vector_app.ai.types import LLMSettings
from vector_app.services.questioning_service import (
    get_questioning_service,
)

if TYPE_CHECKING:
    from vector_app.models import QuestioningSession

__all__ = [
    "AgentState",
    "AgentDecision",
    "MainAgentService",
    "get_main_agent_service",
]

logger = logging.getLogger(__name__)


class AgentState(StrEnum):
    """State machine states for the questioning flow."""
    IDLE = "idle"
    QUESTIONING = "questioning"
    SKIP = "skip"
    EXTRACTED = "extracted"
    READY = "ready"


@dataclass
class AgentDecision:
    """Result of an agent decision.

    Attributes:
        action: What to do next (ask_question, extract, proceed)
        next_state: State to transition to
        question: The question to ask (if action is ask_question)
        extracted_facts: Facts extracted (if action is extract)
        reasoning: Why this decision was made
    """
    action: str  # "ask_question" | "extract" | "proceed" | "skip"
    next_state: AgentState
    question: Optional[str] = None
    extracted_facts: Optional[Dict[str, Any]] = None
    reasoning: str = ""


class MainAgentService:
    """
    Central orchestrator for the questioning flow.

    Makes control-flow decisions:
    - Should I ask another question?
    - Is the last answer vague or clear?
    - Did the user request to skip?
    - Do I have enough information to proceed?

    These are CONTROL-FLOW decisions, not CONTENT decisions.
    The QuestioningService provides the raw facts; this service decides what to do.
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
            return self._handle_skip(session, chat_history, skip_keyword)

        # Decide if we need more questions or have enough
        return self._decide_next_step(session, chat_history)

    def _handle_skip(
        self,
        session: "QuestioningSession",
        chat_history: List[Dict],
        skip_keyword: str,
    ) -> AgentDecision:
        """Handle user skip request — extract what we have and proceed."""
        # Extract whatever facts we have
        facts = self._questioning.extract_facts(
            chat_history=chat_history,
            initial_request=session.initial_request,
        )

        return AgentDecision(
            action="skip",
            next_state=AgentState.READY,
            extracted_facts=facts,
            reasoning=f"User requested skip with keyword: '{skip_keyword}'",
        )

    def _decide_next_step(
        self,
        session: "QuestioningSession",
        chat_history: List[Dict],
    ) -> AgentDecision:
        """Decide whether to ask more questions or proceed.

        Uses LLM to make control-flow decision based on:
        - Chat history and what's been answered
        - Number of questions already asked
        - Clarity of recent answers
        """
        # First extract current facts to inform decision
        facts = self._questioning.extract_facts(
            chat_history=chat_history,
            initial_request=session.initial_request,
        )

        # Use LLM to decide: ask more or proceed?
        should_continue = self._should_continue_questioning(
            session=session,
            chat_history=chat_history,
            extracted_facts=facts,
        )

        if should_continue:
            # Generate next question
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
                    reasoning="More clarification needed",
                )

        # We have enough — extract and proceed
        return AgentDecision(
            action="proceed",
            next_state=AgentState.READY,
            extracted_facts=facts,
            reasoning="Sufficient information gathered",
        )

    def _should_continue_questioning(
        self,
        session: "QuestioningSession",
        chat_history: List[Dict],
        extracted_facts: Optional[Dict[str, Any]],
    ) -> bool:
        """LLM-based decision: should we ask more questions?

        Considers:
        - How many unknowns remain?
        - Are answers clear or vague?
        - How many questions have we asked?
        - Is the request simple enough to proceed?
        """
        # TODO: Implement LLM-based sufficiency check in Task 3
        # For now, simple heuristic: max 5 questions
        if session.question_count >= 5:
            return False

        # Check if we have critical unknowns
        if extracted_facts:
            unknowns = extracted_facts.get("unknowns", [])
            # If many unknowns and we haven't asked much, continue
            if len(unknowns) > 2 and session.question_count < 3:
                return True

        return False


# Singleton instance
_main_agent_service: Optional[MainAgentService] = None


def get_main_agent_service() -> MainAgentService:
    """Get singleton main agent service instance."""
    global _main_agent_service
    if _main_agent_service is None:
        _main_agent_service = MainAgentService()
    return _main_agent_service
