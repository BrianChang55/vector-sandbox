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

        Uses the sufficiency decision prompt to have the LLM assess:
        - How many unknowns remain?
        - Are answers clear or vague?
        - How many questions have we asked?
        - Is the request simple enough to proceed?
        """
        import json

        from vector_app.prompts.main_agent import (
            SUFFICIENCY_DECISION_SYSTEM_PROMPT,
            build_sufficiency_decision_prompt,
        )

        # Hard limit: don't exceed max questions
        MAX_QUESTIONS = 5
        if session.question_count >= MAX_QUESTIONS:
            return False

        # Format chat history for prompt
        formatted_history = self._format_chat_history(chat_history)

        prompt = build_sufficiency_decision_prompt(
            initial_request=session.initial_request,
            chat_history=formatted_history,
            extracted_facts=extracted_facts or {},
            question_count=session.question_count,
        )

        try:
            result = get_llm_client().run(
                system_prompt=SUFFICIENCY_DECISION_SYSTEM_PROMPT,
                user_prompt=prompt,
                llm_settings=LLMSettings(
                    model=AIModel.CLAUDE_HAIKU_4_5,  # Fast model for control decisions
                    temperature=0.1,
                    max_tokens=500,
                    timeout=15.0,
                ),
                json_mode=True,
            )

            data = result.validated(json.loads, default=None)
            if data and isinstance(data, dict):
                should_continue = data.get("should_continue", False)
                reasoning = data.get("reasoning", "")
                logger.debug("Sufficiency decision: continue=%s, reason=%s", should_continue, reasoning)
                return should_continue

            return False

        except Exception as e:
            logger.warning("Sufficiency check failed: %s, defaulting to proceed", e)
            return False

    def _format_chat_history(self, chat_history: List[Dict]) -> str:
        """Format chat history for prompt."""
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
