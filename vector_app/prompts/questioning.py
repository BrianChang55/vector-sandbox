"""
Questioning Phase Prompts - Distillation Layer

Prompts for multi-turn requirement gathering before app generation.

ARCHITECTURAL PRINCIPLE: This module is a pure distillation layer.
- It extracts ONLY what the user explicitly said
- It does NOT decide sufficiency or when to stop questioning
- It does NOT infer requirements or fill gaps with defaults
- The main agent receives extracted facts and makes all decisions

Two prompt pairs:
1. Question Generation - Ask clarifying questions one at a time (always generates)
2. Extraction - Extract facts from conversation (no inference)
"""

# =============================================================================
# Question Generation Prompts
# =============================================================================

QUESTION_GENERATION_SYSTEM_PROMPT = """You are an expert requirements gatherer for an AI app builder.
Your job is to ask clarifying questions to understand what the user wants to build.

Guidelines:
- Ask ONE focused question at a time
- Build on previous answers in the conversation
- Be conversational and friendly, not interrogative
- Focus on understanding: app type, data requirements, key features, user workflows

Key areas to explore:
- What problem is the app solving?
- What data will it work with (types, sources, relationships)?
- What are the main features or capabilities needed?
- How will users interact with it (views, forms, dashboards)?
- Any specific UI preferences or constraints?

IMPORTANT: Always generate a question. The main agent decides when questioning is complete."""

QUESTION_GENERATION_PROMPT = """Based on the user's request and our conversation so far, ask a clarifying question.

## Original Request
{initial_request}

## Conversation So Far
{chat_history}

## Questions Asked: {question_count}

Generate your next clarifying question. Be specific and build on what you've learned.

Return ONLY your question, no other text."""

# =============================================================================
# Extraction Prompts (Distillation Only - NO INFERENCE)
# =============================================================================

EXTRACTION_SYSTEM_PROMPT = """You are an expert at extracting information from conversations.
Your job is to identify what the user EXPLICITLY stated — nothing more.

CRITICAL RULES:
- Extract ONLY what the user actually said
- DO NOT infer anything not explicitly stated
- DO NOT fill gaps with "reasonable defaults"
- DO NOT assume anything the user didn't mention
- If something wasn't mentioned, it goes in "unknowns"

Output structure:
- goals: List of explicitly stated goals/purposes
- explicit_requirements: List of requirements the user actually mentioned
- ui_mentions: List of UI/UX specifics the user said they want
- unknowns: List of things NOT answered OR answers that were unclear/vague

For unknowns, include things like:
- Questions that were asked but not answered
- Vague answers that need clarification
- Critical aspects not discussed (data source, user types, etc.)"""

EXTRACTION_PROMPT = """Extract facts from this conversation. NO INFERENCE — only what was explicitly said.

## Original Request
{initial_request}

## Full Conversation
{chat_history}

Extract information into these categories:
- goals: What the user explicitly said they want to achieve
- explicit_requirements: Features/capabilities the user actually mentioned
- ui_mentions: Any UI/UX preferences or specifics the user stated
- unknowns: Things not answered, unclear, or not discussed

Return ONLY valid JSON:
{{
  "goals": ["explicitly stated goal 1", "explicitly stated goal 2"],
  "explicit_requirements": ["user said they want X", "user mentioned Y"],
  "ui_mentions": ["user said dark mode", "user wants sidebar"],
  "unknowns": ["data source not specified", "unclear if real-time needed"]
}}"""


# =============================================================================
# Builder Functions
# =============================================================================


def build_question_generation_prompt(
    initial_request: str,
    chat_history: str,
    question_count: int,
) -> str:
    """Build the question generation prompt.

    Args:
        initial_request: The user's original request that triggered questioning
        chat_history: Formatted Q&A conversation so far
        question_count: Number of questions already asked
    """
    return QUESTION_GENERATION_PROMPT.format(
        initial_request=initial_request,
        chat_history=chat_history or "No questions asked yet.",
        question_count=question_count,
    )


def build_extraction_prompt(
    initial_request: str,
    chat_history: str,
) -> str:
    """Build the extraction prompt.

    NOTE: This extracts facts only — no inference, no gap-filling.
    The main agent receives this and decides what to do next.

    Args:
        initial_request: The user's original request that triggered questioning
        chat_history: Full Q&A conversation
    """
    return EXTRACTION_PROMPT.format(
        initial_request=initial_request,
        chat_history=chat_history,
    )
