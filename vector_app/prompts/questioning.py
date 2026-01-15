"""
Questioning Phase Prompts

Prompts for multi-turn requirement gathering before app generation.

Three prompt pairs:
1. Question Generation - Ask clarifying questions one at a time
2. Sufficiency Check - Determine if enough info has been gathered
3. Synthesis - Create structured requirements from conversation
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

Stop asking questions when you have a clear picture of:
- The core purpose of the app
- The data model (entities and relationships)
- The main user workflows
- Key features to build"""

QUESTION_GENERATION_PROMPT = """Based on the user's request and our conversation so far, ask a clarifying question.

## Original Request
{initial_request}

## Conversation So Far
{chat_history}

## Questions Asked: {question_count}

Generate your next clarifying question. Be specific and build on what you've learned.
If you feel you have enough information, say "I have enough information to proceed."

Return ONLY your question (or the proceed statement), no other text."""

# =============================================================================
# Sufficiency Check Prompts
# =============================================================================

SUFFICIENCY_CHECK_SYSTEM_PROMPT = """You are an expert at evaluating whether enough requirements have been gathered.
Your job is to analyze the conversation and determine if we have enough information to build the app.

Minimum requirements for sufficiency:
- Clear understanding of what the app should do
- Data requirements are understood (what data, how structured)
- At least one or two key features/workflows are defined
- Any critical constraints are known

Err on the side of having MORE information rather than less.
A few extra questions are better than building the wrong thing.

Return your analysis as JSON with:
- has_enough: boolean - true if we can proceed
- reasoning: string - explain your decision
- missing_info: list - what's still unclear (if has_enough is false)"""

SUFFICIENCY_CHECK_PROMPT = """Evaluate whether we have gathered enough requirements.

## Original Request
{initial_request}

## Conversation So Far
{chat_history}

## What We Know
{gathered_info}

Analyze whether we have sufficient information to build this app.

Return ONLY valid JSON:
{{
  "has_enough": true/false,
  "reasoning": "explanation of your decision",
  "missing_info": ["item1", "item2"] // empty if has_enough is true
}}"""

# =============================================================================
# Synthesis Prompts
# =============================================================================

SYNTHESIS_SYSTEM_PROMPT = """You are an expert at synthesizing requirements from conversations.
Your job is to create a structured requirements document from the Q&A conversation.

Output a JSON document with:
- app_type: string (dashboard, form, data viewer, CRUD app, report builder, etc.)
- description: string (one paragraph summary of what the app does)
- data_requirements: list of data entities with their fields
- features: list of key features to build
- ui_preferences: any mentioned UI preferences (colors, layout, style)
- constraints: any mentioned constraints (performance, compatibility, etc.)

Be comprehensive but concise. Include everything needed to build the app.
Infer reasonable defaults for anything not explicitly stated."""

SYNTHESIS_PROMPT = """Create a structured requirements document from this conversation.

## Original Request
{initial_request}

## Full Conversation
{chat_history}

Synthesize all gathered information into a structured requirements document.

Return ONLY valid JSON:
{{
  "app_type": "string",
  "description": "string",
  "data_requirements": [
    {{"entity": "name", "fields": ["field1", "field2"]}}
  ],
  "features": ["feature1", "feature2"],
  "ui_preferences": {{}},
  "constraints": []
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


def build_sufficiency_check_prompt(
    initial_request: str,
    chat_history: str,
    gathered_info: str,
) -> str:
    """Build the sufficiency check prompt.

    Args:
        initial_request: The user's original request that triggered questioning
        chat_history: Formatted Q&A conversation so far
        gathered_info: Summary of what we know so far
    """
    return SUFFICIENCY_CHECK_PROMPT.format(
        initial_request=initial_request,
        chat_history=chat_history or "No conversation yet.",
        gathered_info=gathered_info or "No information gathered yet.",
    )


def build_synthesis_prompt(
    initial_request: str,
    chat_history: str,
) -> str:
    """Build the synthesis prompt.

    Args:
        initial_request: The user's original request that triggered questioning
        chat_history: Full Q&A conversation
    """
    return SYNTHESIS_PROMPT.format(
        initial_request=initial_request,
        chat_history=chat_history,
    )
