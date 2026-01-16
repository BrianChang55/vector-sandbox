"""
Main Agent Prompts

Prompts for the MainAgentService control-flow decisions.
These prompts help the agent decide WHEN to act, not WHAT to build.

PRINCIPLE: Prompts here are for PROCEDURAL decisions about the conversation,
NOT semantic judgments about extracted content.
"""

__all__ = [
    "CONTINUATION_DECISION_SYSTEM_PROMPT",
    "build_continuation_decision_prompt",
]

CONTINUATION_DECISION_SYSTEM_PROMPT = """You are a procedural conversation controller.

Your task is NOT to decide whether an app could be built.
Your task is to decide whether continuing without clarification would
force the system to silently choose between multiple plausible interpretations.

You are given:
- The user's initial request
- The conversation so far
- The number of questions already asked

You MUST CONTINUE QUESTIONING if ANY of the following are true:

- A core term is ambiguous and has multiple reasonable meanings
  (e.g., "my PRs", "dashboard", "analytics", "tracking")
- The user's answer avoided commitment or deferred choice
- Proceeding would require assuming user intent that was not stated
- Multiple reasonable app shapes are possible

You MUST STOP QUESTIONING if ALL of the following are true:

- The user has clearly defined the core object or domain
  AND that domain has a widely accepted default behavior
  (e.g., "todo app", "counter", "URL shortener")
- The primary action the user wants to perform is explicit
- There are no unresolved ambiguities that would affect core behavior

IMPORTANT:
- Do NOT assume defaults.
- Do NOT say "we can make something reasonable".
- If you would have to pick one interpretation over another, you MUST continue questioning.

Respond ONLY with JSON:
{{
  "should_continue": true/false,
  "reasoning": "Short explanation",
  "unclear_aspects": ["ambiguous or unresolved items"]
}}
"""


def build_continuation_decision_prompt(
    initial_request: str,
    chat_history: str,
) -> str:
    """Build prompt for continuation decision.

    PRINCIPLE: This prompt contains ONLY raw conversation data.
    No extracted facts, no inferred structure, no semantic analysis.

    Args:
        initial_request: The original user request (raw)
        chat_history: Formatted conversation history (raw Q&A)
        question_count: Number of questions asked so far

    Returns:
        Formatted prompt string
    """
    return f"""## Initial Request
{initial_request}

## Conversation So Far
{chat_history}


## Your Decision
Do we know WHAT to build, or would we be guessing about core functionality?
If we'd be guessing, ask another question.
If we know what to build, proceed."""
