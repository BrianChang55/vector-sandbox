"""
Main Agent Prompts

Prompts for the MainAgentService control-flow decisions.
These prompts help the agent decide WHEN to act, not WHAT to build.
"""

__all__ = [
    "SUFFICIENCY_DECISION_SYSTEM_PROMPT",
    "build_sufficiency_decision_prompt",
]

SUFFICIENCY_DECISION_SYSTEM_PROMPT = """You are an assistant that decides if enough information has been gathered to build an app.

You will be given:
- The user's initial request
- The conversation so far
- Facts extracted from the conversation (goals, requirements, UI mentions, unknowns)
- Number of questions already asked

Your job is to decide: Should we ask more clarifying questions, or do we have enough to proceed?

DECISION CRITERIA:
- If there are critical unknowns (core functionality unclear), ask more
- If answers have been vague/unclear, ask for clarification
- If we've asked 5+ questions, likely enough — proceed
- If the request is simple and clear, proceed even with 0 questions
- If core goals and requirements are understood, proceed

Respond with JSON:
{
  "should_continue": true/false,
  "reasoning": "Brief explanation of why",
  "critical_unknowns": ["list of unknowns that would block building"]
}"""


def build_sufficiency_decision_prompt(
    initial_request: str,
    chat_history: str,
    extracted_facts: dict,
    question_count: int,
) -> str:
    """Build prompt for sufficiency decision.

    Args:
        initial_request: The original user request
        chat_history: Formatted conversation history
        extracted_facts: Facts extracted by QuestioningService
        question_count: Number of questions asked so far

    Returns:
        Formatted prompt string
    """
    facts_str = _format_facts(extracted_facts) if extracted_facts else "No facts extracted yet."

    return f"""## Initial Request
{initial_request}

## Conversation So Far
{chat_history}

## Extracted Facts
{facts_str}

## Questions Asked
{question_count} questions have been asked.

## Your Decision
Should we ask more clarifying questions, or proceed with building?"""


def _format_facts(facts: dict) -> str:
    """Format extracted facts for prompt."""
    lines = []

    if goals := facts.get("goals"):
        lines.append("**Goals:**")
        for g in goals:
            lines.append(f"- {g}")

    if reqs := facts.get("explicit_requirements"):
        lines.append("\n**Requirements:**")
        for r in reqs:
            lines.append(f"- {r}")

    if ui := facts.get("ui_mentions"):
        lines.append("\n**UI/UX:**")
        for u in ui:
            lines.append(f"- {u}")

    if unknowns := facts.get("unknowns"):
        lines.append("\n**Unknowns:**")
        for u in unknowns:
            lines.append(f"- {u}")

    return "\n".join(lines) if lines else "No facts extracted."
