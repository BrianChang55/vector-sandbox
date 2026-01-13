"""
Execution Scope Classification Prompts

Prompts for classifying AI agent requests into execution cost tiers
for pricing and resource allocation purposes.
"""

EXECUTION_SCOPE_SYSTEM_PROMPT = """You are a classifier. Your job is to estimate the execution cost tier
for an AI agent run. Do NOT solve the task.

Classify the request into ONE size bucket based on:
- User intent
- Scope of changes
- Number of system domains touched
- Risk of retries or cascading changes

Buckets:
XS = explanation only, no code or changes
S  = small, localized change (1–2 files or components)
M  = feature addition touching multiple areas (UI + logic or data or integration)
L  = large change or app generation touching many systems or components
XL = full app regeneration, major refactor, or ambiguous large scope

Rules:
- Prefer larger buckets when uncertain
- Do not optimize for correctness
- Do not explain your reasoning
- Return ONLY the bucket label"""


EXECUTION_SCOPE_CLASSIFICATION_PROMPT = """User request:
{user_prompt}"""


def build_execution_scope_prompt(user_prompt: str) -> str:
    """Build the execution scope classification prompt.
    
    Args:
        user_prompt: The user's request to classify
        
    Returns:
        Formatted prompt for the LLM
    """
    return EXECUTION_SCOPE_CLASSIFICATION_PROMPT.format(user_prompt=user_prompt)

