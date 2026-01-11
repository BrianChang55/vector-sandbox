"""
Centralized prompt templates for backend AI interactions.

Each module contains focused helpers to keep prompt construction consistent
and discoverable across the codebase.
"""

from .agentic import (  # noqa: F401
    DESIGN_STYLE_PROMPT,
    CODEGEN_SYSTEM_PROMPT_TEMPLATE,
    FINAL_APP_SYSTEM_PROMPT,
    apply_design_style_prompt,
    build_plan_prompt,
    build_step_prompt,
    build_codegen_system_prompt,
    build_final_app_prompt,
)
from .openrouter import (  # noqa: F401
    build_system_prompt as build_openrouter_system_prompt,
    build_user_prompt as build_openrouter_user_prompt,
)
from .ai import (  # noqa: F401
    build_system_prompt as build_ai_system_prompt,
    build_user_prompt as build_ai_user_prompt,
)
from .error_fix import (  # noqa: F401
    ERROR_FIX_SYSTEM_PROMPT,
    build_error_fix_prompt,
    build_bundler_error_fix_prompt,
)
