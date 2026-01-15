"""
Centralized prompt templates for backend AI interactions.

Each module contains focused helpers to keep prompt construction consistent
and discoverable across the codebase.
"""

from .agentic.codegen import (  # noqa: F401
    CODEGEN_SYSTEM_PROMPT_TEMPLATE,
    build_codegen_system_prompt,
)
from .agentic.design import DESIGN_STYLE_PROMPT  # noqa: F401
from .agentic.execution import (  # noqa: F401
    FINAL_APP_SYSTEM_PROMPT,
    apply_design_style_prompt,
    build_final_app_prompt,
    build_step_prompt,
)
from .agentic.planning import build_plan_prompt  # noqa: F401
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
