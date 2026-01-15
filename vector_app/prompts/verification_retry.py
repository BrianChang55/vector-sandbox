"""
Verification Retry Prompts

Prompt templates for regenerating code after verification failures.
Used by verify_and_retry() callback to provide error context to the LLM.
"""

from typing import Optional

from vector_app.services.types import FileChange


VERIFICATION_RETRY_CONTEXT = """
## VERIFICATION ERROR - FIX REQUIRED

The code you generated failed syntax verification. Fix the following error and regenerate the file.

**File:** {file_path}
**Error:**
```
{error_message}
```

## INSTRUCTIONS FOR FIX:
1. Analyze the error message carefully
2. Identify the syntax issue (missing bracket, invalid syntax, type error, etc.)
3. Fix ONLY the syntax error - do not change functionality
4. Regenerate the COMPLETE file with the fix applied
5. Ensure the file compiles without syntax errors

## PREVIOUS CODE (with error):
```{language}
{content}
```

Generate the corrected version of this file. Output the complete file content.
"""


def build_verification_retry_prompt(
    file_change: FileChange,
    error_message: str,
    original_prompt: Optional[str] = None,
    attempt_number: int = 2,
    max_attempts: int = 3,
) -> str:
    """
    Build a prompt for retrying code generation after verification failure.

    Args:
        file_change: The file that failed verification
        error_message: The verification error message (e.g., tsc syntax errors)
        original_prompt: The original generation prompt (optional, for context)
        attempt_number: Current attempt number (for logging/context)
        max_attempts: Maximum attempts allowed

    Returns:
        A prompt string that includes the error context for the LLM

    Usage in handler callback:
        def regenerate(file: FileChange, error: str) -> FileChange:
            prompt = build_verification_retry_prompt(file, error, original_prompt)
            new_content = llm_call(prompt)
            return FileChange(..., content=new_content)
    """
    # Determine language for syntax highlighting
    language = _get_language_for_path(file_change.path)

    # Build the error context section
    error_context = VERIFICATION_RETRY_CONTEXT.format(
        file_path=file_change.path,
        error_message=error_message or "Unknown verification error",
        language=language,
        content=file_change.content,
    )

    # Add attempt tracking info
    attempt_info = f"\n\n(Attempt {attempt_number} of {max_attempts})"

    # If original prompt provided, append error context to it
    if original_prompt:
        return f"{original_prompt}\n\n{error_context}{attempt_info}"

    # Otherwise, return just the error context (caller handles system prompt)
    return f"{error_context}{attempt_info}"


def format_verification_error(error_message: str, file_path: str) -> str:
    """
    Format a verification error for display or logging.

    Args:
        error_message: Raw error from verifier
        file_path: Path to the file that failed

    Returns:
        Formatted error string
    """
    if not error_message:
        return f"Verification failed for {file_path} (no error details)"

    # Clean up error message (remove excessive whitespace, etc.)
    cleaned = "\n".join(line.strip() for line in error_message.split("\n") if line.strip())

    return f"Verification failed for {file_path}:\n{cleaned}"


def _get_language_for_path(file_path: str) -> str:
    """Get language identifier for syntax highlighting based on file extension."""
    extension_map = {
        ".ts": "typescript",
        ".tsx": "tsx",
        ".js": "javascript",
        ".jsx": "jsx",
        ".py": "python",
        ".css": "css",
        ".json": "json",
        ".html": "html",
    }

    for ext, lang in extension_map.items():
        if file_path.endswith(ext):
            return lang

    return ""  # No highlighting
