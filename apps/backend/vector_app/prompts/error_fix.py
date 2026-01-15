from vector_app.services.diff import format_with_line_numbers

"""
Error Fix Prompts

Carefully crafted prompts that constrain the agent to ONLY fix compilation errors.
The agent must NOT change core functionality, refactor code, or add features.

Uses unified diff format for minimal, surgical fixes.
"""

ERROR_FIX_SYSTEM_PROMPT = """You are a code repair specialist. Your ONLY job is to fix TypeScript compilation errors using minimal, surgical changes.

CRITICAL RULES - YOU MUST FOLLOW THESE EXACTLY:
1. DO NOT change any working functionality
2. DO NOT refactor or improve code style  
3. DO NOT add new features or modify behavior
4. DO NOT remove or change any existing logic
5. ONLY fix the specific compilation errors listed
6. Make the MINIMUM changes necessary to fix each error
7. Preserve ALL existing code structure, variable names, and patterns

If you cannot fix an error without changing core functionality, explain why and leave the code unchanged.

COMMON FIX PATTERNS:
- Missing import: Add the required import statement
- Type error: Add type annotation or cast
- Undefined variable: Check if it's a typo or missing declaration
- Missing property: Add optional chaining (?.) or default value
- Wrong argument count: Fix the function call to match the signature

OUTPUT FORMAT: Unified Diff

For each file you fix, output a unified diff inside a ```diff code block.
Only output diffs for files that need changes. Do not output unchanged files.
NEVER output duplicate hunks - each line range in a file must appear exactly ONCE.

Example:
```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,5 +1,6 @@
 import React from "react";
+import { useState } from "react";
 
 function App() {
   return (
```"""


def build_error_fix_prompt(
    files: list,
    errors: list,
    attempt: int = 1,
    max_attempts: int = 2,
) -> str:
    """
    Build a focused prompt for fixing compilation errors using unified diffs.
    
    Args:
        files: List of FileChange objects with current code
        errors: List of CompilationError objects to fix
        attempt: Current fix attempt number
        max_attempts: Maximum number of fix attempts
    """

    # Group errors by file
    errors_by_file = {}
    for error in errors:
        file_path = error.file if hasattr(error, 'file') else error.get('file', 'unknown')
        if file_path not in errors_by_file:
            errors_by_file[file_path] = []
        errors_by_file[file_path].append(error)
    
    # Build error summary
    error_lines = []
    for file_path, file_errors in errors_by_file.items():
        error_lines.append(f"\n## Errors in {file_path}:")
        for err in file_errors:
            if hasattr(err, 'line'):
                line = err.line
                col = err.column
                msg = err.message
                code = err.code or ''
            else:
                line = err.get('line', '?')
                col = err.get('column', '?')
                msg = err.get('message', 'Unknown error')
                code = err.get('code', '')
            
            error_lines.append(f"  - Line {line}, Column {col}: {msg} [{code}]")
    
    errors_section = '\n'.join(error_lines)
    
    # Build files section with line numbers for accurate diff generation
    files_section_parts = []
    for file in files:
        file_path = file.path if hasattr(file, 'path') else file.get('path', '')
        content = file.content if hasattr(file, 'content') else file.get('content', '')
        
        # Include file if it has errors or if it might be referenced
        if file_path in errors_by_file or file_path.endswith('.tsx') or file_path.endswith('.ts'):
            # Use format_with_line_numbers for consistent formatting
            numbered_content = format_with_line_numbers(content)
            
            files_section_parts.append(f"""
### {file_path}
```
{numbered_content}
```
""")
    
    files_section = '\n'.join(files_section_parts)
    
    # Build the prompt (diff format instructions are added by build_diff_prompts)
    prompt = f"""Fix the following TypeScript compilation errors. This is attempt {attempt} of {max_attempts}.

## COMPILATION ERRORS TO FIX:
{errors_section}

## INSTRUCTIONS:
1. Analyze each error carefully - look at the line number and error message
2. Make the MINIMUM change needed to fix each error
3. DO NOT change any working code
4. Output only unified diffs for files that need changes
5. Context lines must match the source file EXACTLY (character-for-character)

Remember: Your ONLY goal is to fix compilation errors. Do not refactor, improve, or change any functionality."""

    return prompt


def build_bundler_error_fix_prompt(
    files: list,
    bundler_errors: list,
    attempt: int = 1,
    max_attempts: int = 2,
) -> str:
    """
    Build a prompt for fixing Sandpack bundler errors using unified diffs.
    
    These are runtime/bundler errors that TypeScript didn't catch.
    """
    # Build error summary
    error_lines = []
    for err in bundler_errors:
        if isinstance(err, dict):
            title = err.get('title', 'Error')
            message = err.get('message', 'Unknown error')
            file_path = err.get('file', 'unknown')
            line = err.get('line', '?')
        else:
            title = getattr(err, 'title', 'Error')
            message = getattr(err, 'message', str(err))
            file_path = getattr(err, 'file', 'unknown')
            line = getattr(err, 'line', '?')
        
        error_lines.append(f"- [{title}] {file_path}:{line} - {message}")
    
    errors_section = '\n'.join(error_lines)
    
    # Build files section with line numbers
    files_section_parts = []
    for file in files:
        file_path = file.path if hasattr(file, 'path') else file.get('path', '')
        content = file.content if hasattr(file, 'content') else file.get('content', '')
        
        if file_path.endswith('.tsx') or file_path.endswith('.ts'):
            # Use format_with_line_numbers for consistent formatting
            numbered_content = format_with_line_numbers(content)
            
            files_section_parts.append(f"""
### {file_path}
```
{numbered_content}
```
""")
    
    files_section = '\n'.join(files_section_parts)
    
    # Build the prompt (diff format instructions are added by build_diff_prompts)
    prompt = f"""Fix the following bundler/runtime errors. This is attempt {attempt} of {max_attempts}.

## BUNDLER ERRORS:
{errors_section}

## INSTRUCTIONS:
1. These errors were caught by the bundler, not TypeScript
2. Common causes: circular imports, missing exports, runtime errors
3. Fix ONLY the specific errors listed
4. DO NOT change working functionality
5. Output unified diffs for files that need changes
6. Context lines must match the source file EXACTLY (character-for-character)

Make minimal changes to fix the errors."""

    return prompt
