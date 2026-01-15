"""
Diff Application Service

Provides a unified interface for parsing diffs from LLM responses and applying them to files.
Consolidates the common pattern used across handlers:
1. Stream LLM response
2. Parse unified diffs from response
3. Apply diffs to original files
4. Fall back to full file parsing if no diffs found
"""

import json
import logging
from dataclasses import dataclass, field
from typing import Any, Callable, Dict, Generator, List, Optional, Set

import httpx
from django.conf import settings

from vector_app.services.diff import parse_diffs, apply_diff, FileDiff, format_with_line_numbers
from vector_app.services.types import FileChange, AgentEvent

logger = logging.getLogger(__name__)

# Default protected files that should never be overwritten
DEFAULT_PROTECTED_FILES: Set[str] = {
    "src/lib/types.ts",
}

# Shared diff format instructions (consolidated for single-point editing)
LINE_NUMBER_INSTRUCTIONS = """**LINE NUMBERS**: The file contents above include line numbers in the format "  42| code here".
Use these line numbers for your @@ hunk headers. The patch system has fuzz matching (±3 lines tolerance), but accurate numbers improve reliability.
Do NOT include the line number prefix (e.g., "  42| ") in your diff output.

**CRITICAL - CONTEXT LINES MUST BE EXACT**:
The patch system can tolerate slight line number errors, but context lines MUST match the source file CHARACTER-FOR-CHARACTER.
- Copy context lines exactly as shown (same whitespace, same quotes, same everything)
- If the source has `  return <div className="p-4">` your context must be identical
- Mismatched context lines will cause the patch to fail"""

DIFF_OUTPUT_FORMAT = """## Output Format: Unified Diff

For each file you modify, output a unified diff inside a ```diff code block:

```diff
--- src/path/to/file.tsx
+++ src/path/to/file.tsx
@@ -LINE,COUNT +LINE,COUNT @@
 context line (unchanged, starts with space)
 context line (unchanged, starts with space)
 context line (unchanged, starts with space)
-line to remove (starts with minus)
+line to add (starts with plus)
 context line (unchanged, starts with space)
 context line (unchanged, starts with space)
 context line (unchanged, starts with space)
```

### Diff Rules
- Start with `--- path` and `+++ path` header lines
- Each hunk starts with `@@ -old_start,old_count +new_start,new_count @@`
- Include exactly 3 lines of unchanged context before and after changes
- Lines starting with ` ` (space) are unchanged context - these MUST be copied EXACTLY from the source file
- Lines starting with `-` are removed
- Lines starting with `+` are added
- For multiple non-adjacent changes in the same file, use multiple hunks
- **CRITICAL**: Context lines must match the source file EXACTLY. Do not paraphrase, reformat, or invent context lines."""

HUNK_STRUCTURE_RULES = """### Hunk Structure Rule (CRITICAL)

Within each hunk, lines MUST appear in this exact order:
1. Context lines (space prefix) - unchanged lines before the change
2. ALL removed lines (minus prefix) - grouped together consecutively
3. ALL added lines (plus prefix) - grouped together consecutively
4. Context lines (space prefix) - unchanged lines after the change

**NEVER interleave removals and additions.** Each contiguous change region gets its own hunk.

BAD (interleaved - will break):
```
@@ -1,10 +1,10 @@
 import React from 'react';
-old interface
+new interface
-old export        <- WRONG: more removals after additions
+new export
```

GOOD (separate hunks):
```
@@ -1,4 +1,4 @@
 import React from 'react';
-old interface
+new interface

@@ -8,3 +8,3 @@
 // context line
-old export
+new export
```"""

NO_DUPLICATE_HUNKS = """### CRITICAL - NO DUPLICATE HUNKS

Each location in a file must have exactly ONE hunk. Never repeat or duplicate hunks.

**RULES:**
1. Each line range can only appear ONCE in your output
2. If you need multiple changes at the same location, combine them into ONE hunk
3. NEVER output the same hunk multiple times - this breaks the patch
4. Before outputting, mentally verify you haven't already written a hunk for that location
5. If you catch yourself repeating, STOP and consolidate"""

NEW_FILE_INSTRUCTIONS = """### Creating NEW Files

For new files that don't exist yet, use `--- /dev/null` and start line numbers at 1.
The `+1,N` in the hunk header means "starting at line 1, adding N lines":

```diff
--- /dev/null
+++ src/components/NewComponent.tsx
@@ -0,0 +1,12 @@
+import React from "react";
+
+interface NewComponentProps {
+  title: string;
+}
+
+export function NewComponent({ title }: NewComponentProps) {
+  return (
+    <div className="p-4">{title}</div>
+  );
+}
```"""

SYNTAX_BALANCE_CHECK = """### Syntax Balance Check (REQUIRED before outputting)

Before finalizing each hunk, verify:
1. Count removed `{` equals removed `}`
2. Count removed `(` equals removed `)`
3. Count removed `<Tag>` equals removed `</Tag>` or `<Tag />`
4. If removing an opening line (e.g., `if (condition) {`), the closing `}` MUST also be removed
5. If removing JSX like `{condition && (`, you MUST also remove the matching `)}`

If your diff would leave unbalanced syntax, expand the hunk to include the matching closing element."""


@dataclass
class DiffApplicationConfig:
    """Configuration for diff application behavior."""
    
    protected_files: Set[str] = field(default_factory=set)
    normalize_paths: bool = False  # Handle src/ prefix normalization
    allow_new_files: bool = True   # Handle /dev/null or empty originals
    fallback_to_full_file: bool = True  # Fall back to parse_code_blocks if no diffs
    verify_changes: bool = False   # Skip files where diff results in no change


def _get_language(file_path: str) -> str:
    """Determine language from file extension."""
    ext = file_path.split('.')[-1] if '.' in file_path else 'tsx'
    lang_map = {
        'tsx': 'tsx',
        'ts': 'ts', 
        'jsx': 'jsx',
        'js': 'js',
        'css': 'css',
        'json': 'json',
        'html': 'html',
    }
    return lang_map.get(ext, 'tsx')


def _normalize_path(path: str, file_contents: Dict[str, str]) -> str:
    """
    Normalize path to match keys in file_contents.
    
    Handles cases where LLM returns path with/without src/ prefix.
    """
    if path in file_contents:
        return path
    
    # Try with src/ prefix
    if not path.startswith('src/') and f"src/{path}" in file_contents:
        return f"src/{path}"
    
    # Try without src/ prefix
    if path.startswith('src/') and path[4:] in file_contents:
        return path[4:]
    
    return path


def _extract_new_file_content(diff: FileDiff) -> str:
    """Extract content for new files from diff's added lines."""
    lines = []
    for hunk in diff.hunks:
        for line in hunk.split('\n'):
            if line.startswith('+') and not line.startswith('+++'):
                lines.append(line[1:])
    return '\n'.join(lines)


def _apply_diffs_from_llm_response(
    llm_response: str,
    file_contents: Dict[str, str],
    config: Optional[DiffApplicationConfig] = None,
    parse_full_files_fallback: Optional[Callable[[str], List[FileChange]]] = None,
) -> List[FileChange]:
    """
    Parse diffs from LLM response and apply them to files.
    
    Internal method that consolidates the common diff application pattern.
    
    Args:
        llm_response: Full LLM response text containing ```diff blocks
        file_contents: Dict mapping file paths to their original content
        config: Optional configuration for behavior (protected files, etc.)
        parse_full_files_fallback: Optional function to parse full file blocks 
                                    if no diffs found (e.g., parse_code_blocks)
    
    Returns:
        List of FileChange objects with applied diffs
    """
    config = config or DiffApplicationConfig()

    # Parse diffs from response
    diffs = parse_diffs(llm_response)

    if not diffs:
        if config.fallback_to_full_file and parse_full_files_fallback:
            logger.warning("No diffs found in LLM response, falling back to full file parsing")
            return parse_full_files_fallback(llm_response)
        logger.debug("No diffs found in LLM response")
        return []

    logger.debug(f"Parsed {len(diffs)} diff(s) from LLM response")

    files: List[FileChange] = []

    for diff in diffs:
        file_path = diff.path

        # Path normalization
        if config.normalize_paths:
            file_path = _normalize_path(file_path, file_contents)
        
        # Skip protected files
        if file_path in config.protected_files:
            logger.info(f"Skipping protected file: {file_path}")
            continue

        original = file_contents.get(file_path, "")

        # Handle new files
        if not original:
            if config.allow_new_files:
                new_content = _extract_new_file_content(diff)
                if new_content:
                    files.append(FileChange(
                        path=file_path,
                        action="create",
                        language=_get_language(file_path),
                        content=new_content,
                        lines_added=diff.lines_added,
                        lines_removed=0,
                    ))
                    logger.debug(f"Created new file: {file_path}")
            else:
                logger.warning(f"No original content for {file_path}, skipping (allow_new_files=False)")
            continue

        # Apply diff to existing file
        try:
            new_content = apply_diff(original, diff)
        except Exception as e:
            logger.error(f"Failed to apply diff to {file_path}: {e}")
            continue

        # Optionally verify the diff changed something
        if config.verify_changes and new_content == original:
            logger.debug(f"Diff for {file_path} resulted in no changes, skipping")
            continue

        files.append(FileChange(
            path=file_path,
            action="modify",
            language=_get_language(file_path),
            content=new_content,
            previous_content=original,
            lines_added=diff.lines_added,
            lines_removed=diff.lines_removed,
        ))
        logger.debug(f"Modified file: {file_path} (+{diff.lines_added}/-{diff.lines_removed})")

    files = [
        file for file in files if file.content != file.previous_content
        or file.path in config.protected_files
    ]

    logger.info(f"Applied {len(files)} diff(s) successfully")
    return files


class DiffApplicationService:
    """
    Service for applying diffs from LLM responses to files.
    
    Provides both low-level diff application and high-level streaming
    LLM integration with diff parsing and application.
    """
    
    OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions"
    
    def __init__(self):
        self.api_key = getattr(settings, 'OPENROUTER_API_KEY', None)
    
    def _build_headers(self) -> Dict[str, str]:
        """Build headers for OpenRouter API requests."""
        return {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json",
            "HTTP-Referer": "https://relay.app",
            "X-Title": "Relay Internal Apps",
        }
    
    def apply_diffs(
        self,
        llm_response: str,
        file_contents: Dict[str, str],
        config: Optional[DiffApplicationConfig] = None,
        parse_full_files_fallback: Optional[Callable[[str], List[FileChange]]] = None,
    ) -> List[FileChange]:
        """
        Parse diffs from LLM response and apply them to files.
        
        This is the main entry point for applying diffs without LLM streaming.
        
        Args:
            llm_response: Full LLM response text containing ```diff blocks
            file_contents: Dict mapping file paths to their original content
            config: Optional configuration for behavior
            parse_full_files_fallback: Optional function to parse full file blocks
        
        Returns:
            List of FileChange objects with applied diffs
        """
        return _apply_diffs_from_llm_response(
            llm_response=llm_response,
            file_contents=file_contents,
            config=config,
            parse_full_files_fallback=parse_full_files_fallback,
        )
    
    def stream_and_apply_diffs(
        self,
        # TODO: move system prompt and user prompt to this
        system_prompt: str,
        user_prompt: str,
        file_contents: Dict[str, str],
        model: str,
        *,
        temperature: float = 0.2,
        timeout: float = 120.0,
        config: Optional[DiffApplicationConfig] = None,
        parse_full_files_fallback: Optional[Callable[[str], List[FileChange]]] = None,
        on_chunk: Optional[Callable[[str, int], Optional[AgentEvent]]] = None,
        streaming_validator: Optional[Any] = None,  # StreamingValidator from base_handler
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """
        Stream LLM response, parse diffs, and apply them to files.
        
        Consolidates the full LLM → diff → apply pipeline.
        
        Args:
            system_prompt: System prompt for the LLM
            user_prompt: User prompt for the LLM
            file_contents: Dict mapping file paths to their original content
            model: Model identifier (e.g., "anthropic/claude-sonnet-4")
            temperature: LLM temperature (default 0.2 for consistent edits)
            timeout: HTTP timeout in seconds
            config: Optional DiffApplicationConfig
            parse_full_files_fallback: Optional function to parse full file blocks
            on_chunk: Optional callback(chunk, chunk_count) that can return AgentEvent
            streaming_validator: Optional validator with check_chunk/final_check methods
        
        Yields:
            AgentEvent objects for progress/warnings during streaming
            
        Returns:
            List of FileChange objects with applied diffs
        """
        full_content = ""
        chunk_count = 0
        
        try:
            with httpx.Client(timeout=timeout) as client:
                with client.stream(
                    "POST",
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json={
                        "model": model,
                        "messages": [
                            {"role": "system", "content": system_prompt},
                            {"role": "user", "content": user_prompt},
                        ],
                        "temperature": temperature,
                        "stream": True,
                    },
                ) as response:
                    response.raise_for_status()

                    for line in response.iter_lines():
                        if not line:
                            continue
                        
                        if line.startswith("data: "):
                            data = line[6:]
                            if data == "[DONE]":
                                break
                            
                            try:
                                chunk_data = json.loads(data)
                                delta = chunk_data.get("choices", [{}])[0].get("delta", {})
                                content = delta.get("content", "")
                                
                                if content:
                                    full_content += content
                                    chunk_count += 1
                                    
                                    # Streaming validation
                                    if streaming_validator:
                                        warnings = streaming_validator.check_chunk(content, full_content)
                                        for warning in warnings:
                                            yield AgentEvent(
                                                "streaming_warning",
                                                {"warning": warning, "severity": "low"},
                                            )

                                    # Progress callback
                                    if on_chunk:
                                        event = on_chunk(content, chunk_count)
                                        if event:
                                            yield event
                            
                            except json.JSONDecodeError:
                                continue
        
        except Exception as e:
            logger.error(f"LLM streaming error: {e}")
            raise
        
        # Final validation
        if streaming_validator:
            final_warnings = streaming_validator.final_check(full_content)
            for warning in final_warnings:
                yield AgentEvent(
                    "streaming_warning",
                    {"warning": warning, "severity": "low"},
                )

        # Apply diffs
        files = _apply_diffs_from_llm_response(
            llm_response=full_content,
            file_contents=file_contents,
            config=config,
            parse_full_files_fallback=parse_full_files_fallback,
        )
        files = [
            file for file in files if file.content != file.previous_content
            or file.path in config.protected_files
        ]

        # Emit file_generated events for each file
        for file_change in files:
            yield AgentEvent(
                "file_generated",
                {"file": file_change.to_dict()},
            )

        return files


# Singleton instance
_diff_application_service: Optional[DiffApplicationService] = None


def get_diff_application_service() -> DiffApplicationService:
    """Get or create the diff application service singleton."""
    global _diff_application_service
    if _diff_application_service is None:
        _diff_application_service = DiffApplicationService()
    return _diff_application_service


def build_diff_prompts(
    edit_style_prompt: str,
    file_changes: List[FileChange],
    user_message: str,
    *,
    allow_new_files: bool = False,
    extra_context: Optional[str] = None,
) -> tuple[str, str]:
    """
    Build system and user prompts for diff-based editing.
    
    Args:
        edit_style_prompt: Handler-specific system prompt (e.g., EDIT_SYSTEM_PROMPT)
        file_changes: List of FileChange objects representing files to edit
        user_message: The user's request/context message
        allow_new_files: Whether to include new file creation instructions
        extra_context: Optional additional context (data store, MCP tools, etc.)
    
    Returns:
        Tuple of (system_prompt, user_prompt)
    """
    # Build system prompt: combine edit_style_prompt with diff format instructions
    system_prompt = f"""{edit_style_prompt}

{DIFF_OUTPUT_FORMAT}

{HUNK_STRUCTURE_RULES}

{NO_DUPLICATE_HUNKS}"""
    
    # Build file context with line numbers
    files_context_parts = []
    for file_change in file_changes:
        numbered_content = format_with_line_numbers(file_change.content)
        files_context_parts.append(f"### {file_change.path}\n```\n{numbered_content}\n```")
    
    files_context = "\n\n".join(files_context_parts) if files_context_parts else "No existing files to modify."
    
    # Build user prompt
    user_prompt_parts = [
        f"## Request\n{user_message}",
        f"## Current Files (with line numbers)\n{files_context}",
        LINE_NUMBER_INSTRUCTIONS,
    ]
    
    if allow_new_files:
        user_prompt_parts.append(NEW_FILE_INSTRUCTIONS)
    
    user_prompt_parts.append(SYNTAX_BALANCE_CHECK)
    
    if extra_context:
        user_prompt_parts.append(f"\n## Additional Context\n{extra_context}")
    
    user_prompt = "\n\n".join(user_prompt_parts)
    return system_prompt, user_prompt
