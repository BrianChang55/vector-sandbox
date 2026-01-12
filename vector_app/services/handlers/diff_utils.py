"""
Diff Utilities for Surgical Edits

Provides utilities to parse unified diffs from LLM output and apply them to files.
Uses subprocess patch command as primary (with fuzz matching), whatthepatch as fallback.
"""

import logging
import os
import re
import subprocess
import tempfile
from dataclasses import dataclass, field
from typing import List, Optional, Tuple

import whatthepatch

logger = logging.getLogger(__name__)


def format_with_line_numbers(content: str) -> str:
    """
    Format file content with line numbers for LLM context.
    
    This helps the LLM generate accurate line numbers in diff hunk headers
    by showing exactly which line number each piece of code is on.
    
    Args:
        content: The file content to format
        
    Returns:
        Content with line numbers prefixed (e.g., "  42| const x = 1;")
    """
    if not content:
        return content
    
    lines = content.split('\n')
    # Calculate width needed for line numbers (minimum 4 for alignment)
    width = max(4, len(str(len(lines))))
    numbered_lines = [f"{i+1:>{width}}| {line}" for i, line in enumerate(lines)]
    return '\n'.join(numbered_lines)


@dataclass
class FileDiff:
    """A unified diff for a single file."""
    path: str
    hunks: List[str] = field(default_factory=list)
    lines_added: int = 0
    lines_removed: int = 0


def parse_diffs(full_llm_str: str) -> List[FileDiff]:
    """
    Parse unified diffs from LLM response into FileDiff objects.
    
    Expected format:
    ```diff
    --- src/App.tsx
    +++ src/App.tsx
    @@ -10,6 +10,7 @@ function App() {
       const [count, setCount] = useState(0);
    +  const [name, setName] = useState('');
       
       return (
    ```
    
    Args:
        full_llm_str: The complete LLM response containing diff blocks
        
    Returns:
        List of FileDiff objects, one per file
    """
    diffs: List[FileDiff] = []
    
    # Extract diff code blocks (```diff ... ```)
    diff_block_pattern = r'```diff\s*\n(.*?)```'
    diff_blocks = re.findall(diff_block_pattern, full_llm_str, re.DOTALL)
    
    for block in diff_blocks:
        # Parse this diff block which may contain multiple files
        file_diffs = _parse_diff_block(block)
        
        # Merge with existing diffs for same path
        for fd in file_diffs:
            existing = next((d for d in diffs if d.path == fd.path), None)
            if existing:
                existing.hunks.extend(fd.hunks)
                existing.lines_added += fd.lines_added
                existing.lines_removed += fd.lines_removed
            else:
                diffs.append(fd)
    
    return diffs


def _parse_diff_block(block: str) -> List[FileDiff]:
    """Parse a single diff block that may contain multiple files."""
    diffs: List[FileDiff] = []
    lines = block.split('\n')
    
    current_path: Optional[str] = None
    current_hunks: List[str] = []
    current_hunk_lines: List[str] = []
    in_hunk = False
    
    i = 0
    while i < len(lines):
        line = lines[i]
        
        # Check for file header (--- path)
        if line.startswith('--- '):
            # Save previous file if exists
            if current_path and (current_hunks or current_hunk_lines):
                if current_hunk_lines:
                    current_hunks.append('\n'.join(current_hunk_lines))
                added, removed = _count_lines_from_hunks(current_hunks)
                diffs.append(FileDiff(path=current_path, hunks=current_hunks, lines_added=added, lines_removed=removed))
            
            # Parse new file path
            # Handle formats: "--- src/file.tsx", "--- a/src/file.tsx"
            path = line[4:].strip()
            if path.startswith('a/'):
                path = path[2:]
            
            current_path = path
            current_hunks = []
            current_hunk_lines = []
            in_hunk = False
            
            # Parse the +++ line to get the destination path
            # Always prefer the destination path for:
            # - New files: --- /dev/null, +++ src/new.tsx
            # - Renamed files: --- a/old.tsx, +++ b/new.tsx
            # - Modified files: paths are the same, so either works
            if i + 1 < len(lines) and lines[i + 1].startswith('+++ '):
                i += 1
                dest_path = lines[i][4:].strip()
                if dest_path.startswith('b/'):
                    dest_path = dest_path[2:]
                # Use destination path if it's a real file (not /dev/null for deletions)
                if dest_path and dest_path != '/dev/null':
                    current_path = dest_path

        # Check for hunk header (@@ ... @@)
        elif line.startswith('@@'):
            # Save previous hunk if exists
            if current_hunk_lines:
                current_hunks.append('\n'.join(current_hunk_lines))
                current_hunk_lines = []
            
            current_hunk_lines.append(line)
            in_hunk = True
        
        # Collect hunk content
        elif in_hunk and current_path:
            # Lines starting with space, +, -, or empty lines in context
            if line.startswith(' ') or line.startswith('+') or line.startswith('-') or line == '':
                current_hunk_lines.append(line)
            # Check if this looks like a context line that lost its leading space
            elif not line.startswith('@@') and not line.startswith('---') and not line.startswith('+++'):
                # Could be a context line, treat as such
                current_hunk_lines.append(' ' + line if line else '')
        
        i += 1
    
    # Save final file
    if current_path and (current_hunks or current_hunk_lines):
        if current_hunk_lines:
            current_hunks.append('\n'.join(current_hunk_lines))
        added, removed = _count_lines_from_hunks(current_hunks)
        diffs.append(FileDiff(path=current_path, hunks=current_hunks, lines_added=added, lines_removed=removed))
    
    return diffs


def _apply_with_subprocess_patch(original_content: str, diff_text: str) -> str:
    """
    Apply patch using subprocess, with proper error handling.
    
    This is our own implementation because whatthepatch's use_patch=True has a bug
    where it tries to read the output file before checking if patch succeeded.
    
    Args:
        original_content: The original file content
        diff_text: The unified diff text to apply
        
    Returns:
        The patched content
        
    Raises:
        Exception: If the patch command fails
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        orig_file = os.path.join(tmpdir, "original")
        patch_file = os.path.join(tmpdir, "diff.patch")
        out_file = os.path.join(tmpdir, "output")
        
        with open(orig_file, 'w') as f:
            f.write(original_content)
        with open(patch_file, 'w') as f:
            f.write(diff_text)
        
        result = subprocess.run(
            ['patch', '--fuzz=3', '-o', out_file, orig_file, '-i', patch_file],
            capture_output=True,
            text=True
        )
        
        # Check return code BEFORE reading output (fixes whatthepatch bug)
        if result.returncode != 0:
            raise Exception(f"patch failed (code {result.returncode}): {result.stderr}")
        
        with open(out_file) as f:
            return f.read()


def apply_diff(original_content: str, diff: FileDiff) -> str:
    """
    Apply a unified diff to original file content.
    
    Uses subprocess patch command as primary (has fuzz matching and handles
    multi-hunk offsets automatically). Falls back to whatthepatch for systems
    without the patch command.
    
    Args:
        original_content: The original file content
        diff: FileDiff containing hunks to apply
        
    Returns:
        The modified file content
    """
    if not diff.hunks:
        return original_content
    
    # Reconstruct full diff with all hunks - patch handles offsets internally
    diff_text = _reconstruct_diff(diff)
    
    try:
        # Use our own subprocess patch implementation (with proper error handling)
        # patch command has fuzz matching and handles multi-hunk offsets automatically
        result = _apply_with_subprocess_patch(original_content, diff_text)
        return result
    except Exception as e:
        logger.warning(f"Subprocess patch failed for {diff.path}: {e}, falling back to whatthepatch")
    
    # Fallback to whatthepatch for systems without patch command
    try:
        parsed_diffs = list(whatthepatch.parse_patch(diff_text))
        if not parsed_diffs:
            logger.warning(f"[DIFF] whatthepatch could not parse diff for {diff.path}")
            return original_content
        
        parsed_diff = parsed_diffs[0]
        current_lines = original_content.split('\n')
        result = whatthepatch.apply_diff(parsed_diff, current_lines)
        new_lines = list(result)
        
        if new_lines is None:
            logger.warning(f"[DIFF] whatthepatch returned None for {diff.path}")
            return original_content
        
        return '\n'.join(new_lines)
    except Exception as e2:
        logger.error(f"[DIFF] Both patch methods failed for {diff.path}: {e2}")
        return original_content


def _fix_hunk_header(hunk: str) -> str:
    """
    Fix the line counts in a hunk header based on actual content.
    
    LLMs often generate incorrect line counts in @@ headers. This function
    recalculates them based on the actual lines in the hunk.
    
    Args:
        hunk: A single hunk string starting with @@ header
        
    Returns:
        The hunk with corrected @@ header line counts
    """
    lines = hunk.split('\n')
    if not lines or not lines[0].startswith('@@'):
        return hunk
    
    header = lines[0]
    content_lines = lines[1:]
    
    # Parse the original header to get starting line numbers
    # Format: @@ -start,count +start,count @@ optional context
    match = re.match(r'@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)?(?: @@(.*))?', header)
    if not match:
        return hunk
    
    old_start = match.group(1)
    new_start = match.group(2)
    context_text = match.group(3) or ''
    
    # Count actual lines
    old_count = 0  # Lines in original (context + removed)
    new_count = 0  # Lines in new (context + added)
    
    for line in content_lines:
        if line.startswith('-'):
            old_count += 1
        elif line.startswith('+'):
            new_count += 1
        elif line.startswith(' ') or line == '':
            # Context line (or empty line treated as context)
            old_count += 1
            new_count += 1
    
    # Reconstruct header with correct counts
    new_header = f'@@ -{old_start},{old_count} +{new_start},{new_count} @@{context_text}'
    
    return new_header + '\n' + '\n'.join(content_lines)


def _get_hunk_start_line(hunk: str) -> int:
    """
    Extract the starting line number from a hunk header.
    
    Args:
        hunk: A hunk string starting with @@ header
        
    Returns:
        The starting line number in the original file, or 0 if parsing fails
    """
    if not hunk.startswith('@@'):
        return 0
    
    match = re.match(r'@@ -(\d+)', hunk)
    return int(match.group(1)) if match else 0


def _reconstruct_diff(diff: FileDiff) -> str:
    """
    Reconstruct a proper unified diff string from FileDiff for whatthepatch.
    
    Fixes hunk headers with incorrect line counts (common LLM error).
    Sorts hunks by starting line number to ensure correct order.
    
    Args:
        diff: FileDiff object with path and hunks
        
    Returns:
        A unified diff string that whatthepatch can parse
    """
    lines = []
    
    # Add file headers
    # Use /dev/null for new files
    if diff.path == '/dev/null':
        lines.append('--- /dev/null')
    else:
        lines.append(f'--- {diff.path}')
    lines.append(f'+++ {diff.path}')
    
    # Sort hunks by starting line number (LLMs sometimes generate out of order)
    sorted_hunks = sorted(diff.hunks, key=_get_hunk_start_line)
    
    # Add all hunks with fixed headers
    for hunk in sorted_hunks:
        fixed_hunk = _fix_hunk_header(hunk)
        lines.append(fixed_hunk)

    return '\n'.join(lines)


def _count_lines_from_hunks(hunks: List[str]) -> Tuple[int, int]:
    """Count lines added/removed from hunks.
    
    Returns:
        Tuple of (lines_added, lines_removed)
    """
    lines_added = 0
    lines_removed = 0
    for hunk in hunks:
        for line in hunk.split('\n'):
            if line.startswith('+') and not line.startswith('+++'):
                lines_added += 1
            elif line.startswith('-') and not line.startswith('---'):
                lines_removed += 1
    return lines_added, lines_removed
