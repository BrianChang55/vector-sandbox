"""
Diff Utilities for Surgical Edits

Provides utilities to parse unified diffs from LLM output and apply them to files.
"""

import logging
import re
from dataclasses import dataclass, field
from typing import List, Optional, Tuple

logger = logging.getLogger(__name__)


@dataclass
class FileDiff:
    """A unified diff for a single file."""
    path: str
    hunks: List[str] = field(default_factory=list)


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
                diffs.append(FileDiff(path=current_path, hunks=current_hunks))
            
            # Parse new file path
            # Handle formats: "--- src/file.tsx", "--- a/src/file.tsx"
            path = line[4:].strip()
            if path.startswith('a/'):
                path = path[2:]
            
            current_path = path
            current_hunks = []
            current_hunk_lines = []
            in_hunk = False
            
            # Skip the +++ line if present
            if i + 1 < len(lines) and lines[i + 1].startswith('+++ '):
                i += 1
        
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
        diffs.append(FileDiff(path=current_path, hunks=current_hunks))
    
    return diffs


def apply_diff(original_content: str, diff: FileDiff) -> str:
    """
    Apply a unified diff to original file content.
    
    Args:
        original_content: The original file content
        diff: FileDiff containing hunks to apply
        
    Returns:
        The modified file content
    """
    if not diff.hunks:
        return original_content
    
    lines = original_content.split('\n')
    
    # Apply hunks in reverse order to preserve line numbers
    # Parse all hunks first
    parsed_hunks = []
    for hunk in diff.hunks:
        parsed = _parse_hunk(hunk)
        if parsed:
            parsed_hunks.append(parsed)
    
    # Sort by start line descending so we apply from bottom to top
    parsed_hunks.sort(key=lambda h: h[0], reverse=True)
    
    for start_line, lines_to_remove, new_lines, context_before in parsed_hunks:
        lines = _apply_hunk(lines, start_line, lines_to_remove, new_lines, context_before)
    
    return '\n'.join(lines)


def _parse_hunk(hunk: str) -> Optional[Tuple[int, int, List[str], List[str]]]:
    """
    Parse a single hunk into its components.
    
    Returns:
        Tuple of (start_line, lines_to_remove, new_lines, context_before)
        where lines_to_remove is the actual count of '-' lines (not old_count from header)
        or None if parsing fails
    """
    lines = hunk.split('\n')
    if not lines:
        return None
    
    # Parse hunk header: @@ -start,count +start,count @@ optional context
    header_match = re.match(r'@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@', lines[0])
    if not header_match:
        return None
    
    old_start = int(header_match.group(1))
    
    # Parse hunk content
    context_before: List[str] = []
    new_lines: List[str] = []
    in_change = False
    lines_to_remove = 0
    
    for line in lines[1:]:
        if line.startswith(' '):
            # Context line
            if not in_change:
                # Context BEFORE changes - used for matching location
                context_before.append(line[1:])
            # else: Context AFTER changes - ignore, these already exist in source file
        elif line.startswith('-'):
            # Removal
            in_change = True
            lines_to_remove += 1
        elif line.startswith('+'):
            # Addition - these are the actual new lines to insert
            in_change = True
            new_lines.append(line[1:])
        elif line == '':
            # Empty line without prefix - could be context or end of hunk
            # Only add to context_before if we haven't started changes yet
            if not in_change:
                context_before.append('')
            # After changes, bare empty lines are context-after - ignore them
    
    # DEBUG: Log parsed hunk details
    logger.debug(f"[DIFF] Parsed hunk: start={old_start}, lines_to_remove={lines_to_remove}, "
                 f"new_lines_count={len(new_lines)}, context_before_count={len(context_before)}")
    
    # Return actual lines_to_remove count (not old_count from header)
    # This fixes the bug where pure additions incorrectly removed context-after lines
    return (old_start, lines_to_remove, new_lines, context_before)


def _apply_hunk(
    lines: List[str],
    start_line: int,
    lines_to_remove: int,
    new_lines: List[str],
    context_before: List[str],
) -> List[str]:
    """
    Apply a single parsed hunk to the file lines.
    
    Uses context matching to find the correct location if line numbers
    don't match exactly (handles file drift).
    
    Args:
        lines: Original file lines
        start_line: 1-indexed start line from hunk header
        lines_to_remove: Actual count of '-' lines in the hunk
        new_lines: Lines to insert ('+' lines and context-after)
        context_before: Context lines before the change for matching
    """
    # Convert to 0-indexed
    start_idx = start_line - 1
    
    # Try exact position first
    if _context_matches(lines, start_idx, context_before):
        return _splice_lines(lines, start_idx, lines_to_remove, new_lines, len(context_before))
    
    # Search nearby for matching context (handle drift)
    search_range = 50  # Search up to 50 lines in either direction
    for offset in range(1, search_range + 1):
        # Try above
        if start_idx - offset >= 0:
            if _context_matches(lines, start_idx - offset, context_before):
                return _splice_lines(lines, start_idx - offset, lines_to_remove, new_lines, len(context_before))
        
        # Try below
        if start_idx + offset < len(lines):
            if _context_matches(lines, start_idx + offset, context_before):
                return _splice_lines(lines, start_idx + offset, lines_to_remove, new_lines, len(context_before))
    
    # Fallback: apply at original position even if context doesn't match
    return _splice_lines(lines, start_idx, lines_to_remove, new_lines, len(context_before))


def _context_matches(lines: List[str], start_idx: int, context: List[str]) -> bool:
    """Check if context lines match at the given position."""
    if not context:
        return True
    
    if start_idx < 0 or start_idx + len(context) > len(lines):
        return False
    
    for i, ctx_line in enumerate(context):
        if start_idx + i >= len(lines):
            return False
        # Fuzzy match: strip whitespace for comparison
        if lines[start_idx + i].strip() != ctx_line.strip():
            return False
    
    return True


def _splice_lines(
    lines: List[str],
    start_idx: int,
    lines_to_remove: int,
    new_lines: List[str],
    context_len: int,
) -> List[str]:
    """
    Replace lines from start_idx, removing lines_to_remove and inserting new_lines.
    
    Args:
        lines: Original file lines
        start_idx: 0-indexed start position
        lines_to_remove: Actual count of lines to remove (from '-' lines in diff)
        new_lines: Lines to insert
        context_len: Number of context-before lines (to skip past)
    """
    # The actual change starts after the context lines
    change_start = start_idx + context_len
    
    # Handle edge cases
    if change_start < 0:
        change_start = 0
    if change_start > len(lines):
        change_start = len(lines)
    
    # Ensure we don't remove more lines than exist
    if lines_to_remove < 0:
        lines_to_remove = 0
    end_idx = min(change_start + lines_to_remove, len(lines))
    
    # DEBUG: Log splice operation
    logger.debug(f"[DIFF] Splice: change_start={change_start}, lines_to_remove={lines_to_remove}, "
                 f"new_lines_count={len(new_lines)}, end_idx={end_idx}")
    
    # Splice: keep before, add new, keep after
    result = lines[:change_start] + new_lines + lines[end_idx:]
    
    return result
