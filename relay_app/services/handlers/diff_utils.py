"""
Diff Utilities for Surgical Edits

Provides utilities to parse unified diffs from LLM output and apply them to files.
"""

import re
from dataclasses import dataclass, field
from typing import List, Optional, Tuple


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
    
    for start_line, old_count, new_lines, context_before in parsed_hunks:
        lines = _apply_hunk(lines, start_line, old_count, new_lines, context_before)
    
    return '\n'.join(lines)


def _parse_hunk(hunk: str) -> Optional[Tuple[int, int, List[str], List[str]]]:
    """
    Parse a single hunk into its components.
    
    Returns:
        Tuple of (start_line, old_line_count, new_lines, context_before)
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
    old_count = int(header_match.group(2)) if header_match.group(2) else 1
    
    # Parse hunk content
    context_before: List[str] = []
    new_lines: List[str] = []
    in_change = False
    lines_to_remove = 0
    
    for line in lines[1:]:
        if line.startswith(' '):
            # Context line
            if not in_change:
                context_before.append(line[1:])
            else:
                new_lines.append(line[1:])
        elif line.startswith('-'):
            # Removal
            in_change = True
            lines_to_remove += 1
        elif line.startswith('+'):
            # Addition
            in_change = True
            new_lines.append(line[1:])
        elif line == '':
            # Empty line - could be context or end of hunk
            if in_change:
                new_lines.append('')
            else:
                context_before.append('')
    
    return (old_start, old_count, new_lines, context_before)


def _apply_hunk(
    lines: List[str],
    start_line: int,
    old_count: int,
    new_lines: List[str],
    context_before: List[str],
) -> List[str]:
    """
    Apply a single parsed hunk to the file lines.
    
    Uses context matching to find the correct location if line numbers
    don't match exactly (handles file drift).
    """
    # Convert to 0-indexed
    start_idx = start_line - 1
    
    # Try exact position first
    if _context_matches(lines, start_idx, context_before):
        return _splice_lines(lines, start_idx, old_count, new_lines, len(context_before))
    
    # Search nearby for matching context (handle drift)
    search_range = 50  # Search up to 50 lines in either direction
    for offset in range(1, search_range + 1):
        # Try above
        if start_idx - offset >= 0:
            if _context_matches(lines, start_idx - offset, context_before):
                return _splice_lines(lines, start_idx - offset, old_count, new_lines, len(context_before))
        
        # Try below
        if start_idx + offset < len(lines):
            if _context_matches(lines, start_idx + offset, context_before):
                return _splice_lines(lines, start_idx + offset, old_count, new_lines, len(context_before))
    
    # Fallback: apply at original position even if context doesn't match
    return _splice_lines(lines, start_idx, old_count, new_lines, len(context_before))


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
    old_count: int,
    new_lines: List[str],
    context_len: int,
) -> List[str]:
    """Replace lines from start_idx, removing old_count and inserting new_lines."""
    # The actual change starts after the context lines
    change_start = start_idx + context_len
    
    # Calculate how many lines to remove (old_count includes context)
    lines_to_remove = old_count - context_len
    
    # Handle edge cases
    if change_start < 0:
        change_start = 0
    if change_start > len(lines):
        change_start = len(lines)
    
    # Ensure we don't remove more lines than exist
    if lines_to_remove < 0:
        lines_to_remove = 0
    end_idx = min(change_start + lines_to_remove, len(lines))
    
    # Splice: keep before, add new, keep after
    result = lines[:change_start] + new_lines + lines[end_idx:]
    
    return result
