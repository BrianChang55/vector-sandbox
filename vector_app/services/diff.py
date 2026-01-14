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
from typing import Any, Dict, Generator, Iterator, List, Optional, Tuple

import whatthepatch

logger = logging.getLogger(__name__)

MAX_HUNKS_TO_APPLY_WITH_PATCH = 5


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


def _apply_with_subprocess_patch(original_content: str, diff_text: str, fuzz: int = 3) -> str:
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
        
        try:
            result = subprocess.run(
                ['patch', f'--fuzz={fuzz}', "--batch", "--posix", '-o', out_file, orig_file, '-i', patch_file],
                capture_output=True,
                text=True,
                timeout=2.0  # 2 second timeout to prevent hanging
            )
        except subprocess.TimeoutExpired:
            raise Exception("patch command timed out after 10 seconds")
        
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
    
    optional_result = _maybe_apply_with_subprocess_patch(original_content, diff)
    if optional_result:
        return optional_result

    # Fallback to whatthepatch - apply hunks one by one backwards
    # Applying backwards (highest line numbers first) prevents line number shifts
    # from affecting subsequent hunk applications
    return _apply_hunks_backwards(original_content, diff)


def _try_apply_single_hunk(hunk: str, current_content: str, file_path: str) -> Optional[str]:
    """
    Try to apply a single hunk to the content.
    
    Args:
        hunk: The hunk string to apply
        current_content: The current file content
        file_path: The file path (for constructing the diff)
        
    Returns:
        The new content if successful, None if failed
    """
    single_hunk_diff = f"--- {file_path}\n+++ {file_path}\n{_fix_hunk_header(hunk)}"
    
    try:
        parsed_diffs = list(whatthepatch.parse_patch(single_hunk_diff))
        if not parsed_diffs:
            return None
        
        parsed_diff = parsed_diffs[0]
        current_lines = current_content.split('\n')
        result = whatthepatch.apply_diff(parsed_diff, current_lines)
        new_lines = list(result)
        
        if new_lines is None:
            return None
        
        return '\n'.join(new_lines)
        
    except Exception:
        return None


def _apply_hunks_backwards(original_content: str, diff: FileDiff) -> str:
    """
    Apply hunks one by one in reverse order (highest line numbers first).
    
    This prevents line number shifts from earlier hunks affecting later ones.
    When applying from bottom to top, line numbers for earlier hunks remain valid.
    
    If a hunk fails to apply at its stated location, attempts to relocate it
    by searching for the hunk's old lines (context + removed) in the file. 
    If found at exactly one location, the hunk is retried there.
    
    If the full context search fails (0 matches), falls back to searching
    with just the removed lines. This is more resilient to AI-hallucinated
    context lines while still being precise if the removed lines are unique.
    
    If found at multiple locations (ambiguous), a warning is logged and the 
    hunk is skipped.
    
    Args:
        original_content: The original file content
        diff: FileDiff containing hunks to apply
        
    Returns:
        The modified file content
    """
    if not diff.hunks:
        return original_content
    
    # Sort hunks by starting line number in descending order to avoid shifting every applied hunk
    sorted_hunks = sorted(diff.hunks, key=_get_hunk_start_line, reverse=True)
    
    current_content = original_content
    applied_count = 0
    relocated_count = 0
    failed_hunks = []  # List of tuples: (start_line, hunk_content)
    
    for hunk in sorted_hunks:
        start_line = _get_hunk_start_line(hunk)
        
        # First attempt: apply at stated location
        result = _try_apply_single_hunk(hunk, current_content, diff.path)
        
        if result is not None:
            current_content = result
            applied_count += 1
            continue

        # Failed - attempt relocation by searching for old lines
        old_lines = _extract_old_lines(hunk)
        if not old_lines:
            formatted_content = format_with_line_numbers(current_content)
            logger.warning(
                f"[DIFF] Failed to apply hunk at line {start_line}, no old lines to relocate with\n"
                f"Failed hunk:\n{hunk}\n"
                f"Current file content:\n{formatted_content}"
            )
            failed_hunks.append((start_line, hunk))
            continue
        
        file_lines = current_content.split('\n')
        locations = _find_hunk_locations(old_lines, file_lines)
        
        # Track whether we used removed-lines-only fallback (need to reconstruct hunk)
        used_removed_only_fallback = False
        removed_only = []
        
        if len(locations) == 0:
            # Fallback: try searching with just the removed lines (more resilient to hallucinated context)
            removed_only = _extract_removed_lines_only(hunk)
            if removed_only:
                locations = _find_hunk_locations(removed_only, file_lines)
                if len(locations) == 1:
                    logger.info(f"[DIFF] Full context search failed, but found unique match using removed lines only at line {locations[0]}")
                    used_removed_only_fallback = True
                elif len(locations) > 1:
                    formatted_content = format_with_line_numbers(current_content)
                    logger.warning(
                        f"[DIFF] Failed to apply hunk at line {start_line}, "
                        f"removed-lines-only search found {len(locations)} possible locations: {locations}. Skipping.\n"
                        f"Failed hunk:\n{hunk}\n"
                        f"Current file content:\n{formatted_content}"
                    )
                    failed_hunks.append((start_line, hunk))
                    continue
            
            if len(locations) == 0:
                formatted_content = format_with_line_numbers(current_content)
                logger.warning(
                    f"[DIFF] Failed to apply hunk at line {start_line}, could not find matching lines in file\n"
                    f"Failed hunk:\n{hunk}\n"
                    f"Current file content:\n{formatted_content}"
                )
                failed_hunks.append((start_line, hunk))
                continue
        
        if len(locations) > 1:
            formatted_content = format_with_line_numbers(current_content)
            logger.warning(
                f"[DIFF] Failed to apply hunk at line {start_line}, "
                f"found {len(locations)} possible locations: {locations}. Skipping relocation.\n"
                f"Failed hunk:\n{hunk}\n"
                f"Current file content:\n{formatted_content}"
            )
            failed_hunks.append((start_line, hunk))
            continue
        
        # Exactly one location found - relocate and retry
        new_location = locations[0]
        
        if used_removed_only_fallback:
            # Reconstruct hunk with actual file context (since AI context was hallucinated)
            added_lines = _extract_added_lines_only(hunk)
            reconstructed_hunk = _reconstruct_hunk_with_actual_context(
                file_lines, new_location, removed_only, added_lines
            )
            result = _try_apply_single_hunk(reconstructed_hunk, current_content, diff.path)
            
            if result is not None:
                logger.info(f"[DIFF] Reconstructed and applied hunk at line {new_location} for {diff.path} (original line {start_line})")
                current_content = result
                applied_count += 1
                relocated_count += 1
            else:
                formatted_content = format_with_line_numbers(current_content)
                logger.warning(
                    f"[DIFF] Failed to apply reconstructed hunk at line {new_location} for {diff.path}\n"
                    f"Failed hunk (reconstructed):\n{reconstructed_hunk}\n"
                    f"Original hunk:\n{hunk}\n"
                    f"Current file content:\n{formatted_content}"
                )
                failed_hunks.append((start_line, hunk))
        else:
            # Full context search succeeded - just relocate header
            relocated_hunk = _relocate_hunk_header(hunk, new_location)
            result = _try_apply_single_hunk(relocated_hunk, current_content, diff.path)
            
            if result is not None:
                logger.info(f"[DIFF] Relocated hunk from line {start_line} to line {new_location} for {diff.path}")
                current_content = result
                applied_count += 1
                relocated_count += 1
            else:
                formatted_content = format_with_line_numbers(current_content)
                logger.warning(
                    f"[DIFF] Failed to apply hunk even after relocating from line {start_line} to {new_location}\n"
                    f"Failed hunk (relocated):\n{relocated_hunk}\n"
                    f"Original hunk:\n{hunk}\n"
                    f"Current file content:\n{formatted_content}"
                )
                failed_hunks.append((start_line, hunk))
    
    if failed_hunks:
        formatted_content = format_with_line_numbers(current_content)
        failed_lines = [line for line, _ in failed_hunks]
        failed_hunks_text = "\n\n".join([
            f"Failed hunk at line {line}:\n{hunk_content}"
            for line, hunk_content in failed_hunks
        ])
        logger.warning(
            f"[DIFF] {len(failed_hunks)} hunk(s) failed to apply for {diff.path}: lines {failed_lines}\n\n"
            f"Failed hunks:\n{failed_hunks_text}\n\n"
            f"Current file content:\n{formatted_content}"
        )
    
    if applied_count > 0:
        if relocated_count > 0:
            logger.info(f"[DIFF] Applied {applied_count}/{len(diff.hunks)} hunks for {diff.path} ({relocated_count} relocated)")
        else:
            logger.info(f"[DIFF] Applied {applied_count}/{len(diff.hunks)} hunks for {diff.path}")
        return current_content
    else:
        logger.error(f"[DIFF] All hunks failed to apply for {diff.path}")
        return original_content


def _fix_hunk_header(hunk: str) -> str:
    """
    Fix the line counts in a hunk header based on actual content.
    
    LLMs often generate incorrect line counts in @@ headers. This function
    recalculates them based on the actual lines in the hunk.
    
    Ensures the hunk ends with a single newline.
    
    Args:
        hunk: A single hunk string starting with @@ header
        
    Returns:
        The hunk with corrected @@ header line counts
    """
    hunk = hunk.strip()
    lines = hunk.split('\n')
    if not lines or not lines[0].startswith('@@'):
        return hunk + '\n'
    
    header = lines[0]
    content_lines = lines[1:]
    
    # Parse the original header to get starting line numbers
    # Format: @@ -start,count +start,count @@ optional context
    match = re.match(r'@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)?(?: @@(.*))?', header)
    if not match:
        return hunk + '\n'
    
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
    
    return new_header + '\n' + '\n'.join(content_lines) + '\n'


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


def _extract_old_lines(hunk: str) -> List[str]:
    """
    Extract lines expected in the original file from a hunk.
    
    These are the context lines (starting with ' ') and removed lines 
    (starting with '-'). This sequence can be used to locate where the
    hunk should be applied if the line numbers are incorrect.
    
    Args:
        hunk: A hunk string starting with @@ header
        
    Returns:
        List of lines (without the leading space/minus) that should exist
        in the original file at the hunk's target location
    """
    old_lines = []
    for line in hunk.split('\n'):
        # Skip the @@ header
        if line.startswith('@@'):
            continue
        # Removed lines (strip the - prefix)
        if line.startswith('-') and not line.startswith('---'):
            old_lines.append(line[1:])
        # Context lines (strip the space prefix)
        elif line.startswith(' '):
            old_lines.append(line[1:])
        # Empty lines in context (no prefix)
        elif line == '' and old_lines:
            old_lines.append('')
    return old_lines


def _extract_removed_lines_only(hunk: str) -> List[str]:
    """
    Extract only the removed lines from a hunk (lines starting with '-').
    
    This is used as a fallback for relocation when the full context+removed
    search fails. Searching for just the removed lines is more resilient to
    AI-hallucinated context lines, though less precise.
    
    Args:
        hunk: A hunk string starting with @@ header
        
    Returns:
        List of removed lines (without the - prefix)
    """
    removed_lines = []
    for line in hunk.split('\n'):
        # Skip the @@ header and --- file header
        if line.startswith('@@') or line.startswith('---'):
            continue
        # Only removed lines (strip the - prefix)
        if line.startswith('-'):
            removed_lines.append(line[1:])
    return removed_lines


def _extract_added_lines_only(hunk: str) -> List[str]:
    """
    Extract only the added lines from a hunk (lines starting with '+').
    
    Args:
        hunk: A hunk string starting with @@ header
        
    Returns:
        List of added lines (without the + prefix)
    """
    added_lines = []
    for line in hunk.split('\n'):
        # Skip the @@ header and +++ file header
        if line.startswith('@@') or line.startswith('+++'):
            continue
        # Only added lines (strip the + prefix)
        if line.startswith('+'):
            added_lines.append(line[1:])
    return added_lines


def _reconstruct_hunk_with_actual_context(
    file_lines: List[str],
    location: int,
    removed_lines: List[str],
    added_lines: List[str],
    context_lines: int = 3
) -> str:
    """
    Reconstruct a hunk using actual file content for context lines.
    
    This is used when the removed-lines-only fallback finds a unique match.
    Instead of using the AI's potentially hallucinated context, we extract
    the real context from the file around the found location.
    
    Args:
        file_lines: The file content split into lines
        location: 1-indexed line number where removed_lines start
        removed_lines: The lines being removed (without - prefix)
        added_lines: The lines being added (without + prefix)
        context_lines: Number of context lines before and after (default 3)
        
    Returns:
        A properly formatted hunk string with actual context
    """
    # Convert to 0-indexed
    start_idx = location - 1
    end_idx = start_idx + len(removed_lines)
    
    # Get context before (up to context_lines, but not before file start)
    context_before_start = max(0, start_idx - context_lines)
    context_before = file_lines[context_before_start:start_idx]
    
    # Get context after (up to context_lines, but not past file end)
    context_after_end = min(len(file_lines), end_idx + context_lines)
    context_after = file_lines[end_idx:context_after_end]
    
    # Calculate line numbers for header
    old_start = context_before_start + 1  # 1-indexed
    old_count = len(context_before) + len(removed_lines) + len(context_after)
    new_count = len(context_before) + len(added_lines) + len(context_after)
    
    # Build the hunk
    hunk_lines = [f'@@ -{old_start},{old_count} +{old_start},{new_count} @@']
    
    # Add context before
    for line in context_before:
        hunk_lines.append(' ' + line)
    
    # Add removed lines
    for line in removed_lines:
        hunk_lines.append('-' + line)
    
    # Add added lines
    for line in added_lines:
        hunk_lines.append('+' + line)
    
    # Add context after
    for line in context_after:
        hunk_lines.append(' ' + line)
    
    return '\n'.join(hunk_lines)


def _find_hunk_locations(old_lines: List[str], file_lines: List[str]) -> List[int]:
    """
    Find all positions where the old_lines sequence appears in file_lines.
    
    Uses exact matching - all lines must match in sequence.
    
    Args:
        old_lines: The sequence of lines to search for (from _extract_old_lines)
        file_lines: The file content split into lines
        
    Returns:
        List of 1-indexed line numbers where the sequence starts.
        Empty list if no matches, multiple entries if found in multiple places.
    """
    if not old_lines:
        return []
    
    matches = []
    search_range = len(file_lines) - len(old_lines) + 1
    
    for i in range(search_range):
        # Check if old_lines matches starting at position i
        if file_lines[i:i + len(old_lines)] == old_lines:
            matches.append(i + 1)  # Convert to 1-indexed
    
    return matches


def _relocate_hunk_header(hunk: str, new_start_line: int) -> str:
    """
    Adjust a hunk's @@ header to use a new starting line number.
    
    The "new" side start line is adjusted by the same offset as the "old" side,
    maintaining the relative difference between them.
    
    Args:
        hunk: A hunk string starting with @@ header
        new_start_line: The 1-indexed line number where the hunk should be applied
        
    Returns:
        The hunk with updated @@ header line numbers
    """
    lines = hunk.split('\n')
    if not lines or not lines[0].startswith('@@'):
        return hunk
    
    header = lines[0]
    content_lines = lines[1:]
    
    # Parse the original header
    # Format: @@ -old_start,old_count +new_start,new_count @@ optional context
    match = re.match(r'@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))?(?: @@(.*))?', header)
    if not match:
        return hunk
    
    old_start = int(match.group(1))
    old_count = match.group(2) or '1'
    orig_new_start = int(match.group(3))
    new_count = match.group(4) or '1'
    context_text = match.group(5) or ''
    
    # Calculate the offset between old and new start lines
    # This preserves the relationship (e.g., if adding lines, new_start > old_start)
    offset = orig_new_start - old_start
    relocated_new_start = new_start_line + offset
    
    # Reconstruct header with new line numbers
    new_header = f'@@ -{new_start_line},{old_count} +{relocated_new_start},{new_count} @@{context_text}'
    
    return new_header + '\n' + '\n'.join(content_lines)


def _reconstruct_diff(diff: FileDiff) -> str:
    """
    Reconstruct a proper unified diff string from FileDiff for whatthepatch.
    
    Fixes hunk headers with incorrect line counts (common LLM error).
    Deduplicates hunks (last suggestion wins) and sorts them by starting line 
    number to ensure correct order.
    
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
    
    # Deduplicate hunks: last suggestion wins for same start line
    # LLMs sometimes repeat the same hunk or provide multiple versions of the same edit
    unique_hunks = {}
    for hunk in diff.hunks:
        start_line = _get_hunk_start_line(hunk)
        unique_hunks[start_line] = hunk

    # Sort hunks by starting line number (LLMs sometimes generate out of order)
    original_start_lines = [_get_hunk_start_line(h) for h in diff.hunks]
    # Filter out duplicates from original order for comparison
    seen = set()
    original_uniques = []
    for sl in original_start_lines:
        if sl not in seen:
            original_uniques.append(sl)
            seen.add(sl)
    
    sorted_start_lines = sorted(unique_hunks.keys())

    if original_uniques != sorted_start_lines:
        logger.warning(f"Misordered or duplicate hunks detected for {diff.path}. Reordering and deduplicating. Original order (uniques): {original_uniques}, Sorted: {sorted_start_lines}")

    # Add all hunks with fixed headers
    for start_line in sorted_start_lines:
        hunk = unique_hunks[start_line]
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


def _maybe_apply_with_subprocess_patch(original_content: str, diff: FileDiff) -> Optional[str]:
    if len(diff.hunks) > MAX_HUNKS_TO_APPLY_WITH_PATCH:
        return None

    diff_text = _reconstruct_diff(diff)
    try:
        # Use our own subprocess patch implementation (with proper error handling)
        # patch command has fuzz matching and handles multi-hunk offsets automatically
        result = _apply_with_subprocess_patch(original_content, diff_text)
        return result
    except Exception as e:
        logger.warning(f"Subprocess patch failed for {diff.path}: {e}, falling back to whatthepatch. Hunk: {diff_text}")
        return None
