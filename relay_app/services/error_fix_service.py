"""
Error Fix Service

LLM-based service for automatically fixing TypeScript compilation errors.
Yields streaming events for Live Activity progress visibility.

Key constraints:
- Maximum 2 fix attempts
- Agent must NOT change core functionality
- Only fix the specific errors reported
- Make minimum changes necessary
"""
import json
import logging
import re
from typing import Dict, Any, List, Generator, Optional

from django.conf import settings
import httpx

from relay_app.prompts.error_fix import (
    ERROR_FIX_SYSTEM_PROMPT,
    build_error_fix_prompt,
    build_bundler_error_fix_prompt,
)
from relay_app.services.agentic_service import (
    AgentEvent,
    FileChange,
    CompilationError,
)

logger = logging.getLogger(__name__)


class ErrorFixService:
    """
    Service for automatically fixing compilation errors in generated code.
    
    Uses an LLM with carefully constrained prompts to fix errors without
    changing core functionality. Streams progress events for Live Activity.
    """
    
    OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions"
    MAX_ATTEMPTS = 2  # Maximum fix attempts before giving up
    
    def __init__(self):
        self.api_key = getattr(settings, 'OPENROUTER_API_KEY', None) or \
                      getattr(settings, 'OPENAI_API_KEY', None)
        self.app_name = getattr(settings, 'OPENROUTER_APP_NAME', 'Internal Apps Builder')
        self.site_url = getattr(settings, 'BASE_URL', 'http://localhost:8001')
    
    def _build_headers(self) -> Dict[str, str]:
        """Build API headers for OpenRouter."""
        return {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json",
            "HTTP-Referer": self.site_url,
            "X-Title": self.app_name,
        }
    
    def fix_errors(
        self,
        files: List[FileChange],
        errors: List[CompilationError],
        model: str = "anthropic/claude-sonnet-4",
        attempt: int = 1,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """
        Fix compilation errors without changing core functionality.
        
        Yields Live Activity events for progress visibility.
        Returns list of fixed FileChange objects.
        
        Args:
            files: Current list of generated files
            errors: List of compilation errors to fix
            model: LLM model to use
            attempt: Current attempt number (1 or 2)
        
        Yields:
            AgentEvent objects for streaming to frontend
        
        Returns:
            List of FileChange objects with fixes applied
        """
        if not errors:
            return files
        
        logger.info(f"Fixing {len(errors)} errors (attempt {attempt}/{self.MAX_ATTEMPTS})")
        
        # Yield start event
        yield AgentEvent("fix_started", {
            "attempt": attempt,
            "max_attempts": self.MAX_ATTEMPTS,
            "error_count": len(errors),
        })
        
        # Build focused prompt for error fixing
        prompt = build_error_fix_prompt(
            files=files,
            errors=errors,
            attempt=attempt,
            max_attempts=self.MAX_ATTEMPTS,
        )
        
        yield AgentEvent("thinking", {
            "content": f"Analyzing {len(errors)} compilation errors...",
            "type": "observation",
        })
        
        try:
            with httpx.Client(timeout=120.0) as client:
                with client.stream(
                    "POST",
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json={
                        "model": model,
                        "messages": [
                            {"role": "system", "content": ERROR_FIX_SYSTEM_PROMPT},
                            {"role": "user", "content": prompt},
                        ],
                        "temperature": 0.2,  # Lower temperature for more focused fixes
                        "stream": True,
                    },
                ) as response:
                    response.raise_for_status()
                    
                    full_content = ""
                    chunk_count = 0
                    progress_emitted = False
                    
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
                                    
                                    # Emit progress only once after receiving initial content
                                    if not progress_emitted and chunk_count >= 10:
                                        progress_emitted = True
                                        yield AgentEvent("fix_progress", {
                                            "attempt": attempt,
                                            "message": "Generating fixes...",
                                        })
                                        
                            except json.JSONDecodeError:
                                continue
                    
                    # Parse fixed files from response
                    fixed_files = self._parse_fixed_files(full_content, files)
                    
                    # Emit events for each fixed file
                    for fixed_file in fixed_files:
                        yield AgentEvent("fix_file_updated", {
                            "file_path": fixed_file.path,
                            "attempt": attempt,
                        })
                        
                        # Also emit file_generated for consistency
                        yield AgentEvent("file_generated", {
                            "file": {
                                "path": fixed_file.path,
                                "action": "modify",
                                "language": fixed_file.language,
                                "content": fixed_file.content,
                            }
                        })
                    
                    if fixed_files:
                        yield AgentEvent("thinking", {
                            "content": f"Fixed {len(fixed_files)} file(s)",
                            "type": "decision",
                        })
                    else:
                        yield AgentEvent("thinking", {
                            "content": "No fixes applied - errors may require manual review",
                            "type": "reflection",
                        })
                    
                    # Merge fixed files back into original list
                    result_files = self._merge_fixed_files(files, fixed_files)
                    
                    return result_files
                    
        except Exception as e:
            logger.error(f"Error fix service error: {e}")
            yield AgentEvent("thinking", {
                "content": f"Error during fix attempt: {str(e)}",
                "type": "reflection",
            })
            # Return original files on error
            return files
    
    def fix_bundler_errors(
        self,
        files: List[FileChange],
        bundler_errors: List[Dict[str, Any]],
        model: str = "anthropic/claude-sonnet-4",
        attempt: int = 1,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """
        Fix bundler/runtime errors that TypeScript didn't catch.
        
        Similar to fix_errors but uses bundler-specific prompts.
        """
        if not bundler_errors:
            return files
        
        logger.info(f"Fixing {len(bundler_errors)} bundler errors (attempt {attempt}/{self.MAX_ATTEMPTS})")
        
        yield AgentEvent("fix_started", {
            "attempt": attempt,
            "max_attempts": self.MAX_ATTEMPTS,
            "error_count": len(bundler_errors),
            "error_type": "bundler",
        })
        
        prompt = build_bundler_error_fix_prompt(
            files=files,
            bundler_errors=bundler_errors,
            attempt=attempt,
            max_attempts=self.MAX_ATTEMPTS,
        )
        
        yield AgentEvent("thinking", {
            "content": f"Analyzing {len(bundler_errors)} bundler errors...",
            "type": "observation",
        })
        
        try:
            with httpx.Client(timeout=120.0) as client:
                with client.stream(
                    "POST",
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json={
                        "model": model,
                        "messages": [
                            {"role": "system", "content": ERROR_FIX_SYSTEM_PROMPT},
                            {"role": "user", "content": prompt},
                        ],
                        "temperature": 0.2,
                        "stream": True,
                    },
                ) as response:
                    response.raise_for_status()
                    
                    full_content = ""
                    
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
                                    
                            except json.JSONDecodeError:
                                continue
                    
                    fixed_files = self._parse_fixed_files(full_content, files)
                    
                    for fixed_file in fixed_files:
                        yield AgentEvent("fix_file_updated", {
                            "file_path": fixed_file.path,
                            "attempt": attempt,
                        })
                        
                        yield AgentEvent("file_generated", {
                            "file": {
                                "path": fixed_file.path,
                                "action": "modify",
                                "language": fixed_file.language,
                                "content": fixed_file.content,
                            }
                        })
                    
                    return self._merge_fixed_files(files, fixed_files)
                    
        except Exception as e:
            logger.error(f"Bundler error fix error: {e}")
            yield AgentEvent("thinking", {
                "content": f"Error during bundler fix: {str(e)}",
                "type": "reflection",
            })
            return files
    
    def _parse_fixed_files(
        self,
        content: str,
        original_files: List[FileChange],
    ) -> List[FileChange]:
        """Parse fixed files from LLM response."""
        fixed_files = []
        seen_paths = set()
        
        # Build lookup for original file content
        original_by_path = {f.path: f.content for f in original_files}
        
        # Pattern to match file blocks - order matters
        patterns = [
            # Pattern 1: ```filepath:path/to/file.ext (highest priority)
            r'```filepath:([^\n`]+)\n(.*?)```',
            # Pattern 2: ```src/path/to/file.ext (explicit src path)
            r'```(src/[^\n`]+\.[a-zA-Z]+)\n(.*?)```',
            # Pattern 3: ```path/to/file.ext (with extension, but not filepath:)
            r'```([a-zA-Z][^\n`]*\.[a-zA-Z]+)\n(.*?)```',
        ]
        
        for pattern in patterns:
            matches = re.findall(pattern, content, re.DOTALL)
            for filepath, code in matches:
                filepath = filepath.strip()
                code = code.strip()
                
                # Remove any filepath: prefix that might have been captured
                if filepath.startswith('filepath:'):
                    filepath = filepath[9:].strip()
                
                # Skip language-only markers
                if filepath.lower() in ('tsx', 'ts', 'js', 'jsx', 'css', 'json', 'typescript'):
                    continue
                
                # Skip if path contains 'filepath:' (malformed)
                if 'filepath:' in filepath:
                    continue
                
                if not code or len(code) < 10:
                    continue
                
                # Normalize path
                if not filepath.startswith('src/'):
                    filepath = f"src/{filepath}"
                
                # Skip if already found
                if filepath in seen_paths:
                    continue
                seen_paths.add(filepath)
                
                # Determine language
                ext = filepath.split('.')[-1] if '.' in filepath else 'tsx'
                lang_map = {'tsx': 'tsx', 'ts': 'ts', 'css': 'css', 'json': 'json'}

                # TODO: Add better line counting logic here
                # Calculate lines added/removed
                original_content = original_by_path.get(filepath, "")
                original_lines = original_content.count('\n') + (1 if original_content and not original_content.endswith('\n') else 0)
                new_lines = code.count('\n') + (1 if code and not code.endswith('\n') else 0)
                lines_added = max(0, new_lines - original_lines)
                lines_removed = max(0, original_lines - new_lines)
                
                fixed_files.append(FileChange(
                    path=filepath,
                    action='modify',
                    language=lang_map.get(ext, 'tsx'),
                    content=code,
                    lines_added=lines_added,
                    lines_removed=lines_removed,
                ))
        
        logger.info(f"Parsed {len(fixed_files)} fixed files from response")
        return fixed_files
    
    def _merge_fixed_files(
        self,
        original_files: List[FileChange],
        fixed_files: List[FileChange],
    ) -> List[FileChange]:
        """Merge fixed files back into original file list."""
        # Create lookup for quick access
        fixed_by_path = {f.path: f for f in fixed_files}
        
        result = []
        for orig_file in original_files:
            if orig_file.path in fixed_by_path:
                # Replace with fixed version
                result.append(fixed_by_path[orig_file.path])
            else:
                # Keep original
                result.append(orig_file)
        
        return result


# Singleton
_error_fix_service: Optional[ErrorFixService] = None


def get_error_fix_service() -> ErrorFixService:
    """Get singleton error fix service instance."""
    global _error_fix_service
    if _error_fix_service is None:
        _error_fix_service = ErrorFixService()
    return _error_fix_service

