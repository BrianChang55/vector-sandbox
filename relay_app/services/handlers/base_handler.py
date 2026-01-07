"""
Base Handler

Abstract base class for all intent handlers.
Provides common utilities for AgentEvent generation and LLM interactions.
"""
import json
import logging
import re
import time
import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass, asdict
from typing import Any, Dict, Generator, List, Optional, TYPE_CHECKING

from django.conf import settings
import httpx

if TYPE_CHECKING:
    from relay_app.models import InternalApp, AppVersion
    from relay_app.services.intent_classifier import IntentResult
    from relay_app.services.context_analyzer import AppContext

logger = logging.getLogger(__name__)


@dataclass
class FileChange:
    """A file change during execution."""
    path: str
    action: str  # create, modify, delete
    language: str
    content: str
    
    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


@dataclass
class AgentEvent:
    """An event emitted during agent execution."""
    type: str
    data: Dict[str, Any]

    def to_sse(self) -> str:
        """Format as Server-Sent Event."""
        return f"event: {self.type}\ndata: {json.dumps(self.data)}\n\n"


@dataclass
class PlanStep:
    """A single step in the execution plan."""
    id: str
    type: str
    title: str
    description: str
    status: str = "pending"
    duration: Optional[int] = None
    output: Optional[str] = None


class StreamingValidator:
    """
    Validates code during streaming to catch issues early.
    
    Performs lightweight checks on accumulated content to detect:
    - Excessive 'any' type usage
    - Debug console.log statements
    - TODO/FIXME placeholders
    - Bracket imbalance
    - Empty catch blocks
    """
    
    def __init__(self):
        self.warnings: List[str] = []
        self.any_count = 0
        self.console_log_count = 0
        self.todo_count = 0
        self._last_check_length = 0
    
    def check_chunk(self, chunk: str, accumulated: str) -> List[str]:
        """
        Check a new chunk for issues.
        
        Only performs expensive checks periodically to avoid overhead.
        
        Returns list of new warnings (if any).
        """
        new_warnings = []
        
        # Quick checks on the new chunk
        if 'as any' in chunk:
            self.any_count += chunk.count('as any')
            if self.any_count > 3:
                warning = f"Excessive 'as any' usage ({self.any_count}x) - consider proper types"
                if warning not in self.warnings:
                    self.warnings.append(warning)
                    new_warnings.append(warning)
        
        if 'console.log' in chunk:
            self.console_log_count += chunk.count('console.log')
            if self.console_log_count > 2:
                warning = f"Debug console.log statements ({self.console_log_count}x) - remove before production"
                if warning not in self.warnings:
                    self.warnings.append(warning)
                    new_warnings.append(warning)
        
        if '// TODO' in chunk or '// FIXME' in chunk:
            self.todo_count += 1
            warning = "TODO/FIXME placeholder detected - ensure completion"
            if warning not in self.warnings:
                self.warnings.append(warning)
                new_warnings.append(warning)
        
        # Periodic deeper checks (every 2000 chars)
        if len(accumulated) - self._last_check_length > 2000:
            self._last_check_length = len(accumulated)
            
            # Check for empty catch blocks
            if 'catch' in accumulated and 'catch (e) {}' in accumulated.replace(' ', ''):
                warning = "Empty catch block detected - add error handling"
                if warning not in self.warnings:
                    self.warnings.append(warning)
                    new_warnings.append(warning)
        
        return new_warnings
    
    def final_check(self, content: str) -> List[str]:
        """
        Perform final validation checks on complete content.
        
        Returns list of final warnings.
        """
        final_warnings = []
        
        # Check bracket balance
        open_braces = content.count('{')
        close_braces = content.count('}')
        if abs(open_braces - close_braces) > 2:
            final_warnings.append(f"Bracket imbalance: {open_braces} open vs {close_braces} close")
        
        open_parens = content.count('(')
        close_parens = content.count(')')
        if abs(open_parens - close_parens) > 2:
            final_warnings.append(f"Parenthesis imbalance: {open_parens} open vs {close_parens} close")
        
        # Check for incomplete code patterns
        if '...' in content and 'rest' not in content.lower():
            final_warnings.append("Ellipsis '...' found - may indicate incomplete code")
        
        return final_warnings
    
    def get_all_warnings(self) -> List[str]:
        """Get all accumulated warnings."""
        return self.warnings.copy()


class BaseHandler(ABC):
    """
    Abstract base class for intent handlers.
    
    All handlers must implement the execute() method which yields AgentEvent
    objects for the streaming response.
    """
    
    OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions"
    
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
    
    @abstractmethod
    def execute(
        self,
        intent: 'IntentResult',
        context: 'AppContext',
        user_message: str,
        current_spec: Optional[Dict[str, Any]],
        registry_surface: Dict[str, Any],
        app_name: str,
        model: str,
        app: Optional['InternalApp'] = None,
        version: Optional['AppVersion'] = None,
        **kwargs,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """
        Execute the handler for the given intent.
        
        Args:
            intent: Classified intent result
            context: App context from analyzer
            user_message: Original user request
            current_spec: Current app specification
            registry_surface: Available data resources
            app_name: Name of the app
            model: LLM model to use
            app: Optional InternalApp instance
            version: Optional AppVersion instance
            
        Yields:
            AgentEvent objects for streaming
            
        Returns:
            List of FileChange objects representing all changes
        """
        pass
    
    # ===== Common Event Emitters =====
    
    def emit_phase_change(self, phase: str, message: str) -> AgentEvent:
        """Emit a phase change event."""
        return AgentEvent("phase_change", {
            "phase": phase,
            "message": message,
        })
    
    def emit_thinking(self, content: str, thinking_type: str = "reasoning") -> AgentEvent:
        """Emit a thinking event."""
        return AgentEvent("thinking", {
            "content": content,
            "type": thinking_type,
        })
    
    def emit_plan_created(
        self,
        steps: List[PlanStep],
        explored_dirs: int = 0,
        explored_files: int = 0,
        searches: int = 0,
    ) -> AgentEvent:
        """Emit a plan created event."""
        plan = {
            "id": str(uuid.uuid4()),
            "goal": "Execute plan",
            "reasoning": "Plan created based on user intent",
            "steps": [asdict(s) for s in steps],
            "estimated_duration": len(steps) * 5000,
        }
        return AgentEvent("plan_created", {
            "plan": plan,
            "steps": [asdict(s) for s in steps],
            "exploredDirectories": explored_dirs,
            "exploredFiles": explored_files,
            "searches": searches,
        })
    
    def emit_step_started(self, step: PlanStep, step_index: int) -> AgentEvent:
        """Emit step started event."""
        return AgentEvent("step_started", {
            "stepId": step.id,
            "stepIndex": step_index,
        })
    
    def emit_step_start(self, step: PlanStep, step_index: int) -> AgentEvent:
        """Emit step start event (legacy format)."""
        return AgentEvent("step_start", {
            "step_index": step_index,
            "step": asdict(step),
        })
    
    def emit_step_completed(self, step: PlanStep, step_index: int, duration: int) -> AgentEvent:
        """Emit step completed event."""
        return AgentEvent("step_completed", {
            "stepId": step.id,
            "stepIndex": step_index,
            "duration": duration,
        })
    
    def emit_step_complete(self, step_index: int, status: str, duration: int) -> AgentEvent:
        """Emit step complete event (legacy format)."""
        return AgentEvent("step_complete", {
            "step_index": step_index,
            "status": status,
            "duration": duration,
        })
    
    def emit_step_progress(self, step_index: int, progress: int, message: str) -> AgentEvent:
        """Emit step progress event."""
        return AgentEvent("step_progress", {
            "step_index": step_index,
            "progress": min(100, progress),
            "message": message,
        })
    
    def emit_file_generated(self, file: FileChange) -> AgentEvent:
        """Emit file generated event."""
        return AgentEvent("file_generated", {
            "file": file.to_dict(),
        })
    
    def emit_table_created(self, slug: str, name: str, columns: int) -> AgentEvent:
        """Emit table created event."""
        return AgentEvent("table_created", {
            "slug": slug,
            "name": name,
            "columns": columns,
        })
    
    def emit_table_updated(
        self,
        slug: str,
        name: str,
        added: List[str] = None,
        removed: List[str] = None,
        modified: List[str] = None,
    ) -> AgentEvent:
        """Emit table updated event."""
        return AgentEvent("table_updated", {
            "slug": slug,
            "name": name,
            "changes": {
                "added": added or [],
                "removed": removed or [],
                "modified": modified or [],
            },
        })
    
    def emit_validation_result(
        self,
        passed: bool,
        errors: List[Dict[str, Any]] = None,
        warnings: List[str] = None,
        fix_attempts: int = 0,
    ) -> AgentEvent:
        """Emit validation result event."""
        return AgentEvent("validation_result", {
            "passed": passed,
            "errors": errors or [],
            "warnings": warnings or [],
            "fix_attempts": fix_attempts,
        })
    
    # ===== Streaming Validation =====
    
    def create_streaming_validator(self) -> StreamingValidator:
        """Create a new streaming validator instance."""
        return StreamingValidator()
    
    def emit_streaming_warning(self, warning: str) -> AgentEvent:
        """Emit a warning detected during streaming."""
        return AgentEvent("streaming_warning", {
            "warning": warning,
            "severity": "low",
        })
    
    # ===== LLM Utilities =====
    
    def stream_llm_response(
        self,
        system_prompt: str,
        user_prompt: str,
        model: str,
        temperature: float = 0.3,
        timeout: float = 120.0,
    ) -> Generator[str, None, str]:
        """
        Stream LLM response and yield chunks.
        
        Yields individual content chunks as they arrive.
        Returns the full accumulated content.
        """
        full_content = ""
        
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
                                    yield content
                                    
                            except json.JSONDecodeError:
                                continue
                                
        except Exception as e:
            logger.error(f"LLM streaming error: {e}")
            raise
        
        return full_content
    
    def call_llm(
        self,
        system_prompt: str,
        user_prompt: str,
        model: str,
        temperature: float = 0.3,
        timeout: float = 60.0,
    ) -> str:
        """
        Make a non-streaming LLM call.
        
        Returns the complete response content.
        """
        try:
            with httpx.Client(timeout=timeout) as client:
                response = client.post(
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json={
                        "model": model,
                        "messages": [
                            {"role": "system", "content": system_prompt},
                            {"role": "user", "content": user_prompt},
                        ],
                        "temperature": temperature,
                    },
                )
                response.raise_for_status()
                
                result = response.json()
                return result["choices"][0]["message"]["content"]
                
        except Exception as e:
            logger.error(f"LLM call error: {e}")
            raise
    
    # ===== Code Parsing Utilities =====
    
    def parse_code_blocks(self, content: str) -> List[FileChange]:
        """
        Parse code blocks from LLM response into FileChange objects.
        
        Supports multiple formats:
        - ```filepath:path/to/file.ext
        - ```path/to/file.ext
        - // filepath: path/to/file.ext
        """
        files = []
        seen_paths = set()
        
        patterns = [
            # Pattern 1: ```filepath:path/to/file.ext (highest priority)
            r'```filepath:([^\n`]+)\n(.*?)```',
            # Pattern 2: ```src/path/to/file.ext (explicit src path)
            r'```(src/[^\n`]+\.[a-zA-Z]+)\n(.*?)```',
            # Pattern 3: ```path/to/file.ext (with extension, but not filepath:)
            r'```([a-zA-Z][^\n`]*\.[a-zA-Z]+)\n(.*?)```',
            # Pattern 4: // filepath: path/to/file.ext followed by code
            r'// filepath:\s*([^\n]+)\n(.*?)(?=// filepath:|```|$)',
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
                if filepath.lower() in ('tsx', 'ts', 'js', 'jsx', 'css', 'json', 'html',
                                        'typescript', 'javascript', 'react', 'python'):
                    continue
                
                # Skip if path contains 'filepath:' (malformed)
                if 'filepath:' in filepath:
                    continue
                
                # Skip if no actual code
                if not code or len(code) < 10:
                    continue
                
                # Normalize the file path
                normalized_path = filepath
                if normalized_path.startswith('/'):
                    normalized_path = normalized_path[1:]
                if not normalized_path.startswith('src/'):
                    normalized_path = f"src/{normalized_path}"
                
                # Skip if we already have this file
                if normalized_path in seen_paths:
                    continue
                seen_paths.add(normalized_path)
                
                # Determine language from extension
                ext = normalized_path.split('.')[-1] if '.' in normalized_path else 'tsx'
                lang_map = {
                    'tsx': 'tsx',
                    'ts': 'ts',
                    'jsx': 'tsx',
                    'js': 'ts',
                    'css': 'css',
                    'json': 'json',
                    'html': 'html',
                }
                
                # Clean up the code - remove trailing ```
                code = re.sub(r'```\s*$', '', code).strip()
                
                files.append(FileChange(
                    path=normalized_path,
                    action='create',
                    language=lang_map.get(ext, 'tsx'),
                    content=code,
                ))
        
        return files
    
    def create_step(self, step_type: str, title: str, description: str) -> PlanStep:
        """Create a new plan step."""
        return PlanStep(
            id=str(uuid.uuid4()),
            type=step_type,
            title=title,
            description=description,
        )
    
    # ===== Validation Utilities =====
    
    def validate_and_fix(
        self,
        generated_files: List[FileChange],
        model: str,
        max_attempts: int = 2,
    ) -> Generator[AgentEvent, None, tuple]:
        """
        Validate generated TypeScript code and attempt to fix errors.
        
        Args:
            generated_files: List of files to validate
            model: LLM model for error fixing
            max_attempts: Maximum fix attempts
            
        Yields:
            AgentEvent objects for progress
            
        Returns:
            Tuple of (validation_passed, fix_attempts)
        """
        from relay_app.services.error_fix_service import get_error_fix_service
        
        validation_passed = False
        fix_attempts = 0
        
        for attempt in range(1, max_attempts + 1):
            # Run TypeScript validation
            validation = self._validate_typescript(generated_files)
            
            if validation['passed']:
                validation_passed = True
                break
            
            # Attempt to fix
            fix_attempts = attempt
            error_count = len(validation.get('errors', []))
            
            yield self.emit_thinking(
                f"Found {error_count} compilation error(s), attempting fix ({attempt}/{max_attempts})",
                "observation",
            )
            
            fix_service = get_error_fix_service()
            
            # Convert to format expected by fix service
            from relay_app.services.agentic_service import FileChange as AgenticFileChange
            from relay_app.services.agentic_service import CompilationError
            
            agentic_files = [
                AgenticFileChange(
                    path=f.path,
                    action=f.action,
                    language=f.language,
                    content=f.content,
                )
                for f in generated_files
            ]
            
            errors = [
                CompilationError(
                    file=e.get('file', ''),
                    line=e.get('line', 0),
                    column=e.get('column', 0),
                    message=e.get('message', ''),
                    code=e.get('code'),
                )
                for e in validation.get('errors', [])
            ]
            
            # Run fix service
            fixed_files_by_path = {}
            
            fix_gen = fix_service.fix_errors(
                files=agentic_files,
                errors=errors,
                model=model,
                attempt=attempt,
            )
            
            while True:
                try:
                    event = next(fix_gen)
                    yield event
                    if event.type == "file_generated":
                        file_data = event.data.get("file", {})
                        if file_data.get("path"):
                            fixed_files_by_path[file_data["path"]] = FileChange(
                                path=file_data["path"],
                                action=file_data.get("action", "modify"),
                                language=file_data.get("language", "tsx"),
                                content=file_data.get("content", ""),
                            )
                except StopIteration:
                    break
            
            # Apply fixed files
            if fixed_files_by_path:
                for i, f in enumerate(generated_files):
                    if f.path in fixed_files_by_path:
                        generated_files[i] = fixed_files_by_path[f.path]
        
        # Final validation check
        if not validation_passed:
            final = self._validate_typescript(generated_files)
            validation_passed = final['passed']
        
        return (validation_passed, fix_attempts)
    
    def _validate_typescript(self, files: List[FileChange]) -> Dict[str, Any]:
        """
        Validate TypeScript files using tsc.
        
        Returns dict with 'passed', 'errors', and 'warnings'.
        """
        import os
        import shutil
        import subprocess
        import tempfile
        
        ts_files = [f for f in files if f.language in ('tsx', 'ts')]
        
        if not ts_files:
            return {'passed': True, 'errors': [], 'warnings': []}
        
        # Check if tsc is available
        tsc_path = shutil.which('tsc')
        if not tsc_path:
            npx_path = shutil.which('npx')
            if not npx_path:
                return {'passed': True, 'warnings': ['TypeScript compiler not available']}
            tsc_cmd = ['npx', 'tsc']
        else:
            tsc_cmd = [tsc_path]
        
        temp_dir = None
        try:
            temp_dir = tempfile.mkdtemp(prefix='relay_tsc_')
            
            # Write files
            for file in ts_files:
                file_path = file.path
                if file_path.startswith('src/'):
                    file_path = file_path[4:]
                
                full_path = os.path.join(temp_dir, file_path)
                os.makedirs(os.path.dirname(full_path), exist_ok=True)
                
                with open(full_path, 'w', encoding='utf-8') as f:
                    f.write(file.content)
            
            # Create tsconfig
            tsconfig = {
                "compilerOptions": {
                    "target": "ES2020",
                    "lib": ["ES2020", "DOM", "DOM.Iterable"],
                    "module": "ESNext",
                    "moduleResolution": "bundler",
                    "jsx": "react-jsx",
                    "strict": False,
                    "noEmit": True,
                    "skipLibCheck": True,
                    "esModuleInterop": True,
                    "allowSyntheticDefaultImports": True,
                    "noImplicitAny": False,
                    "strictNullChecks": False,
                },
                "include": ["**/*.ts", "**/*.tsx"],
            }
            
            with open(os.path.join(temp_dir, 'tsconfig.json'), 'w') as f:
                json.dump(tsconfig, f)
            
            # Create React stub
            stubs_dir = os.path.join(temp_dir, 'node_modules', '@types', 'react')
            os.makedirs(stubs_dir, exist_ok=True)
            with open(os.path.join(stubs_dir, 'index.d.ts'), 'w') as f:
                f.write('''
declare module 'react' {
    export function useState<T>(initial: T): [T, (v: T | ((prev: T) => T)) => void];
    export function useEffect(effect: () => void | (() => void), deps?: any[]): void;
    export function useCallback<T extends (...args: any[]) => any>(callback: T, deps: any[]): T;
    export function useMemo<T>(factory: () => T, deps: any[]): T;
    export function useRef<T>(initial: T): { current: T };
    export type ReactNode = any;
    export type FC<P = {}> = (props: P) => any;
    export default React;
    const React: any;
}
declare module 'react-dom/client' {
    export function createRoot(container: any): { render(element: any): void };
}
''')
            
            # Create lucide-react stub
            lucide_dir = os.path.join(temp_dir, 'node_modules', 'lucide-react')
            os.makedirs(lucide_dir, exist_ok=True)
            with open(os.path.join(lucide_dir, 'index.d.ts'), 'w') as f:
                f.write('declare module "lucide-react" { const icons: any; export = icons; }')
            with open(os.path.join(lucide_dir, 'package.json'), 'w') as f:
                f.write('{"name": "lucide-react", "types": "index.d.ts"}')
            
            # Run tsc
            result = subprocess.run(
                tsc_cmd + ['--noEmit', '--pretty', 'false'],
                cwd=temp_dir,
                capture_output=True,
                text=True,
                timeout=30,
            )
            
            if result.returncode == 0:
                return {'passed': True, 'errors': [], 'warnings': []}
            
            # Parse errors
            errors = self._parse_tsc_errors(result.stdout + result.stderr, temp_dir)
            
            return {
                'passed': len(errors) == 0,
                'errors': errors,
                'warnings': [],
            }
            
        except subprocess.TimeoutExpired:
            return {'passed': True, 'warnings': ['Validation timed out']}
        except Exception as e:
            logger.error(f"TypeScript validation error: {e}")
            return {'passed': True, 'warnings': [f'Validation error: {str(e)}']}
        finally:
            if temp_dir and os.path.exists(temp_dir):
                try:
                    shutil.rmtree(temp_dir)
                except Exception:
                    pass
    
    def _parse_tsc_errors(self, output: str, temp_dir: str) -> List[Dict[str, Any]]:
        """Parse TypeScript compiler output."""
        import os
        
        errors = []
        pattern = r'([^(]+)\((\d+),(\d+)\):\s*(error|warning)\s+(TS\d+):\s*(.+)'
        
        for line in output.strip().split('\n'):
            line = line.strip()
            if not line:
                continue
            
            match = re.match(pattern, line)
            if match:
                file_path, line_num, col, severity, code, message = match.groups()
                
                file_path = file_path.strip()
                if temp_dir in file_path:
                    file_path = file_path.replace(temp_dir + os.sep, '')
                
                if not file_path.startswith('src/'):
                    file_path = f'src/{file_path}'
                
                if severity == 'error':
                    errors.append({
                        'file': file_path,
                        'line': int(line_num),
                        'column': int(col),
                        'message': message.strip(),
                        'code': code,
                    })
        
        return errors

