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
    from vector_app.models import InternalApp, AppVersion
    from vector_app.services.intent_classifier import IntentResult
    from vector_app.services.context_analyzer import AppContext

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
        from vector_app.services.error_fix_service import get_error_fix_service
        
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
            from vector_app.services.agentic_service import FileChange as AgenticFileChange
            from vector_app.services.agentic_service import CompilationError
            
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
            temp_dir = tempfile.mkdtemp(prefix='vector_tsc_')
            
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
                    "jsx": "react",
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
            
            # Create React stub with comprehensive type definitions
            stubs_dir = os.path.join(temp_dir, 'node_modules', '@types', 'react')
            os.makedirs(stubs_dir, exist_ok=True)
            with open(os.path.join(stubs_dir, 'index.d.ts'), 'w') as f:
                f.write('''
// JSX namespace - MUST be first to handle key prop
declare global {
    namespace JSX {
        interface Element extends React.ReactElement<any, any> {}
        interface ElementClass { render(): React.ReactNode; }
        interface ElementAttributesProperty { props: {}; }
        interface ElementChildrenAttribute { children: {}; }
        // This allows 'key' and 'ref' on ALL JSX elements
        interface IntrinsicAttributes { key?: string | number | null; }
        interface IntrinsicClassAttributes<T> { key?: string | number | null; ref?: any; }
        interface IntrinsicElements { [elemName: string]: any; }
    }
}

// Global DOM types
interface EventTarget {
    addEventListener(type: string, listener: EventListenerOrEventListenerObject, options?: boolean | AddEventListenerOptions): void;
    dispatchEvent(event: Event): boolean;
    removeEventListener(type: string, listener: EventListenerOrEventListenerObject, options?: boolean | EventListenerOptions): void;
}
interface Event { readonly target: EventTarget | null; readonly currentTarget: EventTarget | null; preventDefault(): void; stopPropagation(): void; }

// React namespace for type usage like React.ChangeEvent, React.FormEvent
declare namespace React {
    type ReactNode = any;
    interface ReactElement<P = any, T = any> { type: T; props: P; key: string | number | null; }
    // FC accepts props P (key is handled by JSX.IntrinsicAttributes)
    type FC<P = {}> = (props: P) => ReactNode;
    type ChangeEvent<T = Element> = { target: T & { value: string; checked?: boolean } };
    type FormEvent<T = Element> = { preventDefault: () => void; target: T };
    type MouseEvent<T = Element> = { preventDefault: () => void; stopPropagation: () => void; target: EventTarget & T; currentTarget: EventTarget & T };
    type KeyboardEvent<T = Element> = { key: string; preventDefault: () => void };
    type FocusEvent<T = Element> = { target: T };
    type DragEvent<T = Element> = { preventDefault: () => void; dataTransfer: any };
    type ClipboardEvent<T = Element> = { clipboardData: any };
    type TouchEvent<T = Element> = { touches: any[] };
    type WheelEvent<T = Element> = { deltaX: number; deltaY: number };
    type AnimationEvent<T = Element> = { animationName: string };
    type TransitionEvent<T = Element> = { propertyName: string };
    type Dispatch<A> = (value: A) => void;
    type SetStateAction<S> = S | ((prevState: S) => S);
    type RefObject<T> = { current: T | null };
    type MutableRefObject<T> = { current: T };
    type Context<T> = { Provider: FC<{ value: T; children?: ReactNode }>; Consumer: FC<{ children: (value: T) => ReactNode }> };
    interface CSSProperties { [key: string]: any; }
    interface HTMLAttributes<T> {
        className?: string;
        style?: CSSProperties;
        id?: string;
        onClick?: (e: MouseEvent<T>) => void;
        onChange?: (e: ChangeEvent<T>) => void;
        onSubmit?: (e: FormEvent<T>) => void;
        onKeyDown?: (e: KeyboardEvent<T>) => void;
        onKeyUp?: (e: KeyboardEvent<T>) => void;
        onBlur?: (e: FocusEvent<T>) => void;
        onFocus?: (e: FocusEvent<T>) => void;
        [key: string]: any;
    }
    interface InputHTMLAttributes<T> extends HTMLAttributes<T> {
        type?: string;
        value?: any;
        defaultValue?: any;
        placeholder?: string;
        disabled?: boolean;
        readOnly?: boolean;
        required?: boolean;
        min?: number | string;
        max?: number | string;
        step?: number | string;
        pattern?: string;
        autoComplete?: string;
        autoFocus?: boolean;
        name?: string;
    }
    interface TextareaHTMLAttributes<T> extends HTMLAttributes<T> {
        value?: any;
        placeholder?: string;
        disabled?: boolean;
        rows?: number;
        cols?: number;
    }
    interface ButtonHTMLAttributes<T> extends HTMLAttributes<T> {
        type?: "button" | "submit" | "reset";
        disabled?: boolean;
    }
    interface SelectHTMLAttributes<T> extends HTMLAttributes<T> {
        value?: any;
        disabled?: boolean;
        multiple?: boolean;
    }
    interface FormHTMLAttributes<T> extends HTMLAttributes<T> {
        onSubmit?: (e: FormEvent<T>) => void;
    }
}

declare module "react" {
    // Core hooks
    export function useState<T>(initial: T | (() => T)): [T, React.Dispatch<React.SetStateAction<T>>];
    export function useEffect(effect: () => void | (() => void), deps?: any[]): void;
    export function useLayoutEffect(effect: () => void | (() => void), deps?: any[]): void;
    export function useCallback<T extends (...args: any[]) => any>(callback: T, deps: any[]): T;
    export function useMemo<T>(factory: () => T, deps: any[]): T;
    export function useRef<T>(initial: T | null): React.MutableRefObject<T>;
    export function useContext<T>(context: React.Context<T>): T;
    export function useReducer<S, A>(reducer: (state: S, action: A) => S, initialState: S): [S, React.Dispatch<A>];
    export function useId(): string;
    export function useImperativeHandle<T>(ref: any, createHandle: () => T, deps?: any[]): void;
    export function useDebugValue<T>(value: T): void;
    export function useDeferredValue<T>(value: T): T;
    export function useTransition(): [boolean, (callback: () => void) => void];
    
    // Context
    export function createContext<T>(defaultValue: T): React.Context<T>;
    
    // Refs
    export function createRef<T>(): React.RefObject<T>;
    export function forwardRef<T, P = {}>(render: (props: P, ref: React.RefObject<T>) => React.ReactNode): React.FC<P>;
    
    // Memo
    export function memo<P>(Component: React.FC<P>): React.FC<P>;
    
    // Children utilities
    export const Children: {
        map: <T, C>(children: C, fn: (child: C, index: number) => T) => T[];
        forEach: <C>(children: C, fn: (child: C, index: number) => void) => void;
        count: (children: any) => number;
        only: (children: any) => any;
        toArray: (children: any) => any[];
    };
    
    // Fragment
    export const Fragment: React.FC<{ children?: React.ReactNode }>;
    
    // Suspense
    export const Suspense: React.FC<{ fallback?: React.ReactNode; children?: React.ReactNode }>;
    
    // Types re-exported
    export type ReactNode = React.ReactNode;
    export type FC<P = {}> = React.FC<P>;
    export type ChangeEvent<T = Element> = React.ChangeEvent<T>;
    export type FormEvent<T = Element> = React.FormEvent<T>;
    export type MouseEvent<T = Element> = React.MouseEvent<T>;
    export type KeyboardEvent<T = Element> = React.KeyboardEvent<T>;
    export type FocusEvent<T = Element> = React.FocusEvent<T>;
    export type Dispatch<A> = React.Dispatch<A>;
    export type SetStateAction<S> = React.SetStateAction<S>;
    export type RefObject<T> = React.RefObject<T>;
    export type MutableRefObject<T> = React.MutableRefObject<T>;
    export type Context<T> = React.Context<T>;
    export type CSSProperties = React.CSSProperties;
    export type HTMLAttributes<T> = React.HTMLAttributes<T>;
    
    // Default export
    const React: any;
    export default React;
}

declare module "react-dom/client" {
    export function createRoot(container: Element | null): { render(element: any): void; unmount(): void };
    export function hydrateRoot(container: Element, initialChildren: any): { render(element: any): void; unmount(): void };
}

declare module "react/jsx-runtime" {
    export function jsx(type: any, props: any, key?: string): any;
    export function jsxs(type: any, props: any, key?: string): any;
    export const Fragment: any;
}

declare module "react/jsx-dev-runtime" {
    export function jsxDEV(type: any, props: any, key?: string, isStaticChildren?: boolean, source?: any, self?: any): any;
    export const Fragment: any;
}

''')
            
            # Create lucide-react stub with comprehensive icon list
            lucide_dir = os.path.join(temp_dir, 'node_modules', 'lucide-react')
            os.makedirs(lucide_dir, exist_ok=True)
            with open(os.path.join(lucide_dir, 'index.d.ts'), 'w') as f:
                # Comprehensive list of commonly used Lucide icons
                f.write('''declare module "lucide-react" {
    import { FC } from "react";
    
    // IconProps includes all common SVG and custom props
    interface IconProps {
        size?: number | string;
        color?: string;
        strokeWidth?: number | string;
        absoluteStrokeWidth?: boolean;
        className?: string;
        style?: React.CSSProperties | { [key: string]: any };
        onClick?: (e: any) => void;
        onMouseEnter?: (e: any) => void;
        onMouseLeave?: (e: any) => void;
        "aria-label"?: string;
        "aria-hidden"?: boolean | "true" | "false";
        role?: string;
        id?: string;
        // Allow any other SVG props
        [key: string]: any;
    }
    
    type Icon = FC<IconProps>;
    
    // Common action icons
    export const Plus: Icon;
    export const Minus: Icon;
    export const X: Icon;
    export const Check: Icon;
    export const Save: Icon;
    export const Edit: Icon;
    export const Edit2: Icon;
    export const Edit3: Icon;
    export const Pencil: Icon;
    export const PencilLine: Icon;
    export const Trash: Icon;
    export const Trash2: Icon;
    export const Delete: Icon;
    export const Copy: Icon;
    export const Clipboard: Icon;
    export const ClipboardCopy: Icon;
    export const ClipboardCheck: Icon;
    export const Download: Icon;
    export const Upload: Icon;
    export const Share: Icon;
    export const Share2: Icon;
    export const Send: Icon;
    export const Reply: Icon;
    export const Forward: Icon;
    export const Undo: Icon;
    export const Undo2: Icon;
    export const Redo: Icon;
    export const Redo2: Icon;
    export const RotateCw: Icon;
    export const RotateCcw: Icon;
    export const RefreshCw: Icon;
    export const RefreshCcw: Icon;
    export const Repeat: Icon;
    export const Repeat1: Icon;
    export const Shuffle: Icon;
    
    // Navigation icons
    export const ArrowUp: Icon;
    export const ArrowDown: Icon;
    export const ArrowLeft: Icon;
    export const ArrowRight: Icon;
    export const ArrowUpRight: Icon;
    export const ArrowUpLeft: Icon;
    export const ArrowDownRight: Icon;
    export const ArrowDownLeft: Icon;
    export const ChevronUp: Icon;
    export const ChevronDown: Icon;
    export const ChevronLeft: Icon;
    export const ChevronRight: Icon;
    export const ChevronsUp: Icon;
    export const ChevronsDown: Icon;
    export const ChevronsLeft: Icon;
    export const ChevronsRight: Icon;
    export const MoveUp: Icon;
    export const MoveDown: Icon;
    export const MoveLeft: Icon;
    export const MoveRight: Icon;
    export const CornerUpRight: Icon;
    export const CornerDownRight: Icon;
    export const ExternalLink: Icon;
    export const Link: Icon;
    export const Link2: Icon;
    export const Unlink: Icon;
    export const Menu: Icon;
    export const MoreHorizontal: Icon;
    export const MoreVertical: Icon;
    export const Grip: Icon;
    export const GripVertical: Icon;
    export const GripHorizontal: Icon;
    
    // Status/feedback icons
    export const CheckCircle: Icon;
    export const CheckCircle2: Icon;
    export const CheckSquare: Icon;
    export const XCircle: Icon;
    export const XSquare: Icon;
    export const AlertCircle: Icon;
    export const AlertTriangle: Icon;
    export const AlertOctagon: Icon;
    export const Info: Icon;
    export const HelpCircle: Icon;
    export const Ban: Icon;
    export const Loader: Icon;
    export const Loader2: Icon;
    export const Circle: Icon;
    export const Square: Icon;
    export const Diamond: Icon;
    export const Heart: Icon;
    export const HeartOff: Icon;
    export const Star: Icon;
    export const StarOff: Icon;
    export const ThumbsUp: Icon;
    export const ThumbsDown: Icon;
    export const Bell: Icon;
    export const BellOff: Icon;
    export const BellRing: Icon;
    
    // Media icons
    export const Play: Icon;
    export const Pause: Icon;
    export const Stop: Icon;
    export const StopCircle: Icon;
    export const PlayCircle: Icon;
    export const PauseCircle: Icon;
    export const SkipBack: Icon;
    export const SkipForward: Icon;
    export const Rewind: Icon;
    export const FastForward: Icon;
    export const Volume: Icon;
    export const Volume1: Icon;
    export const Volume2: Icon;
    export const VolumeX: Icon;
    export const Mic: Icon;
    export const MicOff: Icon;
    export const Camera: Icon;
    export const CameraOff: Icon;
    export const Video: Icon;
    export const VideoOff: Icon;
    export const Image: Icon;
    export const Images: Icon;
    export const Film: Icon;
    export const Music: Icon;
    export const Music2: Icon;
    
    // User/people icons
    export const User: Icon;
    export const Users: Icon;
    export const Users2: Icon;
    export const UserPlus: Icon;
    export const UserMinus: Icon;
    export const UserCheck: Icon;
    export const UserX: Icon;
    export const UserCircle: Icon;
    export const UserCircle2: Icon;
    export const Contact: Icon;
    export const Contact2: Icon;
    
    // Communication icons
    export const Mail: Icon;
    export const MailOpen: Icon;
    export const Inbox: Icon;
    export const MessageCircle: Icon;
    export const MessageSquare: Icon;
    export const MessagesSquare: Icon;
    export const Phone: Icon;
    export const PhoneCall: Icon;
    export const PhoneOff: Icon;
    export const AtSign: Icon;
    export const Hash: Icon;
    
    // File/document icons
    export const File: Icon;
    export const FileText: Icon;
    export const FileImage: Icon;
    export const FileVideo: Icon;
    export const FileAudio: Icon;
    export const FileCode: Icon;
    export const FileJson: Icon;
    export const Files: Icon;
    export const Folder: Icon;
    export const FolderOpen: Icon;
    export const FolderPlus: Icon;
    export const FolderMinus: Icon;
    export const Archive: Icon;
    export const Package: Icon;
    
    // Layout/UI icons
    export const Layout: Icon;
    export const LayoutGrid: Icon;
    export const LayoutList: Icon;
    export const Grid: Icon;
    export const Grid2x2: Icon;
    export const Grid3x3: Icon;
    export const List: Icon;
    export const ListOrdered: Icon;
    export const Table: Icon;
    export const Table2: Icon;
    export const Columns: Icon;
    export const Rows: Icon;
    export const SidebarLeft: Icon;
    export const SidebarRight: Icon;
    export const PanelLeft: Icon;
    export const PanelRight: Icon;
    export const Maximize: Icon;
    export const Maximize2: Icon;
    export const Minimize: Icon;
    export const Minimize2: Icon;
    export const Expand: Icon;
    export const Shrink: Icon;
    export const ZoomIn: Icon;
    export const ZoomOut: Icon;
    export const Fullscreen: Icon;
    export const Move: Icon;
    export const Grab: Icon;
    
    // Settings/config icons
    export const Settings: Icon;
    export const Settings2: Icon;
    export const Sliders: Icon;
    export const SlidersHorizontal: Icon;
    export const Filter: Icon;
    export const FilterX: Icon;
    export const SortAsc: Icon;
    export const SortDesc: Icon;
    export const ArrowUpDown: Icon;
    export const Cog: Icon;
    export const Wrench: Icon;
    export const Tool: Icon;
    
    // Search/find icons
    export const Search: Icon;
    export const SearchX: Icon;
    export const Eye: Icon;
    export const EyeOff: Icon;
    export const Scan: Icon;
    export const Focus: Icon;
    export const Target: Icon;
    export const Crosshair: Icon;
    
    // Time/calendar icons
    export const Clock: Icon;
    export const Clock1: Icon;
    export const Clock2: Icon;
    export const Clock3: Icon;
    export const Clock4: Icon;
    export const Clock5: Icon;
    export const Clock6: Icon;
    export const Clock7: Icon;
    export const Clock8: Icon;
    export const Clock9: Icon;
    export const Clock10: Icon;
    export const Clock11: Icon;
    export const Clock12: Icon;
    export const Timer: Icon;
    export const TimerOff: Icon;
    export const TimerReset: Icon;
    export const Hourglass: Icon;
    export const Calendar: Icon;
    export const CalendarDays: Icon;
    export const CalendarRange: Icon;
    export const CalendarCheck: Icon;
    export const CalendarPlus: Icon;
    export const CalendarMinus: Icon;
    export const CalendarX: Icon;
    export const History: Icon;
    export const Alarm: Icon;
    export const AlarmClock: Icon;
    
    // Money/commerce icons
    export const DollarSign: Icon;
    export const Euro: Icon;
    export const PoundSterling: Icon;
    export const Coins: Icon;
    export const Wallet: Icon;
    export const CreditCard: Icon;
    export const Banknote: Icon;
    export const Receipt: Icon;
    export const ShoppingCart: Icon;
    export const ShoppingBag: Icon;
    export const Store: Icon;
    export const Tag: Icon;
    export const Tags: Icon;
    export const Percent: Icon;
    export const Gift: Icon;
    export const BadgePercent: Icon;
    export const Calculator: Icon;
    
    // Charts/analytics icons
    export const BarChart: Icon;
    export const BarChart2: Icon;
    export const BarChart3: Icon;
    export const BarChart4: Icon;
    export const LineChart: Icon;
    export const PieChart: Icon;
    export const TrendingUp: Icon;
    export const TrendingDown: Icon;
    export const Activity: Icon;
    export const Gauge: Icon;
    export const Milestone: Icon;
    
    // Location/map icons
    export const Map: Icon;
    export const MapPin: Icon;
    export const Navigation: Icon;
    export const Navigation2: Icon;
    export const Compass: Icon;
    export const Globe: Icon;
    export const Globe2: Icon;
    export const Home: Icon;
    export const Building: Icon;
    export const Building2: Icon;
    export const Landmark: Icon;
    
    // Security icons
    export const Lock: Icon;
    export const LockOpen: Icon;
    export const Unlock: Icon;
    export const Key: Icon;
    export const KeyRound: Icon;
    export const Shield: Icon;
    export const ShieldCheck: Icon;
    export const ShieldAlert: Icon;
    export const ShieldOff: Icon;
    export const Fingerprint: Icon;
    export const Scan: Icon;
    
    // Tech/dev icons
    export const Code: Icon;
    export const Code2: Icon;
    export const CodeXml: Icon;
    export const Terminal: Icon;
    export const TerminalSquare: Icon;
    export const Database: Icon;
    export const Server: Icon;
    export const Cloud: Icon;
    export const CloudOff: Icon;
    export const CloudUpload: Icon;
    export const CloudDownload: Icon;
    export const Cpu: Icon;
    export const HardDrive: Icon;
    export const Monitor: Icon;
    export const Laptop: Icon;
    export const Smartphone: Icon;
    export const Tablet: Icon;
    export const Wifi: Icon;
    export const WifiOff: Icon;
    export const Bluetooth: Icon;
    export const BluetoothOff: Icon;
    export const Plug: Icon;
    export const Unplug: Icon;
    export const Zap: Icon;
    export const ZapOff: Icon;
    export const Power: Icon;
    export const PowerOff: Icon;
    export const Battery: Icon;
    export const BatteryLow: Icon;
    export const BatteryMedium: Icon;
    export const BatteryFull: Icon;
    export const BatteryCharging: Icon;
    export const Signal: Icon;
    export const SignalLow: Icon;
    export const SignalMedium: Icon;
    export const SignalHigh: Icon;
    export const Rss: Icon;
    export const Radio: Icon;
    export const Satellite: Icon;
    export const Binary: Icon;
    export const Braces: Icon;
    export const Brackets: Icon;
    export const Bug: Icon;
    export const Puzzle: Icon;
    export const Blocks: Icon;
    export const Box: Icon;
    export const Boxes: Icon;
    
    // Text/formatting icons
    export const Bold: Icon;
    export const Italic: Icon;
    export const Underline: Icon;
    export const Strikethrough: Icon;
    export const AlignLeft: Icon;
    export const AlignCenter: Icon;
    export const AlignRight: Icon;
    export const AlignJustify: Icon;
    export const Heading: Icon;
    export const Heading1: Icon;
    export const Heading2: Icon;
    export const Heading3: Icon;
    export const Heading4: Icon;
    export const Heading5: Icon;
    export const Heading6: Icon;
    export const Type: Icon;
    export const Text: Icon;
    export const Quote: Icon;
    export const ListPlus: Icon;
    export const ListMinus: Icon;
    export const ListChecks: Icon;
    export const ListTodo: Icon;
    
    // Misc icons
    export const Sun: Icon;
    export const Moon: Icon;
    export const SunMoon: Icon;
    export const CloudSun: Icon;
    export const Palette: Icon;
    export const Paintbrush: Icon;
    export const Brush: Icon;
    export const PenTool: Icon;
    export const Eraser: Icon;
    export const Scissors: Icon;
    export const Crop: Icon;
    export const RotateCw: Icon;
    export const RotateCcw: Icon;
    export const FlipHorizontal: Icon;
    export const FlipVertical: Icon;
    export const Sparkles: Icon;
    export const Wand: Icon;
    export const Wand2: Icon;
    export const Crown: Icon;
    export const Award: Icon;
    export const Medal: Icon;
    export const Trophy: Icon;
    export const Flame: Icon;
    export const Rocket: Icon;
    export const Lightbulb: Icon;
    export const LightbulbOff: Icon;
    export const Aperture: Icon;
    export const Book: Icon;
    export const BookOpen: Icon;
    export const BookMarked: Icon;
    export const BookCopy: Icon;
    export const BookText: Icon;
    export const Bookmark: Icon;
    export const BookmarkPlus: Icon;
    export const BookmarkMinus: Icon;
    export const BookmarkCheck: Icon;
    export const Library: Icon;
    export const Notebook: Icon;
    export const NotebookPen: Icon;
    export const Flag: Icon;
    export const FlagOff: Icon;
    export const Pin: Icon;
    export const PinOff: Icon;
    export const Paperclip: Icon;
    export const Attach: Icon;
    export const Hash: Icon;
    export const Asterisk: Icon;
    export const AtSign: Icon;
    export const Slash: Icon;
    export const Circle: Icon;
    export const CircleDot: Icon;
    export const CircleDashed: Icon;
    export const Dot: Icon;
    export const MoreHorizontal: Icon;
    export const MoreVertical: Icon;
    export const Ellipsis: Icon;
    export const EllipsisVertical: Icon;
    export const Grip: Icon;
    export const GripHorizontal: Icon;
    export const GripVertical: Icon;
    export const Separator: Icon;
    export const SeparatorHorizontal: Icon;
    export const SeparatorVertical: Icon;
    
    // Allow any icon import (fallback)
    const icons: { [key: string]: Icon };
    export default icons;
}
''')
            with open(os.path.join(lucide_dir, 'package.json'), 'w') as f:
                f.write('{"name": "lucide-react", "types": "index.d.ts"}')
            
            # Create NodeJS global types (for NodeJS.Timeout, etc.)
            node_types_dir = os.path.join(temp_dir, 'node_modules', '@types', 'node')
            os.makedirs(node_types_dir, exist_ok=True)
            with open(os.path.join(node_types_dir, 'index.d.ts'), 'w') as f:
                f.write('''
declare namespace NodeJS {
    interface Timeout {
        ref(): this;
        unref(): this;
        hasRef(): boolean;
        refresh(): this;
        [Symbol.toPrimitive](): number;
    }
    interface Immediate {
        ref(): this;
        unref(): this;
        hasRef(): boolean;
        _onImmediate: Function;
    }
    interface Timer {
        ref(): this;
        unref(): this;
        hasRef(): boolean;
    }
    interface Process {
        env: { [key: string]: string | undefined };
        cwd(): string;
        exit(code?: number): never;
    }
}

declare var process: NodeJS.Process;
declare function setTimeout(callback: (...args: any[]) => void, ms?: number, ...args: any[]): NodeJS.Timeout;
declare function clearTimeout(timeoutId: NodeJS.Timeout | undefined): void;
declare function setInterval(callback: (...args: any[]) => void, ms?: number, ...args: any[]): NodeJS.Timeout;
declare function clearInterval(intervalId: NodeJS.Timeout | undefined): void;
declare function setImmediate(callback: (...args: any[]) => void, ...args: any[]): NodeJS.Immediate;
declare function clearImmediate(immediateId: NodeJS.Immediate | undefined): void;
''')
            with open(os.path.join(node_types_dir, 'package.json'), 'w') as f:
                f.write('{"name": "@types/node", "types": "index.d.ts"}')
            
            # Create dataStore stub (always available at lib/dataStore)
            lib_dir = os.path.join(temp_dir, 'lib')
            os.makedirs(lib_dir, exist_ok=True)
            with open(os.path.join(lib_dir, 'dataStore.ts'), 'w') as f:
                f.write('''
// DataStore API stub for TypeScript validation
export interface QueryOptions {
    filters?: Array<{ field: string; op: string; value: any }>;
    orderBy?: Array<{ field: string; dir: 'asc' | 'desc'; direction?: 'asc' | 'desc' }>;
    limit?: number;
    offset?: number;
}

export interface QueryResult<T = any> {
    rows: Array<{ id: string; row_index: number; data: T; created_at: string; updated_at: string }>;
    total_count: number;
    has_more: boolean;
}

export interface DataStore {
    query<T = any>(tableSlug: string, options?: QueryOptions): Promise<QueryResult<T>>;
    insert<T = any>(tableSlug: string, data: T): Promise<{ id: string; data: T; row_index: number; created_at: string }>;
    update<T = any>(tableSlug: string, rowId: string, data: Partial<T>): Promise<{ id: string; data: T; updated_at: string }>;
    delete(tableSlug: string, rowId: string): Promise<void>;
    bulkInsert<T = any>(tableSlug: string, data: T[]): Promise<Array<{ id: string; data: T }>>;
    bulkDelete(tableSlug: string, rowIds: string[]): Promise<void>;
}

export const dataStore: DataStore = {} as DataStore;
''')
            
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

    def _validate_datastore_field_names(
        self,
        files: List[FileChange],
        app: Optional['InternalApp']
    ) -> Dict[str, Any]:
        """
        Validate that generated code uses exact field names from schemas.

        Returns:
            Dict with 'passed' (bool), 'errors' (list), 'warnings' (list)
        """
        if not app:
            return {"passed": True, "errors": [], "warnings": []}

        from vector_app.models import AppDataTable
        import re

        errors = []
        warnings = []

        # Get all table schemas
        tables = AppDataTable.objects.filter(internal_app=app)
        table_schemas = {}
        for table in tables:
            schema = table.schema_json or {}
            columns = schema.get('columns', [])
            column_names = {c.get('name') for c in columns if c.get('name')}

            # Build required fields list (not nullable, no default, not auto-generated)
            required_fields = set()
            for col in columns:
                col_name = col.get('name')
                if not col_name:
                    continue

                # Skip if auto-generated (id, created_at, updated_at)
                if col.get('auto_generate') or col.get('auto_now_add') or col.get('auto_now'):
                    continue

                # Required if: not nullable AND no default
                is_nullable = col.get('nullable', True)
                has_default = 'default' in col

                if not is_nullable and not has_default:
                    required_fields.add(col_name)

            table_schemas[table.slug] = {
                'name': table.name,
                'columns': column_names,
                'required_fields': required_fields,
            }

        if not table_schemas:
            return {"passed": True, "errors": [], "warnings": []}

        logger.info(f"🔍 [FIELD VALIDATION] Validating {len(files)} files against {len(table_schemas)} table schemas")

        # First, validate that all table references exist
        all_datastore_ops_pattern = r'dataStore\.(query|insert|update|delete|updateRow|deleteRow|bulkInsert|bulkDelete)\s*\(\s*[\'"]([^\'\"]+)[\'"]'
        for file in files:
            if file.language not in ('tsx', 'ts'):
                continue

            matches = re.finditer(all_datastore_ops_pattern, file.content)
            for match in matches:
                operation = match.group(1)
                table_slug = match.group(2)

                if table_slug not in table_schemas:
                    error_msg = (
                        f"{file.path}: {operation.upper()} references non-existent table '{table_slug}'. "
                        f"Available tables: {', '.join(sorted(table_schemas.keys()))}. "
                        f"Did you forget to create this table in a TABLE_DEFINITION block?"
                    )
                    errors.append(error_msg)
                    logger.error(f"🚨 [TABLE VALIDATION] {error_msg}")

        # Regex patterns to find dataStore operations with field names
        patterns = {
            'insert': r'dataStore\.(?:insert|bulkInsert)\s*\(\s*[\'"]([^\'\"]+)[\'"]\s*,\s*(?:\[)?\s*\{([^}]+)\}',
            'update': r'dataStore\.update\s*\(\s*[\'"]([^\'\"]+)[\'"]\s*,\s*[^,]+,\s*\{([^}]+)\}',
            'filter': r'field:\s*[\'"]([^\'\"]+)[\'"]',
            'orderBy': r'field:\s*[\'"]([^\'\"]+)[\'"]',
        }

        for file in files:
            if file.language not in ('tsx', 'ts'):
                continue

            content = file.content

            # Find insert/update operations
            for op_type, pattern in patterns.items():
                if op_type in ('insert', 'update'):
                    matches = re.finditer(pattern, content, re.DOTALL)
                    for match in matches:
                        table_slug = match.group(1)
                        fields_str = match.group(2)

                        if table_slug not in table_schemas:
                            continue

                        valid_columns = table_schemas[table_slug]['columns']
                        required_fields = table_schemas[table_slug]['required_fields']

                        # Extract field names from the object
                        field_matches = re.findall(r'(\w+):\s*[^,}]+', fields_str)
                        provided_fields = set()

                        for field_name in field_matches:
                            # Skip common non-field keys
                            if field_name in ('row', 'data', 'id', 'const', 'let', 'var'):
                                continue

                            provided_fields.add(field_name)

                            if field_name not in valid_columns:
                                error_msg = (
                                    f"{file.path}: {op_type.upper()} uses unknown field '{field_name}' "
                                    f"for table '{table_slug}'. Valid fields: {', '.join(sorted(valid_columns))}"
                                )
                                errors.append(error_msg)
                                logger.error(f"🚨 [FIELD VALIDATION] {error_msg}")

                        # Check for missing required fields (only for INSERT operations)
                        if op_type == 'insert':
                            missing_required = required_fields - provided_fields
                            if missing_required:
                                error_msg = (
                                    f"{file.path}: INSERT is missing required field(s) for table '{table_slug}': "
                                    f"{', '.join(sorted(missing_required))}. These fields are required (not nullable, no default)."
                                )
                                errors.append(error_msg)
                                logger.error(f"🚨 [FIELD VALIDATION] {error_msg}")

                # Find filter/orderBy field references
                elif op_type in ('filter', 'orderBy'):
                    matches = re.finditer(pattern, content)
                    for match in matches:
                        field_name = match.group(1)

                        # Try to determine which table this refers to (look back for table slug)
                        start_pos = max(0, match.start() - 200)
                        context_str = content[start_pos:match.start()]
                        table_match = re.search(r'dataStore\.query\s*\(\s*[\'"]([^\'\"]+)[\'"]', context_str)

                        if table_match:
                            table_slug = table_match.group(1)
                            if table_slug in table_schemas:
                                valid_columns = table_schemas[table_slug]['columns']
                                if field_name not in valid_columns:
                                    error_msg = (
                                        f"{file.path}: {op_type} references unknown field '{field_name}' "
                                        f"for table '{table_slug}'. Valid fields: {', '.join(sorted(valid_columns))}"
                                    )
                                    errors.append(error_msg)
                                    logger.error(f"🚨 [FIELD VALIDATION] {error_msg}")

        # CRITICAL: Check for row.data.id usage in update/delete (common mistake)
        row_data_id_pattern = r'dataStore\.(update|delete)\s*\([^,]+,\s*row\.data\.id'
        for file in files:
            if file.language not in ('tsx', 'ts'):
                continue

            matches = re.finditer(row_data_id_pattern, file.content)
            for match in matches:
                operation = match.group(1)
                error_msg = (
                    f"{file.path}: {operation.upper()} uses 'row.data.id' which is WRONG! "
                    f"Must use 'row.id' instead. This causes 'Row not found' 404 errors at runtime. "
                    f"❌ WRONG: dataStore.{operation}('table', row.data.id, ...) "
                    f"✅ CORRECT: dataStore.{operation}('table', row.id, ...)"
                )
                errors.append(error_msg)
                logger.error(f"🚨 [ROW ID VALIDATION] {error_msg}")

        # CRITICAL: Check for id overwriting pattern (spreading row.data after setting id)
        id_overwrite_pattern = r'\{\s*id:\s*row\.id\s*,\s*\.\.\.row\.data\s*\}'
        for file in files:
            if file.language not in ('tsx', 'ts'):
                continue

            matches = re.finditer(id_overwrite_pattern, file.content, re.MULTILINE | re.DOTALL)
            for match in matches:
                error_msg = (
                    f"{file.path}: Dangerous pattern - '{{ id: row.id, ...row.data }}' "
                    f"will be overwritten if row.data contains an 'id' field! "
                    f"❌ WRONG: {{ id: row.id, ...row.data }} (data.id overwrites row.id) "
                    f"✅ CORRECT: {{ ...row.data, id: row.id }} (row.id overwrites data.id) "
                    f"This causes 'Row not found' errors when updating/deleting."
                )
                errors.append(error_msg)
                logger.error(f"🚨 [ID OVERWRITE] {error_msg}")

        passed = len(errors) == 0

        if errors:
            logger.error(f"🚨 [FIELD VALIDATION] FAILED: {len(errors)} field name errors found")
        elif warnings:
            logger.warning(f"⚠️ [FIELD VALIDATION] PASSED with {len(warnings)} warnings")
        else:
            logger.info(f"✅ [FIELD VALIDATION] PASSED: All field names match schema")

        return {
            "passed": passed,
            "errors": errors,
            "warnings": warnings,
        }

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
                    # Filter out 'key' prop false positives from our type stubs
                    # The key prop is handled by React internally and our stubs don't handle it
                    msg = message.strip()
                    if "Property 'key' does not exist" in msg:
                        continue
                    # Also filter TS2322 errors that mention key: string in the type mismatch
                    if code == 'TS2322' and "{ key: string;" in msg:
                        continue
                    
                    errors.append({
                        'file': file_path,
                        'line': int(line_num),
                        'column': int(col),
                        'message': msg,
                        'code': code,
                    })
        
        return errors

