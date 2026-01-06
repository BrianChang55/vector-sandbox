"""
Generate Handler

Full app generation from scratch - refactored from the original agentic_service.
This handler is used when the user intent is GENERATE_NEW.
"""
import json
import logging
import time
import uuid
from typing import Any, Dict, Generator, List, Optional, TYPE_CHECKING

from .base_handler import BaseHandler, AgentEvent, FileChange, PlanStep

if TYPE_CHECKING:
    from relay_app.models import InternalApp, AppVersion
    from relay_app.services.intent_classifier import IntentResult
    from relay_app.services.context_analyzer import AppContext

logger = logging.getLogger(__name__)


class GenerateHandler(BaseHandler):
    """
    Handler for full app generation from scratch.
    
    This is the default handler used when:
    - No existing app exists
    - User explicitly asks to build/create/generate
    - Intent is classified as GENERATE_NEW
    """
    
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
        Execute full app generation.
        
        Follows the Research → Plan → Execute → Validate flow.
        """
        start_time = time.time()
        generated_files: List[FileChange] = []
        
        # Get additional context
        data_store_context = kwargs.get('data_store_context')
        mcp_tools_context = kwargs.get('mcp_tools_context')
        
        # Import prompts
        from relay_app.prompts.agentic import (
            build_plan_prompt,
            build_step_prompt,
            build_codegen_system_prompt,
            apply_design_style_prompt,
        )
        
        # Apply design style to user message
        styled_user_message = apply_design_style_prompt(
            user_message,
            data_store_context,
            mcp_tools_context,
        )
        
        # ===== PHASE 1: PLANNING =====
        yield self.emit_phase_change("planning", "Creating implementation plan...")
        yield self.emit_thinking("Breaking down the task into executable steps...", "decision")
        
        # Build context for planning
        plan_context = self._build_plan_context(
            app_name=app_name,
            current_spec=current_spec,
            registry_surface=registry_surface,
            context=context,
            mcp_tools_context=mcp_tools_context,
        )
        
        # Generate plan
        plan_steps = self._create_plan(styled_user_message, plan_context, model)
        
        yield self.emit_plan_created(
            steps=plan_steps,
            explored_dirs=3,
            explored_files=len(registry_surface.get("resources", [])) + 5,
            searches=1,
        )
        
        # ===== PHASE 2: EXECUTE =====
        yield self.emit_phase_change("executing", "Building your app...")
        
        for i, step in enumerate(plan_steps):
            step_start = time.time()
            
            yield self.emit_step_started(step, i)
            yield self.emit_step_start(step, i)
            yield self.emit_thinking(step.title, "decision")
            
            try:
                # Execute the step
                for event in self._execute_step(
                    step=step,
                    step_index=i,
                    user_message=styled_user_message,
                    context=plan_context,
                    existing_files=generated_files,
                    registry_surface=registry_surface,
                    model=model,
                    app=app,
                    version=version,
                    data_store_context=data_store_context,
                    mcp_tools_context=mcp_tools_context,
                ):
                    yield event
                    if event.type == "file_generated":
                        file_data = event.data.get("file", {})
                        generated_files.append(FileChange(
                            path=file_data.get("path", ""),
                            action=file_data.get("action", "create"),
                            language=file_data.get("language", "tsx"),
                            content=file_data.get("content", ""),
                        ))
                
                step.status = "complete"
                step.duration = int((time.time() - step_start) * 1000)
                
                yield self.emit_step_completed(step, i, step.duration)
                yield self.emit_step_complete(i, "complete", step.duration)
                
            except Exception as e:
                logger.error(f"Step execution error: {e}")
                step.status = "error"
                yield self.emit_step_complete(i, "error", int((time.time() - step_start) * 1000))
        
        # ===== PHASE 3: VALIDATE =====
        yield self.emit_phase_change("validating", "Validating generated code...")
        
        validation_passed, fix_attempts = yield from self._validate_and_fix(
            generated_files=generated_files,
            model=model,
        )
        
        yield self.emit_validation_result(
            passed=validation_passed,
            fix_attempts=fix_attempts,
        )
        
        return generated_files
    
    def _build_plan_context(
        self,
        app_name: str,
        current_spec: Optional[Dict[str, Any]],
        registry_surface: Dict[str, Any],
        context: 'AppContext',
        mcp_tools_context: Optional[str] = None,
    ) -> Dict[str, Any]:
        """Build context dictionary for planning."""
        resources = registry_surface.get("resources", [])
        
        # Build data store summary
        data_store_summary = ""
        if context.existing_tables:
            table_names = [t.name for t in context.existing_tables]
            data_store_summary = f"Tables: {', '.join(table_names)}"
        
        # Build MCP tools summary
        mcp_summary = ""
        if mcp_tools_context:
            mcp_summary = "MCP integrations available"
        
        return {
            "app_name": app_name,
            "has_existing_spec": current_spec is not None,
            "available_resources": [r.get("resource_id", "") for r in resources],
            "resource_details": resources,
            "data_store_summary": data_store_summary,
            "has_data_store": len(context.existing_tables) > 0,
            "connectors_summary": mcp_summary,
            "has_mcp_tools": bool(mcp_tools_context),
        }
    
    def _create_plan(
        self,
        user_message: str,
        context: Dict[str, Any],
        model: str,
    ) -> List[PlanStep]:
        """Create an execution plan for the app generation."""
        from relay_app.prompts.agentic import build_plan_prompt
        
        plan_prompt = build_plan_prompt(user_message, context)
        
        try:
            response = self.call_llm(
                system_prompt="You are an expert at planning app development tasks.",
                user_prompt=plan_prompt,
                model=model,
                temperature=0.3,
            )
            
            # Parse JSON from response
            import re
            json_match = re.search(r'```(?:json)?\s*(\{.*?\})\s*```', response, re.DOTALL)
            if json_match:
                response = json_match.group(1)
            
            # Clean up content
            response = response.strip()
            if not response.startswith('{'):
                start = response.find('{')
                end = response.rfind('}')
                if start >= 0 and end > start:
                    response = response[start:end+1]
            
            plan_data = json.loads(response)
            
            return [
                PlanStep(
                    id=str(uuid.uuid4()),
                    type=s.get("type", "code"),
                    title=s.get("title", "Generate Code"),
                    description=s.get("description", ""),
                )
                for s in plan_data.get("steps", [])
            ]
            
        except Exception as e:
            logger.error(f"Plan generation error: {e}")
            # Fallback to default plan
            return [
                self.create_step("design", "Design App Structure", 
                                "Plan the component hierarchy and data flow"),
                self.create_step("component", "Create Main Component", 
                                "Build the primary app component"),
                self.create_step("component", "Build UI Components", 
                                "Create reusable UI components"),
                self.create_step("integration", "Connect Data Layer", 
                                "Integrate with the runtime API"),
                self.create_step("styling", "Apply Styling", 
                                "Add professional styling with Tailwind"),
            ]
    
    def _execute_step(
        self,
        step: PlanStep,
        step_index: int,
        user_message: str,
        context: Dict[str, Any],
        existing_files: List[FileChange],
        registry_surface: Dict[str, Any],
        model: str,
        app: Optional['InternalApp'] = None,
        version: Optional['AppVersion'] = None,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
    ) -> Generator[AgentEvent, None, None]:
        """Execute a single plan step and yield progress events."""
        from relay_app.prompts.agentic import build_step_prompt, build_codegen_system_prompt
        
        has_data_store = context.get('has_data_store', False)
        
        # Build prompt for this step
        prompt = build_step_prompt(
            step, step_index, user_message, context,
            existing_files, registry_surface, data_store_context,
            connectors_context=mcp_tools_context,
        )
        
        system_prompt = build_codegen_system_prompt(registry_surface, has_data_store)
        
        try:
            full_content = ""
            chunk_count = 0
            
            for chunk in self.stream_llm_response(
                system_prompt=system_prompt,
                user_prompt=prompt,
                model=model,
                temperature=0.3,
            ):
                full_content += chunk
                chunk_count += 1
                
                # Emit progress periodically
                if chunk_count % 20 == 0:
                    yield self.emit_step_progress(
                        step_index,
                        min(90, chunk_count),
                        "Generating code...",
                    )
            
            # Parse and emit table definitions if app and version provided
            if app and version:
                table_defs = self._parse_table_definitions(full_content)
                if table_defs:
                    yield self.emit_thinking(
                        f"Creating {len(table_defs)} data table(s)...",
                        "decision",
                    )
                    for event in self._apply_table_definitions(table_defs, app, version):
                        yield event
            
            # Parse generated files
            files = self.parse_code_blocks(full_content)
            
            for file in files:
                yield self.emit_file_generated(file)
                
        except Exception as e:
            logger.error(f"Step execution error: {e}")
            yield self.emit_thinking(f"Error during step: {str(e)}", "reflection")
            raise
    
    def _parse_table_definitions(self, content: str) -> List[Dict[str, Any]]:
        """Parse TABLE_DEFINITION blocks from agent output."""
        import re
        
        tables = []
        pattern = r'```table:([a-z0-9-]+)\n(.*?)```'
        matches = re.findall(pattern, content, re.DOTALL | re.IGNORECASE)
        
        for slug, table_content in matches:
            try:
                table_def = self._parse_single_table(slug.strip(), table_content.strip())
                if table_def:
                    tables.append(table_def)
            except Exception as e:
                logger.warning(f"Failed to parse table definition for {slug}: {e}")
        
        return tables
    
    def _parse_single_table(self, slug: str, content: str) -> Optional[Dict[str, Any]]:
        """Parse a single table definition."""
        lines = content.strip().split('\n')
        
        name = slug.replace('-', ' ').title()
        description = ''
        columns = []
        in_columns = False
        
        for line in lines:
            line = line.strip()
            
            if line.startswith('name:'):
                name = line[5:].strip()
            elif line.startswith('description:'):
                description = line[12:].strip()
            elif line.startswith('columns:'):
                in_columns = True
            elif in_columns and line.startswith('- '):
                col = self._parse_column(line[2:].strip())
                if col:
                    columns.append(col)
        
        if not columns:
            return None
        
        return {
            'slug': slug,
            'name': name,
            'description': description,
            'columns': columns,
        }
    
    def _parse_column(self, line: str) -> Optional[Dict[str, Any]]:
        """Parse a column definition line."""
        import re
        
        col = {}
        
        # Extract list values first
        list_pattern = r'(\w+):\s*\[([^\]]+)\]'
        list_matches = re.findall(list_pattern, line)
        for key, value_str in list_matches:
            values = [v.strip().strip('"').strip("'") for v in value_str.split(',')]
            col[key] = values
            line = re.sub(rf'{key}:\s*\[[^\]]+\]', '', line)
        
        # Parse remaining key-value pairs
        parts = [p.strip() for p in line.split(',') if p.strip()]
        
        for part in parts:
            if ':' in part:
                key, value = part.split(':', 1)
                key = key.strip()
                value = value.strip()
                
                if key in col:
                    continue
                
                if value.lower() == 'true':
                    value = True
                elif value.lower() == 'false':
                    value = False
                elif value.isdigit():
                    value = int(value)
                
                col[key] = value
        
        if 'name' not in col or 'type' not in col:
            return None
        
        return col
    
    def _apply_table_definitions(
        self,
        table_defs: List[Dict[str, Any]],
        app: 'InternalApp',
        version: 'AppVersion',
    ) -> Generator[AgentEvent, None, None]:
        """Apply table definitions to the app's data store."""
        from relay_app.models import AppDataTable
        from relay_app.services.app_data_service import AppDataService
        
        for table_def in table_defs:
            slug = table_def['slug']
            
            # Check if table exists
            existing = AppDataTable.objects.filter(
                internal_app=app,
                slug=slug,
            ).first()
            
            if existing:
                logger.info(f"Table {slug} already exists, skipping")
                continue
            
            schema = {'columns': table_def['columns']}
            
            table, errors = AppDataService.create_table_versioned(
                app=app,
                version=version,
                name=table_def['name'],
                schema=schema,
                description=table_def.get('description', ''),
            )
            
            if table:
                yield self.emit_table_created(
                    slug=table.slug,
                    name=table.name,
                    columns=len(table_def['columns']),
                )
            else:
                logger.warning(f"Failed to create table {slug}: {errors}")
    
    def _validate_and_fix(
        self,
        generated_files: List[FileChange],
        model: str,
    ) -> Generator[AgentEvent, None, tuple]:
        """
        Validate generated code and attempt to fix errors.
        
        Returns (validation_passed, fix_attempts)
        """
        from relay_app.services.error_fix_service import get_error_fix_service
        
        MAX_FIX_ATTEMPTS = 2
        validation_passed = False
        fix_attempts = 0
        
        for attempt in range(1, MAX_FIX_ATTEMPTS + 1):
            # Run TypeScript validation
            validation = self._validate_typescript(generated_files)
            
            if validation['passed']:
                validation_passed = True
                break
            
            # Attempt to fix
            fix_attempts = attempt
            error_count = len(validation.get('errors', []))
            
            yield self.emit_thinking(
                f"Found {error_count} compilation error(s), attempting fix ({attempt}/{MAX_FIX_ATTEMPTS})",
                "observation",
            )
            
            fix_service = get_error_fix_service()
            
            # Convert FileChange to the format expected by fix service
            from relay_app.services.agentic_service import FileChange as AgenticFileChange
            
            agentic_files = [
                AgenticFileChange(
                    path=f.path,
                    action=f.action,
                    language=f.language,
                    content=f.content,
                )
                for f in generated_files
            ]
            
            # Create CompilationError objects
            from relay_app.services.agentic_service import CompilationError
            
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
                updated = []
                for f in generated_files:
                    if f.path in fixed_files_by_path:
                        updated.append(fixed_files_by_path[f.path])
                    else:
                        updated.append(f)
                generated_files.clear()
                generated_files.extend(updated)
        
        # Final validation check
        if not validation_passed:
            final = self._validate_typescript(generated_files)
            validation_passed = final['passed']
        
        return (validation_passed, fix_attempts)
    
    def _validate_typescript(self, files: List[FileChange]) -> Dict[str, Any]:
        """
        Validate TypeScript files.
        
        Uses the validation logic from the original agentic service.
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
        import re
        
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

