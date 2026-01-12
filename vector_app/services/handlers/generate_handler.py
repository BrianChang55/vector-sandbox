"""
Generate Handler

Full app generation from scratch - refactored from the original agentic_service.
This handler is used when the user intent is GENERATE_NEW.

OPTIMIZED: Uses parallel execution for independent plan steps when possible.
"""
import json
import logging
import time
import uuid
from typing import Any, Dict, Generator, List, Optional, Union, TYPE_CHECKING

from .base_handler import BaseHandler, AgentEvent, FileChange, PlanStep
from .parallel_executor import create_parallel_executor, ParallelStepExecutor
from vector_app.models import AppDataTable
from vector_app.services.app_data_service import AppDataService
from vector_app.services.typescript_types_generator import generate_typescript_types

if TYPE_CHECKING:
    from vector_app.models import InternalApp, AppVersion
    from vector_app.services.intent_classifier import IntentResult
    from vector_app.services.context_analyzer import AppContext

logger = logging.getLogger(__name__)


class GenerateHandler(BaseHandler):
    """
    Handler for full app generation from scratch.
    
    This is the default handler used when:
    - No existing app exists
    - User explicitly asks to build/create/generate
    - Intent is classified as GENERATE_NEW
    
    OPTIMIZED: Uses parallel execution for independent plan steps.
    """
    
    def __init__(self):
        super().__init__()
        # Create parallel executor for step execution (max 5 concurrent steps)
        self._parallel_executor: Optional[ParallelStepExecutor] = None
    
    @property
    def parallel_executor(self) -> ParallelStepExecutor:
        """Lazy-load the parallel executor."""
        if self._parallel_executor is None:
            self._parallel_executor = create_parallel_executor(max_workers=5)
        return self._parallel_executor
    
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
        
        # Debug log the context being passed to LLM
        self.log_llm_context(
            intent_name=intent.intent.value if intent else "GENERATE_NEW",
            handler_name="GenerateHandler",
            data_store_context=data_store_context,
            mcp_tools_context=mcp_tools_context,
        )
        
        # Import prompts
        from vector_app.prompts.agentic import (
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
        
        # Use parallel execution for independent steps
        generated_files = yield from self._execute_steps_parallel(
            plan_steps=plan_steps,
            user_message=styled_user_message,
            context=plan_context,
            registry_surface=registry_surface,
            model=model,
            app=app,
            version=version,
            data_store_context=data_store_context,
            mcp_tools_context=mcp_tools_context,
        )
        
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
    
    def _execute_steps_parallel(
        self,
        plan_steps: List[PlanStep],
        user_message: str,
        context: Dict[str, Any],
        registry_surface: Dict[str, Any],
        model: str,
        app: Optional['InternalApp'] = None,
        version: Optional['AppVersion'] = None,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """
        Execute plan steps with parallel execution for independent steps.
        
        Uses the ParallelStepExecutor to analyze dependencies and execute
        independent steps concurrently while maintaining event ordering.
        
        Args:
            plan_steps: List of steps to execute
            user_message: The styled user message
            context: Plan context dictionary
            registry_surface: Available resources
            model: LLM model to use
            app: Optional InternalApp for data store
            version: Optional AppVersion for versioned operations
            data_store_context: Data store context string
            mcp_tools_context: MCP tools context string
            
        Yields:
            AgentEvent objects in proper order
            
        Returns:
            List of all generated files
        """
        # Create a step executor closure that captures the context
        def step_executor(
            step: PlanStep,
            step_index: int,
            existing_files: List[FileChange],
        ) -> Generator[AgentEvent, None, None]:
            """Execute a single step with full context."""
            step_start = time.time()
            
            # Emit step start events
            yield self.emit_step_started(step, step_index)
            yield self.emit_step_start(step, step_index)
            yield self.emit_thinking(step.title, "decision")
            
            try:
                # Execute the step
                for event in self._execute_step(
                    step=step,
                    step_index=step_index,
                    user_message=user_message,
                    context=context,
                    existing_files=existing_files,
                    registry_surface=registry_surface,
                    model=model,
                    app=app,
                    version=version,
                    data_store_context=data_store_context,
                    mcp_tools_context=mcp_tools_context,
                ):
                    yield event
                
                step.status = "complete"
                step.duration = int((time.time() - step_start) * 1000)
                
                yield self.emit_step_completed(step, step_index, step.duration)
                yield self.emit_step_complete(step_index, "complete", step.duration)
                
            except Exception as e:
                logger.error(f"Step execution error: {e}")
                step.status = "error"
                step.duration = int((time.time() - step_start) * 1000)
                yield self.emit_step_complete(step_index, "error", step.duration)
                raise
        
        # Use the parallel executor to run steps
        generated_files = yield from self.parallel_executor.execute_steps(
            steps=plan_steps,
            step_executor=step_executor,
            initial_files=[],
        )
        
        return generated_files
    
    def _create_plan(
        self,
        user_message: str,
        context: Dict[str, Any],
        model: str,
    ) -> List[PlanStep]:
        """Create an execution plan for the app generation."""
        from vector_app.prompts.agentic import build_plan_prompt
        
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
    
    def _stream_llm_with_validation(
        self,
        system_prompt: str,
        user_prompt: str,
        model: str,
        step_index: int
    ) -> Generator[tuple, None, str]:
        """Stream LLM response with real-time validation. Yields (warnings, content_so_far), returns full_content."""
        validator = self.create_streaming_validator()
        full_content = ""
        chunk_count = 0

        for chunk in self.stream_llm_response(
            system_prompt=system_prompt,
            user_prompt=user_prompt,
            model=model,
            temperature=0.3,
        ):
            full_content += chunk
            chunk_count += 1

            # Real-time validation during streaming
            streaming_warnings = validator.check_chunk(chunk, full_content)
            if streaming_warnings:
                yield (streaming_warnings, full_content)

            # Emit progress periodically
            if chunk_count % 20 == 0:
                yield ([], full_content)  # Signal progress without warnings

        # Final validation check
        final_warnings = validator.final_check(full_content)
        if final_warnings:
            yield (final_warnings, full_content)

        return full_content

    def _handle_table_creation(
        self,
        full_content: str,
        app: Optional['InternalApp'],
        version: Optional['AppVersion']
    ) -> Generator[Union[AgentEvent, bool], None, None]:
        """
        Handle table creation from generated content.
        Yields events, and yields True as final item if tables were created (signals early return).
        """
        if not (app and version):
            return

        table_defs = self._parse_table_definitions(full_content)
        if not table_defs:
            return

        yield self.emit_thinking(
            f"Creating {len(table_defs)} data table(s)...",
            "decision",
        )
        for event in self._apply_table_definitions(table_defs, app, version):
            yield event

        # Generate TypeScript types file for new app
        types_file = self._generate_types_file(app)
        if types_file:
            yield self.emit_file_generated(types_file)

        # Early return after table creation (table/code separation)
        logger.info(f"🚫 [TABLE/CODE SEPARATION] Tables created - returning early")
        yield self.emit_thinking(
            "Data tables created successfully. Code generation will happen in next step with complete schema.",
            "observation"
        )
        # Yield True as signal to exit early
        yield True

    def _validate_and_fix_fields(
        self,
        files: List[FileChange],
        full_content: str,
        app: Optional['InternalApp'],
        version: Optional['AppVersion'],
        model: str
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """Validate field names and request fix if needed. Returns final files."""
        if not (app and files):
            return files

        validation_result = self._validate_datastore_field_names(files, app)
        if validation_result["passed"]:
            return files

        # Validation failed - request fix
        yield self.emit_thinking(
            f"❌ Field validation failed with {len(validation_result['errors'])} error(s). Requesting fix from Claude...",
            "reflection",
        )
        logger.error(f"🚨 [FIELD VALIDATION] BLOCKING generation due to field errors")

        # Build fix prompt with fresh schema context
        from vector_app.services.data_store_context import build_data_store_context
        data_store_context = build_data_store_context(app)

        fix_prompt = self._build_field_fix_prompt(
            original_content=full_content,
            errors=validation_result["errors"],
            data_store_context=data_store_context,
        )

        # Request fix from Claude
        fixed_content = ""
        for chunk in self.stream_llm_response(
            system_prompt="You are a code generation assistant. Fix the code based on validation errors.",
            user_prompt=fix_prompt,
            model=model,
            temperature=0.3,
        ):
            fixed_content += chunk

        # Re-parse fixed content
        files = self.parse_code_blocks(fixed_content)

        # Check if Claude created new tables in the fix response
        fix_tables = self._parse_table_definitions(fixed_content)
        if fix_tables:
            logger.info(f"🔧 [FIX TABLES] Found {len(fix_tables)} table definitions in fix response")
            for event in self._apply_table_definitions(fix_tables, app, version):
                yield event

        # Re-validate
        revalidation = self._validate_datastore_field_names(files, app)
        if revalidation["passed"]:
            yield self.emit_thinking("✅ Field validation passed after fix", "observation")
        else:
            logger.error(f"❌ [FIELD VALIDATION] Still has errors after fix: {revalidation['errors']}")
            yield self.emit_thinking(
                f"⚠️ Field validation still has {len(revalidation['errors'])} error(s) after fix attempt",
                "reflection",
            )

        return files

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
        from vector_app.prompts.agentic import build_step_prompt, build_codegen_system_prompt

        has_data_store = context.get('has_data_store', False)

        # Build prompts
        prompt = build_step_prompt(
            step, step_index, user_message, context,
            existing_files, registry_surface, data_store_context,
            connectors_context=mcp_tools_context,
        )
        system_prompt = build_codegen_system_prompt(registry_surface, has_data_store)

        try:
            # Stream LLM response with validation
            generator = self._stream_llm_with_validation(
                system_prompt, prompt, model, step_index
            )
            full_content = ""
            try:
                while True:
                    warnings, content = next(generator)
                    full_content = content
                    for warning in warnings:
                        yield self.emit_streaming_warning(warning)
                    if not warnings:  # Progress signal
                        yield self.emit_step_progress(step_index, min(90, len(full_content) // 50), "Generating code...")
            except StopIteration as e:
                # Capture the return value from the generator
                full_content = e.value if e.value is not None else full_content

            # Handle table creation (with early return if tables created)
            for event in self._handle_table_creation(full_content, app, version):
                if event is True:  # Tables were created
                    return  # Early exit
                yield event

            # Parse files
            files = self.parse_code_blocks(full_content)

            # Validate and fix field names
            for event in self._validate_and_fix_fields(files, full_content, app, version, model):
                if isinstance(event, list):  # Final files returned
                    files = event
                    break
                yield event

            # Emit all files
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
        RESERVED_FIELDS = {'id', 'created_at', 'updated_at'}

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
                    # Filter out reserved fields - they're auto-generated
                    col_name = col.get('name', '').lower()
                    if col_name not in RESERVED_FIELDS:
                        columns.append(col)
                    else:
                        logger.info(f"Filtered out reserved field '{col_name}' from table '{slug}'")

        # Reject tables that only have reserved fields (no user-defined columns)
        if not columns:
            logger.warning(f"Table '{slug}' has no user-defined columns after filtering reserved fields")
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

            # 🚨 CRITICAL: Auto-generate system columns (id, created_at, updated_at)
            # These should be filtered out by _parse_single_table, but be defensive
            columns = table_def['columns'].copy()

            # Check if 'id' column already exists (shouldn't, but be defensive)
            has_id = any(col.get('name', '').lower() == 'id' for col in columns)

            if not has_id:
                # Prepend the auto-generated id column
                columns.insert(0, {
                    'name': 'id',
                    'type': 'uuid',
                    'primary_key': True,
                    'auto_generate': True,
                    'nullable': False,
                })

            # Check for created_at
            has_created_at = any(col.get('name', '').lower() == 'created_at' for col in columns)
            if not has_created_at:
                columns.append({
                    'name': 'created_at',
                    'type': 'datetime',
                    'auto_now_add': True,
                    'nullable': False,
                })

            # Check for updated_at
            has_updated_at = any(col.get('name', '').lower() == 'updated_at' for col in columns)
            if not has_updated_at:
                columns.append({
                    'name': 'updated_at',
                    'type': 'datetime',
                    'auto_now': True,
                    'nullable': False,
                })

            schema = {'columns': columns}

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
    
    def _generate_types_file(
        self,
        app: 'InternalApp',
    ) -> Optional[FileChange]:
        """Generate TypeScript types file for all app tables."""
        tables = AppDataTable.objects.filter(internal_app=app)
        if not tables.exists():
            return None
        
        types_content = generate_typescript_types(list(tables))
        if not types_content:
            return None
        
        logger.debug(f"Generated types file content: {types_content}")
        return FileChange(
            path="src/lib/types.ts",
            action="create",
            language="ts",
            content=types_content,
            lines_added=types_content.count('\n') + 1,
            lines_removed=0,
        )
    
    def _validate_and_fix(
        self,
        generated_files: List[FileChange],
        model: str,
    ) -> Generator[AgentEvent, None, tuple]:
        """
        Validate generated code and attempt to fix errors.
        
        Returns (validation_passed, fix_attempts)
        """
        from vector_app.services.error_fix_service import get_error_fix_service
        
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
            from vector_app.services.agentic_service import FileChange as AgenticFileChange
            
            agentic_files = [
                AgenticFileChange(
                    path=f.path,
                    action=f.action,
                    language=f.language,
                    content=f.content,
                    previous_content=f.previous_content,
                    lines_added=f.lines_added,
                    lines_removed=f.lines_removed,
                )
                for f in generated_files
            ]
            
            # Create CompilationError objects
            from vector_app.services.agentic_service import CompilationError
            
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
                                previous_content=file_data.get("previous_content", ""),
                                lines_added=file_data.get("lines_added", 0),
                                lines_removed=file_data.get("lines_removed", 0),
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
        Validate TypeScript code.
        
        Returns:
            Dictionary with 'passed' (bool), 'errors' (list), 'warnings' (list)
        """
        from vector_app.services.validation_service import get_validation_service
        validation_service = get_validation_service()
        return validation_service.validate_typescript(files)
