"""
Generate Handler

Full app generation from scratch - refactored from the original agentic_service.
This handler is used when the user intent is GENERATE_NEW.

OPTIMIZED: Uses parallel execution for independent plan steps when possible.
"""

import json
import logging
import queue
import re
import time
import uuid
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import Any, Dict, Generator, List, Optional, TYPE_CHECKING

from .base_handler import BaseHandler, AgentEvent, FileChange, exclude_protected_files
from .parallel_executor import create_parallel_executor, ParallelStepExecutor
from ..datastore import TableDefinitionParser, build_data_store_context, get_system_columns
from vector_app.models import AppDataTable
from vector_app.prompts.agentic import (
    apply_design_style_prompt,
    build_codegen_system_prompt,
    build_step_prompt,
)
from vector_app.services.typescript_types_generator import generate_typescript_types
from vector_app.services.app_data_service import AppDataService
from vector_app.services.error_fix_service import get_error_fix_service
from vector_app.services.planning_service import PlanStep, PlanStepStatus, PlanOperationType, get_planning_service, AgentPlan
from vector_app.services.types import CompilationError
from vector_app.services.validation_service import get_validation_service
from vector_app.prompts.agentic import build_file_prompt

if TYPE_CHECKING:
    from vector_app.models import InternalApp, AppVersion
    from vector_app.services.intent_classifier import IntentResult
    from vector_app.services.context_analyzer import AppContext

logger = logging.getLogger(__name__)


PROTECTED_FILES = {
    'src/lib/types.ts',      # Auto-generated TypeScript types from table schemas
}


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
        intent: "IntentResult",
        context: "AppContext",
        user_message: str,
        current_spec: Optional[Dict[str, Any]],
        registry_surface: Dict[str, Any],
        app_name: str,
        model: str,
        app: Optional["InternalApp"] = None,
        version: Optional["AppVersion"] = None,
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

        # Generate plan using the planning service
        try:
            planning_service = get_planning_service()
            plan: AgentPlan = planning_service.create_plan(styled_user_message, plan_context, model)
            plan_steps = plan.steps
        except Exception as e:
            logger.error(f"Planning service failed: {e}, using default plan")
            plan_steps = self._create_default_plan()

        yield self.emit_plan_created(
            steps=plan_steps,
            explored_dirs=3,
            explored_files=len(registry_surface.get("resources", [])) + 5,
            searches=1,
        )
        
        # ===== PHASE 2: EXECUTE (Two-Phase) =====
        yield self.emit_phase_change("executing", "Building your app...")

        # Separate data and code steps
        data_steps = [s for s in plan_steps if s.type == "data"]
        code_steps = [s for s in plan_steps if s.type != "data"]

        # Track files generated in data phase (including TypeScript types)
        data_phase_files: List[FileChange] = []

        # PHASE 2a: Execute data steps sequentially (create DB + TypeScript types)
        if data_steps:
            logger.info(f"📊 [DATA PHASE] Executing {len(data_steps)} data step(s) sequentially")
            for event in self._execute_data_steps_sequential(
                data_steps=data_steps,
                user_message=styled_user_message,
                context=plan_context,
                registry_surface=registry_surface,
                model=model,
                app=app,
                version=version,
                data_store_context=data_store_context,
                mcp_tools_context=mcp_tools_context,
            ):
                # Collect files generated during data phase (especially database.ts)
                if hasattr(event, 'type') and event.type == 'file_generated':
                    file_data = event.data.get('file')
                    if file_data:
                        data_phase_files.append(FileChange(**file_data))
                        logger.info(f"📁 [DATA PHASE] Captured file: {file_data.get('path', 'unknown')}")
                yield event

            # Refresh data store context after data phase
            if app:
                data_store_context = build_data_store_context(app)
                logger.info(f"🔄 [DATA PHASE COMPLETE] Schema frozen, TypeScript types generated")

                # Log which files will be available to code phase
                if data_phase_files:
                    file_paths = [str(f.path) for f in data_phase_files]
                    logger.info(f"📁 [DATA PHASE] Generated {len(data_phase_files)} file(s) for code phase: {', '.join(file_paths)}")

        # Generate types.ts from existing tables if no data phase but tables exist
        if not data_steps and app and code_steps:
            tables = AppDataTable.objects.filter(internal_app=app).order_by('name')
            if tables.exists():
                logger.info(f"📝 [PRE-CODE] No data steps but {tables.count()} table(s) exist - generating types.ts")

                ts_types_content = generate_typescript_types(list(tables))
                types_file = FileChange(
                    path='src/lib/types.ts',
                    action='create',
                    language='typescript',
                    content=ts_types_content,
                    lines_added=ts_types_content.count('\n') + 1,
                )

                data_phase_files.append(types_file)
                yield self.emit_file_generated(types_file)
                logger.info(f"✅ [PRE-CODE] Generated src/lib/types.ts from existing tables")

        # PHASE 2b: Execute code steps in parallel (NO DB changes allowed)
        if code_steps:
            logger.info(f"💻 [CODE PHASE] Executing {len(code_steps)} code step(s) in parallel with {len(data_phase_files)} existing file(s)")
            generated_files = yield from self._execute_steps_parallel(
                plan_steps=code_steps,
                user_message=styled_user_message,
                context=plan_context,
                registry_surface=registry_surface,
                model=model,
                app=app,
                version=version,
                data_store_context=data_store_context,  # Frozen schema
                mcp_tools_context=mcp_tools_context,
                allow_table_creation=False,  # Block DB changes in code phase
                initial_files=data_phase_files,  # Pass TypeScript types file to code phase
            )
        else:
            generated_files = data_phase_files if data_phase_files else []
        
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
        context: "AppContext",
        mcp_tools_context: Optional[str] = None,
    ) -> Dict[str, Any]:
        """Build context dictionary for planning."""
        resources = registry_surface.get("resources", [])

        # Build data store summary
        data_store_summary = ""
        if context.existing_tables:
            table_names = [str(t.name) for t in context.existing_tables]
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

    def _create_default_plan(self) -> List[PlanStep]:
        """
        Create a default fallback plan when the planning service fails.
        """
        return [
            PlanStep(
                id=str(uuid.uuid4()),
                type="design",
                title="Design App Structure",
                description="Plan the component hierarchy and data flow",
                step_order=0,
                target_files=["src/App.tsx"],
                operation_type=PlanOperationType.GENERATE,
            ),
            PlanStep(
                id=str(uuid.uuid4()),
                type="component",
                title="Create Main Component",
                description="Create src/App.tsx with main app structure",
                step_order=1,
                target_files=["src/App.tsx"],
                operation_type=PlanOperationType.GENERATE,
            ),
            PlanStep(
                id=str(uuid.uuid4()),
                type="component",
                title="Build UI Components",
                description="Create src/components/ with reusable UI components",
                step_order=1,
                target_files=["src/components/ui/Button.tsx", "src/components/ui/Card.tsx"],
                operation_type=PlanOperationType.GENERATE,
            ),
            PlanStep(
                id=str(uuid.uuid4()),
                type="integration",
                title="Connect Data Layer",
                description="Modify src/App.tsx to integrate with the runtime API",
                step_order=2,
                target_files=["src/App.tsx"],
                operation_type=PlanOperationType.EDIT,
            ),
            PlanStep(
                id=str(uuid.uuid4()),
                type="styling",
                title="Apply Styling",
                description="Add professional styling with Tailwind to all components",
                step_order=3,
                target_files=["src/App.tsx", "src/components/ui/Button.tsx", "src/components/ui/Card.tsx"],
                operation_type=PlanOperationType.EDIT,
            ),
        ]
    
    def _execute_data_steps_sequential(
        self,
        data_steps: List[PlanStep],
        user_message: str,
        context: Dict[str, Any],
        registry_surface: Dict[str, Any],
        model: str,
        app: Optional['InternalApp'] = None,
        version: Optional['AppVersion'] = None,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
    ) -> Generator[AgentEvent, None, None]:
        """
        Execute data steps sequentially to create all tables before code generation.

        This ensures all DB schema is defined before any code generation starts.
        """
        for step_index, step in enumerate(data_steps):
            step_start = time.time()

            yield self.emit_step_started(step, step_index)
            yield self.emit_step_start(step, step_index)
            yield self.emit_thinking(step.title, "decision")

            try:
                # Execute data step (tables will be created, TypeScript types generated)
                for event in self._execute_step(
                    step=step,
                    step_index=step_index,
                    user_message=user_message,
                    context=context,
                    existing_files=[],
                    registry_surface=registry_surface,
                    model=model,
                    app=app,
                    version=version,
                    data_store_context=data_store_context,
                    mcp_tools_context=mcp_tools_context,
                    allow_table_creation=True,  # Allow in data phase
                ):
                    yield event

                step.status = PlanStepStatus.COMPLETE
                step.duration = int((time.time() - step_start) * 1000)

                yield self.emit_step_completed(step, step_index)
                yield self.emit_step_complete(step_index, PlanStepStatus.COMPLETE.value, step.duration)

            except Exception as e:
                logger.error(f"Data step execution error: {e}")
                step.status = PlanStepStatus.ERROR
                step.duration = int((time.time() - step_start) * 1000)
                yield self.emit_step_complete(step_index, PlanStepStatus.ERROR.value, step.duration)
                raise

    def _execute_steps_parallel(
        self,
        plan_steps: List[PlanStep],
        user_message: str,
        context: Dict[str, Any],
        registry_surface: Dict[str, Any],
        model: str,
        app: Optional["InternalApp"] = None,
        version: Optional["AppVersion"] = None,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
        allow_table_creation: bool = True,
        initial_files: Optional[List[FileChange]] = None,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """
        Execute plan steps with parallel execution for independent steps.

        Args:
            allow_table_creation: If False, blocks table creation (for code phase)
            initial_files: Files to pass as existing context (e.g., database.ts from data phase)
        """
        # Create step executor closure
        def step_executor(
            step: PlanStep,
            step_index: int,
            existing_files: List[FileChange],
        ) -> Generator[AgentEvent, None, None]:
            """Execute a single step with full context."""
            step_start = time.time()

            yield self.emit_step_started(step, step_index)
            yield self.emit_step_start(step, step_index)
            yield self.emit_thinking(step.title, "decision")

            try:
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
                    allow_table_creation=allow_table_creation,
                ):
                    yield event

                step.status = PlanStepStatus.COMPLETE
                step.duration = int((time.time() - step_start) * 1000)

                yield self.emit_step_completed(step, step_index)
                yield self.emit_step_complete(step_index, PlanStepStatus.COMPLETE.value, step.duration)

            except Exception as e:
                logger.error(f"Step execution error: {e}")
                step.status = PlanStepStatus.ERROR
                step.duration = int((time.time() - step_start) * 1000)
                yield self.emit_step_complete(step_index, PlanStepStatus.ERROR.value, step.duration)
                raise

        # Use the parallel executor to run steps
        generated_files = yield from self.parallel_executor.execute_steps(
            steps=plan_steps,
            step_executor=step_executor,
            initial_files=initial_files or [],
        )

        return generated_files

    def _stream_llm_with_validation(
        self, system_prompt: str, user_prompt: str, model: str, step_index: int
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
        version: Optional['AppVersion'],
        allow_creation: bool = True,
    ) -> Generator[AgentEvent, None, None]:
        """
        Handle table creation from generated content.

        Args:
            allow_creation: If False, blocks table creation (for code phase)
        """
        if not (app and version):
            return

        table_defs = self._parse_table_definitions(full_content)
        if not table_defs:
            return

        # Block table creation in code phase
        if not allow_creation:
            logger.warning(
                f"🚫 [CODE PHASE] Blocked {len(table_defs)} table definition(s) - "
                f"tables can only be created in data phase"
            )
            yield self.emit_thinking(
                "⚠️ Table definitions detected in code phase. All tables must be defined in the data step.",
                "reflection"
            )
            return

        # Create tables
        yield self.emit_thinking(
            f"Creating {len(table_defs)} data table(s)...",
            "decision",
        )
        for event in self._apply_table_definitions(table_defs, app, version):
            yield event

        # Generate TypeScript types file after tables are created
        if app:
            tables = AppDataTable.objects.filter(internal_app=app).order_by('name')
            if tables.exists():
                logger.info(f"📝 [TYPESCRIPT] Generating types file for {tables.count()} table(s)")

                ts_types_content = generate_typescript_types(list(tables))

                types_file = FileChange(
                    path='src/lib/types.ts',
                    action='create',
                    language='typescript',
                    content=ts_types_content,
                    lines_added=ts_types_content.count('\n') + 1,
                )

                yield self.emit_file_generated(types_file)
                logger.info(f"✅ [TYPESCRIPT] Generated src/lib/types.ts ({len(ts_types_content)} chars)")

    def _build_tables_summary_for_system_prompt(self, app: 'InternalApp') -> str:
        """Build concise table schema summary for system prompt."""
        tables = AppDataTable.objects.filter(internal_app=app).order_by('name')
        if not tables.exists():
            return "No tables defined."

        lines = []
        for table in tables:
            schema = table.schema_json or {}
            columns = schema.get('columns', [])
            field_names = [str(col.get('name')) for col in columns]
            lines.append(f"- '{table.slug}': {', '.join(field_names)}")

        return "\n".join(lines)

    def _build_syntax_fix_prompt(self, original_content: str, syntax_errors: List[Dict]) -> str:
        """Build prompt to fix TypeScript syntax errors."""
        error_list = "\n".join(f"- {err['file']}: {err['error']}\n  Fix: {err['fix']}" for err in syntax_errors)

        return f"""🚨 YOUR CODE HAS TYPESCRIPT SYNTAX ERRORS 🚨

{'='*80}
SYNTAX ERRORS FOUND:
{'='*80}
{error_list}

{'='*80}
HOW TO FIX THESE ERRORS:
{'='*80}

1. INLINE IMPORTS IN TYPES (ts(2499)):
   ❌ WRONG: Database[import('./types').TableSlug.Projects]
   ✅ CORRECT:
   ```typescript
   import type {{ Database, TableSlug }} from './lib/types';
   // ... then use:
   const items: Database[TableSlug.Projects]['row'][] = result.rows;
   ```

2. INTERFACE EXTENDS WITH INDEXED ACCESS (ts(2499)):
   ❌ WRONG: interface Foo extends Database[TableSlug.Projects]['row']
   ✅ CORRECT: Use type alias instead:
   ```typescript
   type FooBase = Database[TableSlug.Projects]['row'];
   interface Foo extends FooBase {{
     extraField: string;
   }}
   ```
   OR use intersection types:
   ```typescript
   type Foo = Database[TableSlug.Projects]['row'] & {{
     extraField: string;
   }};
   ```

3. STRAY QUOTES:
   ❌ WRONG: const foo = 'bar';'
   ✅ CORRECT: const foo = 'bar';

{'='*80}

NOW REGENERATE THE COMPLETE, CORRECTED CODE.
Include ALL files, not just the ones with errors.
DO NOT output explanations - ONLY output the fixed code blocks.

Original code that needs fixing:
{original_content}
"""

    def _validate_and_fix_fields(
        self,
        files: List[FileChange],
        full_content: str,
        app: Optional["InternalApp"],
        version: Optional["AppVersion"],
        model: str,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """Validate field names and request fix if needed. Returns final files."""
        if not (app and files):
            return files

        # NEW: Check TypeScript syntax FIRST
        code_blocks = {f.path: f.content for f in files}
        syntax_errors = self._validate_typescript_syntax(code_blocks)
        if syntax_errors:
            logger.error(f"🚨 [SYNTAX VALIDATION] Found {len(syntax_errors)} syntax errors")
            for err in syntax_errors:
                logger.error(f"   {err['file']}: {err['error']}")

            yield self.emit_thinking(
                f"❌ TypeScript syntax errors found. Requesting fix from Claude...",
                "reflection",
            )

            # Build fix prompt for syntax errors
            fix_prompt = self._build_syntax_fix_prompt(full_content, syntax_errors)

            # Request fix
            fixed_content = ""
            for chunk in self.stream_llm_response(
                system_prompt="You are a code generation assistant. Fix TypeScript syntax errors.",
                user_prompt=fix_prompt,
                model=model,
                temperature=0.3,
            ):
                fixed_content += chunk

            # Re-parse and update files
            files = self.parse_code_blocks(fixed_content)
            files = exclude_protected_files(files, PROTECTED_FILES)
            full_content = fixed_content  # Update for field validation below

            yield self.emit_thinking("✅ TypeScript syntax fixed", "observation")

        # Continue with EXISTING field validation logic
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
        data_store_context = build_data_store_context(app)

        fix_prompt = self._build_field_fix_prompt(
            original_content=full_content,
            errors=validation_result["errors"],
            data_store_context=data_store_context,
        )

        # Build data-aware system prompt
        all_tables_summary = self._build_tables_summary_for_system_prompt(app)
        system_prompt = f"""You are a code generation assistant fixing field validation errors.

DATABASE SCHEMA CONTEXT:
{all_tables_summary}

Your task: Fix field errors by using ONLY fields that exist in the schema above.
Common issue: Code queries the WRONG table - check if the field exists on a different table.
"""

        # Request fix from Claude
        fixed_content = ""
        for chunk in self.stream_llm_response(
            system_prompt=system_prompt,
            user_prompt=fix_prompt,
            model=model,
            temperature=0.3,
        ):
            fixed_content += chunk

        # Re-parse fixed content
        files = self.parse_code_blocks(fixed_content)
        
        # Filter out protected files (e.g., auto-generated types.ts)
        files = exclude_protected_files(files, PROTECTED_FILES)

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
        app: Optional["InternalApp"] = None,
        version: Optional["AppVersion"] = None,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
        allow_table_creation: bool = True,
    ) -> Generator[AgentEvent, None, None]:
        """Execute a single plan step and yield progress events.
        
        Automatically uses parallel sub-agents when the step has multiple
        explicitly defined target files.
        """
        # Check if step has multiple explicit target files
        target_files = getattr(step, 'target_files', None) or []
        use_subagents = len(target_files) >= 2
        
        if use_subagents:
            logger.info(f"[STEP {step_index}] Using sub-agents for {len(target_files)} target files: {target_files}")
            yield from self._execute_step_with_subagents(
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
                allow_table_creation=allow_table_creation,
            )
        else:
            # Single-agent execution (original behavior)
            yield from self._execute_step_single_agent(
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
                allow_table_creation=allow_table_creation,
            )

    def _execute_step_with_subagents(
        self,
        step: PlanStep,
        step_index: int,
        user_message: str,
        context: Dict[str, Any],
        existing_files: List[FileChange],
        registry_surface: Dict[str, Any],
        model: str,
        app: Optional["InternalApp"] = None,
        version: Optional["AppVersion"] = None,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
        allow_table_creation: bool = True,
    ) -> Generator[AgentEvent, None, None]:
        """
        Execute a plan step using parallel sub-agents, one per file.
        
        Each target file is generated by a separate LLM call running in parallel.
        This is only called when step.target_files has 2+ entries.
        """
        has_data_store = context.get('has_data_store', False)
        
        # Use explicit target files from the step
        target_files = list(step.target_files)
        
        logger.info(f"[SUBAGENTS] Executing step {step_index} with {len(target_files)} parallel sub-agents: {target_files}")
        # Build system prompt once (shared across all sub-agents)
        system_prompt = build_codegen_system_prompt(registry_surface, has_data_store)
        
        # Thread-safe queue for collecting events from parallel threads
        event_queue: queue.Queue = queue.Queue()
        
        # Results storage
        all_files: List[FileChange] = []
        all_content: List[str] = []  # For table creation handling
        errors: List[Exception] = []
        
        def generate_file(file_path: str, file_index: int) -> tuple:
            """Generate a single file in a thread."""
            try:
                # Build focused prompt for this file
                prompt = build_file_prompt(
                    file_path=file_path,
                    step=step,
                    step_index=step_index,
                    user_message=user_message,
                    context=context,
                    existing_files=existing_files,
                    other_target_files=[f for f in target_files if f != file_path],
                    registry_surface=registry_surface,
                    data_store_context=data_store_context,
                    connectors_context=mcp_tools_context,
                )

                # Signal start
                event_queue.put((time.time(), file_index, "start", file_path))
                
                # Call LLM (non-streaming for parallel execution)
                content = self.call_llm(
                    system_prompt=system_prompt,
                    user_prompt=prompt,
                    model=model,
                    temperature=0.3,
                    timeout=120.0,
                )
                
                # Parse the generated file
                files = self.parse_code_blocks(content)
                
                # Filter to only the target file (in case LLM generates extra files)
                target_file = None
                for f in files:
                    if f.path == file_path or f.path.endswith(file_path.split('/')[-1]):
                        target_file = f
                        break
                
                # If no exact match, take the first file
                if target_file is None and files:
                    target_file = files[0]
                    # Correct the path
                    target_file = FileChange(
                        path=file_path,
                        action=target_file.action,
                        language=target_file.language,
                        content=target_file.content,
                        previous_content=target_file.previous_content,
                        lines_added=target_file.lines_added,
                        lines_removed=target_file.lines_removed,
                    )
                
                # Signal completion
                event_queue.put((time.time(), file_index, "complete", file_path))
                
                return (file_path, target_file, content, None)
                
            except Exception as e:
                logger.error(f"[SUBAGENTS] Error generating {file_path}: {e}")
                event_queue.put((time.time(), file_index, "error", str(e)))
                return (file_path, None, "", e)
        
        # Execute all file generations in parallel
        with ThreadPoolExecutor(max_workers=min(5, len(target_files))) as executor:
            futures = {
                executor.submit(generate_file, file_path, idx): file_path
                for idx, file_path in enumerate(target_files)
            }
            
            # Emit progress events as files complete
            completed_count = 0
            while completed_count < len(futures):
                try:
                    # Check for events from threads
                    while True:
                        try:
                            _, file_idx, event_type, data = event_queue.get_nowait()
                            if event_type == "start":
                                yield self.emit_thinking(f"Generating {data}...", "decision")
                            elif event_type == "complete":
                                yield self.emit_step_progress(
                                    step_index,
                                    min(90, ((completed_count + 1) * 100) // len(target_files)),
                                    f"Generated {data}"
                                )
                            elif event_type == "error":
                                yield self.emit_thinking(f"Error: {data}", "reflection")
                        except queue.Empty:
                            break
                    
                    # Check for completed futures
                    for future in list(futures.keys()):
                        if future.done():
                            file_path, file_change, content, error = future.result()
                            if error:
                                errors.append(error)
                            else:
                                if file_change:
                                    all_files.append(file_change)
                                if content:
                                    all_content.append(content)
                            completed_count += 1
                            del futures[future]
                    
                    time.sleep(0.1)  # Brief sleep to avoid busy-waiting
                    
                except Exception as e:
                    logger.error(f"[SUBAGENTS] Error in event loop: {e}")
                    break

        # Handle any errors
        if errors and not all_files:
            raise errors[0]  # Re-raise first error if no files generated
        
        # Filter out protected files
        all_files = exclude_protected_files(all_files, PROTECTED_FILES)
        
        # Handle table creation from all content
        combined_content = "\n\n".join(all_content)
        for event in self._handle_table_creation(combined_content, app, version, allow_table_creation):
            yield event
        
        # Validate and fix field names
        for event in self._validate_and_fix_fields(all_files, combined_content, app, version, model):
            if isinstance(event, list):  # Final files returned
                all_files = event
                break
            yield event
        
        # Emit all files
        for file in all_files:
            yield self.emit_file_generated(file)
        
        logger.info(f"[SUBAGENTS] Step {step_index} complete: generated {len(all_files)} file(s)")

    def _execute_step_single_agent(
        self,
        step: PlanStep,
        step_index: int,
        user_message: str,
        context: Dict[str, Any],
        existing_files: List[FileChange],
        registry_surface: Dict[str, Any],
        model: str,
        app: Optional["InternalApp"] = None,
        version: Optional["AppVersion"] = None,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
        allow_table_creation: bool = True,
    ) -> Generator[AgentEvent, None, None]:
        """
        Execute a plan step using a single LLM call (original behavior).
        
        This is the fallback when sub-agents cannot be used.
        """
        has_data_store = context.get('has_data_store', False)

        # Build prompts
        prompt = build_step_prompt(
            step,
            step_index,
            user_message,
            context,
            existing_files,
            registry_surface,
            data_store_context,
            connectors_context=mcp_tools_context,
        )
        system_prompt = build_codegen_system_prompt(registry_surface, has_data_store)

        try:
            # Stream LLM response with validation
            generator = self._stream_llm_with_validation(system_prompt, prompt, model, step_index)
            full_content = ""
            try:
                while True:
                    warnings, content = next(generator)
                    full_content = content
                    for warning in warnings:
                        yield self.emit_streaming_warning(warning)
                    if not warnings:  # Progress signal
                        yield self.emit_step_progress(
                            step_index, min(90, len(full_content) // 50), "Generating code..."
                        )
            except StopIteration as e:
                # Capture the return value from the generator
                full_content = e.value if e.value is not None else full_content

            # Handle table creation (blocked in code phase)
            for event in self._handle_table_creation(full_content, app, version, allow_table_creation):
                yield event

            # Parse files
            files = self.parse_code_blocks(full_content)
            
            # Filter out protected files (e.g., auto-generated types.ts)
            files = exclude_protected_files(files, PROTECTED_FILES)

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
        return TableDefinitionParser.parse_table_definitions(content)
    
    def _apply_table_definitions(
        self,
        table_defs: List[Dict[str, Any]],
        app: "InternalApp",
        version: "AppVersion",
    ) -> Generator[AgentEvent, None, None]:
        """Apply table definitions to the app's data store."""
        for table_def in table_defs:
            slug = table_def["slug"]

            # Check if table exists
            existing = AppDataTable.objects.filter(
                internal_app=app,
                slug=slug,
            ).first()

            if existing:
                logger.info(f"Table {slug} already exists, skipping")
                continue

            # 🚨 CRITICAL: Add system columns (id, created_at, updated_at)
            # The parser filters these out if LLM defines them
            # We add them here before validation so they're part of the schema
            # Start with user-defined columns
            columns = table_def['columns'].copy()

            # Prepend system columns (id first, then user columns, then timestamps)
            system_cols = get_system_columns()
            id_col = [col for col in system_cols if col['name'] == 'id'][0]
            timestamp_cols = [col for col in system_cols if col['name'] in ('created_at', 'updated_at')]

            # Build final column list: id, user columns, timestamps
            final_columns = [id_col] + columns + timestamp_cols

            schema = {'columns': final_columns}

            table, errors = AppDataService.create_table_versioned(
                app=app,
                version=version,
                name=table_def["name"],
                schema=schema,
                description=table_def.get("description", ""),
            )

            if table:
                yield self.emit_table_created(
                    slug=table.slug,
                    name=table.name,
                    columns=len(table_def["columns"]),
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
        MAX_FIX_ATTEMPTS = 2
        validation_passed = False
        fix_attempts = 0

        for attempt in range(1, MAX_FIX_ATTEMPTS + 1):
            # Run TypeScript validation
            validation = self._validate_typescript(generated_files)

            if validation["passed"]:
                validation_passed = True
                break

            # Attempt to fix
            fix_attempts = attempt
            error_count = len(validation.get("errors", []))

            yield self.emit_thinking(
                f"Found {error_count} compilation error(s), attempting fix ({attempt}/{MAX_FIX_ATTEMPTS})",
                "observation",
            )

            fix_service = get_error_fix_service()

            # Convert FileChange to the format expected by fix service
            # Note: FileChange from types.py is compatible with agentic_service
            agentic_files = [
                FileChange(
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
            errors = [
                CompilationError(
                    file=e.get("file", ""),
                    line=e.get("line", 0),
                    column=e.get("column", 0),
                    message=e.get("message", ""),
                    code=e.get("code"),
                )
                for e in validation.get("errors", [])
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
            validation_passed = final["passed"]

        return (validation_passed, fix_attempts)

    def _validate_typescript(self, files: List[FileChange]) -> Dict[str, Any]:
        """
        Validate TypeScript files using ValidationService.
        """
        return get_validation_service().validate_typescript(files).to_dict()
