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
from ..diff import format_with_line_numbers
from ..diff_application_service import (
    _apply_diffs_from_llm_response,
    build_diff_prompts,
    DiffApplicationConfig,
)
from vector_app.models import AppDataTable, VersionFile
from vector_app.ai.models import AIModel
from vector_app.prompts.agentic import (
    apply_design_style_prompt,
    build_codegen_system_prompt,
    build_step_prompt,
)
from vector_app.services.typescript_types_generator import generate_typescript_types
from vector_app.services.app_data_service import AppDataService
from vector_app.services.error_fix_service import get_error_fix_service
from vector_app.services.planning_service import PlanStep, PlanStepStatus, PlanOperationType, get_planning_service, AgentPlan
from vector_app.services.intent_classifier import UserIntent
from vector_app.services.types import CompilationError
from vector_app.services.validation_service import get_validation_service
from vector_app.prompts.agentic import build_file_prompt
from vector_app.services.schema_extraction_service import get_schema_extraction_service
from vector_app.services.datastore.table_creator import create_tables_from_definitions

if TYPE_CHECKING:
    from vector_app.models import InternalApp, AppVersion
    from vector_app.services.intent_classifier import IntentResult
    from vector_app.services.context_analyzer import AppContext

logger = logging.getLogger(__name__)


PROTECTED_FILES = {
    'src/lib/types.ts',      # Auto-generated TypeScript types from table schemas
}


# =============================================================================
# STEP EDIT PROMPT TEMPLATE
# =============================================================================
# Used when a plan step has operation_type=EDIT to make surgical changes
# instead of regenerating complete files.

STEP_EDIT_SYSTEM_PROMPT = """You are an expert React/TypeScript developer making targeted, surgical edits to existing code using unified diffs.

## Surgical Edit Rules (CRITICAL)

1. **Preserve Everything Except What's Requested**
   - You MUST keep all existing functionality intact
   - Do NOT "improve" or "clean up" adjacent code
   - Do NOT add features "while you're at it"
   - Do NOT refactor unless explicitly asked

2. **Minimal Changes Only**
   - Change ONLY the specific lines needed
   - Keep existing variable names, patterns, and style
   - Maintain the exact same imports unless changes require new ones
   - Do NOT reorganize or restructure code

3. **Match Existing Style Exactly**
   - Use the same indentation (tabs vs spaces)
   - Use the same quote style (single vs double)
   - Use the same semicolon conventions
   - Match the existing naming conventions

4. **Output Format: Unified Diff**
   - Output changes as unified diffs (like git diff)
   - Include 3 lines of context before and after each change
   - Use `-` prefix for removed lines, `+` for added lines
   - Space prefix for unchanged context lines
   - Multiple hunks allowed for non-adjacent changes in the same file
"""


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
        model: AIModel,
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
            plan: AgentPlan = planning_service.create_plan(
                styled_user_message,
                plan_context,
                model,
                intent_type=UserIntent.GENERATE_NEW,
            )
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

        # ===== SCHEMA EXTRACTION: Create tables from plan =====
        if app is not None:
            logger.info("🔍 Starting schema extraction from plan...")
            schema_extraction_service = get_schema_extraction_service()
            try:
                schema_content = schema_extraction_service.extract_schema_from_plan(
                    plan,
                    styled_user_message,
                    model
                )
                logger.info(f"📋 Schema extraction complete. Content length: {len(schema_content) if schema_content else 0} chars")

                # Parse and create tables if schema was extracted
                if schema_content and "NO_TABLES_NEEDED" not in schema_content.upper():
                    logger.info("📊 Parsing table definitions from schema...")

                    # Extract table definitions from the LLM response
                    table_definitions = schema_extraction_service.parse_table_definitions(schema_content)

                    if table_definitions:
                        logger.info(f"🏗️  Creating {len(table_definitions)} tables: {[t['slug'] for t in table_definitions]}")
                        # Create tables from the definitions
                        created_tables = create_tables_from_definitions(app, table_definitions)
                        logger.info(f"✅ Created {len(created_tables)} tables from plan: {[t.slug for t in created_tables]}")

                        # Refresh data store context after creating tables
                        data_store_context = build_data_store_context(app)
                        logger.info("🔄 Refreshed data store context with newly created tables")
                    else:
                        logger.warning("⚠️  No valid table definitions found after parsing")
                else:
                    logger.info("ℹ️  No database tables needed for this plan")
            except Exception as e:
                logger.error(f"❌ Schema extraction failed: {e}")
                import traceback
                traceback.print_exc()
        else:
            logger.info("ℹ️  Skipping schema extraction: no app provided")

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
                logger.info(f"[PRE-CODE] No data steps but {tables.count()} table(s) exist - generating types.ts")

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
                logger.info(f"[PRE-CODE] Generated src/lib/types.ts from existing tables")

        # PHASE 2b: Execute code steps in parallel (NO DB changes allowed)
        if code_steps:
            logger.info(f"[CODE PHASE] Executing {len(code_steps)} code step(s) in parallel with {len(data_phase_files)} existing file(s)")
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

        # Fetch context files for validation (data phase files + existing version files)
        context_files = self._fetch_context_files(
            data_phase_files=data_phase_files,
            generated_files=generated_files,
            version=version,
        )

        validation_passed, fix_attempts = yield from self._validate_and_fix(
            generated_files=generated_files,
            context_files=context_files,
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
        model: AIModel,
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
        model: AIModel,
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
        self, system_prompt: str, user_prompt: str, model: AIModel, step_index: int
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
                f"[CODE PHASE] Blocked {len(table_defs)} table definition(s) - "
                f"tables can only be created in data phase"
            )
            yield self.emit_thinking(
                "Table definitions detected in code phase. All tables must be defined in the data step.",
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
                logger.info(f"[TYPESCRIPT] Generating types file for {tables.count()} table(s)")

                ts_types_content = generate_typescript_types(list(tables))

                types_file = FileChange(
                    path='src/lib/types.ts',
                    action='create',
                    language='typescript',
                    content=ts_types_content,
                    lines_added=ts_types_content.count('\n') + 1,
                )

                yield self.emit_file_generated(types_file)
                logger.info(f"[TYPESCRIPT] Generated src/lib/types.ts ({len(ts_types_content)} chars)")

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

    def _build_syntax_fix_prompt(self, files: List[FileChange], syntax_errors: List[Dict]) -> str:
        """Build prompt to fix TypeScript syntax errors using diffs."""
        error_list = "\n".join(f"- {err['file']}: {err['error']}\n  Fix: {err['fix']}" for err in syntax_errors)

        # Build file contents with line numbers for accurate diff generation
        files_with_lines = []
        for f in files:
            numbered_content = format_with_line_numbers(f.content)
            files_with_lines.append(f"### {f.path}\n```typescript\n{numbered_content}\n```")
        files_section = "\n\n".join(files_with_lines)

        return f"""Fix the TypeScript syntax errors below using unified diffs.

SYNTAX ERRORS:
{error_list}

COMMON FIXES:
1. INLINE IMPORTS IN TYPES (ts(2499)):
   WRONG: Database[import('./types').TableSlug.Projects]
   CORRECT: Import at top, then use: Database[TableSlug.Projects]['row']
   
   Proper Import of TableSlug (when there is already a database). They must be separate:
   import type {{ Database }} from './lib/types';
   import {{ TableSlug }} from './lib/types';

2. INTERFACE EXTENDS WITH INDEXED ACCESS (ts(2499)):
   WRONG: interface Foo extends Database[TableSlug.Projects]['row']
   CORRECT: Use type alias: type FooBase = Database[TableSlug.Projects]['row'];

3. STRAY QUOTES:
   WRONG: const foo = 'bar';'
   CORRECT: const foo = 'bar';


OUTPUT FORMAT:
Return ONLY unified diffs in ```diff blocks. Example:

```diff
--- src/App.tsx
+++ src/App.tsx
@@ -5,3 +5,4 @@
 import React from 'react';
+import type {{ Database }} from './lib/types';

import {{ TableSlug }} from './lib/types'; 
```

FILES WITH LINE NUMBERS:
{files_section}
"""

    def _validate_and_fix_fields(
        self,
        files: List[FileChange],
        full_content: str,
        app: Optional["InternalApp"],
        version: Optional["AppVersion"],
        model: AIModel,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """Validate field names and request fix if needed. Returns final files."""
        if not (app and files):
            return files

        # NEW: Check TypeScript syntax FIRST
        code_blocks = {f.path: f.content for f in files}
        syntax_errors = self._validate_typescript_syntax(code_blocks)
        if syntax_errors:
            logger.error(f"[SYNTAX VALIDATION] Found {len(syntax_errors)} syntax errors")
            for err in syntax_errors:
                logger.error(f"   {err['file']}: {err['error']}")

            yield self.emit_thinking(
                f"TypeScript syntax errors detected. Initiating automated fix process...",
                "reflection",
            )

            # Build fix prompt for syntax errors (now asks for diffs)
            fix_prompt = self._build_syntax_fix_prompt(files, syntax_errors)

            # Request fix via diffs
            diff_response = ""
            for chunk in self.stream_llm_response(
                system_prompt="You are a code generation assistant. Fix TypeScript syntax errors using unified diffs only.",
                user_prompt=fix_prompt,
                model=model,
                temperature=0.3,
            ):
                diff_response += chunk

            # Apply diffs using centralized service
            # Convert List[FileChange] to Dict[str, str] for service
            file_contents = {f.path: f.content for f in files}
            
            config = DiffApplicationConfig(
                protected_files=PROTECTED_FILES,  # Protect auto-generated types file
                normalize_paths=False,            # Paths match exactly
                allow_new_files=False,            # Syntax fix only modifies existing
                fallback_to_full_file=False,      # No fallback - just warn
                verify_changes=False,             # Apply all diffs
            )
            
            fixed_files = _apply_diffs_from_llm_response(
                llm_response=diff_response,
                file_contents=file_contents,
                config=config,
            )
            
            if fixed_files:
                # Merge fixed files back into original list, preserving action/language
                files_by_path = {f.path: f for f in files}
                for fixed_file in fixed_files:
                    original_file = files_by_path.get(fixed_file.path)
                    if original_file:
                        # Preserve original action and language, update content
                        files_by_path[fixed_file.path] = FileChange(
                            path=original_file.path,
                            action=original_file.action,
                            language=original_file.language,
                            content=fixed_file.content,
                            previous_content=fixed_file.previous_content,
                            lines_added=fixed_file.lines_added,
                            lines_removed=fixed_file.lines_removed,
                        )
                files = list(files_by_path.values())
                logger.info(f"[SYNTAX FIX] Applied {len(fixed_files)} diff(s)")
            else:
                logger.warning("[SYNTAX FIX] No diffs found in LLM response")

            yield self.emit_thinking("TypeScript syntax errors resolved", "observation")

        # Continue with EXISTING field validation logic
        validation_result = self._validate_datastore_field_names(files, app)
        if validation_result["passed"]:
            return files

        # Validation failed - request fix
        yield self.emit_thinking(
            f"Field validation failed with {len(validation_result['errors'])} error(s). Initiating automated fix process...",
            "reflection",
        )
        logger.error(f"[FIELD VALIDATION] BLOCKING generation due to field errors")

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
            yield self.emit_thinking("Field validation passed after fix", "observation")
        else:
            logger.error(f"[FIELD VALIDATION] Still has errors after fix: {revalidation['errors']}")
            yield self.emit_thinking(
                f"Field validation still has {len(revalidation['errors'])} error(s) after fix attempt",
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
        model: AIModel,
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
        model: AIModel,
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
                    timeout=180.0,
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
        model: AIModel,
        app: Optional["InternalApp"] = None,
        version: Optional["AppVersion"] = None,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
        allow_table_creation: bool = True,
    ) -> Generator[AgentEvent, None, None]:
        """
        Execute a plan step using a single LLM call.
        
        Dispatches to either generate or edit mode based on step.operation_type:
        - GENERATE: Creates complete file content (default behavior)
        - EDIT: Uses diff-based surgical edits for modifying existing files
        """
        # Dispatch based on operation type
        if step.operation_type == PlanOperationType.EDIT:
            # Use diff-based approach for edit operations
            logger.info(f"[STEP {step_index}] Using diff-based edit mode for operation_type=EDIT")
            yield from self._execute_step_via_edit(
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
            # Use full file generation for generate operations (default)
            logger.info(f"[STEP {step_index}] Using generate mode for operation_type={step.operation_type}")
            yield from self._execute_step_via_generate(
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

    def _execute_step_via_generate(
        self,
        step: PlanStep,
        step_index: int,
        user_message: str,
        context: Dict[str, Any],
        existing_files: List[FileChange],
        registry_surface: Dict[str, Any],
        model: AIModel,
        app: Optional["InternalApp"] = None,
        version: Optional["AppVersion"] = None,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
        allow_table_creation: bool = True,
    ) -> Generator[AgentEvent, None, None]:
        """
        Execute a plan step by generating complete file content.
        
        This is the original behavior - generates full file blocks from scratch.
        Used when operation_type is GENERATE, SCHEMA, ADD_FEATURE, etc.
        """
        logger.info(
            f"🚀 [STEP {step_index}] Starting GENERATE mode: '{step.title}' "
            f"(operation_type={step.operation_type}, target_files={getattr(step, 'target_files', [])})"
        )
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

    def _execute_step_via_edit(
        self,
        step: PlanStep,
        step_index: int,
        user_message: str,
        context: Dict[str, Any],
        existing_files: List[FileChange],
        registry_surface: Dict[str, Any],
        model: AIModel,
        app: Optional["InternalApp"] = None,
        version: Optional["AppVersion"] = None,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
        allow_table_creation: bool = True,
    ) -> Generator[AgentEvent, None, None]:
        """
        Execute a plan step using diff-based surgical edits.
        
        Used when operation_type is EDIT. This approach:
        - Shows existing file contents with line numbers
        - Requests unified diffs from the LLM
        - Applies diffs to existing files for minimal changes
        - Falls back to generate mode if no existing files to edit
        """
        logger.info(
            f"🚀 [STEP {step_index}] Starting EDIT mode (diff-based): '{step.title}' "
            f"(target_files={getattr(step, 'target_files', [])}, existing_files_count={len(existing_files)})"
        )
        # Build map of existing file contents
        file_contents: Dict[str, str] = {}
        for f in existing_files:
            file_contents[f.path] = f.content
        
        # Also include files from version if available
        if version:
            try:
                for vf in VersionFile.objects.filter(app_version=version):
                    if vf.path not in file_contents:
                        file_contents[vf.path] = vf.content or ""
            except Exception as e:
                logger.warning(f"[EDIT] Error fetching version files: {e}")
        
        # Get target files for this step
        target_files = getattr(step, 'target_files', None) or []
        
        # Filter to only files that exist (for editing)
        files_to_edit = {path: content for path, content in file_contents.items() 
                         if path in target_files or not target_files}
        
        # If no existing files to edit, fall back to generate mode
        if not files_to_edit:
            logger.info(f"[STEP {step_index}] No existing files to edit, falling back to generate mode")
            yield from self._execute_step_via_generate(
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
            return
        
        # Convert Dict[str, str] to List[FileChange]
        file_changes = []
        for path, content in files_to_edit.items():
            ext = path.split('.')[-1] if '.' in path else 'tsx'
            lang_map = {'tsx': 'tsx', 'ts': 'ts', 'jsx': 'jsx', 'js': 'js', 'css': 'css', 'json': 'json'}
            file_changes.append(FileChange(
                path=path,
                action="modify",
                language=lang_map.get(ext, 'tsx'),
                content=content,
                previous_content=content,
            ))

        # Build user message with step context using helper
        user_message_with_step, extra_context = self._build_step_edit_prompt(
            step=step,
            step_index=step_index,
            user_message=user_message,
            data_store_context=data_store_context,
        )

        # Build prompts using centralized method
        system_prompt, user_prompt = build_diff_prompts(
            edit_style_prompt=STEP_EDIT_SYSTEM_PROMPT,
            file_changes=file_changes,
            user_message=user_message_with_step,
            allow_new_files=False,
            extra_context=extra_context,
        )
        
        try:
            full_content = ""
            chunk_count = 0
            
            # Create streaming validator for real-time checks
            validator = self.create_streaming_validator()
            
            for chunk in self.stream_llm_response(
                system_prompt=system_prompt,
                user_prompt=user_prompt,
                model=model,
                temperature=0.2,  # Lower temperature for more consistent edits
            ):
                full_content += chunk
                chunk_count += 1
                
                # Real-time validation during streaming
                streaming_warnings = validator.check_chunk(chunk, full_content)
                for warning in streaming_warnings:
                    yield self.emit_streaming_warning(warning)
                
                if chunk_count % 10 == 0:
                    yield self.emit_step_progress(step_index, min(80, chunk_count), "Applying edits...")
            
            # Final validation check
            final_warnings = validator.final_check(full_content)
            for warning in final_warnings:
                yield self.emit_streaming_warning(warning)
            
            # Apply diffs using centralized service
            config = DiffApplicationConfig(
                protected_files=PROTECTED_FILES,  # Protect auto-generated types file
                normalize_paths=False,            # Paths match exactly from step context
                allow_new_files=True,             # Edit steps can create new files
                fallback_to_full_file=True,       # Fall back to full file parsing
                verify_changes=False,             # Apply all diffs
            )
            
            def fallback_with_protection(content: str) -> List[FileChange]:
                """Fallback that also excludes protected files."""
                parsed_files = self.parse_code_blocks(content)
                return exclude_protected_files(parsed_files, PROTECTED_FILES)
            
            files = _apply_diffs_from_llm_response(
                llm_response=full_content,
                file_contents=file_contents,
                config=config,
                parse_full_files_fallback=fallback_with_protection,
            )
            
            # Ensure correct action based on original existence
            for f in files:
                if f.path in file_contents:
                    f.action = "modify"
                else:
                    f.action = "create"
                yield self.emit_file_generated(f)
            
            logger.info(f"[EDIT STEP] Applied {len(files)} diff(s) successfully")
            
            # Validate and fix field names (same as generate path)
            for event in self._validate_and_fix_fields(files, full_content, app, version, model):
                if isinstance(event, list):
                    files = event
                    break
                yield event
                
        except Exception as e:
            logger.error(f"[EDIT STEP] Execution error: {e}")
            yield self.emit_thinking(f"Error during edit step: {str(e)}", "reflection")
            raise

    def _build_step_edit_prompt(
        self,
        step: PlanStep,
        step_index: int,
        user_message: str,
        data_store_context: Optional[str] = None,
    ) -> tuple[str, Optional[str]]:
        """
        Build user message content for diff-based step execution.
        
        Returns the user message and extra context to pass to build_diff_prompts.
        The actual file context and diff format instructions are added by build_diff_prompts.
        
        Returns:
            Tuple of (user_message, extra_context)
        """
        # Build user message with step context
        user_message_with_step = f"""## Step {step_index + 1}: {getattr(step, 'title', '')}
Description: {getattr(step, 'description', '')}

## User's Original Request
{user_message}"""
        
        # Build extra context
        extra_context = None
        if data_store_context and "No data tables" not in data_store_context:
            extra_context = f"## Available Data Store\n{data_store_context}"
        
        return user_message_with_step, extra_context

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

            # CRITICAL: Add system columns (id, created_at, updated_at)
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
    
    def _fetch_context_files(
        self,
        data_phase_files: List[FileChange],
        generated_files: List[FileChange],
        version: Optional["AppVersion"] = None,
    ) -> List[FileChange]:
        """
        Fetch context files for validation and error fixing.
        
        Includes:
        - Data phase files (e.g., types.ts)
        - Existing version files that weren't regenerated
        
        Args:
            data_phase_files: Files generated during the data phase
            generated_files: Files being generated in this run
            version: App version to fetch existing files from
            
        Returns:
            List of FileChange objects to use as context
        """
        context_files: List[FileChange] = list(data_phase_files)
        
        # Add existing version files that weren't regenerated (provides full context for validation)
        if version:
            generated_paths = {f.path for f in generated_files}
            context_paths = {f.path for f in context_files}
            
            existing_files = VersionFile.objects.filter(app_version=version)
            for vf in existing_files:
                if vf.path not in generated_paths and vf.path not in context_paths:
                    # Determine language from extension
                    ext = vf.path.split('.')[-1] if '.' in vf.path else 'tsx'
                    lang_map = {'tsx': 'tsx', 'ts': 'ts', 'css': 'css', 'json': 'json'}
                    context_files.append(FileChange(
                        path=vf.path,
                        action='existing',
                        language=lang_map.get(ext, 'tsx'),
                        content=vf.content,
                    ))
            
            if context_files:
                logger.info(f"📁 [VALIDATION] Added {len(context_files)} context file(s): {', '.join([f.path for f in context_files])}")
        return context_files

    def _validate_and_fix(
        self,
        generated_files: List[FileChange],
        context_files: List[FileChange],
        model: AIModel,
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
            validation = self._validate_typescript(generated_files + context_files)

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
            final = self._validate_typescript(generated_files + context_files)
            validation_passed = final["passed"]

        return (validation_passed, fix_attempts)

    def _validate_typescript(self, files: List[FileChange]) -> Dict[str, Any]:
        """
        Validate TypeScript files using ValidationService.
        """
        # Debug: Check if types.ts is included in files for validation
        file_paths = [f.path for f in files]
        has_types = 'src/lib/types.ts' in file_paths
        logger.debug(f"[VALIDATION DEBUG] Files being validated: {file_paths}")
        logger.debug(f"[VALIDATION DEBUG] src/lib/types.ts included: {has_types}")

        return get_validation_service().validate_typescript(files).to_dict()
