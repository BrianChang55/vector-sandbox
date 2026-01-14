"""
Schema Handler

Handles data model changes - creating tables, modifying schemas,
and updating code that depends on those schemas.
"""

import logging
import re
import time
from typing import Any, Dict, Generator, List, Optional, TYPE_CHECKING

from vector_app.ai.models import AIModel
from .base_handler import BaseHandler, AgentEvent, FileChange
from .generate_handler import GenerateHandler
from ..datastore import TableDefinitionParser
from vector_app.models import AppDataTable
from vector_app.prompts.agentic import DESIGN_STYLE_PROMPT
from vector_app.services.app_data_service import AppDataService
from vector_app.services.planning_service import PlanStepStatus

if TYPE_CHECKING:
    from vector_app.models import InternalApp, AppVersion
    from vector_app.services.intent_classifier import IntentResult
    from vector_app.services.context_analyzer import AppContext

logger = logging.getLogger(__name__)


SCHEMA_SYSTEM_PROMPT = """You are an expert at designing data schemas for internal business applications.

When creating or modifying tables:
1. Use clear, descriptive column names
2. Include appropriate data types
3. Add sensible defaults where appropriate
4. Consider relationships between tables
5. Follow naming conventions (snake_case for columns, kebab-case for slugs)

Supported column types:
- uuid: UUID identifier (use for primary keys)
- string: Short text (max 255 chars)
- text: Long text (unlimited)
- integer: Whole numbers
- float: Decimal numbers
- boolean: True/false
- datetime: Date and time
- date: Date only
- enum: Fixed values (requires enum_values)
- json: Arbitrary JSON data"""


SCHEMA_CHANGE_PROMPT = """Make the following data schema change:

## User Request
{user_message}

## Existing Tables
{tables_context}

## Instructions

Based on the user's request, determine what schema changes are needed:

1. If CREATING a new table, output a TABLE_DEFINITION block:
```table:table-slug
name: Display Name
description: What this table stores
columns:
  - name: id, type: uuid, primary_key: true, auto_generate: true
  - name: column_name, type: string, nullable: false
  - name: created_at, type: datetime, auto_now_add: true
```

2. If MODIFYING an existing table, describe the changes and output the updated TABLE_DEFINITION.

3. After schema changes, if code needs to be updated to use the new/modified table,
   output the updated code files.

## Output Format

First output any TABLE_DEFINITION blocks for new/modified tables.
Then output any code files that need updating:

```filepath:src/components/DataDisplay.tsx
// Updated component code
```"""


CODE_UPDATE_PROMPT = f"""Update the existing code to use the new/modified data schema.

## Schema Changes
{{schema_changes}}

## Existing Code
{{code_context}}

## User Request
{{user_message}}

## Instructions
1. Update any code that references the modified tables/columns
2. Add new data fetching if new tables were created
3. Update TypeScript types if needed
4. Preserve existing functionality

{DESIGN_STYLE_PROMPT}

Output the complete updated files:

```filepath:path/to/file.tsx
// Complete updated file content
```"""


class SchemaHandler(BaseHandler):
    """
    Handler for data schema modifications.

    This handler is used when:
    - User wants to create new tables
    - User wants to add/remove/modify columns
    - Intent is classified as MODIFY_SCHEMA
    """

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
        Execute schema modifications.

        First modifies the data schema, then updates dependent code.
        """
        start_time = time.time()
        generated_files: List[FileChange] = []

        # Get additional context from kwargs
        data_store_context = kwargs.get('data_store_context')
        mcp_tools_context = kwargs.get('mcp_tools_context')
        
        # Debug log the context being passed to LLM
        self.log_llm_context(
            intent_name=intent.intent.value if intent else "MODIFY_SCHEMA",
            handler_name="SchemaHandler",
            data_store_context=data_store_context,
            mcp_tools_context=mcp_tools_context,
        )
        
        if not app or not version:
            yield self.emit_thinking(
                "Cannot modify schema without app context - falling back to generation",
                "reflection",
            )
            # Fallback to generate handler
            gen_handler = GenerateHandler()
            yield from gen_handler.execute(
                intent,
                context,
                user_message,
                current_spec,
                registry_surface,
                app_name,
                model,
                app,
                version,
                **kwargs,
            )
            return generated_files

        # ===== PHASE 1: ANALYZE =====
        yield self.emit_phase_change("researching", "Analyzing schema requirements...")

        # Get existing tables
        tables_context = self._build_tables_context(context)

        yield self.emit_thinking(
            f"Analyzing schema changes for {len(context.existing_tables)} existing tables",
            "observation",
        )

        # Determine what needs to change
        affected_tables = intent.affected_tables if intent.affected_tables else []

        # ===== PHASE 2: PLAN =====
        yield self.emit_phase_change("planning", "Planning schema modifications...")

        plan_steps = [
            self.create_step("data", "Apply Schema Changes", "Create or modify data tables"),
        ]

        # Add code update step if there's existing code
        if context.has_existing_app:
            plan_steps.append(
                self.create_step("code", "Update Dependent Code", "Update code that uses the modified tables")
            )

        plan_steps.append(
            self.create_step("validation", "Validate Changes", "Ensure schema and code are consistent")
        )

        yield self.emit_plan_created(
            steps=plan_steps,
            explored_dirs=1,
            explored_files=len(context.existing_files),
            searches=1,
        )

        # ===== PHASE 3: EXECUTE SCHEMA CHANGES =====
        yield self.emit_phase_change("executing", "Applying schema changes...")

        step = plan_steps[0]
        step_start = time.time()

        yield self.emit_step_started(step, 0)
        yield self.emit_step_start(step, 0)

        try:
            schema_changes = yield from self._apply_schema_changes(
                user_message=user_message,
                tables_context=tables_context,
                model=model,
                app=app,
                version=version,
            )
            
            step.status = PlanStepStatus.COMPLETE
            step.duration = int((time.time() - step_start) * 1000)
            
            yield self.emit_step_completed(step, 0)
            yield self.emit_step_complete(0, PlanStepStatus.COMPLETE.value, step.duration)
            
        except Exception as e:
            logger.error(f"Schema change error: {e}")
            step.status = PlanStepStatus.ERROR
            yield self.emit_step_complete(0, PlanStepStatus.ERROR.value, int((time.time() - step_start) * 1000))
            yield self.emit_thinking(f"Error during schema change: {str(e)}", "reflection")
            return generated_files

        # ===== PHASE 4: UPDATE DEPENDENT CODE =====
        if context.has_existing_app and len(plan_steps) > 2:
            step = plan_steps[1]
            step_start = time.time()

            yield self.emit_step_started(step, 1)
            yield self.emit_step_start(step, 1)

            try:
                code_files = yield from self._update_dependent_code(
                    user_message=user_message,
                    schema_changes=schema_changes,
                    context=context,
                    version=version,
                    model=model,
                    data_store_context=data_store_context,
                    mcp_tools_context=mcp_tools_context,
                )

                generated_files.extend(code_files)
                
                step.status = PlanStepStatus.COMPLETE
                step.duration = int((time.time() - step_start) * 1000)
                
                yield self.emit_step_completed(step, 1)
                yield self.emit_step_complete(1, PlanStepStatus.COMPLETE.value, step.duration)
                
            except Exception as e:
                logger.error(f"Code update error: {e}")
                step.status = PlanStepStatus.ERROR
                yield self.emit_step_complete(1, PlanStepStatus.ERROR.value, int((time.time() - step_start) * 1000))
        
        # ===== PHASE 5: VALIDATE & FIX =====
        validation_step_idx = len(plan_steps) - 1
        step = plan_steps[validation_step_idx]
        step_start = time.time()

        yield self.emit_step_started(step, validation_step_idx)
        yield self.emit_step_start(step, validation_step_idx)

        # Run TypeScript validation with error fixing if we have generated files
        validation_passed = True
        fix_attempts = 0

        if generated_files:
            all_files = self._merge_with_existing(context, version, generated_files)

            validation_passed, fix_attempts = yield from self.validate_and_fix(
                generated_files=all_files,
                model=model,
            )

            # Update generated_files with any fixes
            generated_paths = {f.path for f in generated_files}
            for f in all_files:
                if f.path in generated_paths:
                    for i, gf in enumerate(generated_files):
                        if gf.path == f.path:
                            generated_files[i] = f
                            break
        else:
            # Schema-only changes are valid if we applied any
            validation_passed = len(schema_changes) > 0

        step.duration = int((time.time() - step_start) * 1000)
        yield self.emit_step_completed(step, validation_step_idx)
        yield self.emit_step_complete(validation_step_idx, PlanStepStatus.COMPLETE.value, step.duration)
        
        yield self.emit_validation_result(
            passed=validation_passed,
            fix_attempts=fix_attempts,
        )

        return generated_files

    def _build_tables_context(self, context: "AppContext") -> str:
        """Build a description of existing tables."""
        if not context.existing_tables:
            return "No existing tables."

        parts = []
        for table in context.existing_tables:
            columns_str = ", ".join(table.columns[:10])
            if len(table.columns) > 10:
                columns_str += f", ... (+{len(table.columns) - 10} more)"

            parts.append(
                f"- **{table.name}** (`{table.slug}`): {table.column_count} columns\n"
                f"  Columns: {columns_str}"
            )

        return "\n".join(parts)

    def _apply_schema_changes(
        self,
        user_message: str,
        tables_context: str,
        model: AIModel,
        app: "InternalApp",
        version: "AppVersion",
    ) -> Generator[AgentEvent, None, List[Dict[str, Any]]]:
        """Generate and apply schema changes."""
        applied_changes = []

        prompt = SCHEMA_CHANGE_PROMPT.format(
            user_message=user_message,
            tables_context=tables_context,
        )

        try:
            full_content = ""

            for chunk in self.stream_llm_response(
                system_prompt=SCHEMA_SYSTEM_PROMPT,
                user_prompt=prompt,
                model=model,
                temperature=0.2,
            ):
                full_content += chunk

            # Parse table definitions
            table_defs = self._parse_table_definitions(full_content)

            # Apply table definitions
            for table_def in table_defs:
                result = yield from self._create_or_update_table(
                    table_def=table_def,
                    app=app,
                    version=version,
                )
                if result:
                    applied_changes.append(result)

            # Also parse any code files
            code_files = self.parse_code_blocks(full_content)
            for f in code_files:
                yield self.emit_file_generated(f)

            return applied_changes

        except Exception as e:
            logger.error(f"Schema change generation error: {e}")
            raise

    def _parse_table_definitions(self, content: str) -> List[Dict[str, Any]]:
        """Parse TABLE_DEFINITION blocks from content."""
        return TableDefinitionParser.parse_table_definitions(content)
    
    def _create_or_update_table(
        self,
        table_def: Dict[str, Any],
        app: "InternalApp",
        version: "AppVersion",
    ) -> Generator[AgentEvent, None, Optional[Dict[str, Any]]]:
        """Create or update a table based on definition."""
        slug = table_def["slug"]

        # Check if table exists
        existing = AppDataTable.objects.filter(
            internal_app=app,
            slug=slug,
        ).first()

        if existing:
            # Update existing table
            try:
                # Get current columns
                current_schema = existing.schema_json or {}
                current_columns = {c["name"]: c for c in current_schema.get("columns", [])}
                new_columns = table_def["columns"]

                # Determine changes
                added = []
                modified = []

                for col in new_columns:
                    col_name = col["name"]
                    if col_name not in current_columns:
                        added.append(col_name)
                    elif current_columns[col_name] != col:
                        modified.append(col_name)

                # Update schema
                existing.schema_json = {"columns": new_columns}
                existing.save()

                yield self.emit_table_updated(
                    slug=slug,
                    name=existing.name,
                    added=added,
                    modified=modified,
                )

                return {
                    "action": "updated",
                    "slug": slug,
                    "name": existing.name,
                    "added": added,
                    "modified": modified,
                }

            except Exception as e:
                logger.error(f"Error updating table {slug}: {e}")
                return None
        else:
            # Create new table
            schema = {"columns": table_def["columns"]}

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

                return {
                    "action": "created",
                    "slug": table.slug,
                    "name": table.name,
                    "columns": len(table_def["columns"]),
                }
            else:
                logger.warning(f"Failed to create table {slug}: {errors}")
                return None

    def _update_dependent_code(
        self,
        user_message: str,
        schema_changes: List[Dict[str, Any]],
        context: "AppContext",
        version: "AppVersion",
        model: AIModel,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """Update code that depends on modified schemas."""
        files = []

        if not schema_changes:
            return files

        # Build schema changes description
        changes_desc = []
        for change in schema_changes:
            if change["action"] == "created":
                changes_desc.append(f"Created table '{change['name']}' with {change['columns']} columns")
            elif change["action"] == "updated":
                added = change.get("added", [])
                modified = change.get("modified", [])
                parts = []
                if added:
                    parts.append(f"added columns: {', '.join(added)}")
                if modified:
                    parts.append(f"modified columns: {', '.join(modified)}")
                changes_desc.append(f"Updated table '{change['name']}' - {'; '.join(parts)}")

        schema_changes_text = "\n".join(changes_desc)

        # Get existing code that might need updating
        code_files = [
            f
            for f in context.existing_files
            if f.language in ("tsx", "ts") and "dataStore" in "".join(f.imports)
        ]

        if not code_files and context.entry_points:
            # At least check the entry point
            code_files = [f for f in context.existing_files if f.path in context.entry_points]

        if not code_files:
            yield self.emit_thinking("No code files need updating", "observation")
            return files

        # Get file contents
        file_contents = {}
        try:
            for vf in version.files.filter(path__in=[f.path for f in code_files]):
                file_contents[vf.path] = vf.content or ""
        except Exception as e:
            logger.warning(f"Error getting file contents: {e}")

        if not file_contents:
            return files

        # Build code context
        code_context = ""
        for path, content in file_contents.items():
            code_context += f"\n### {path}\n```\n{content[:2000]}...\n```\n"

        # Build prompt with additional context
        prompt_parts = [
            CODE_UPDATE_PROMPT.format(
                schema_changes=schema_changes_text,
                code_context=code_context,
                user_message=user_message,
            )
        ]

        # Add MCP tools context if available
        if mcp_tools_context:
            prompt_parts.append(f"\n## Available Integrations\n{mcp_tools_context}")

        prompt = "\n".join(prompt_parts)

        try:
            full_content = ""

            for chunk in self.stream_llm_response(
                system_prompt=SCHEMA_SYSTEM_PROMPT,
                user_prompt=prompt,
                model=model,
                temperature=0.3,
            ):
                full_content += chunk

            # Parse code files
            code_files = self.parse_code_blocks(full_content)

            for f in code_files:
                f.action = "modify"
                yield self.emit_file_generated(f)
                files.append(f)

            return files

        except Exception as e:
            logger.error(f"Code update generation error: {e}")
            return files

    def _merge_with_existing(
        self,
        context: "AppContext",
        version: Optional["AppVersion"],
        new_files: List[FileChange],
    ) -> List[FileChange]:
        """Merge new files with existing for validation."""
        all_files = []
        new_paths = {f.path for f in new_files}

        if version:
            try:
                for vf in version.files.all():
                    if vf.path not in new_paths:
                        all_files.append(
                            FileChange(
                                path=vf.path,
                                action="create",
                                language=vf.path.split(".")[-1] if "." in vf.path else "tsx",
                                content=vf.content or "",
                                previous_content=vf.content or "",
                            )
                        )
            except Exception as e:
                logger.warning(f"Error merging existing files: {e}")

        all_files.extend(new_files)
        return all_files
