"""
Edit Handler

Surgical code modifications without regenerating the entire app.
Used for targeted changes like styling, text updates, and small fixes.
"""

import logging
import time
from typing import Any, Dict, Generator, List, Optional, TYPE_CHECKING

from vector_app.ai.models import AIModel
from .base_handler import BaseHandler, AgentEvent, FileChange
from vector_app.services.context_analyzer import get_context_analyzer
from vector_app.services.diff_application_service import (
    _apply_diffs_from_llm_response,
    build_diff_prompts,
    DiffApplicationConfig,
)
from vector_app.services.planning_service import PlanStepStatus

if TYPE_CHECKING:
    from vector_app.models import InternalApp, AppVersion
    from vector_app.services.intent_classifier import IntentResult
    from vector_app.services.context_analyzer import AppContext

logger = logging.getLogger(__name__)


# Import design style and guards from prompts
from vector_app.prompts.agentic.design import DESIGN_STYLE_PROMPT
from vector_app.prompts.agentic.guards import OVER_EAGERNESS_GUARD


# Prompt for surgical edits using unified diff format
EDIT_SYSTEM_PROMPT = f"""You are an expert React/TypeScript developer making targeted, surgical edits to existing code using unified diffs.

{OVER_EAGERNESS_GUARD}

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

{DESIGN_STYLE_PROMPT}"""


# Handler-specific user message template (diff format instructions added by build_diff_prompts)
EDIT_PROMPT_TEMPLATE = """Make the following TARGETED change to the existing code.

## User Request
{user_message}

## Surgical Edit Instructions

1. **Analyze the request**: Identify exactly what needs to change
2. **Locate the target**: Find the specific line(s) or section(s) to modify
3. **Make minimal changes**: Change ONLY what's needed to fulfill the request
4. **Preserve everything else**: Keep all other code exactly as-is

## CRITICAL: Use ONLY the Actual File Content Above

**DO NOT HALLUCINATE OR INVENT CODE.** You MUST:
- Read the ACTUAL file contents provided above carefully
- Copy context lines EXACTLY as they appear in the file (character-for-character)
- Match the REAL structure of the file (if it's a table, use table elements; if it's divs, use divs)
- Never assume or imagine what the file looks like - USE WHAT IS PROVIDED

If the file uses `<table>`, `<tr>`, `<td>` elements, your diff must use those same elements.
If the file uses `<div>` with flex classes, your diff must match that structure.
Context lines that don't exist in the source file will cause the diff to fail.

## Critical Reminders
- Output ONLY the diff, not the complete file
- Do NOT add new features or "improvements"
- Do NOT refactor or reorganize code
- Do NOT change styling unless specifically requested
- If unsure about something, leave it unchanged
- Context lines MUST be copied exactly from the provided file - NEVER invent or hallucinate code structure"""


class EditHandler(BaseHandler):
    """
    Handler for surgical code modifications.

    This handler is used when:
    - User wants to change specific code (styling, text, behavior)
    - Intent is classified as EDIT_CODE or REFACTOR
    - Changes are targeted and don't require rebuilding the app
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
        Execute surgical code edits.

        Only modifies the files that need to change.
        """
        start_time = time.time()
        generated_files: List[FileChange] = []

        # Get additional context from kwargs
        data_store_context = kwargs.get('data_store_context')
        mcp_tools_context = kwargs.get('mcp_tools_context')
        
        # Debug log the context being passed to LLM
        self.log_llm_context(
            intent_name=intent.intent.value if intent else "EDIT_CODE",
            handler_name="EditHandler",
            data_store_context=data_store_context,
            mcp_tools_context=mcp_tools_context,
        )
        
        # ===== PHASE 1: ANALYZE =====
        yield self.emit_phase_change("researching", "Analyzing what needs to change...")

        # Determine which files to modify
        target_files = self._identify_target_files(intent, context, user_message)

        if not target_files:
            # Fallback: modify the main App.tsx
            target_files = ["src/App.tsx"]
            if context.entry_points:
                target_files = context.entry_points[:1]

        yield self.emit_thinking(
            f"Identified {len(target_files)} file(s) to modify: {', '.join(target_files)}",
            "observation",
        )

        # ===== PHASE 2: PLAN =====
        yield self.emit_phase_change("planning", "Planning modifications...")

        plan_steps = [
            self.create_step("code", "Apply Changes", f"Modify {len(target_files)} file(s) based on request"),
            self.create_step("validation", "Validate Changes", "Ensure code compiles and works correctly"),
        ]

        yield self.emit_plan_created(
            steps=plan_steps,
            explored_dirs=1,
            explored_files=len(target_files),
            searches=0,
        )

        # ===== PHASE 3: EXECUTE EDITS =====
        yield self.emit_phase_change("executing", "Applying changes...")

        step = plan_steps[0]
        step_start = time.time()

        yield self.emit_step_started(step, 0)
        yield self.emit_step_start(step, 0)

        # Get current file contents
        file_contents = self._get_file_contents(version, target_files)

        if not file_contents:
            yield self.emit_thinking(
                "Could not read existing files - generating new content",
                "reflection",
            )
        # Generate edits
        try:
            edited_files = yield from self._generate_edits(
                user_message=user_message,
                file_contents=file_contents,
                model=model,
                data_store_context=data_store_context,
                mcp_tools_context=mcp_tools_context,
            )

            generated_files.extend(edited_files)
            
            step.status = PlanStepStatus.COMPLETE
            step.duration = int((time.time() - step_start) * 1000)
            
            yield self.emit_step_completed(step, 0)
            yield self.emit_step_complete(0, PlanStepStatus.COMPLETE.value, step.duration)
            
        except Exception as e:
            logger.error(f"Edit execution error: {e}")
            step.status = PlanStepStatus.ERROR
            yield self.emit_step_complete(0, PlanStepStatus.ERROR.value, int((time.time() - step_start) * 1000))
            yield self.emit_thinking(f"Error during edit: {str(e)}", "reflection")

        # ===== PHASE 4: VALIDATE & FIX =====
        if generated_files:
            step = plan_steps[1]
            step_start = time.time()

            yield self.emit_step_started(step, 1)
            yield self.emit_step_start(step, 1)

            # Merge with existing files for validation
            all_files = self._merge_with_existing(context, version, generated_files)

            # Run TypeScript validation with error fixing
            validation_passed, fix_attempts = yield from self.validate_and_fix(
                generated_files=all_files,
                model=model,
            )

            # Update generated_files with any fixes
            generated_paths = {f.path for f in generated_files}
            for f in all_files:
                if f.path in generated_paths:
                    # Find and update the corresponding file
                    for i, gf in enumerate(generated_files):
                        if gf.path == f.path:
                            generated_files[i] = f
                            break

            step.duration = int((time.time() - step_start) * 1000)
            yield self.emit_step_completed(step, 1)
            yield self.emit_step_complete(1, PlanStepStatus.COMPLETE.value, step.duration)

            yield self.emit_validation_result(
                passed=validation_passed,
                fix_attempts=fix_attempts,
            )

        return generated_files

    def _identify_target_files(
        self,
        intent: "IntentResult",
        context: "AppContext",
        user_message: str,
    ) -> List[str]:
        """Identify which files need to be modified.

        Uses cascade detection to find all potentially affected files.
        """
        target_files = []

        # First, use files identified by intent classifier
        if intent.affected_files:
            for path in intent.affected_files:
                if path in context.file_paths:
                    target_files.append(path)

        if target_files:
            # Use cascade detection to find additional affected files
            analyzer = get_context_analyzer()

            cascade_affected = set()
            for primary_file in target_files:
                affected = analyzer.find_cascade_affected_files(context, primary_file, max_depth=1)
                cascade_affected.update(affected)

            # Add cascade files that aren't already in target_files
            for path in cascade_affected:
                if path not in target_files and path in context.file_paths:
                    target_files.append(path)

            return target_files

        # Try to find files based on keywords in the message
        message_lower = user_message.lower()

        # Map common keywords to file patterns
        keyword_patterns = {
            "header": ["Header", "header", "Nav", "nav"],
            "button": ["Button", "button"],
            "form": ["Form", "form", "Input", "input"],
            "table": ["Table", "table", "List", "list"],
            "card": ["Card", "card"],
            "modal": ["Modal", "modal", "Dialog", "dialog"],
            "sidebar": ["Sidebar", "sidebar", "Menu", "menu"],
            "footer": ["Footer", "footer"],
            "app": ["App"],
        }

        for keyword, patterns in keyword_patterns.items():
            if keyword in message_lower:
                for f in context.existing_files:
                    for pattern in patterns:
                        if pattern in f.path:
                            if f.path not in target_files:
                                target_files.append(f.path)

        # If still no matches, use main entry point
        if not target_files and context.entry_points:
            target_files = [context.entry_points[0]]

        # Fallback to App.tsx
        if not target_files:
            if "src/App.tsx" in context.file_paths:
                target_files = ["src/App.tsx"]

        return target_files

    def _get_file_contents(
        self,
        version: Optional["AppVersion"],
        file_paths: List[str],
    ) -> Dict[str, str]:
        """Get the current contents of files."""
        if not version:
            return {}

        try:
            analyzer = get_context_analyzer()
            return analyzer.get_file_contents(version, file_paths)
        except Exception as e:
            logger.warning(f"Error getting file contents: {e}")
            return {}

    def _generate_edits(
        self,
        user_message: str,
        file_contents: Dict[str, str],
        model: AIModel,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """Generate edited versions of files."""
        # Convert Dict[str, str] to List[FileChange]
        file_changes = []
        for path, content in file_contents.items():
            ext = path.split('.')[-1] if '.' in path else 'tsx'
            lang_map = {'tsx': 'tsx', 'ts': 'ts', 'jsx': 'jsx', 'js': 'js', 'css': 'css', 'json': 'json'}
            file_changes.append(FileChange(
                path=path,
                action="modify",
                language=lang_map.get(ext, 'tsx'),
                content=content,
                previous_content=content,
            ))
        
        # Build user message using handler-specific template
        formatted_user_message = EDIT_PROMPT_TEMPLATE.format(user_message=user_message)
        
        # Build extra context
        extra_context_parts = []
        if data_store_context and "No data tables" not in data_store_context:
            extra_context_parts.append(f"## Available Data Store\n{data_store_context}")
        if mcp_tools_context:
            extra_context_parts.append(f"## Available Integrations\n{mcp_tools_context}")
        extra_context = "\n\n".join(extra_context_parts) if extra_context_parts else None
        
        # Build prompts using centralized method
        system_prompt, user_prompt = build_diff_prompts(
            edit_style_prompt=EDIT_SYSTEM_PROMPT,
            file_changes=file_changes,
            user_message=formatted_user_message,
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
                    yield self.emit_step_progress(0, min(80, chunk_count), "Generating edits...")

            # Final validation check
            final_warnings = validator.final_check(full_content)
            for warning in final_warnings:
                yield self.emit_streaming_warning(warning)

            # Apply diffs using centralized service
            config = DiffApplicationConfig(
                protected_files=set(),        # Edit handler doesn't protect any files
                normalize_paths=False,        # Paths match exactly from context
                allow_new_files=False,        # Edits only modify existing files
                fallback_to_full_file=True,   # Backwards compatibility with full file output
                verify_changes=False,         # Apply all diffs regardless
            )
            
            files = _apply_diffs_from_llm_response(
                llm_response=full_content,
                file_contents=file_contents,
                config=config,
                parse_full_files_fallback=self.parse_code_blocks,
            )
            
            # Emit file_generated events for each file
            for file_change in files:
                yield self.emit_file_generated(file_change)

            return files

        except Exception as e:
            logger.error(f"Edit generation error: {e}")
            raise

    def _merge_with_existing(
        self,
        context: "AppContext",
        version: Optional["AppVersion"],
        edited_files: List[FileChange],
    ) -> List[FileChange]:
        """Merge edited files with existing files for validation."""
        all_files = []
        edited_paths = {f.path for f in edited_files}

        # Get all existing files
        if version:
            try:
                for vf in version.files.all():
                    if vf.path in edited_paths:
                        # Use edited version
                        continue
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
                logger.warning(f"Error getting existing files: {e}")

        # Add edited files
        all_files.extend(edited_files)

        return all_files

    def _quick_validate(self, files: List[FileChange]) -> bool:
        """Quick validation of edited files."""
        for f in files:
            if not f.content or len(f.content) < 10:
                return False

            # Basic syntax check for TSX/TS files
            if f.language in ("tsx", "ts"):
                # Check for obvious syntax issues
                if f.content.count("{") != f.content.count("}"):
                    logger.warning(f"Mismatched braces in {f.path}")
                    return False
                if f.content.count("(") != f.content.count(")"):
                    logger.warning(f"Mismatched parentheses in {f.path}")
                    return False

        return True
