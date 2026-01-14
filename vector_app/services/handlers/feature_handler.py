"""
Feature Handler

Adds new features to existing apps while preserving current functionality.
Creates new components and integrates them into the existing structure.
"""

import json
import logging
import re
import time
from typing import Any, Dict, Generator, List, Optional, TYPE_CHECKING

from vector_app.ai.models import AIModel
from .base_handler import BaseHandler, AgentEvent, FileChange
from .generate_handler import GenerateHandler
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
from vector_app.prompts.agentic import DESIGN_STYLE_PROMPT, OVER_EAGERNESS_GUARD


FEATURE_SYSTEM_PROMPT = f"""You are an expert React/TypeScript developer adding new features to existing applications.

{OVER_EAGERNESS_GUARD}

## Feature Addition Rules (CRITICAL)

1. **PRESERVE all existing functionality**
   - Do NOT break or remove what's already there
   - Do NOT refactor existing code while adding features
   - Keep existing patterns and conventions

2. **ADD the new feature cleanly**
   - Create new components if needed, in the components/ directory
   - Update the main App.tsx to include the new feature
   - Ensure proper state management integration
   - Follow the existing code style and patterns

3. **Stay focused on the requested feature**
   - Do NOT add related features that weren't requested
   - Do NOT add "nice to have" extras
   - Do NOT refactor or reorganize existing code
   - Complete only what was asked for

4. **Integration requirements**
   - Update imports and exports correctly
   - Maintain TypeScript types consistency
   - Ensure the new feature works with existing components

{DESIGN_STYLE_PROMPT}"""


FEATURE_ANALYSIS_PROMPT = """Analyze the existing app and plan how to add the requested feature.

## User Request
{user_message}

## Existing App Structure
{app_structure}

## Existing Main App Code
{main_app_code}

## Instructions
1. Identify where the new feature should be integrated
2. Determine what new components are needed
3. Plan the integration points

Return your analysis as JSON:
{{
    "feature_name": "Name of the feature",
    "new_components": ["ComponentName1", "ComponentName2"],
    "files_to_modify": ["src/App.tsx"],
    "integration_point": "Where in the app to add this feature",
    "reasoning": "Brief explanation of the approach"
}}"""


# Handler-specific user message template (diff format instructions added by build_diff_prompts)
FEATURE_GENERATION_PROMPT = """Add the following feature to the existing application.

## User Request
{user_message}

## Feature Analysis
{feature_analysis}
{reusable_components}
{codebase_style}

## Instructions
1. **Check for reusable components first** - extend existing components when possible
2. Create new components ONLY if no existing component can be reused
3. Update the main App.tsx to integrate the new feature
4. Preserve ALL existing functionality
5. Follow the existing code style exactly

CRITICAL:
- Output unified diffs, NOT complete file contents
- Preserve ALL existing functionality in modified files
- Match existing code style (naming, patterns, etc.)
- Do NOT add features that weren't requested"""


class FeatureHandler(BaseHandler):
    """
    Handler for adding new features to existing apps.

    This handler is used when:
    - User wants to add new functionality
    - Intent is classified as ADD_FEATURE
    - App already has existing code to build upon
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
        Execute feature addition.

        Creates new components and integrates them with existing app.
        """
        start_time = time.time()
        generated_files: List[FileChange] = []

        # Get additional context from kwargs
        data_store_context = kwargs.get('data_store_context')
        mcp_tools_context = kwargs.get('mcp_tools_context')
        
        # Debug log the context being passed to LLM
        self.log_llm_context(
            intent_name=intent.intent.value if intent else "ADD_FEATURE",
            handler_name="FeatureHandler",
            data_store_context=data_store_context,
            mcp_tools_context=mcp_tools_context,
        )
        
        # If no existing app, fall back to generation
        if not context.has_existing_app:
            yield self.emit_thinking(
                "No existing app to add features to - generating new app",
                "reflection",
            )

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
        yield self.emit_phase_change("researching", "Analyzing existing app structure...")

        # Get existing code
        main_app_code = ""
        existing_code = {}

        if version:
            existing_code = self._get_relevant_code(version, context)
            main_app_code = existing_code.get("src/App.tsx", "")
            if not main_app_code and context.entry_points:
                main_app_code = existing_code.get(context.entry_points[0], "")

        app_structure = self._build_app_structure(context)

        yield self.emit_thinking(
            f"Analyzed app with {len(context.existing_files)} files and {len(context.entry_points)} entry points",
            "observation",
        )

        # Analyze how to add the feature
        feature_analysis = self._analyze_feature(
            user_message=user_message,
            app_structure=app_structure,
            main_app_code=main_app_code,
            model=model,
        )

        yield self.emit_thinking(
            f"Planning to add: {feature_analysis.get('feature_name', 'new feature')}",
            "decision",
        )

        # ===== PHASE 2: PLAN =====
        yield self.emit_phase_change("planning", "Planning feature implementation...")

        new_components = feature_analysis.get("new_components", [])
        files_to_modify = feature_analysis.get("files_to_modify", ["src/App.tsx"])

        plan_steps = []

        # Step for each new component
        if new_components:
            plan_steps.append(
                self.create_step(
                    "component", "Create New Components", f"Create {len(new_components)} new component(s)"
                )
            )

        # Step for integration
        plan_steps.append(
            self.create_step(
                "integration", "Integrate Feature", f"Update {len(files_to_modify)} existing file(s)"
            )
        )

        # Validation step
        plan_steps.append(
            self.create_step("validation", "Validate Changes", "Ensure feature works with existing code")
        )

        yield self.emit_plan_created(
            steps=plan_steps,
            explored_dirs=2,
            explored_files=len(context.existing_files),
            searches=1,
        )

        # ===== PHASE 3: EXECUTE =====
        yield self.emit_phase_change("executing", "Adding new feature...")

        step_idx = 0

        # Generate new components and updates
        for i, step in enumerate(plan_steps[:-1]):  # Skip validation step
            step_start = time.time()

            yield self.emit_step_started(step, step_idx)
            yield self.emit_step_start(step, step_idx)

            try:
                if step.type == "component":
                    # Generate new components
                    new_files = yield from self._generate_feature(
                        user_message=user_message,
                        feature_analysis=feature_analysis,
                        existing_code=existing_code,
                        model=model,
                        context=context,
                        data_store_context=data_store_context,
                        mcp_tools_context=mcp_tools_context,
                    )
                    generated_files.extend(new_files)

                elif step.type == "integration":
                    # If we didn't get files in the component step, generate now
                    if not generated_files:
                        new_files = yield from self._generate_feature(
                            user_message=user_message,
                            feature_analysis=feature_analysis,
                            existing_code=existing_code,
                            model=model,
                            context=context,
                            data_store_context=data_store_context,
                            mcp_tools_context=mcp_tools_context,
                        )
                        generated_files.extend(new_files)
                
                step.status = PlanStepStatus.COMPLETE
                step.duration = int((time.time() - step_start) * 1000)
                
                yield self.emit_step_completed(step, step_idx)
                yield self.emit_step_complete(step_idx, PlanStepStatus.COMPLETE.value, step.duration)
                
            except Exception as e:
                logger.error(f"Feature step error: {e}")
                step.status = PlanStepStatus.ERROR
                yield self.emit_step_completed(step, step_idx)
                yield self.emit_step_complete(step_idx, PlanStepStatus.ERROR.value, int((time.time() - step_start) * 1000))
            
            step_idx += 1

        # ===== PHASE 4: VALIDATE & FIX =====
        validation_step = plan_steps[-1]
        step_start = time.time()

        yield self.emit_step_started(validation_step, step_idx)
        yield self.emit_step_start(validation_step, step_idx)

        # Merge and validate with error fixing
        all_files = self._merge_with_existing(version, context, generated_files)

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

        validation_step.duration = int((time.time() - step_start) * 1000)
        yield self.emit_step_completed(validation_step, step_idx)
        yield self.emit_step_complete(step_idx, PlanStepStatus.COMPLETE.value, validation_step.duration)
        
        yield self.emit_validation_result(
            passed=validation_passed,
            fix_attempts=fix_attempts,
        )

        return generated_files

    def _build_app_structure(self, context: "AppContext") -> str:
        """Build a description of the app structure."""
        parts = []

        # Group files by directory
        dirs = {}
        for f in context.existing_files:
            dir_path = "/".join(f.path.split("/")[:-1])
            if dir_path not in dirs:
                dirs[dir_path] = []
            dirs[dir_path].append(f.path.split("/")[-1])

        for dir_path, files in sorted(dirs.items()):
            parts.append(f"{dir_path}/")
            for file in files[:10]:  # Limit files shown
                parts.append(f"  - {file}")
            if len(files) > 10:
                parts.append(f"  ... and {len(files) - 10} more")

        # Add component exports
        components = [f for f in context.existing_files if f.is_component]
        if components:
            parts.append("\nComponents:")
            for c in components[:10]:
                exports = ", ".join(c.exports[:3]) if c.exports else "unknown"
                parts.append(f"  - {c.path}: exports {exports}")

        return "\n".join(parts)

    def _get_relevant_code(
        self,
        version: "AppVersion",
        context: "AppContext",
    ) -> Dict[str, str]:
        """Get relevant code files for the feature addition."""
        code = {}

        try:
            # Get entry points first
            target_files = list(context.entry_points)

            # Also get component files
            for f in context.existing_files:
                if f.is_component and f.path not in target_files:
                    target_files.append(f.path)

            # Limit to prevent too much context
            target_files = target_files[:10]

            for vf in version.files.filter(path__in=target_files):
                code[vf.path] = vf.content or ""

        except Exception as e:
            logger.warning(f"Error getting code: {e}")

        return code

    def _analyze_feature(
        self,
        user_message: str,
        app_structure: str,
        main_app_code: str,
        model: AIModel,
    ) -> Dict[str, Any]:
        """Analyze how to add the feature.

        Note: This is NOT a generator - returns a dict directly.
        """
        prompt = FEATURE_ANALYSIS_PROMPT.format(
            user_message=user_message,
            app_structure=app_structure,
            main_app_code=main_app_code[:3000] if main_app_code else "No main app code found",
        )

        try:
            response = self.call_llm(
                system_prompt="You are an expert at planning feature additions to React apps.",
                user_prompt=prompt,
                model=model,
                temperature=0.2,
            )

            # Parse JSON
            json_match = re.search(r"\{.*\}", response, re.DOTALL)
            if json_match:
                return json.loads(json_match.group())

        except Exception as e:
            logger.warning(f"Feature analysis error: {e}")

        # Default analysis
        return {
            "feature_name": "New Feature",
            "new_components": [],
            "files_to_modify": ["src/App.tsx"],
            "integration_point": "Main app component",
            "reasoning": "Adding feature to main app",
        }

    def _generate_feature(
        self,
        user_message: str,
        feature_analysis: Dict[str, Any],
        existing_code: Dict[str, str],
        model: AIModel,
        context: Optional["AppContext"] = None,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """Generate the new feature code."""
        # Convert Dict[str, str] to List[FileChange]
        file_changes = []
        for path, content in existing_code.items():
            ext = path.split('.')[-1] if '.' in path else 'tsx'
            lang_map = {'tsx': 'tsx', 'ts': 'ts', 'jsx': 'jsx', 'js': 'js', 'css': 'css', 'json': 'json'}
            file_changes.append(FileChange(
                path=path,
                action="modify",
                language=lang_map.get(ext, 'tsx'),
                content=content,
                previous_content=content,
            ))

        feature_analysis_str = json.dumps(feature_analysis, indent=2)

        # Build reusable components section
        reusable_components = ""
        codebase_style = ""
        if context:
            analyzer = get_context_analyzer()

            # Find reusable components
            reusable_prompt = analyzer.build_reusable_components_prompt(context, user_message)
            if reusable_prompt:
                reusable_components = f"\n{reusable_prompt}\n"

            # Get codebase style
            if context.codebase_style:
                codebase_style = f"\n{context.codebase_style.to_prompt_context()}\n"

        # Build user message using handler-specific template
        user_message_with_feature = FEATURE_GENERATION_PROMPT.format(
            user_message=user_message,
            feature_analysis=feature_analysis_str,
            reusable_components=reusable_components,
            codebase_style=codebase_style,
        )

        # Build extra context
        extra_context_parts = []
        if data_store_context and "No data tables" not in data_store_context:
            extra_context_parts.append(f"## Available Data Store\n{data_store_context}")
        if mcp_tools_context:
            extra_context_parts.append(f"## Available Integrations\n{mcp_tools_context}")
        extra_context = "\n\n".join(extra_context_parts) if extra_context_parts else None

        # Build prompts using centralized method
        system_prompt, user_prompt = build_diff_prompts(
            edit_style_prompt=FEATURE_SYSTEM_PROMPT,
            file_changes=file_changes,
            user_message=user_message_with_feature,
            allow_new_files=True,  # Feature handler can create new files
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
                temperature=0.3,
            ):
                full_content += chunk
                chunk_count += 1

                # Real-time validation during streaming
                streaming_warnings = validator.check_chunk(chunk, full_content)
                for warning in streaming_warnings:
                    yield self.emit_streaming_warning(warning)

                if chunk_count % 15 == 0:
                    yield self.emit_step_progress(0, min(85, chunk_count), "Generating feature...")

            # Final validation check
            final_warnings = validator.final_check(full_content)
            for warning in final_warnings:
                yield self.emit_streaming_warning(warning)

            # Apply diffs using centralized service
            config = DiffApplicationConfig(
                protected_files=set(),        # Feature handler doesn't protect any files
                normalize_paths=False,        # Paths match exactly from context
                allow_new_files=True,         # Features often create new components
                fallback_to_full_file=True,   # Backwards compatibility
                verify_changes=False,         # Apply all diffs regardless
            )
            
            files = _apply_diffs_from_llm_response(
                llm_response=full_content,
                file_contents=existing_code,
                config=config,
                parse_full_files_fallback=self.parse_code_blocks,
            )
            
            # Emit file_generated events for each file
            for file_change in files:
                yield self.emit_file_generated(file_change)

            return files

        except Exception as e:
            logger.error(f"Feature generation error: {e}")
            raise

    def _merge_with_existing(
        self,
        version: Optional["AppVersion"],
        context: "AppContext",
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
            except Exception:
                pass

        all_files.extend(new_files)
        return all_files

    def _quick_validate(self, files: List[FileChange]) -> bool:
        """Quick validation of files."""
        for f in files:
            if not f.content or len(f.content) < 10:
                continue

            if f.language in ("tsx", "ts"):
                # Basic bracket matching
                if abs(f.content.count("{") - f.content.count("}")) > 2:
                    return False
                if abs(f.content.count("(") - f.content.count(")")) > 2:
                    return False

        return True
