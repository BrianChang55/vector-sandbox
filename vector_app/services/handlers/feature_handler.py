"""
Feature Handler

Adds new features to existing apps while preserving current functionality.
Creates new components and integrates them into the existing structure.
"""

import json
import logging
import re
import time
import uuid
from enum import StrEnum
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
from vector_app.services.planning_service import (
    PlanStep,
    PlanStepStatus,
    PlanOperationType,
    get_planning_service,
    AgentPlan,
)
from vector_app.services.intent_classifier import UserIntent
from vector_app.services.schema_extraction_service import get_schema_extraction_service
from vector_app.services.datastore.table_creator import create_tables_from_definitions
from vector_app.services.datastore import build_data_store_context

if TYPE_CHECKING:
    from vector_app.models import InternalApp, AppVersion
    from vector_app.services.intent_classifier import IntentResult
    from vector_app.services.context_analyzer import AppContext

logger = logging.getLogger(__name__)


# Import design style and guards from prompts
from vector_app.prompts.agentic.execution import apply_design_style_prompt
from vector_app.prompts.agentic.design import DESIGN_STYLE_PROMPT
from vector_app.prompts.agentic.guards import OVER_EAGERNESS_GUARD


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

## Step Instructions
{step_warning}
{step_description}

## Target Files
{target_files}
{reusable_components}
{codebase_style}

## Instructions
1. **Check for reusable components first** - extend existing components when possible
2. Create new components ONLY if no existing component can be reused
3. Preserve ALL existing functionality
4. Follow the existing code style exactly

CRITICAL:
- Output unified diffs, NOT complete file contents
- Preserve ALL existing functionality in modified files
- Match existing code style (naming, patterns, etc.)
- Do NOT add features that weren't requested
- ONLY modify files listed in Target Files above"""


class DefaultStepType(StrEnum):
    COMPONENT = "component"
    INTEGRATION = "integration"
    CODE = "code"
    STYLING = "styling"
    VALIDATION = "validation"


# Step-type specific scope restriction warnings
STEP_TYPE_WARNINGS: Dict[DefaultStepType, str] = {
    DefaultStepType.COMPONENT: (
        "SCOPE RESTRICTION: You are creating/modifying COMPONENTS ONLY.\n"
        "- DO NOT modify App.tsx or routing\n"
        "- DO NOT add integration logic\n"
        "- Focus ONLY on the component files in target_files"
    ),
    DefaultStepType.INTEGRATION: (
        "SCOPE RESTRICTION: You are handling INTEGRATION ONLY.\n"
        "- DO NOT create new component files\n"
        "- DO NOT refactor existing component internals\n"
        "- Focus ONLY on wiring up existing components in target_files"
    ),
    DefaultStepType.CODE: (
        "SCOPE RESTRICTION: You are writing UTILITY/SERVICE CODE ONLY.\n"
        "- DO NOT modify UI components\n"
        "- DO NOT touch App.tsx\n"
        "- Focus ONLY on the service/utility files in target_files"
    ),
    DefaultStepType.STYLING: (
        "SCOPE RESTRICTION: You are handling STYLING ONLY.\n"
        "- DO NOT modify component logic\n"
        "- DO NOT add new functionality\n"
        "- Focus ONLY on CSS/styling in target_files"
    ),
    DefaultStepType.VALIDATION: (
        "SCOPE RESTRICTION: You are VALIDATING existing changes.\n"
        "- DO NOT add new features\n"
        "- DO NOT refactor code\n"
        "- Only fix breaking issues if found"
    ),
}


# =============================================================================
# Step Description Templates
# =============================================================================

COMPONENT_STEP_TEMPLATE = """{warning}

## Feature Context
Feature: {feature_name}
User Request: {user_message}
Reasoning: {reasoning}

## Feature Analysis
{feature_analysis}

## Your Task
Create the following new component(s):
{components_list}

Place them in src/components/ directory. Do NOT integrate them into App.tsx yet - that will be done in the next step.
"""

INTEGRATION_STEP_TEMPLATE = """{warning}

## Feature Context
Feature: {feature_name}
User Request: {user_message}
Integration Point: {integration_point}
Reasoning: {reasoning}

## Feature Analysis
{feature_analysis}

## Your Task
{integration_task}
{components_list}

Modify the following files to complete the integration:
{files_list}

Ensure the feature is properly wired up and accessible from the UI.
"""

VALIDATION_STEP_TEMPLATE = """{warning}

## Feature Context
Feature: {feature_name}

## Your Task
Validate that the feature implementation is complete and working:
- Check that all imports are correct
- Verify TypeScript types are consistent
- Ensure no existing functionality was broken
"""


def _format_list(items: List[str], prefix: str = "- ") -> str:
    """Format a list of items as a bulleted string."""
    if not items:
        return ""
    return "\n".join(f"{prefix}{item}" for item in items)


def _get_step_warning(step_type: DefaultStepType) -> str:
    """Get the scope restriction warning for a step type.
    
    Args:
        step_type: The step type as a string (e.g., "component", "integration")
        
    Returns:
        The warning string for the step type, or empty string if unknown
    """
    try:
        return STEP_TYPE_WARNINGS[DefaultStepType(step_type)]
    except ValueError:
        return ""


def _build_component_step_description(
    feature_name: str,
    user_message: str,
    reasoning: str,
    new_components: List[str],
    feature_analysis: Dict[str, Any],
) -> str:
    """Build the description for a component creation step."""
    return COMPONENT_STEP_TEMPLATE.format(
        warning=_get_step_warning(DefaultStepType.COMPONENT),
        feature_name=feature_name,
        user_message=user_message,
        reasoning=reasoning,
        components_list=_format_list(new_components),
        feature_analysis=str(feature_analysis),
    )


def _build_integration_step_description(
    feature_name: str,
    user_message: str,
    integration_point: str,
    reasoning: str,
    new_components: List[str],
    files_to_modify: List[str],
    feature_analysis: Dict[str, Any],
) -> str:
    """Build the description for an integration step."""
    if new_components:
        integration_task = "Import and integrate the newly created components:"
        components_list = _format_list(new_components)
    else:
        integration_task = "Implement the feature directly in existing files:"
        components_list = ""
    
    return INTEGRATION_STEP_TEMPLATE.format(
        warning=_get_step_warning(DefaultStepType.INTEGRATION),
        feature_name=feature_name,
        user_message=user_message,
        integration_point=integration_point,
        reasoning=reasoning,
        integration_task=integration_task,
        components_list=components_list,
        files_list=_format_list(files_to_modify),
        feature_analysis=str(feature_analysis),
    )


def _build_validation_step_description(feature_name: str) -> str:
    """Build the description for a validation step."""
    return VALIDATION_STEP_TEMPLATE.format(
        warning=_get_step_warning(DefaultStepType.VALIDATION),
        feature_name=feature_name,
    )


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

        # ===== PHASE 2: PLAN using Planning Service =====
        yield self.emit_phase_change("planning", "Planning feature implementation...")
        yield self.emit_thinking("Breaking down the feature into executable steps...", "decision")

        # Apply design style to user message
        styled_user_message = apply_design_style_prompt(
            user_message,
            # do not pass table context to plan prompt
            data_store_context=None,
            connectors_context=mcp_tools_context,
        )
        styled_user_message_with_data_store = apply_design_style_prompt(
            styled_user_message,
            data_store_context=data_store_context,
            connectors_context=mcp_tools_context,
        )

        # Build context for planning
        plan_context = {
            "app_name": app_name,
            "has_existing_spec": current_spec is not None,
            "available_resources": list(registry_surface.get("resources", [])),
            "data_store_summary": data_store_context[:500] if data_store_context else "",
            "connectors_summary": mcp_tools_context[:500] if mcp_tools_context else "",
        }

        # Get existing file names for the feature planning prompt
        existing_file_names: List[str] = []
        if context.existing_files:
            existing_file_names = [f.path for f in context.existing_files]

        # Generate plan using the planning service with ADD_FEATURE intent
        plan: Optional[AgentPlan] = None
        used_default_plan: bool = False
        try:
            planning_service = get_planning_service()
            plan = planning_service.create_plan(
                styled_user_message,
                plan_context,
                model,
                intent_type=UserIntent.ADD_FEATURE,
                existing_files=existing_file_names,
            )
            plan_steps = plan.steps
        except Exception as e:
            logger.error(f"Planning service failed: {e}, using fallback plan")
            used_default_plan = True
            plan_steps = self._default_feature_planning(
                user_message=user_message,
                app_structure=app_structure,
                main_app_code=main_app_code,
                model=model,
            )

        yield self.emit_plan_created(
            steps=plan_steps,
            explored_dirs=2,
            explored_files=len(context.existing_files),
            searches=1,
        )

        # ===== SCHEMA EXTRACTION: Create tables from plan if needed =====
        if app is not None and plan is not None:
            logger.info("[FeatureHandler] Starting schema extraction from feature plan")
            schema_extraction_service = get_schema_extraction_service()
            try:
                schema_content = schema_extraction_service.extract_schema_from_plan(
                    plan,
                    styled_user_message_with_data_store,
                    model
                )
                logger.info(f"[FeatureHandler] Schema extraction complete, content_length={len(schema_content) if schema_content else 0}")

                # Parse and create tables if schema was extracted
                if schema_content and "NO_TABLES_NEEDED" not in schema_content.upper():
                    logger.debug("[FeatureHandler] Parsing table definitions from schema")

                    # Extract table definitions from the LLM response
                    table_definitions = schema_extraction_service.parse_table_definitions(schema_content)

                    if table_definitions:
                        table_slugs = [t['slug'] for t in table_definitions]
                        logger.info(f"[FeatureHandler] Creating {len(table_definitions)} tables: {table_slugs}")
                        # Create tables from the definitions
                        created_tables = create_tables_from_definitions(app, table_definitions)
                        logger.info(f"[FeatureHandler] Created {len(created_tables)} tables: {[t.slug for t in created_tables]}")

                        # Refresh data store context after creating tables
                        data_store_context = build_data_store_context(app)
                        logger.debug("[FeatureHandler] Refreshed data store context with newly created tables")
                    else:
                        logger.debug("[FeatureHandler] No new table definitions found in feature plan")
                else:
                    logger.debug("[FeatureHandler] No new database tables needed for this feature")
            except Exception as e:
                logger.error(f"[FeatureHandler] Schema extraction failed: {e}", exc_info=True)
        else:
            logger.debug("[FeatureHandler] Skipping schema extraction: app or plan not provided")

        # ===== PHASE 3: EXECUTE =====
        yield self.emit_phase_change("executing", "Adding new feature...")

        yield self.emit_thinking(
            f"Executing {len(plan_steps)} plan steps for feature implementation",
            "decision",
        )

        step_idx = 0

        # Generate new components and updates
        execute_plan_steps = plan_steps[:-1] if used_default_plan else plan_steps
        for i, step in enumerate(execute_plan_steps):  # Skip validation step
            step_start = time.time()

            yield self.emit_step_started(step, step_idx)
            yield self.emit_step_start(step, step_idx)

            try:
                if step.type in (DefaultStepType.COMPONENT, DefaultStepType.CODE, DefaultStepType.INTEGRATION, DefaultStepType.STYLING):
                    # Generate new components or code files
                    new_files = yield from self._generate_feature(
                        user_message=user_message,
                        plan_step=step,
                        existing_code=existing_code,
                        model=model,
                        context=context,
                        data_store_context=data_store_context,
                        mcp_tools_context=mcp_tools_context,
                        used_default_plan=used_default_plan,
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

    def _default_feature_planning(
        self,
        user_message: str,
        app_structure: str,
        main_app_code: str,
        model: str,
    ) -> List[PlanStep]:
        """Create a default feature plan when the planning service fails.
        
        Uses feature analysis to generate plan steps with embedded context and warnings.
        Each step description includes the analysis context and scope restrictions.
        
        Args:
            user_message: The user's feature request
            app_structure: Description of the app structure
            main_app_code: Content of the main App.tsx
            model: LLM model to use for analysis
            
        Returns:
            List of PlanStep objects for the feature
        """
        feature_analysis = self._analyze_feature(
            user_message=user_message,
            app_structure=app_structure,
            main_app_code=main_app_code,
            model=model,
        )

        feature_name = feature_analysis.get("feature_name", "New Feature")
        new_components = feature_analysis.get("new_components", [])
        files_to_modify = feature_analysis.get("files_to_modify", ["src/App.tsx"])
        integration_point = feature_analysis.get("integration_point", "Main app component")
        reasoning = feature_analysis.get("reasoning", "Adding feature to main app")
        
        # Build component file paths
        component_files = [f"src/components/{comp}.tsx" for comp in new_components]
        
        plan_steps: List[PlanStep] = []
        step_order = 0
        
        # Component creation step (if new components needed)
        if new_components:
            plan_steps.append(
                PlanStep(
                    id=str(uuid.uuid4()),
                    type=DefaultStepType.COMPONENT,
                    title="Create New Components",
                    description=_build_component_step_description(
                        feature_name=feature_name,
                        user_message=user_message,
                        reasoning=reasoning,
                        new_components=new_components,
                        feature_analysis=feature_analysis,
                    ),
                    step_order=step_order,
                    target_files=component_files,
                    operation_type=PlanOperationType.GENERATE,
                )
            )
            step_order += 1
        
        # Integration step
        plan_steps.append(
            PlanStep(
                id=str(uuid.uuid4()),
                type=DefaultStepType.INTEGRATION,
                title="Integrate Feature",
                description=_build_integration_step_description(
                    feature_name=feature_name,
                    user_message=user_message,
                    integration_point=integration_point,
                    reasoning=reasoning,
                    new_components=new_components,
                    files_to_modify=files_to_modify,
                    feature_analysis=feature_analysis,
                ),
                step_order=step_order,
                target_files=files_to_modify,
                operation_type=PlanOperationType.EDIT,
            )
        )
        step_order += 1
        
        # Validation step
        plan_steps.append(
            PlanStep(
                id=str(uuid.uuid4()),
                type=DefaultStepType.VALIDATION,
                title="Validate Changes",
                description=_build_validation_step_description(feature_name),
                step_order=step_order,
                target_files=component_files + files_to_modify,
                operation_type=PlanOperationType.FIX,
            )
        )
        
        return plan_steps

    def _generate_feature(
        self,
        user_message: str,
        plan_step: PlanStep,
        existing_code: Dict[str, str],
        model: AIModel,
        context: Optional["AppContext"] = None,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
        used_default_plan: bool = False,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """Generate the new feature code for a specific plan step.
        
        Args:
            user_message: The original user request
            plan_step: The current plan step with description and target_files
            existing_code: Dict of file paths to their current content
            model: LLM model to use
            context: Optional app context for reusable components
            data_store_context: Optional data store context
            mcp_tools_context: Optional MCP tools context
            
        Returns:
            List of FileChange objects with generated/modified code
        """
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

        # Get step-type warning (default to empty if unknown type)
        step_warning = _get_step_warning(plan_step.type) if not used_default_plan else ""
        
        # Format target files list
        target_files_str = _format_list(plan_step.target_files) if plan_step.target_files else "- (determined by step description)"

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
            step_warning=step_warning,
            step_description=plan_step.description,
            target_files=target_files_str,
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
