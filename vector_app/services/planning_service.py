"""
Planning Service

Service for generating execution plans for app generation.
Uses OpenRouter API to create structured plans with steps.
"""
import json
import logging
import re
import uuid
from dataclasses import dataclass, field
from collections import deque
from typing import Any, Dict, List, Optional, Set, cast
from enum import StrEnum

from vector_app.ai.client import get_llm_client
from vector_app.ai.models import AIModel
from vector_app.ai.types import LLMSettings
from vector_app.prompts.agentic.planning import build_plan_prompt
from vector_app.services.validation_service import get_validation_service
from vector_app.services.intent_classifier import UserIntent
from vector_app.utils.enum_utils import safe_str_enum

logger = logging.getLogger(__name__)

# FORCING OPUS for plan generation
FORCE_OPUS_PLAN_GENERATION = True


class PlanStepStatus(StrEnum):
    """Status values for PlanStep execution."""
    PENDING = "pending"
    COMPLETE = "complete"
    ERROR = "error"


class PlanOperationType(StrEnum):
    """Operation types for PlanStep execution."""
    GENERATE = "generate"
    EDIT = "edit"
    ADD_FEATURE = "add_feature"
    SCHEMA = "schema"
    FIX = "fix"
    REFACTOR = "refactor"


@dataclass
class PlanStep:
    """A single step in the execution plan."""
    id: str
    type: str
    title: str
    description: str
    step_order: int = 0  # Wave number for parallel execution (0 = first)
    target_files: List[str] = field(default_factory=list)  # Files to create/modify
    dependent_files: List[str] = field(default_factory=list)  # Files this step depends on (must exist before running)
    operation_type: PlanOperationType = PlanOperationType.GENERATE
    status: PlanStepStatus = PlanStepStatus.PENDING
    duration: Optional[int] = None
    output: Optional[str] = None


def recompute_step_orders(steps: List[PlanStep]) -> List[PlanStep]:
    """
    Build dependency graph and compute optimal step_order for each step.
    Assume LLM can hallucinate dependent files that are not in the existing files.
    
    Uses the relationship between target_files and dependent_files to build
    a dependency graph, then computes the earliest possible step_order for
    each step using BFS/topological ordering.
    
    Algorithm:
    1. Build file_to_step map: which step creates each file
    2. Build step dependency graph: step -> set of steps it depends on
    3. Use BFS to assign step_order (earliest possible wave)
    
    Args:
        steps: List of PlanStep objects with target_files and dependent_files
        
    Returns:
        The same list of steps with step_order values recomputed
    """
    if not steps:
        return steps
    
    # Step 0: Find all target files, Remove Dependencies that arent in the Target Files
    all_target_files = [file for step in steps for file in step.target_files]
    for step in steps:
        step.dependent_files = [file for file in step.dependent_files if file in all_target_files]
    
    
    # Step 1: Build file_to_step map (which step creates/modifies each file)
    file_to_step: Dict[str, int] = {}
    for idx, step in enumerate(steps):
        for target_file in step.target_files:
            if target_file in file_to_step:
                # Multiple steps target the same file - use the first one
                logger.warning(
                    "File '%s' is targeted by multiple steps: %d and %d. Using step %d.",
                    target_file, file_to_step[target_file], idx, file_to_step[target_file]
                )
            else:
                file_to_step[target_file] = idx
    
    # Step 2: Build step dependency graph (step_idx -> set of step indices it depends on)
    step_dependencies: Dict[int, Set[int]] = {i: set() for i in range(len(steps))}
    
    for idx, step in enumerate(steps):
        for dep_file in step.dependent_files:
            if dep_file in file_to_step:
                dep_step_idx = file_to_step[dep_file]
                if dep_step_idx != idx:  # Don't add self-dependency
                    step_dependencies[idx].add(dep_step_idx)
            else:
                # Dependent file not created by any step - might be pre-existing
                logger.debug(
                    "Step '%s' depends on '%s' which is not created by any step (may be pre-existing)",
                    step.title, dep_file
                )
    
    # Step 3: Detect cycles using DFS
    def has_cycle() -> bool:
        """Check if the dependency graph has a cycle."""
        WHITE, GRAY, BLACK = 0, 1, 2
        color = {i: WHITE for i in range(len(steps))}
        
        def dfs(node: int) -> bool:
            color[node] = GRAY
            for neighbor in step_dependencies[node]:
                if color[neighbor] == GRAY:
                    return True  # Back edge found - cycle detected
                if color[neighbor] == WHITE and dfs(neighbor):
                    return True
            color[node] = BLACK
            return False
        
        for node in range(len(steps)):
            if color[node] == WHITE:
                if dfs(node):
                    return True
        return False
    
    if has_cycle():
        logger.warning(
            "Cycle detected in step dependencies. Keeping LLM-provided step_order values."
        )
        return steps
    
    # Step 4: Compute step_order using BFS (topological-like ordering)
    # step_order = 1 + max(step_order of all dependencies), or 0 if no dependencies
    computed_orders: Dict[int, int] = {}
    
    def compute_order(idx: int, visited: Set[int]) -> int:
        """Recursively compute the step_order for a step."""
        if idx in computed_orders:
            return computed_orders[idx]
        
        if idx in visited:
            # This shouldn't happen if cycle detection worked, but be safe
            logger.error("Unexpected cycle at step %d", idx)
            return steps[idx].step_order
        
        visited.add(idx)
        
        deps = step_dependencies[idx]
        if not deps:
            computed_orders[idx] = 0
        else:
            max_dep_order = max(compute_order(dep, visited) for dep in deps)
            computed_orders[idx] = max_dep_order + 1
        
        return computed_orders[idx]
    
    # Compute order for all steps
    for idx in range(len(steps)):
        compute_order(idx, set())
    
    # Step 5: Normalize step_orders to remove gaps (e.g., 0,2,4 -> 0,1,2)
    unique_orders = sorted(set(computed_orders.values()))
    order_map = {old: new for new, old in enumerate(unique_orders)}
    computed_orders = {idx: order_map[order] for idx, order in computed_orders.items()}
    
    # Step 6: Update step_order values and log any changes
    for idx, step in enumerate(steps):
        old_order = step.step_order
        new_order = computed_orders[idx]
        if old_order != new_order:
            logger.info(
                "Adjusted step_order for '%s': %d -> %d (based on dependencies)",
                step.title, old_order, new_order
            )
            step.step_order = new_order
    
    return steps


@dataclass
class AgentPlan:
    """The complete execution plan."""
    id: str
    goal: str
    reasoning: str
    steps: List[PlanStep]
    estimated_duration: int


class PlanningService:
    """
    Service for generating execution plans for app generation.
    
    Uses OpenRouter API to create structured plans with steps that can be
    executed in sequence or parallel (based on step_order).
    """
    
    def create_plan(
        self,
        user_message: str,
        context: Dict[str, Any],
        model: str = AIModel.CLAUDE_OPUS_4_5,
        intent_type: Optional[UserIntent] = None,
        existing_files: Optional[List[str]] = None,
    ) -> AgentPlan:
        """
        Create an execution plan for the app generation.
        
        Args:
            user_message: The user's request/requirement
            context: Context dictionary with app info, resources, etc.
            model: LLM model to use for plan generation
            intent_type: The classified intent (GENERATE_NEW, ADD_FEATURE, etc.)
            existing_files: List of existing file paths (for features, empty for new apps)
            
        Returns:
            AgentPlan with steps and reasoning
        """
        if FORCE_OPUS_PLAN_GENERATION:
            model = AIModel.CLAUDE_OPUS_4_5

        # Use [] if existing_files is None
        existing_files = existing_files or []

        # Use AI to generate a smart plan
        plan_prompt = build_plan_prompt(
            user_message,
            context,
            intent_type=intent_type,
            existing_files=existing_files,
        )
        # TODO: figure out why the tools arent passed as context
        logger.debug(f"Plan prompt: {plan_prompt}")

        try:
            result = get_llm_client().run(
                system_prompt="You are a planning assistant. Follow the instructions and return JSON only.",
                user_prompt=plan_prompt,
                llm_settings=LLMSettings(model=model, temperature=0.3, timeout=60.0),
            )
            plan_data = result.validated(self._parse_plan_response, default=None)
            if not plan_data:
                raise ValueError("Plan response was empty or invalid")

            reasoning = plan_data.get("reasoning", "Building the requested app.")
            steps_data = plan_data.get("steps", [])

            steps = [
                PlanStep(
                    id=str(uuid.uuid4()),
                    type=s.get("type", "code"),
                    title=s.get("title", "Generate Code"),
                    description=s.get("description", ""),
                    step_order=s.get("step_order", 0),
                    target_files=s.get("target_files", []),
                    dependent_files=s.get("dependent_files", []),
                    operation_type=safe_str_enum(
                        s.get("operation_type", PlanOperationType.GENERATE.value),
                        PlanOperationType.GENERATE,
                        PlanOperationType
                    ),
                    status=PlanStepStatus.PENDING,
                    duration=s.get("duration", None),
                    output=s.get("output", None),
                )
                for s in steps_data
            ]

            # Recompute step_order based on dependency graph
            steps = recompute_step_orders(steps)
            steps = sorted(steps, key=lambda x: x.step_order)

            plan = AgentPlan(
                id=str(uuid.uuid4()),
                goal=user_message,
                reasoning=reasoning,
                steps=steps,
                estimated_duration=len(steps) * 5000,  # 5s per step estimate
            )

            # Log the plan details
            logger.debug("=" * 80)
            logger.debug("📋 GENERATED PLAN")
            logger.debug("=" * 80)
            logger.debug("Reasoning: %s", reasoning)
            logger.debug("Total Steps: %d", len(steps))
            logger.debug("-" * 80)
            for i, step in enumerate(steps, 1):
                logger.debug("Step %d: [%s] %s (Order: %d)", i, step.type, step.title, step.step_order)
                logger.debug("  Description: %s", step.description)
                logger.debug("  Operation Type: %s", step.operation_type)
                if step.target_files:
                    logger.debug("  Target Files: %s", ', '.join(step.target_files))
                if step.dependent_files:
                    logger.debug("  Dependent Files: %s", ', '.join(step.dependent_files))
            logger.debug("=" * 80)

            # Validate the plan using ValidationService
            validation_service = get_validation_service()
            validation_errors = validation_service.validate_plan(plan)

            if validation_errors:
                logger.error("🚨 PLAN VALIDATION FAILED 🚨, validation errors: %s", str(validation_errors))
                raise ValueError("Plan validation failed")

            return plan

        except Exception as e:
            logger.error("Plan generation error: %s", e)
            # Fallback to default plan
            return self._create_fallback_plan(user_message)

    def _parse_plan_response(self, content: str) -> Dict[str, Any]:
        """
        Parse plan JSON from API response.
        
        Handles various formats:
        - JSON wrapped in markdown code blocks
        - Raw JSON
        - JSON with extra text before/after
        
        Args:
            content: Raw content from API response
            
        Returns:
            Parsed plan data dictionary
        """
        # Try to parse JSON - handle markdown code blocks
        json_match = re.search(r'```(?:json)?\s*(\{.*?\})\s*```', content, re.DOTALL)
        if json_match:
            content = json_match.group(1)
        
        # Clean up content for JSON parsing
        content = content.strip()
        if not content.startswith('{'):
            # Find first { and last }
            start = content.find('{')
            end = content.rfind('}')
            if start >= 0 and end > start:
                content = content[start:end+1]
        
        return cast(Dict[str, Any], json.loads(content))
    
    def _create_fallback_plan(self, user_message: str) -> AgentPlan:
        """
        Create a fallback plan when API call fails.
        
        Args:
            user_message: The user's request
            
        Returns:
            Default AgentPlan with standard steps
        """
        return AgentPlan(
            id=str(uuid.uuid4()),
            goal=user_message,
            reasoning="Building a React app based on your request.",
            steps=[
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
                    description="Build the primary app component",
                    step_order=1,
                    target_files=["src/components/Main.tsx"],
                    operation_type=PlanOperationType.GENERATE,
                ),
                PlanStep(
                    id=str(uuid.uuid4()),
                    type="component",
                    title="Build UI Components",
                    description="Create reusable UI components",
                    step_order=1,
                    target_files=["src/components/ui/Button.tsx", "src/components/ui/Card.tsx"],
                    operation_type=PlanOperationType.GENERATE,
                ),
                PlanStep(
                    id=str(uuid.uuid4()),
                    type="integration",
                    title="Connect Data Layer",
                    description="Integrate with the runtime API",
                    step_order=2,
                    target_files=["src/App.tsx"],
                    operation_type=PlanOperationType.EDIT,
                ),
                PlanStep(
                    id=str(uuid.uuid4()),
                    type="styling",
                    title="Apply Styling",
                    description="Add professional styling with Tailwind",
                    step_order=3,
                    target_files=["src/App.tsx", "src/components/Main.tsx"],
                    operation_type=PlanOperationType.EDIT,
                ),
            ],
            estimated_duration=25000,
        )


# Singleton instance
_planning_service: Optional[PlanningService] = None


def get_planning_service() -> PlanningService:
    """Get singleton planning service instance."""
    global _planning_service
    if _planning_service is None:
        _planning_service = PlanningService()
    return _planning_service
