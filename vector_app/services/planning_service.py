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
from typing import Any, Dict, List, Optional, cast
from enum import StrEnum

from vector_app.ai.client import get_llm_client
from vector_app.ai.models import AIModel
from vector_app.ai.types import LLMSettings
from vector_app.prompts.agentic import build_plan_prompt
from vector_app.services.validation_service import get_validation_service
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
    operation_type: PlanOperationType = PlanOperationType.GENERATE
    status: PlanStepStatus = PlanStepStatus.PENDING
    duration: Optional[int] = None
    output: Optional[str] = None


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
        model: AIModel = AIModel.CLAUDE_OPUS_4_5,
    ) -> AgentPlan:
        """
        Create an execution plan for the app generation.
        
        Args:
            user_message: The user's request/requirement
            context: Context dictionary with app info, resources, etc.
            model: LLM model to use for plan generation
            
        Returns:
            AgentPlan with steps and reasoning
        """
        if FORCE_OPUS_PLAN_GENERATION:
            model = AIModel.CLAUDE_OPUS_4_5

        # Use AI to generate a smart plan
        plan_prompt = build_plan_prompt(user_message, context)

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
