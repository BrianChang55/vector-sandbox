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
from typing import Any, Dict, List, Optional
from enum import StrEnum

from django.conf import settings
import httpx

from vector_app.prompts.agentic import build_plan_prompt
from vector_app.utils.enum_utils import safe_str_enum
from vector_app.services.openrouter_service import AIModel

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
    
    OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions"
    
    def __init__(self):
        self.api_key = getattr(settings, 'OPENROUTER_API_KEY', None) or \
                      getattr(settings, 'OPENAI_API_KEY', None)
        self.app_name = getattr(settings, 'OPENROUTER_APP_NAME', 'Internal Apps Builder')
        self.site_url = getattr(settings, 'BASE_URL', 'http://localhost:8001')
    
    def _build_headers(self) -> Dict[str, str]:
        """Build API headers."""
        return {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json",
            "HTTP-Referer": self.site_url,
            "X-Title": self.app_name,
        }
    
    def create_plan(
        self,
        user_message: str,
        context: Dict[str, Any],
        model: str = AIModel.CLAUDE_OPUS_4_5.value,
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
            model = AIModel.CLAUDE_OPUS_4_5.value

        # Use AI to generate a smart plan
        plan_prompt = build_plan_prompt(user_message, context)

        try:
            with httpx.Client(timeout=60.0) as client:
                response = client.post(
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json={
                        "model": model,
                        "messages": [
                            {"role": "user", "content": plan_prompt}
                        ],
                        "temperature": 0.3,
                    },
                )
                response.raise_for_status()
                
                result = response.json()
                content = result["choices"][0]["message"]["content"]
                
                # Parse the plan from the response
                plan_data = self._parse_plan_response(content)
                
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
                logger.debug(f"Reasoning: {reasoning}")
                logger.debug(f"Total Steps: {len(steps)}")
                logger.debug("-" * 80)
                for i, step in enumerate(steps, 1):
                    logger.debug(f"Step {i}: [{step.type}] {step.title} (Order: {step.step_order})")
                    logger.debug(f"  Description: {step.description}")
                    logger.debug(f"  Operation Type: {step.operation_type}")
                    if step.target_files:
                        logger.debug(f"  Target Files: {', '.join(step.target_files)}")
                logger.debug("=" * 80)

                # Validate the plan using ValidationService
                from vector_app.services.validation_service import get_validation_service
                validation_service = get_validation_service()
                validation_errors = validation_service.validate_plan(plan)

                if validation_errors:
                    logger.error("🚨 PLAN VALIDATION FAILED 🚨")
                    for error in validation_errors:
                        logger.error(f"  ❌ {error}")
                    raise ValueError(f"Plan validation failed: {'; '.join(validation_errors)}")

                return plan

        except Exception as e:
            logger.error(f"Plan generation error: {e}")
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
        
        return json.loads(content)
    
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
