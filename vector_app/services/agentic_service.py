"""
Agentic Code Generation Service

Implements the Research → Plan → Execute → Validate → Fix workflow
for autonomous app generation with visible progress and thinking.

Includes TypeScript compilation validation and automatic error fixing.

ENHANCED: Intent-aware routing that intelligently decides between:
- Full generation (new apps)
- Surgical edits (small changes)
- Feature addition (new functionality)
- Schema modification (data model changes)
"""
import logging
import json
import os
import re
import shutil
import subprocess
import tempfile
import time
import uuid
from typing import Dict, Any, List, Optional, Generator, Tuple, TYPE_CHECKING
from dataclasses import dataclass, asdict, field
from enum import Enum

from django.conf import settings
import httpx

from vector_app.prompts.agentic import (
    DESIGN_STYLE_PROMPT,
    FINAL_APP_SYSTEM_PROMPT,
    apply_design_style_prompt,
    build_plan_prompt,
    build_step_prompt,
    build_codegen_system_prompt,
    build_final_app_prompt,
)
from vector_app.services.data_store_context import (
    build_data_store_context,
    get_table_summary,
)
from vector_app.services.mcp_context import (
    build_mcp_tools_context,
    MCPToolsContext,
)
from concurrent.futures import ThreadPoolExecutor, as_completed

# Import intent-aware components
from vector_app.services.intent_classifier import (
    get_intent_classifier,
    UserIntent,
)
from vector_app.services.context_analyzer import get_context_analyzer
from vector_app.services.intent_router import get_intent_router

if TYPE_CHECKING:
    from vector_app.models import InternalApp, AppVersion

logger = logging.getLogger(__name__)


class AgentPhase(Enum):
    """Agent workflow phases."""
    RESEARCHING = "researching"
    PLANNING = "planning"
    EXECUTING = "executing"
    VALIDATING = "validating"
    COMPLETE = "complete"
    ERROR = "error"


class StepType(Enum):
    """Types of execution steps."""
    RESEARCH = "research"
    DESIGN = "design"
    CODE = "code"
    COMPONENT = "component"
    STYLING = "styling"
    INTEGRATION = "integration"
    VALIDATION = "validation"


@dataclass
class PlanStep:
    """A single step in the execution plan."""
    id: str
    type: str
    title: str
    description: str
    status: str = "pending"
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


@dataclass
class FileChange:
    """A file change during execution."""
    path: str
    action: str  # create, modify, delete
    language: str
    content: str
    previous_content: str = ""
    lines_added: int = 0
    lines_removed: int = 0


@dataclass
class AgentEvent:
    """An event emitted during agent execution."""
    type: str
    data: Dict[str, Any]

    def to_sse(self) -> str:
        """Format as Server-Sent Event."""
        return f"event: {self.type}\ndata: {json.dumps(self.data)}\n\n"


@dataclass
class TableDefinition:
    """A table definition parsed from agent output."""
    slug: str
    name: str
    description: str
    columns: List[Dict[str, Any]]


@dataclass
class CompilationError:
    """A compilation error from TypeScript validation."""
    file: str
    line: int
    column: int
    message: str
    code: Optional[str] = None  # TypeScript error code like TS2304
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "file": self.file,
            "line": self.line,
            "column": self.column,
            "message": self.message,
            "code": self.code,
        }


@dataclass
class ValidationResult:
    """Result of TypeScript validation."""
    passed: bool
    errors: List[CompilationError] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "passed": self.passed,
            "errors": [e.to_dict() for e in self.errors],
            "warnings": self.warnings,
        }


class AgenticService:
    """
    Agentic code generation service.
    
    Implements a multi-phase workflow:
    1. Research: Analyze context, understand requirements
    2. Plan: Create structured execution plan
    3. Execute: Generate code step-by-step
    4. Validate: Verify generated code compiles (TypeScript)
    5. Fix: Automatically fix compilation errors (max 2 attempts)
    """
    
    OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions"
    MAX_FIX_ATTEMPTS = 2  # Maximum attempts to fix compilation errors
    
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
    
    def _gather_context_parallel(
        self,
        app: Optional['InternalApp'],
    ) -> Tuple[Optional[str], Optional[str]]:
        """
        Gather data store and MCP context in parallel.
        
        Uses ThreadPoolExecutor for concurrent context building,
        reducing latency when multiple context sources are available.
        
        Args:
            app: The InternalApp to gather context for
            
        Returns:
            Tuple of (data_store_context, mcp_tools_context)
        """
        if not app:
            return None, None
        
        data_store_context = None
        mcp_tools_context_str = None
        
        def build_data_context():
            try:
                return build_data_store_context(app)
            except Exception as e:
                logger.warning(f"Failed to build data store context: {e}")
                return None
        
        def build_mcp_context():
            try:
                mcp_context = build_mcp_tools_context(app)
                if mcp_context.has_tools:
                    logger.info(f"MCP context: {len(mcp_context.tools)} tools available")
                    return mcp_context.full_context
                return None
            except Exception as e:
                logger.warning(f"Failed to build MCP context: {e}")
                return None
        
        # Execute both context builders in parallel
        with ThreadPoolExecutor(max_workers=2) as executor:
            futures = {
                executor.submit(build_data_context): 'data_store',
                executor.submit(build_mcp_context): 'mcp_tools',
            }
            
            for future in as_completed(futures):
                context_type = futures[future]
                try:
                    result = future.result(timeout=10.0)
                    if context_type == 'data_store':
                        data_store_context = result
                    else:
                        mcp_tools_context_str = result
                except Exception as e:
                    logger.warning(f"Context gathering ({context_type}) failed: {e}")
        
        return data_store_context, mcp_tools_context_str
    
    def generate_app(
        self,
        user_message: str,
        current_spec: Optional[Dict[str, Any]],
        registry_surface: Dict[str, Any],
        app_name: str,
        model: str = "anthropic/claude-sonnet-4",
        app: Optional['InternalApp'] = None,
        version: Optional['AppVersion'] = None,
        use_intent_routing: bool = True,
    ) -> Generator[AgentEvent, None, None]:
        """
        Generate an app with intelligent intent-aware routing.
        
        PRESERVED: Same signature, same event types, same interface.
        ENHANCED: Smart routing based on user intent.
        
        Yields AgentEvent objects for real-time progress updates.
        
        Args:
            user_message: The user's request
            current_spec: Current app specification if exists
            registry_surface: Available data resources
            app_name: Name of the app
            model: LLM model to use
            app: Optional InternalApp for data store operations
            version: Optional AppVersion for versioned table operations
            use_intent_routing: Whether to use intent-aware routing (default True)
        """
        session_id = str(uuid.uuid4())
        start_time = time.time()
        
        # Build context in parallel for better performance
        data_store_context, mcp_tools_context_str = self._gather_context_parallel(app)
        
        # Emit start event
        yield AgentEvent("agent_start", {
            "session_id": session_id,
            "goal": user_message.strip(),
        })
        
        # ===== INTENT-AWARE ROUTING =====
        if use_intent_routing:
            try:
                yield from self._generate_with_intent_routing(
                    user_message=user_message,
                    current_spec=current_spec,
                    registry_surface=registry_surface,
                    app_name=app_name,
                    model=model,
                    app=app,
                    version=version,
                    session_id=session_id,
                    start_time=start_time,
                    data_store_context=data_store_context,
                    mcp_tools_context=mcp_tools_context_str,
                )
                return  # Intent routing handled everything
            except Exception as e:
                logger.warning(f"Intent routing failed, falling back to legacy: {e}")
                # Fall through to legacy behavior
        
        # ===== LEGACY BEHAVIOR (FALLBACK) =====
        styled_user_message = apply_design_style_prompt(
            user_message, 
            data_store_context,
            mcp_tools_context_str,
        )
        
        # ===== PHASE 1: RESEARCH =====
        yield AgentEvent("phase_change", {
            "phase": AgentPhase.RESEARCHING.value,
            "message": "Analyzing your requirements...",
        })
        
        yield AgentEvent("thinking", {
            "content": f"Understanding the user request: {user_message[:100]}...",
            "type": "observation",
        })
        
        # Analyze context with data store info
        # Note: MCP tools context is already included in styled_user_message
        context_analysis = self._analyze_context(
            styled_user_message, current_spec, registry_surface, app_name, app, None
        )
        
        yield AgentEvent("thinking", {
            "content": context_analysis.get("analysis", "Analyzed current app state and available resources."),
            "type": "reasoning",
        })
        
        # ===== PHASE 2: PLANNING =====
        yield AgentEvent("phase_change", {
            "phase": AgentPhase.PLANNING.value,
            "message": "Creating implementation plan...",
        })
        
        yield AgentEvent("thinking", {
            "content": "Breaking down the task into executable steps...",
            "type": "decision",
        })
        
        # Generate plan
        plan = self._create_plan(styled_user_message, context_analysis, model)
        
        # Convert plan steps to the format frontend expects
        plan_steps = [
            {
                "id": s.id,
                "type": s.type,
                "title": s.title,
                "description": s.description,
                "status": "pending",
            }
            for s in plan.steps
        ]
        
        yield AgentEvent("plan_created", {
            "plan": asdict(plan),
            "steps": plan_steps,
            "exploredDirectories": 3,  # Mock exploration metrics
            "exploredFiles": len(registry_surface.get("resources", [])) + 5,
            "searches": 1,
        })
        
        # ===== PHASE 3: EXECUTE =====
        yield AgentEvent("phase_change", {
            "phase": AgentPhase.EXECUTING.value,
            "message": "Building your app...",
        })
        
        generated_files: List[FileChange] = []
        
        for i, step in enumerate(plan.steps):
            step_start = time.time()
            
            # Emit step_started event (new format)
            yield AgentEvent("step_started", {
                "stepId": step.id,
                "stepIndex": i,
            })
            
            yield AgentEvent("step_start", {
                "step_index": i,
                "step": asdict(step),
            })
            
            # Execute the step and stream code
            try:
                for event in self._execute_step(
                    step, i, styled_user_message, context_analysis, 
                    generated_files, registry_surface, model,
                    app=app, version=version, data_store_context=data_store_context,
                    mcp_tools_context=mcp_tools_context_str,
                ):
                    yield event
                    if event.type == "file_generated":
                        generated_files.append(FileChange(**event.data["file"]))
                
                step.status = "complete"
                step.duration = int((time.time() - step_start) * 1000)
                
                # Emit step_completed event (new format)
                yield AgentEvent("step_completed", {
                    "stepId": step.id,
                    "stepIndex": i,
                    "duration": step.duration,
                })
                
                yield AgentEvent("step_complete", {
                    "step_index": i,
                    "status": "complete",
                    "duration": step.duration,
                })
                
            except Exception as e:
                logger.error(f"Step execution error: {e}")
                step.status = "error"
                yield AgentEvent("step_complete", {
                    "step_index": i,
                    "status": "error",
                    "error": str(e),
                })
        
        # ===== PHASE 4: VALIDATE & FIX =====
        yield AgentEvent("phase_change", {
            "phase": AgentPhase.VALIDATING.value,
            "message": "Validating generated code...",
        })
        
        # Run TypeScript validation with automatic fix loop
        validation_passed = False
        fix_attempts = 0
        
        for attempt in range(1, self.MAX_FIX_ATTEMPTS + 1):
            # Run TypeScript validation
            ts_validation = self._validate_typescript(generated_files)
            
            if ts_validation.passed:
                validation_passed = True
                yield AgentEvent("validation_result", {
                    "passed": True,
                    "errors": [],
                    "warnings": ts_validation.warnings,
                    "fix_attempts": fix_attempts,
                })
                break
            
            # Validation failed - attempt to fix
            fix_attempts = attempt
            error_count = len(ts_validation.errors)
            
            yield AgentEvent("thinking", {
                "content": f"Found {error_count} compilation error(s), attempting fix ({attempt}/{self.MAX_FIX_ATTEMPTS})",
                "type": "observation",
            })
            
            # Import and use error fix service
            from vector_app.services.error_fix_service import get_error_fix_service
            fix_service = get_error_fix_service()
            
            # Fix errors (yields Live Activity events)
            # Track fixed files from file_generated events
            fixed_files_by_path = {}
            
            fix_gen = fix_service.fix_errors(
                files=generated_files,
                errors=ts_validation.errors,
                model=model,
                attempt=attempt,
            )
            
            # Consume events and collect fixed files
            while True:
                try:
                    event = next(fix_gen)
                    yield event
                    # Capture fixed files from file_generated events
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
            
            # Apply fixed files to generated_files
            if fixed_files_by_path:
                updated_files = []
                for f in generated_files:
                    if f.path in fixed_files_by_path:
                        updated_files.append(fixed_files_by_path[f.path])
                    else:
                        updated_files.append(f)
                generated_files = updated_files
        
        # If still not passing after all attempts, emit final validation result
        if not validation_passed:
            final_validation = self._validate_typescript(generated_files)
            
            if final_validation.passed:
                validation_passed = True
                yield AgentEvent("validation_result", {
                    "passed": True,
                    "errors": [],
                    "warnings": final_validation.warnings,
                    "fix_attempts": fix_attempts,
                })
                yield AgentEvent("fix_complete", {
                    "success": True,
                    "fix_attempts": fix_attempts,
                })
            else:
                yield AgentEvent("validation_result", {
                    "passed": False,
                    "errors": [e.to_dict() for e in final_validation.errors],
                    "warnings": final_validation.warnings,
                    "fix_attempts": fix_attempts,
                })
                yield AgentEvent("fix_failed", {
                    "remaining_errors": len(final_validation.errors),
                    "fix_attempts": fix_attempts,
                })
        
        # Also run basic validation checks
        basic_validation = self._validate_code(generated_files)
        if basic_validation.get("warnings"):
            for warning in basic_validation["warnings"]:
                yield AgentEvent("thinking", {
                    "content": f"Warning: {warning}",
                    "type": "reflection",
                })
        
        # ===== COMPLETE =====
        total_duration = int((time.time() - start_time) * 1000)
        
        # Build summary message
        file_types = {}
        for f in generated_files:
            ext = f.path.split('.')[-1] if '.' in f.path else 'unknown'
            file_types[ext] = file_types.get(ext, 0) + 1
        
        summary_parts = [f"Generated {len(generated_files)} {'file' if len(generated_files) == 1 else 'files'}"]
        if file_types:
            summary_parts.append("(" + ", ".join(f"{c} {t}" for t, c in file_types.items()) + ")")
        summary_parts.append(f"in {total_duration / 1000:.1f}s")
        
        if fix_attempts > 0:
            summary_parts.append(f"(fixed {fix_attempts}x)")
        
        yield AgentEvent("agent_complete", {
            "duration": total_duration,
            "filesGenerated": len(generated_files),
            "plan_id": plan.id,
            "summary": " ".join(summary_parts) + ". Your app is ready to preview!",
            "validated": validation_passed,
            "fix_attempts": fix_attempts,
        })
        
        # Emit done event to signal stream completion
        yield AgentEvent("done", {
            "success": True,
            "validated": validation_passed,
            "filesGenerated": len(generated_files),
            "duration": total_duration,
            "fix_attempts": fix_attempts,
        })
    
    def _analyze_context(
        self,
        user_message: str,
        current_spec: Optional[Dict[str, Any]],
        registry_surface: Dict[str, Any],
        app_name: str,
        app: Optional['InternalApp'] = None,
        mcp_context: Optional[MCPToolsContext] = None,
    ) -> Dict[str, Any]:
        """Analyze the current context and requirements."""
        
        resources = registry_surface.get("resources", [])
        
        # Get data store summary if app is provided
        data_store_summary = ""
        if app:
            data_store_summary = get_table_summary(app)
        
        # Get MCP tools summary
        mcp_tools_summary = ""
        has_mcp_tools = False
        if mcp_context and mcp_context.has_tools:
            mcp_tools_summary = mcp_context.connectors_summary
            has_mcp_tools = True
        
        analysis = {
            "app_name": app_name,
            "has_existing_spec": current_spec is not None,
            "available_resources": [r["resource_id"] for r in resources],
            "resource_details": resources,
            "user_intent": user_message,
            "data_store_summary": data_store_summary,
            "has_data_store": bool(app),
            "mcp_tools_summary": mcp_tools_summary,
            "connectors_summary": mcp_tools_summary,  # For build_plan_prompt compatibility
            "has_mcp_tools": has_mcp_tools,
            "analysis": f"Building '{app_name}' with {len(resources)} available data sources.",
        }
        
        if data_store_summary and "No data tables" not in data_store_summary:
            analysis["analysis"] += f" App has existing data tables."
        
        if has_mcp_tools:
            tool_count = len(mcp_context.tools) if mcp_context else 0
            analysis["analysis"] += f" {tool_count} MCP integration tools available."
        
        if current_spec:
            analysis["current_pages"] = len(current_spec.get("pages", []))
            analysis["analysis"] += f" Modifying existing app with {analysis['current_pages']} pages."
        else:
            analysis["analysis"] += " Creating a new app from scratch."
        
        return analysis
    
    def _generate_with_intent_routing(
        self,
        user_message: str,
        current_spec: Optional[Dict[str, Any]],
        registry_surface: Dict[str, Any],
        app_name: str,
        model: str,
        app: Optional['InternalApp'],
        version: Optional['AppVersion'],
        session_id: str,
        start_time: float,
        data_store_context: Optional[str],
        mcp_tools_context: Optional[str],
    ) -> Generator[AgentEvent, None, None]:
        """
        Generate app using intent-aware routing.
        
        This method classifies the user's intent and routes to the
        appropriate specialized handler for optimal results.
        """
        # ===== PHASE 1: RESEARCH & INTENT CLASSIFICATION =====
        yield AgentEvent("phase_change", {
            "phase": AgentPhase.RESEARCHING.value,
            "message": "Analyzing your request...",
        })
        
        yield AgentEvent("thinking", {
            "content": f"Understanding the user request: {user_message[:100]}...",
            "type": "observation",
        })
        
        # Get the latest stable version for context analysis
        latest_version = version
        if not latest_version and app:
            from vector_app.services.version_service import VersionService
            latest_version = VersionService.get_latest_stable_version(app)
        
        # Analyze app context
        context_analyzer = get_context_analyzer()
        app_context = context_analyzer.analyze(app, latest_version)
        
        yield AgentEvent("thinking", {
            "content": f"App context: {app_context.to_summary()}",
            "type": "observation",
        })
        
        # Classify intent
        intent_classifier = get_intent_classifier()
        intent = intent_classifier.classify(user_message, app_context, model)
        
        logger.info(
            f"Intent classified: {intent.intent.value} "
            f"(confidence: {intent.confidence:.0%}, scope: {intent.scope})"
        )
        
        yield AgentEvent("thinking", {
            "content": f"Detected intent: {intent.intent.value} ({intent.confidence:.0%} confidence)",
            "type": "reasoning",
        })
        
        # ===== ROUTE TO APPROPRIATE HANDLER =====
        router = get_intent_router()
        
        generated_files = yield from router.route(
            intent=intent,
            context=app_context,
            user_message=user_message,
            current_spec=current_spec,
            registry_surface=registry_surface,
            app_name=app_name,
            model=model,
            app=app,
            version=version,
            data_store_context=data_store_context,
            mcp_tools_context=mcp_tools_context,
        )
        
        # ===== COMPLETE =====
        total_duration = int((time.time() - start_time) * 1000)
        
        # Build summary message
        file_types = {}
        for f in generated_files:
            ext = f.path.split('.')[-1] if '.' in f.path else 'unknown'
            file_types[ext] = file_types.get(ext, 0) + 1
        
        summary_parts = [f"Generated {len(generated_files)} {'file' if len(generated_files) == 1 else 'files'}"]
        if file_types:
            summary_parts.append("(" + ", ".join(f"{c} {t}" for t, c in file_types.items()) + ")")
        summary_parts.append(f"in {total_duration / 1000:.1f}s")
        
        yield AgentEvent("agent_complete", {
            "duration": total_duration,
            "filesGenerated": len(generated_files),
            "summary": " ".join(summary_parts) + ". Your app is ready to preview!",
            "validated": True,
            "intent": intent.intent.value,
        })
        
        # Emit done event
        yield AgentEvent("done", {
            "success": True,
            "validated": True,
            "filesGenerated": len(generated_files),
            "duration": total_duration,
            "intent": intent.intent.value,
        })
    
    def _create_plan(
        self,
        user_message: str,
        context: Dict[str, Any],
        model: str,
    ) -> AgentPlan:
        """Create an execution plan for the app generation."""
        
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
                
                # Try to parse JSON - handle markdown code blocks
                import re
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
                
                plan_data = json.loads(content)
                
                steps = [
                    PlanStep(
                        id=str(uuid.uuid4()),
                        type=s.get("type", "code"),
                        title=s.get("title", "Generate Code"),
                        description=s.get("description", ""),
                    )
                    for s in plan_data.get("steps", [])
                ]
                
                return AgentPlan(
                    id=str(uuid.uuid4()),
                    goal=user_message,
                    reasoning=plan_data.get("reasoning", "Building the requested app."),
                    steps=steps,
                    estimated_duration=len(steps) * 5000,  # 5s per step estimate
                )
                
        except Exception as e:
            logger.error(f"Plan generation error: {e}")
            # Fallback to default plan
            return AgentPlan(
                id=str(uuid.uuid4()),
                goal=user_message,
                reasoning="Building a React app based on your request.",
                steps=[
                    PlanStep(str(uuid.uuid4()), "design", "Design App Structure", 
                            "Plan the component hierarchy and data flow"),
                    PlanStep(str(uuid.uuid4()), "component", "Create Main Component", 
                            "Build the primary app component"),
                    PlanStep(str(uuid.uuid4()), "component", "Build UI Components", 
                            "Create reusable UI components"),
                    PlanStep(str(uuid.uuid4()), "integration", "Connect Data Layer", 
                            "Integrate with the runtime API"),
                    PlanStep(str(uuid.uuid4()), "styling", "Apply Styling", 
                            "Add professional styling with Tailwind"),
                ],
                estimated_duration=25000,
            )
    
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
        mcp_tools_context: Optional[str] = None,  # MCP integration tools context
    ) -> Generator[AgentEvent, None, None]:
        """Execute a single plan step and yield progress events."""
        
        # Build prompt for this step with data store and MCP tools context
        prompt = build_step_prompt(
            step, step_index, user_message, context, 
            existing_files, registry_surface, data_store_context,
            connectors_context=mcp_tools_context,
        )
        
        has_data_store = context.get('has_data_store', False)
        
        yield AgentEvent("thinking", {
            "content": f"{step.title}",
            "type": "decision",
        })
        
        try:
            with httpx.Client(timeout=120.0) as client:
                with client.stream(
                    "POST",
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json={
                        "model": model,
                        "messages": [
                            {"role": "system", "content": build_codegen_system_prompt(registry_surface, has_data_store)},
                            {"role": "user", "content": prompt},
                        ],
                        "temperature": 0.3,
                        "stream": True,
                    },
                ) as response:
                    response.raise_for_status()
                    
                    full_content = ""
                    current_file_path = None
                    file_content_buffer = ""
                    chunk_count = 0
                    
                    for line in response.iter_lines():
                        if not line:
                            continue
                        
                        if line.startswith("data: "):
                            data = line[6:]
                            if data == "[DONE]":
                                break
                            
                            try:
                                chunk_data = json.loads(data)
                                delta = chunk_data.get("choices", [{}])[0].get("delta", {})
                                content = delta.get("content", "")
                                
                                if content:
                                    full_content += content
                                    chunk_count += 1
                                    
                                    # Emit code chunks periodically
                                    if chunk_count % 5 == 0:
                                        yield AgentEvent("step_progress", {
                                            "step_index": step_index,
                                            "progress": min(90, chunk_count * 2),
                                            "message": f"Generating code...",
                                        })
                                        
                            except json.JSONDecodeError:
                                continue
                    
                    # Parse and apply table definitions if app and version are provided
                    if app and version:
                        table_defs = self._parse_table_definitions(full_content)
                        if table_defs:
                            yield AgentEvent("thinking", {
                                "content": f"Creating {len(table_defs)} data table(s)...",
                                "type": "decision",
                            })
                            # Apply table definitions - this is a generator
                            for event in self._apply_table_definitions(table_defs, app, version):
                                yield event
                    
                    # Parse generated files from response
                    files = self._parse_code_response(full_content, step)
                    
                    for file in files:
                        yield AgentEvent("file_generated", {
                            "file": asdict(file),
                        })
                        
        except Exception as e:
            logger.error(f"Step execution error: {e}")
            yield AgentEvent("thinking", {
                "content": f"Error during step: {str(e)}",
                "type": "reflection",
            })
            raise
    
    def _parse_code_response(self, content: str, step: PlanStep) -> List[FileChange]:
        """Parse code blocks from the AI response into FileChange objects."""
        import re
        
        files = []
        
        # Multiple patterns to catch different formats
        patterns = [
            # Pattern 1: ```filepath:path/to/file.ext
            r'```filepath:([^\n`]+)\n(.*?)```',
            # Pattern 2: ```path/to/file.ext (with extension)
            r'```([^\n`]+\.[a-zA-Z]+)\n(.*?)```',
            # Pattern 3: // filepath: path/to/file.ext followed by code
            r'// filepath:\s*([^\n]+)\n(.*?)(?=// filepath:|```|$)',
        ]
        
        for pattern in patterns:
            matches = re.findall(pattern, content, re.DOTALL)
            for filepath, code in matches:
                filepath = filepath.strip()
                code = code.strip()
                
                # Skip if we already have this file
                if any(f.path == filepath for f in files):
                    continue
                
                # Skip language-only markers
                if filepath.lower() in ('tsx', 'ts', 'js', 'jsx', 'css', 'json', 'html', 
                                        'typescript', 'javascript', 'react', 'python'):
                    continue
                
                # Skip if no actual code
                if not code or len(code) < 10:
                    continue
                
                # Determine language from extension
                ext = filepath.split('.')[-1] if '.' in filepath else 'tsx'
                lang_map = {
                    'tsx': 'tsx',
                    'ts': 'ts',
                    'jsx': 'tsx',
                    'js': 'ts',
                    'css': 'css',
                    'json': 'json',
                    'html': 'html',
                }
                
                # Clean up the code - remove any trailing ``` that might be included
                code = re.sub(r'```\s*$', '', code).strip()
                
                # Normalize the file path - always put in src/ directory
                normalized_path = filepath
                # Remove leading slash if present
                if normalized_path.startswith('/'):
                    normalized_path = normalized_path[1:]
                # Ensure it's in src/ directory
                if not normalized_path.startswith('src/'):
                    normalized_path = f"src/{normalized_path}"
                
                # For new files: lines_added = total lines, lines_removed = 0
                lines_in_code = code.count('\n') + (1 if code and not code.endswith('\n') else 0)
                
                files.append(FileChange(
                    path=normalized_path,
                    action='create',
                    language=lang_map.get(ext, 'tsx'),
                    content=code,
                    lines_added=lines_in_code,
                    lines_removed=0,
                ))
        
        # If no files found, try to extract any code and create a default file
        if not files and '```' in content:
            # Extract all code blocks with language hints
            code_blocks = re.findall(r'```(?:tsx?|jsx?|typescript|javascript)?\n(.*?)```', content, re.DOTALL)
            
            if code_blocks:
                # Use the largest code block as the main component
                largest_block = max(code_blocks, key=len).strip()
                
                if len(largest_block) > 50:  # Only if substantial
                    # Calculate lines for the code block
                    block_lines = largest_block.count('\n') + (1 if largest_block and not largest_block.endswith('\n') else 0)
                    
                    # Determine file based on content and step type
                    if 'export default' in largest_block or 'function App' in largest_block:
                        files.append(FileChange("src/App.tsx", "create", "tsx", largest_block, previous_content="", lines_added=block_lines, lines_removed=0))
                    elif step.type == "styling":
                        files.append(FileChange("src/styles/main.css", "create", "css", largest_block, previous_content="", lines_added=block_lines, lines_removed=0))
                    elif step.type == "component":
                        # Try to extract component name
                        name_match = re.search(r'(?:function|const)\s+(\w+)', largest_block)
                        comp_name = name_match.group(1) if name_match else "Component"
                        files.append(FileChange(f"src/components/{comp_name}.tsx", "create", "tsx", largest_block, previous_content="", lines_added=block_lines, lines_removed=0))
                    else:
                        files.append(FileChange("src/App.tsx", "create", "tsx", largest_block, previous_content="", lines_added=block_lines, lines_removed=0))
        
        logger.info(f"Parsed {len(files)} files from AI response")
        return files
    
    def _generate_final_app(
        self,
        user_message: str,
        context: Dict[str, Any],
        existing_files: List[FileChange],
        registry_surface: Dict[str, Any],
        model: str,
    ) -> Generator[AgentEvent, None, None]:
        """Generate the final integrated App.tsx that uses all generated components."""
        
        # Collect all generated component files
        component_files = []
        other_files = []
        
        for f in existing_files:
            if '/components/' in f.path and f.path.endswith('.tsx'):
                component_files.append(f)
            elif f.path != 'src/App.tsx' and not f.path.endswith('App.tsx'):
                other_files.append(f)
        
        # Build a summary of what components are available
        components_info = ""
        for f in component_files:
            # Extract component name from path
            comp_name = f.path.split('/')[-1].replace('.tsx', '')
            components_info += f"\n- {comp_name} (from '{f.path}'): {f.content[:200]}..."
        
        if not components_info:
            components_info = "No separate components generated."
        
        prompt = build_final_app_prompt(user_message, context, components_info, other_files)
        
        try:
            with httpx.Client(timeout=120.0) as client:
                with client.stream(
                    "POST",
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json={
                        "model": model,
                        "messages": [
                            {"role": "system", "content": FINAL_APP_SYSTEM_PROMPT},
                            {"role": "user", "content": prompt},
                        ],
                        "temperature": 0.3,
                        "stream": True,
                    },
                ) as response:
                    response.raise_for_status()
                    
                    full_content = ""
                    
                    for line in response.iter_lines():
                        if not line:
                            continue
                        
                        if line.startswith("data: "):
                            data = line[6:]
                            if data == "[DONE]":
                                break
                            
                            try:
                                chunk_data = json.loads(data)
                                delta = chunk_data.get("choices", [{}])[0].get("delta", {})
                                content = delta.get("content", "")
                                
                                if content:
                                    full_content += content
                                    
                            except json.JSONDecodeError:
                                continue
                    
                    # Parse the App.tsx from response
                    import re
                    
                    # Look for the App.tsx code block
                    patterns = [
                        r'```src/App\.tsx\n(.*?)```',
                        r'```App\.tsx\n(.*?)```',
                        r'```tsx\n(.*?)```',
                    ]
                    
                    app_content = None
                    for pattern in patterns:
                        match = re.search(pattern, full_content, re.DOTALL)
                        if match:
                            app_content = match.group(1).strip()
                            break
                    
                    if app_content and len(app_content) > 100:
                        # Remove old App.tsx from existing_files list (will be replaced)
                        yield AgentEvent("file_generated", {
                            "file": {
                                "path": "src/App.tsx",
                                "action": "create",
                                "language": "tsx",
                                "content": app_content,
                            }
                        })
                        logger.info(f"Generated final App.tsx: {len(app_content)} chars")
                    else:
                        logger.warning("Failed to generate final App.tsx, keeping existing")
                        
        except Exception as e:
            logger.error(f"Final app generation error: {e}")
            yield AgentEvent("thinking", {
                "content": f"Note: Could not regenerate App.tsx: {str(e)}",
                "type": "reflection",
            })
    
    def _parse_table_definitions(self, content: str) -> List[TableDefinition]:
        """
        Parse TABLE_DEFINITION blocks from agent output.
        
        Format:
        ```table:slug-name
        name: Display Name
        description: Optional description
        columns:
          - name: col_name, type: string, nullable: true
          - name: id, type: uuid, primary_key: true, auto_generate: true
        ```
        """
        tables = []
        
        # Pattern to match table definition blocks
        pattern = r'```table:([a-z0-9-]+)\n(.*?)```'
        matches = re.findall(pattern, content, re.DOTALL | re.IGNORECASE)
        
        for slug, table_content in matches:
            try:
                table_def = self._parse_single_table_definition(slug.strip(), table_content.strip())
                if table_def:
                    tables.append(table_def)
            except Exception as e:
                logger.warning(f"Failed to parse table definition for {slug}: {e}")
        
        return tables
    
    def _parse_single_table_definition(self, slug: str, content: str) -> Optional[TableDefinition]:
        """Parse a single table definition content."""
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
                col_def = self._parse_column_definition(line[2:].strip())
                if col_def:
                    columns.append(col_def)
        
        if not columns:
            return None
        
        return TableDefinition(
            slug=slug,
            name=name,
            description=description,
            columns=columns
        )
    
    def _parse_column_definition(self, line: str) -> Optional[Dict[str, Any]]:
        """Parse a column definition line."""
        # Format: name: col_name, type: string, nullable: true, enum_values: [a, b, c]
        col = {}
        
        # First, extract any list values that might contain commas
        # Pattern: key: [values] - handle this specially
        import re
        list_pattern = r'(\w+):\s*\[([^\]]+)\]'
        list_matches = re.findall(list_pattern, line)
        for key, value_str in list_matches:
            values = [v.strip().strip('"').strip("'") for v in value_str.split(',')]
            col[key] = values
            # Remove from line to avoid re-parsing
            line = re.sub(rf'{key}:\s*\[[^\]]+\]', '', line)
        
        # Split remaining by comma and parse each key-value pair
        parts = [p.strip() for p in line.split(',') if p.strip()]
        
        for part in parts:
            if ':' in part:
                key, value = part.split(':', 1)
                key = key.strip()
                value = value.strip()
                
                # Skip if already parsed as a list
                if key in col:
                    continue
                
                # Handle boolean values
                if value.lower() == 'true':
                    value = True
                elif value.lower() == 'false':
                    value = False
                # Handle integer values
                elif value.isdigit():
                    value = int(value)
                
                col[key] = value
        
        # Validate required fields
        if 'name' not in col or 'type' not in col:
            return None
        
        # If type is enum but no enum_values, convert to string
        if col.get('type') == 'enum' and 'enum_values' not in col:
            col['type'] = 'string'
        
        return col
    
    def _apply_table_definitions(
        self,
        table_defs: List[TableDefinition],
        app: 'InternalApp',
        version: 'AppVersion'
    ) -> Generator[AgentEvent, None, List[Dict[str, Any]]]:
        """
        Apply table definitions to the app's data store.
        
        Yields events for each table created/updated.
        Returns list of created/updated table info.
        """
        from vector_app.models import AppDataTable
        from vector_app.services.app_data_service import AppDataService
        
        created_tables = []
        
        for table_def in table_defs:
            # Check if table already exists
            existing_table = AppDataTable.objects.filter(
                internal_app=app,
                slug=table_def.slug
            ).first()
            
            schema = {'columns': table_def.columns}
            
            if existing_table:
                # Table already exists - skip to avoid duplicate snapshot errors
                # The LLM often repeats table definitions across steps
                logger.info(f"Table {table_def.slug} already exists, skipping")
                continue
            else:
                # Create new table
                table, errors = AppDataService.create_table_versioned(
                    app=app,
                    version=version,
                    name=table_def.name,
                    schema=schema,
                    description=table_def.description
                )
                
                if table:
                    yield AgentEvent("table_created", {
                        "slug": table.slug,
                        "name": table.name,
                        "columns": len(table_def.columns),
                    })
                    created_tables.append({
                        'slug': table.slug,
                        'name': table.name,
                        'operation': 'created',
                        'columns': len(table_def.columns),
                    })
                else:
                    logger.warning(f"Failed to create table {table_def.slug}: {errors}")
        
        return created_tables
    
    def _validate_code(self, files: List[FileChange]) -> Dict[str, Any]:
        """Validate the generated code."""
        errors = []
        warnings = []
        
        for file in files:
            # Basic validation
            if not file.content or len(file.content) < 10:
                errors.append(f"File {file.path} is empty or too short")
                continue
            
            # Check for common issues
            if file.language in ('tsx', 'ts'):
                if 'import React' not in file.content and 'from \'react\'' not in file.content:
                    if 'function' in file.content and 'return' in file.content:
                        # Likely a component without React import
                        warnings.append(f"{file.path}: Consider adding React import")
                
                # Check for undefined references
                if 'undefined' in file.content.lower():
                    warnings.append(f"{file.path}: Contains 'undefined' - verify data handling")
        
        return {
            "passed": len(errors) == 0,
            "errors": errors,
            "warnings": warnings,
        }
    
    def _validate_typescript(self, files: List[FileChange]) -> ValidationResult:
        """
        Validate generated TypeScript/TSX files using the TypeScript compiler.
        
        Writes files to a temp directory, runs tsc --noEmit, and parses errors.
        Returns structured ValidationResult with file, line, and message info.
        """
        # Filter to only TypeScript/TSX files
        ts_files = [f for f in files if f.language in ('tsx', 'ts')]
        
        if not ts_files:
            return ValidationResult(passed=True)
        
        # Check if tsc is available
        tsc_path = shutil.which('tsc')
        if not tsc_path:
            # Try npx tsc as fallback
            npx_path = shutil.which('npx')
            if not npx_path:
                logger.warning("TypeScript compiler not found, skipping validation")
                return ValidationResult(passed=True, warnings=["TypeScript compiler not available"])
            tsc_cmd = ['npx', 'tsc']
        else:
            tsc_cmd = [tsc_path]
        
        temp_dir = None
        try:
            # Create temp directory for validation
            temp_dir = tempfile.mkdtemp(prefix='vector_tsc_')
            
            # Write files to temp directory
            for file in ts_files:
                # Normalize path - remove src/ prefix for temp dir
                file_path = file.path
                if file_path.startswith('src/'):
                    file_path = file_path[4:]
                
                full_path = os.path.join(temp_dir, file_path)
                os.makedirs(os.path.dirname(full_path), exist_ok=True)
                
                with open(full_path, 'w', encoding='utf-8') as f:
                    f.write(file.content)
            
            # Create a minimal tsconfig.json for validation
            tsconfig = {
                "compilerOptions": {
                    "target": "ES2020",
                    "lib": ["ES2020", "DOM", "DOM.Iterable"],
                    "module": "ESNext",
                    "moduleResolution": "bundler",
                    "jsx": "react-jsx",
                    "strict": False,  # Lenient for generated code
                    "noEmit": True,
                    "skipLibCheck": True,
                    "esModuleInterop": True,
                    "allowSyntheticDefaultImports": True,
                    "resolveJsonModule": True,
                    "isolatedModules": True,
                    "noImplicitAny": False,
                    "strictNullChecks": False,
                },
                "include": ["**/*.ts", "**/*.tsx"],
            }
            
            tsconfig_path = os.path.join(temp_dir, 'tsconfig.json')
            with open(tsconfig_path, 'w', encoding='utf-8') as f:
                json.dump(tsconfig, f, indent=2)
            
            # Create stub declarations for common dependencies
            stubs_dir = os.path.join(temp_dir, 'node_modules', '@types')
            os.makedirs(stubs_dir, exist_ok=True)
            
            # React stub
            react_types_dir = os.path.join(stubs_dir, 'react')
            os.makedirs(react_types_dir, exist_ok=True)
            with open(os.path.join(react_types_dir, 'index.d.ts'), 'w') as f:
                f.write('''
declare module 'react' {
    export function useState<T>(initial: T): [T, (v: T | ((prev: T) => T)) => void];
    export function useEffect(effect: () => void | (() => void), deps?: any[]): void;
    export function useCallback<T extends (...args: any[]) => any>(callback: T, deps: any[]): T;
    export function useMemo<T>(factory: () => T, deps: any[]): T;
    export function useRef<T>(initial: T): { current: T };
    export function useContext<T>(context: any): T;
    export function createContext<T>(defaultValue: T): any;
    export function memo<T>(component: T): T;
    export function forwardRef<T, P>(render: (props: P, ref: any) => any): any;
    export type ReactNode = any;
    export type FC<P = {}> = (props: P) => any;
    export type ComponentProps<T> = any;
    export default React;
    const React: any;
}
declare module 'react-dom/client' {
    export function createRoot(container: any): { render(element: any): void };
}
''')
            
            # Lucide-react stub
            lucide_dir = os.path.join(temp_dir, 'node_modules', 'lucide-react')
            os.makedirs(lucide_dir, exist_ok=True)
            with open(os.path.join(lucide_dir, 'index.d.ts'), 'w') as f:
                f.write('declare module "lucide-react" { const icons: any; export = icons; }')
            with open(os.path.join(lucide_dir, 'package.json'), 'w') as f:
                f.write('{"name": "lucide-react", "types": "index.d.ts"}')
            
            # Framer-motion stub
            framer_dir = os.path.join(temp_dir, 'node_modules', 'framer-motion')
            os.makedirs(framer_dir, exist_ok=True)
            with open(os.path.join(framer_dir, 'index.d.ts'), 'w') as f:
                f.write('declare module "framer-motion" { export const motion: any; export const AnimatePresence: any; }')
            with open(os.path.join(framer_dir, 'package.json'), 'w') as f:
                f.write('{"name": "framer-motion", "types": "index.d.ts"}')
            
            # Run tsc
            result = subprocess.run(
                tsc_cmd + ['--noEmit', '--pretty', 'false'],
                cwd=temp_dir,
                capture_output=True,
                text=True,
                timeout=30,
            )
            
            if result.returncode == 0:
                return ValidationResult(passed=True)
            
            # Parse errors from stderr/stdout
            errors = self._parse_tsc_errors(result.stdout + result.stderr, temp_dir)
            
            return ValidationResult(
                passed=len(errors) == 0,
                errors=errors,
            )
            
        except subprocess.TimeoutExpired:
            logger.warning("TypeScript validation timed out")
            return ValidationResult(passed=True, warnings=["Validation timed out"])
        except Exception as e:
            logger.error(f"TypeScript validation error: {e}")
            return ValidationResult(passed=True, warnings=[f"Validation error: {str(e)}"])
        finally:
            # Clean up temp directory
            if temp_dir and os.path.exists(temp_dir):
                try:
                    shutil.rmtree(temp_dir)
                except Exception as e:
                    logger.warning(f"Failed to clean up temp dir: {e}")
    
    def _parse_tsc_errors(self, output: str, temp_dir: str) -> List[CompilationError]:
        """Parse TypeScript compiler output into structured errors."""
        errors = []
        
        # TypeScript error format: file(line,col): error TSxxxx: message
        pattern = r'([^(]+)\((\d+),(\d+)\):\s*(error|warning)\s+(TS\d+):\s*(.+)'
        
        for line in output.strip().split('\n'):
            line = line.strip()
            if not line:
                continue
            
            match = re.match(pattern, line)
            if match:
                file_path, line_num, col, severity, code, message = match.groups()
                
                # Convert temp path back to original path
                file_path = file_path.strip()
                if temp_dir in file_path:
                    file_path = file_path.replace(temp_dir + os.sep, '')
                
                # Add back src/ prefix if not present
                if not file_path.startswith('src/'):
                    file_path = f'src/{file_path}'
                
                if severity == 'error':
                    errors.append(CompilationError(
                        file=file_path,
                        line=int(line_num),
                        column=int(col),
                        message=message.strip(),
                        code=code,
                    ))
        
        return errors


# Singleton
_agentic_service: Optional[AgenticService] = None


def get_agentic_service() -> AgenticService:
    """Get singleton agentic service instance."""
    global _agentic_service
    if _agentic_service is None:
        _agentic_service = AgenticService()
    return _agentic_service

