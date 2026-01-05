"""
Agentic Code Generation Service

Implements the Research → Plan → Execute → Validate workflow
for autonomous app generation with visible progress and thinking.
"""
import logging
import json
import re
import time
import uuid
from typing import Dict, Any, List, Optional, Generator, Tuple, TYPE_CHECKING
from dataclasses import dataclass, asdict, field
from enum import Enum

from django.conf import settings
import httpx

from relay_app.prompts.agentic import (
    DESIGN_STYLE_PROMPT,
    FINAL_APP_SYSTEM_PROMPT,
    apply_design_style_prompt,
    build_plan_prompt,
    build_step_prompt,
    build_codegen_system_prompt,
    build_final_app_prompt,
)
from relay_app.services.data_store_context import (
    build_data_store_context,
    get_table_summary,
)

if TYPE_CHECKING:
    from relay_app.models import InternalApp, AppVersion

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


class AgenticService:
    """
    Agentic code generation service.
    
    Implements a multi-phase workflow:
    1. Research: Analyze context, understand requirements
    2. Plan: Create structured execution plan
    3. Execute: Generate code step-by-step
    4. Validate: Verify generated code works
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
    
    def generate_app(
        self,
        user_message: str,
        current_spec: Optional[Dict[str, Any]],
        registry_surface: Dict[str, Any],
        app_name: str,
        model: str = "anthropic/claude-sonnet-4",
        app: Optional['InternalApp'] = None,
        version: Optional['AppVersion'] = None,
    ) -> Generator[AgentEvent, None, None]:
        """
        Generate an app with the full agentic workflow.
        
        Yields AgentEvent objects for real-time progress updates.
        
        Args:
            user_message: The user's request
            current_spec: Current app specification if exists
            registry_surface: Available data resources
            app_name: Name of the app
            model: LLM model to use
            app: Optional InternalApp for data store operations
            version: Optional AppVersion for versioned table operations
        """
        session_id = str(uuid.uuid4())
        start_time = time.time()
        
        # Build data store context if app is provided
        data_store_context = None
        if app:
            data_store_context = build_data_store_context(app)
        
        styled_user_message = apply_design_style_prompt(user_message, data_store_context)
        
        # Emit start event
        yield AgentEvent("agent_start", {
            "session_id": session_id,
            "goal": user_message.strip(),
        })
        
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
        context_analysis = self._analyze_context(
            styled_user_message, current_spec, registry_surface, app_name, app
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
                    app=app, version=version, data_store_context=data_store_context
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
        
        # ===== PHASE 4: VALIDATE =====
        yield AgentEvent("phase_change", {
            "phase": AgentPhase.VALIDATING.value,
            "message": "Validating generated code...",
        })
        
        validation_result = self._validate_code(generated_files)
        
        yield AgentEvent("validation_result", {
            "passed": validation_result["passed"],
            "errors": validation_result.get("errors", []),
            "warnings": validation_result.get("warnings", []),
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
        
        yield AgentEvent("agent_complete", {
            "duration": total_duration,
            "filesGenerated": len(generated_files),
            "plan_id": plan.id,
            "summary": " ".join(summary_parts) + ". Your app is ready to preview!",
        })
        
        # Emit done event to signal stream completion
        yield AgentEvent("done", {
            "success": True,
            "filesGenerated": len(generated_files),
            "duration": total_duration,
        })
    
    def _analyze_context(
        self,
        user_message: str,
        current_spec: Optional[Dict[str, Any]],
        registry_surface: Dict[str, Any],
        app_name: str,
        app: Optional['InternalApp'] = None,
    ) -> Dict[str, Any]:
        """Analyze the current context and requirements."""
        
        resources = registry_surface.get("resources", [])
        
        # Get data store summary if app is provided
        data_store_summary = ""
        if app:
            data_store_summary = get_table_summary(app)
        
        analysis = {
            "app_name": app_name,
            "has_existing_spec": current_spec is not None,
            "available_resources": [r["resource_id"] for r in resources],
            "resource_details": resources,
            "user_intent": user_message,
            "data_store_summary": data_store_summary,
            "has_data_store": bool(app),
            "analysis": f"Building '{app_name}' with {len(resources)} available data sources.",
        }
        
        if data_store_summary and "No data tables" not in data_store_summary:
            analysis["analysis"] += f" App has existing data tables."
        
        if current_spec:
            analysis["current_pages"] = len(current_spec.get("pages", []))
            analysis["analysis"] += f" Modifying existing app with {analysis['current_pages']} pages."
        else:
            analysis["analysis"] += " Creating a new app from scratch."
        
        return analysis
    
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
    ) -> Generator[AgentEvent, None, None]:
        """Execute a single plan step and yield progress events."""
        
        # Build prompt for this step with data store context
        prompt = build_step_prompt(
            step, step_index, user_message, context, 
            existing_files, registry_surface, data_store_context
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
                
                files.append(FileChange(
                    path=normalized_path,
                    action='create',
                    language=lang_map.get(ext, 'tsx'),
                    content=code,
                ))
        
        # If no files found, try to extract any code and create a default file
        if not files and '```' in content:
            # Extract all code blocks with language hints
            code_blocks = re.findall(r'```(?:tsx?|jsx?|typescript|javascript)?\n(.*?)```', content, re.DOTALL)
            
            if code_blocks:
                # Use the largest code block as the main component
                largest_block = max(code_blocks, key=len).strip()
                
                if len(largest_block) > 50:  # Only if substantial
                    # Determine file based on content and step type
                    if 'export default' in largest_block or 'function App' in largest_block:
                        files.append(FileChange("src/App.tsx", "create", "tsx", largest_block))
                    elif step.type == "styling":
                        files.append(FileChange("src/styles/main.css", "create", "css", largest_block))
                    elif step.type == "component":
                        # Try to extract component name
                        name_match = re.search(r'(?:function|const)\s+(\w+)', largest_block)
                        comp_name = name_match.group(1) if name_match else "Component"
                        files.append(FileChange(f"src/components/{comp_name}.tsx", "create", "tsx", largest_block))
                    else:
                        files.append(FileChange("src/App.tsx", "create", "tsx", largest_block))
        
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
        from relay_app.models import AppDataTable
        from relay_app.services.app_data_service import AppDataService
        
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


# Singleton
_agentic_service: Optional[AgenticService] = None


def get_agentic_service() -> AgenticService:
    """Get singleton agentic service instance."""
    global _agentic_service
    if _agentic_service is None:
        _agentic_service = AgenticService()
    return _agentic_service

