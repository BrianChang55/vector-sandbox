"""
Prompt templates used by the agentic code generation workflow.

These helpers keep every model call aligned on tone, structure, and required
runtime constraints so changes can be made in one place.
"""

from typing import Any, Dict, List, Sequence

# Shared style guide injected into all design-aware prompts.
DESIGN_STYLE_PROMPT = """This base style guide (enterprise, light theme, minimal):
- Backgrounds: bg-gray-50 for canvas, bg-white surfaces; borders: border-gray-200 (hover border-gray-300); subtle shadow-sm only when needed.
- Typography: text-gray-900 primary, text-gray-700 body, placeholders text-gray-500; system font stack.
- Spacing: p-4/p-6 sections, gap-2/3/4; clean layouts, clear hierarchy.
- Buttons: primary bg-gray-900 hover:bg-gray-800, secondary bg-gray-100 hover:bg-gray-200, outline border-gray-200 bg-white hover:bg-gray-50; rounded-md.
- Corners: cards rounded-md, inputs/buttons rounded-md; minimal accents; avoid dark/gradient themes."""

# Base system prompt for codegen; design style is injected dynamically.
CODEGEN_SYSTEM_PROMPT_TEMPLATE = """You are an expert React/TypeScript developer building internal business applications.

CRITICAL: For all data operations, you MUST use the Runtime API:

```typescript
// In src/lib/runtime.ts
export async function runtimeQuery(params: {
  resourceId: string;
  querySpec: {
    select?: string[];
    filters?: Array<{ field: string; op: string; value: any }>;
    orderBy?: Array<{ field: string; dir: 'asc' | 'desc' }>;
    limit?: number;
    offset?: number;
  };
}): Promise<{ data: any[]; count: number }> {
  const config = window.__RELAY_CONFIG__;
  const response = await fetch(`${config.apiBaseUrl}/runtime/query/`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      appId: config.appId,
      versionId: config.versionId,
      ...params,
    }),
  });
  return response.json();
}

export async function runtimeAction(params: {
  actionId: string;
  args: Record<string, any>;
}): Promise<{ success: boolean; data: any }> {
  const config = window.__RELAY_CONFIG__;
  const response = await fetch(`${config.apiBaseUrl}/runtime/action/`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      appId: config.appId,
      versionId: config.versionId,
      ...params,
    }),
  });
  return response.json();
}
```

Stack:
- React 18+ with TypeScript
- TailwindCSS (utility-first)
- Lucide React for icons
- No external state libraries (use React hooks)

Guidelines:
1. Generate complete, runnable code
2. Handle loading and error states elegantly
3. Use professional, modern UI patterns
4. Include proper TypeScript types
5. Make responsive designs
6. Use semantic HTML
7. Add helpful comments

Base design/style requirements (always follow):
{design_style}

NEVER import from node_modules that aren't available. Use inline implementations."""

PLAN_PROMPT_TEMPLATE = """You are planning an internal app generation task.

User Request: {user_message}
App Name: {app_name}
Available Resources: {available_resources}
Has Existing Spec: {has_existing_spec}

Generate a plan with 2-4 steps (fewer for simple requests). Return JSON:
{{
    "reasoning": "Your analysis of what needs to be built",
    "steps": [
        {{"type": "design", "title": "Design App Structure", "description": "..."}}
    ]
}}

Step types: research, design, code, component, styling, integration, validation
Keep it concise but comprehensive."""

STEP_PROMPT_TEMPLATE = """Step {step_number}: {step_title}
Description: {step_description}

User's Original Request: {user_message}
App Name: {app_name}

Available Data Resources:
{resources_info}
{existing_code}

CRITICAL: Generate complete, working React code for this step.

OUTPUT FORMAT - You MUST format each file exactly like this:
```src/App.tsx
import React from 'react';
// ... complete code here
export default function App() {{ ... }}
```

Another example:
```src/components/DataTable.tsx
// Component code here
```

Requirements:
- Use TypeScript with proper types
- Use Tailwind CSS for all styling (available via CDN)
- For data, use realistic mock data arrays (the runtime API will be connected later)
- Create a complete, functional UI that looks professional
- Include proper loading states and error handling
- Make sure the code is complete and runnable - no placeholders or TODOs

If building a table/data display:
- Include sample data inline as a const array
- Add edit/delete functionality with state management
- Include search/filter if appropriate"""

FINAL_APP_PROMPT_TEMPLATE = """Generate the MAIN App.tsx file that creates a complete, polished application.

User's Request: {user_message}
App Name: {app_name}

Generated Components Available:
{components_info}

Other Files Generated:
{other_files}

CRITICAL REQUIREMENTS:
1. Create a COMPLETE App.tsx that is the main entry point
2. Import and USE all the generated components
3. Include proper state management for the app
4. Add a professional header/layout
5. Include realistic mock data for demonstration
6. Make it look polished and production-ready
7. Use Tailwind CSS for all styling
8. Handle loading and error states
9. Make it interactive (search, filters, etc. as appropriate)

OUTPUT FORMAT - Generate ONLY the App.tsx file:
```src/App.tsx
import React, {{ useState }} from 'react';
// ... imports for all components
// ... your complete app code
export default function App() {{
  // ... complete implementation
}}
```

DO NOT generate placeholder code or TODOs. Generate the COMPLETE working app."""

# Dedicated system prompt for the final App.tsx synthesis step.
FINAL_APP_SYSTEM_PROMPT = (
    "You are an expert React developer. Generate complete, production-ready code. Never use placeholders."
)


def apply_design_style_prompt(message: str) -> str:
    """Append the shared design/style guide to a user message."""
    base_message = (message or "").strip()
    design_prompt = f"------ Design & Style Requirements ------\n\n{DESIGN_STYLE_PROMPT}"
    if not base_message:
        return design_prompt
    return f"{base_message}\n\n{design_prompt}"


def build_plan_prompt(user_message: str, context: Dict[str, Any]) -> str:
    """Format the planning prompt with user intent and runtime context."""
    return PLAN_PROMPT_TEMPLATE.format(
        user_message=user_message,
        app_name=context.get("app_name", "App"),
        available_resources=", ".join(context.get("available_resources", ["none"])),
        has_existing_spec=context.get("has_existing_spec", False),
    )




def build_step_prompt(
    step: Any,
    step_index: int,
    user_message: str,
    context: Dict[str, Any],
    existing_files: Sequence[Any],
    registry_surface: Dict[str, Any],
) -> str:
    """Build the per-step execution prompt for streaming code generation."""
    existing_code = ""
    if existing_files:
        existing_code = "\n\nExisting files generated so far:\n"
        for f in list(existing_files)[-3:]:
            path = getattr(f, "path", "")
            content = getattr(f, "content", "")
            existing_code += f"\n--- {path} ---\n{content[:1000]}...\n"

    resources_info = ""
    for r in registry_surface.get("resources", []):
        resources_info += f"- {r['resource_id']}: fields={r.get('exposed_fields', [])}\n"

    return STEP_PROMPT_TEMPLATE.format(
        step_number=step_index + 1,
        step_title=getattr(step, "title", ""),
        step_description=getattr(step, "description", ""),
        user_message=user_message,
        app_name=context.get("app_name", "App"),
        resources_info=resources_info.strip()
        if resources_info
        else "No resources available - use realistic mock data for demonstration",
        existing_code=existing_code.strip(),
    )




def build_codegen_system_prompt(registry_surface: Dict[str, Any]) -> str:
    """Return the system prompt for code generation with injected design style."""
    return CODEGEN_SYSTEM_PROMPT_TEMPLATE.replace("{design_style}", DESIGN_STYLE_PROMPT)


def build_final_app_prompt(
    user_message: str,
    context: Dict[str, Any],
    components_info: str,
    other_files: List[Any],
) -> str:
    """Format the final App.tsx synthesis prompt."""
    return FINAL_APP_PROMPT_TEMPLATE.format(
        user_message=user_message,
        app_name=context.get("app_name", "App"),
        components_info=components_info,
        other_files=[getattr(f, "path", f) for f in other_files],
    )



