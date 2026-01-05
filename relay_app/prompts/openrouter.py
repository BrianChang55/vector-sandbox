"""
Prompt helpers for OpenRouter-backed flows.

Separates system/user prompt construction for AppSpec and direct code generation
so both streaming and non-streaming entry points stay consistent.
"""

import json
from typing import Any, Dict, Optional


APP_SPEC_SYSTEM_PROMPT_TEMPLATE = """You are an expert internal application builder. You generate AppSpec JSON that describes internal apps.

IMPORTANT: Generated apps must call the Relay Runtime API for all data operations:
- Use runtimeQuery() for reads
- Use runtimeAction() for mutations
- Never call Supabase or any backend directly

Available Resources:
{resources_text}

AppSpec JSON Schema:
{{
  "appName": "string",
  "pages": [{{
    "id": "string",
    "title": "string", 
    "layout": "table_detail_drawer" | "tabbed_views" | "dashboard" | "form" | "kanban",
    "primaryResource": "resource_id",
    "view": {{
      "table": {{
        "columns": [{{"field": "string", "label": "string", "type": "text|number|date|badge|avatar"}}],
        "filterableFields": ["string"],
        "searchableFields": ["string"],
        "sort": {{"field": "string", "dir": "asc"|"desc"}},
        "pagination": {{"pageSize": number}},
        "rowActions": [{{"label": "string", "actionId": "string", "confirm": boolean, "variant": "default|destructive"}}],
        "bulkActions": [{{"label": "string", "actionId": "string", "confirm": boolean}}]
      }},
      "detailDrawer": {{
        "titleField": "string",
        "sections": [{{
          "title": "string",
          "fields": [{{"field": "string", "label": "string", "readOnly": boolean, "type": "text|textarea|select|date"}}]
        }}],
        "actions": [{{"label": "string", "actionId": "string", "confirm": boolean}}]
      }},
      "stats": [{{
        "label": "string",
        "value": "string",
        "change": "string",
        "trend": "up|down|neutral"
      }}]
    }}
  }}]
}}

Rules:
1. Only reference resources/fields from the available list
2. Only reference actionIds from allowed_actions
3. Generate complete, valid JSON
4. Design modern, professional UIs
5. Include helpful labels and sensible defaults"""

CODE_SYSTEM_PROMPT_TEMPLATE = """You are an expert React/TypeScript developer. You generate production-quality code for internal applications.

Tech Stack:
- React 18+ with TypeScript
- TailwindCSS for styling
- Lucide React for icons
- Framer Motion for animations
- React Query for data fetching

Available Resources:
{resources_text}

Runtime API (MUST use for all data operations):
```typescript
import {{ runtimeQuery, runtimeAction }} from '../lib/runtimeClient';

// Query data
const result = await runtimeQuery({{
  appId: string,
  versionId: string,
  resourceId: string,
  querySpec: {{
    select: string[],
    filters: Array<{{ field: string, op: string, value: any }}>,
    orderBy: Array<{{ field: string, dir: 'asc' | 'desc' }}>,
    limit: number,
    offset: number
  }}
}});

// Execute action
const result = await runtimeAction({{
  appId: string,
  versionId: string,
  actionId: string,
  args: Record<string, any>
}});
```

Guidelines:
1. Generate clean, modular code
2. Use TypeScript types properly
3. Handle loading and error states
4. Make responsive designs
5. Include helpful comments"""

USER_PROMPT_WITH_SPEC_TEMPLATE = """Current AppSpec:
{current_spec}

User Request: {intent_message}

Generate an updated AppSpec JSON that addresses the request. Return ONLY valid JSON."""

USER_PROMPT_NEW_APP_TEMPLATE = """User Request: {intent_message}

Generate an AppSpec JSON for a new internal app. Return ONLY valid JSON."""


def _format_resources_text(registry_surface: Dict[str, Any]) -> str:
    """Render available resources in a consistent bullet format."""
    if not registry_surface.get("resources"):
        return ""
    return "\n".join(
        [
            f"- {r['resource_id']}: {r['resource_name']} (fields: {', '.join(r.get('exposed_fields', []))})"
            for r in registry_surface.get("resources", [])
        ]
    )


def build_system_prompt(registry_surface: Dict[str, Any], mode: str = "appspec") -> str:
    """Build system prompt based on the requested OpenRouter mode."""
    resources_text = _format_resources_text(registry_surface)

    if mode == "appspec":
        return APP_SPEC_SYSTEM_PROMPT_TEMPLATE.format(
            resources_text=resources_text
            if resources_text
            else "No resources connected yet - generate a placeholder UI"
        )

    if mode == "code":
        return CODE_SYSTEM_PROMPT_TEMPLATE.format(
            resources_text=resources_text
            if resources_text
            else "No resources connected - use placeholder data"
        )

    return "You are a helpful AI coding assistant."




def build_user_prompt(intent_message: str, current_spec: Optional[Dict[str, Any]]) -> str:
    """Generate the user-facing prompt that accompanies OpenRouter system prompts."""
    if current_spec:
        return USER_PROMPT_WITH_SPEC_TEMPLATE.format(
            current_spec=json.dumps(current_spec, indent=2),
            intent_message=intent_message,
        )

    return USER_PROMPT_NEW_APP_TEMPLATE.format(intent_message=intent_message)



