"""
Prompt helpers for the legacy OpenAI AppSpec generation path.

Kept separate from OpenRouter to minimize coupling while ensuring prompts stay
documented and reusable.
"""

import json
from typing import Any, Dict, Optional


SYSTEM_PROMPT_TEMPLATE = """You are an expert at generating internal application specifications.

You generate AppSpec JSON that describes internal apps that read from backend resources via a runtime API.

Available Resources:
{resources_text}

AppSpec Structure:
{{
  "appName": "string",
  "pages": [
    {{
      "id": "string",
      "title": "string",
      "layout": "table_detail_drawer" | "tabbed_views",
      "primaryResource": "resource_id",
      "view": {{
        "table": {{
          "columns": [{{"field": "string", "label": "string"}}],
          "filterableFields": ["string"],
          "searchableFields": ["string"],
          "sort": {{"field": "string", "dir": "asc" | "desc"}},
          "pagination": {{"pageSize": number}},
          "rowActions": [{{"label": "string", "actionId": "string", "confirm": boolean}}],
          "bulkActions": [{{"label": "string", "actionId": "string", "confirm": boolean}}]
        }},
        "detailDrawer": {{
          "titleField": "string",
          "fields": [{{"field": "string", "label": "string", "readOnly": boolean}}],
          "actions": [{{"label": "string", "actionId": "string", "confirm": boolean}}]
        }}
      }}
    }}
  ]
}}

Rules:
- Only use resources and fields from the available resources list
- Only reference actionIds that are in the allowed_actions list
- Generate valid JSON only
- Be concise and practical"""

USER_PROMPT_WITH_SPEC_TEMPLATE = """Current AppSpec:
{current_spec}

User Request: {intent_message}

Generate an updated AppSpec JSON that addresses the user's request."""

USER_PROMPT_NEW_APP_TEMPLATE = """User Request: {intent_message}

Generate an AppSpec JSON for a new internal app based on the user's request."""


def _format_resources_text(registry_surface: Dict[str, Any]) -> str:
    """Render resource metadata for the system prompt."""
    return "\n".join(
        [
            f"- {r['resource_id']}: {r['resource_name']} (fields: {', '.join(r.get('exposed_fields', []))})"
            for r in registry_surface.get("resources", [])
        ]
    )


def build_system_prompt(registry_surface: Dict[str, Any]) -> str:
    """Build system prompt with registry context."""
    resources_text = _format_resources_text(registry_surface)

    return SYSTEM_PROMPT_TEMPLATE.format(resources_text=resources_text)




def build_user_prompt(intent_message: str, current_spec: Optional[Dict[str, Any]]) -> str:
    """Build user prompt with intent and current spec."""
    if current_spec:
        current_spec_text = json.dumps(current_spec, indent=2)
        return USER_PROMPT_WITH_SPEC_TEMPLATE.format(
            current_spec=current_spec_text, intent_message=intent_message
        )

    return USER_PROMPT_NEW_APP_TEMPLATE.format(intent_message=intent_message)



