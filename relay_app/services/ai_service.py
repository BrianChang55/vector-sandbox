"""
AI Service for generating AppSpec from user intent
"""
import logging
import json
from typing import Dict, Any, List
from openai import OpenAI
from django.conf import settings

logger = logging.getLogger(__name__)


class AIService:
    """Service for AI-powered AppSpec generation."""
    
    def __init__(self):
        api_key = getattr(settings, 'OPENAI_API_KEY', None)
        if not api_key:
            logger.warning("OPENAI_API_KEY not set - AI features will not work")
        self.client = OpenAI(api_key=api_key) if api_key else None
    
    def generate_app_spec(
        self,
        intent_message: str,
        current_spec: Dict[str, Any] | None,
        registry_surface: Dict[str, Any],
    ) -> Dict[str, Any]:
        """
        Generate AppSpec JSON from user intent.
        
        Args:
            intent_message: User's intent/prompt
            current_spec: Current AppSpec (if editing)
            registry_surface: Sanitized registry data (enabled resources, exposed fields, allowed actions)
            
        Returns:
            AppSpec JSON dictionary
        """
        if not self.client:
            raise ValueError("OpenAI API key not configured")
        
        # Build prompt
        system_prompt = self._build_system_prompt(registry_surface)
        user_prompt = self._build_user_prompt(intent_message, current_spec)
        
        # Call OpenAI with structured output
        try:
            response = self.client.chat.completions.create(
                model="gpt-4o-mini",  # Use GPT-4o-mini for cost-effectiveness, can upgrade to gpt-4o for better quality
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": user_prompt},
                ],
                response_format={"type": "json_object"},  # Ensure JSON output
                temperature=0.3,  # Lower temperature for more deterministic output
            )
            
            content = response.choices[0].message.content
            spec_json = json.loads(content)
            
            # Validate basic structure
            if not isinstance(spec_json, dict) or 'appName' not in spec_json or 'pages' not in spec_json:
                raise ValueError("Invalid AppSpec structure returned by AI")
            
            return spec_json
            
        except Exception as e:
            logger.error(f"Error generating AppSpec: {e}")
            raise
    
    def _build_system_prompt(self, registry_surface: Dict[str, Any]) -> str:
        """Build system prompt with registry context."""
        resources_text = "\n".join([
            f"- {r['resource_id']}: {r['resource_name']} (fields: {', '.join(r.get('exposed_fields', []))})"
            for r in registry_surface.get('resources', [])
        ])
        
        return f"""You are an expert at generating internal application specifications.

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
    
    def _build_user_prompt(self, intent_message: str, current_spec: Dict[str, Any] | None) -> str:
        """Build user prompt with intent and current spec."""
        if current_spec:
            current_spec_text = json.dumps(current_spec, indent=2)
            return f"""Current AppSpec:
{current_spec_text}

User Request: {intent_message}

Generate an updated AppSpec JSON that addresses the user's request."""
        else:
            return f"""User Request: {intent_message}

Generate an AppSpec JSON for a new internal app based on the user's request."""

