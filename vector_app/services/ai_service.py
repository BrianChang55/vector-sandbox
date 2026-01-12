"""
AI Service for generating AppSpec from user intent
"""

import logging
import json
from typing import Dict, Any, List
from openai import OpenAI
from django.conf import settings

from vector_app.prompts.ai import build_system_prompt, build_user_prompt

logger = logging.getLogger(__name__)


class AIService:
    """Service for AI-powered AppSpec generation."""

    def __init__(self):
        api_key = getattr(settings, "OPENAI_API_KEY", None)
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
        system_prompt = build_system_prompt(registry_surface)
        user_prompt = build_user_prompt(intent_message, current_spec)

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
            if not isinstance(spec_json, dict) or "appName" not in spec_json or "pages" not in spec_json:
                raise ValueError("Invalid AppSpec structure returned by AI")

            return spec_json

        except Exception as e:
            logger.error(f"Error generating AppSpec: {e}")
            raise
