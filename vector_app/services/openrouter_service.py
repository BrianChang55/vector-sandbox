"""
OpenRouter AI Service for Code Generation

Production-ready service supporting multiple AI models via OpenRouter.
Includes streaming support for live updates (like Cursor, Lovable, Replit).
"""
import logging
import json
from typing import Dict, Any, List, Optional, Generator
from dataclasses import dataclass

from vector_app.ai.models import AIModel, MODEL_CONFIGS
from vector_app.ai.client import get_llm_client
from vector_app.ai.types import LLMSettings
from vector_app.prompts.openrouter import build_system_prompt, build_user_prompt

logger = logging.getLogger(__name__)


@dataclass
class StreamChunk:
    """Chunk of streaming response."""
    type: str  # 'content', 'thinking', 'error', 'done'
    content: str
    metadata: Optional[Dict[str, Any]] = None


class OpenRouterService:
    """
    Production-ready AI service using OpenRouter.
    
    Features:
    - Multi-model support with automatic routing
    - Streaming responses for real-time updates
    - Structured JSON output for AppSpec generation
    - Error handling and retries
    - Cost tracking
    """
    
    def get_available_models(self) -> List[Dict[str, Any]]:
        """Get list of available models with their configurations."""
        return [
            {
                "id": key,  # Use dictionary key as unique ID
                "model_id": config.model_id,  # Actual model ID for API calls
                "name": config.display_name,
                "description": config.description,
                "category": config.category,
                "context_length": config.context_length,
                "supports_streaming": config.supports_streaming,
                "recommended_for": config.recommended_for,
                "cost": {
                    "input": config.cost_per_million_input,
                    "output": config.cost_per_million_output,
                },
            }
            for key, config in MODEL_CONFIGS.items()
        ]
    
    def generate_app_spec(
        self,
        intent_message: str,
        current_spec: Optional[Dict[str, Any]],
        registry_surface: Dict[str, Any],
        model: AIModel = AIModel.CLAUDE_SONNET_4_5,
    ) -> Dict[str, Any]:
        """
        Generate AppSpec JSON from user intent (non-streaming).
        
        Args:
            intent_message: User's intent/prompt
            current_spec: Current AppSpec (if editing)
            registry_surface: Sanitized registry data
            model: Model ID to use
            
        Returns:
            AppSpec JSON dictionary
        """
        system_prompt = build_system_prompt(registry_surface, mode="appspec")
        
        user_prompt = build_user_prompt(intent_message, current_spec)
        
        try:
            result = get_llm_client().run(
                system_prompt=system_prompt,
                user_prompt=user_prompt,
                llm_settings=LLMSettings(
                    model=model,
                    temperature=0.3,
                    timeout=180.0,
                ),
                json_mode=True,
            )
            spec_json = result.validated(json.loads, default=None)

            # Validate basic structure
            if not isinstance(spec_json, dict) or 'appName' not in spec_json:
                raise ValueError("Invalid AppSpec structure")

            return spec_json

        except Exception as e:
            logger.error("Error generating AppSpec: %s", e)
            raise
    
    def generate_app_spec_streaming(
        self,
        intent_message: str,
        current_spec: Optional[Dict[str, Any]],
        registry_surface: Dict[str, Any],
        chat_history: Optional[List[Dict[str, str]]] = None,
        model: AIModel = AIModel.CLAUDE_SONNET_4_5,
    ) -> Generator[StreamChunk, None, None]:
        """
        Generate AppSpec with streaming response.
        
        Yields StreamChunk objects for real-time updates.
        """
        system_prompt = build_system_prompt(registry_surface, mode="appspec")

        # Add current request
        user_prompt = build_user_prompt(intent_message, current_spec)
        trimmed_history = chat_history[-10:] if chat_history else None
        
        try:
            full_content = ""
            llm_settings = LLMSettings(
                model=model,
                temperature=0.3,
                timeout=180.0,
            )
            for chunk in get_llm_client().stream(
                system_prompt=system_prompt,
                user_prompt=user_prompt,
                llm_settings=llm_settings,
                chat_history=trimmed_history,
                json_mode=True,
            ):
                if chunk.error:
                    yield StreamChunk(
                        type="error",
                        content=chunk.error,
                    )
                    return

                if chunk.content:
                    full_content += chunk.content
                    yield StreamChunk(
                        type="content",
                        content=chunk.content,
                        metadata={"accumulated": len(full_content)},
                    )

                if chunk.done:
                    break

            # Parse final JSON
            try:
                spec_json = json.loads(full_content)
                yield StreamChunk(
                    type="done",
                    content="",
                    metadata={"spec_json": spec_json},
                )
            except json.JSONDecodeError as e:
                yield StreamChunk(
                    type="error",
                    content=f"Failed to parse response as JSON: {str(e)}",
                    metadata={"raw_content": full_content},
                )

        except Exception as e:
            logger.error("Streaming error: %s", e)
            yield StreamChunk(type="error", content=str(e))
    
    def generate_code_streaming(
        self,
        intent_message: str,
        current_files: Optional[Dict[str, str]],
        registry_surface: Dict[str, Any],
        chat_history: Optional[List[Dict[str, str]]] = None,
        model: AIModel = AIModel.CLAUDE_SONNET_4_5,
    ) -> Generator[StreamChunk, None, None]:
        """
        Generate code files with streaming response.
        
        For direct code generation (bypassing AppSpec when user wants raw code).
        """
        system_prompt = build_system_prompt(registry_surface, mode="code")

        # Build user prompt with current code context
        user_prompt = f"Request: {intent_message}\n\n"
        if current_files:
            user_prompt += "Current files:\n"
            for path, content in current_files.items():
                user_prompt += f"\n--- {path} ---\n{content[:2000]}...\n"
        
        user_prompt += "\nGenerate the requested code changes. Wrap each file in ```filepath:path/to/file.tsx blocks."
        trimmed_history = chat_history[-10:] if chat_history else None
        
        try:
            full_content = ""
            llm_settings = LLMSettings(
                model=model,
                temperature=0.3,
                timeout=180.0,
            )
            for chunk in get_llm_client().stream(
                system_prompt=system_prompt,
                user_prompt=user_prompt,
                llm_settings=llm_settings,
                chat_history=trimmed_history,
            ):
                if chunk.error:
                    yield StreamChunk(type="error", content=chunk.error)
                    return

                if chunk.content:
                    full_content += chunk.content
                    yield StreamChunk(type="content", content=chunk.content)

                if chunk.done:
                    break

            # Parse files from response
            files = self._parse_code_blocks(full_content)
            yield StreamChunk(
                type="done",
                content="",
                metadata={"files": files},
            )

        except Exception as e:
            logger.error("Code generation error: %s", e)
            yield StreamChunk(type="error", content=str(e))
    
    def _parse_code_blocks(self, content: str) -> Dict[str, str]:
        """Parse code blocks from response into file dict."""
        import re
        
        files = {}
        
        # Match ```filepath:path/to/file.ext or ```path/to/file.ext
        pattern = r'```(?:filepath:)?([^\n`]+)\n(.*?)```'
        matches = re.findall(pattern, content, re.DOTALL)
        
        for filepath, code in matches:
            filepath = filepath.strip()
            # Clean up filepath
            if filepath.startswith(('tsx', 'ts', 'js', 'jsx', 'css', 'json')):
                continue  # Skip language-only markers
            files[filepath] = code.strip()
        
        return files


# Singleton instance
_openrouter_service: Optional[OpenRouterService] = None


def get_openrouter_service() -> OpenRouterService:
    """Get singleton OpenRouter service instance."""
    global _openrouter_service
    if _openrouter_service is None:
        _openrouter_service = OpenRouterService()
    return _openrouter_service

