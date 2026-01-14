"""
OpenRouter AI Service for Code Generation

Production-ready service supporting multiple AI models via OpenRouter.
Includes streaming support for live updates (like Cursor, Lovable, Replit).
"""
import logging
import json
from typing import Dict, Any, List, Optional, Generator, AsyncGenerator
from dataclasses import dataclass
import httpx
from django.conf import settings

from vector_app.ai.models import AIModel, MODEL_CONFIGS
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
    
    OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions"
    
    def __init__(self):
        self.api_key = getattr(settings, 'OPENROUTER_API_KEY', None)
        if not self.api_key:
            # Fall back to OpenAI key if OpenRouter not set
            self.api_key = getattr(settings, 'OPENAI_API_KEY', None)
            if self.api_key:
                logger.info("Using OpenAI API key for OpenRouter (fallback mode)")
        
        self.app_name = getattr(settings, 'OPENROUTER_APP_NAME', 'Internal Apps Builder')
        self.site_url = getattr(settings, 'BASE_URL', 'http://localhost:8001')
    
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
    
    def _build_headers(self) -> Dict[str, str]:
        """Build request headers for OpenRouter."""
        return {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json",
            "HTTP-Referer": self.site_url,
            "X-Title": self.app_name,
        }
    
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
        if not self.api_key:
            raise ValueError("OpenRouter/OpenAI API key not configured")
        
        system_prompt = build_system_prompt(registry_surface, mode="appspec")
        
        user_prompt = build_user_prompt(intent_message, current_spec)
        
        try:
            with httpx.Client(timeout=180.0) as client:
                response = client.post(
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json={
                        "model": model,
                        "messages": [
                            {"role": "system", "content": system_prompt},
                            {"role": "user", "content": user_prompt},
                        ],
                        "response_format": {"type": "json_object"},
                        "temperature": 0.3,
                    },
                )
                response.raise_for_status()
                
                result = response.json()
                content = result["choices"][0]["message"]["content"]
                spec_json = json.loads(content)
                
                # Validate basic structure
                if not isinstance(spec_json, dict) or 'appName' not in spec_json:
                    raise ValueError("Invalid AppSpec structure")
                
                return spec_json
                
        except Exception as e:
            logger.error(f"Error generating AppSpec: {e}")
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
        if not self.api_key:
            yield StreamChunk(type="error", content="API key not configured")
            return
        
        system_prompt = build_system_prompt(registry_surface, mode="appspec")
        
        messages = [{"role": "system", "content": system_prompt}]
        
        # Add chat history if provided
        if chat_history:
            for msg in chat_history[-10:]:  # Last 10 messages for context
                messages.append({
                    "role": msg.get("role", "user"),
                    "content": msg.get("content", ""),
                })
        
        # Add current request
        user_prompt = build_user_prompt(intent_message, current_spec)
        messages.append({"role": "user", "content": user_prompt})
        
        try:
            with httpx.Client(timeout=180.0) as client:
                with client.stream(
                    "POST",
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json={
                        "model": model,
                        "messages": messages,
                        "response_format": {"type": "json_object"},
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
                                    yield StreamChunk(
                                        type="content",
                                        content=content,
                                        metadata={"accumulated": len(full_content)},
                                    )
                            except json.JSONDecodeError:
                                continue
                    
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
                        
        except httpx.HTTPStatusError as e:
            yield StreamChunk(
                type="error",
                content=f"API error: {e.response.status_code}",
            )
        except Exception as e:
            logger.error(f"Streaming error: {e}")
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
        if not self.api_key:
            yield StreamChunk(type="error", content="API key not configured")
            return
        
        system_prompt = build_system_prompt(registry_surface, mode="code")
        
        messages = [{"role": "system", "content": system_prompt}]
        
        if chat_history:
            for msg in chat_history[-10:]:
                messages.append({
                    "role": msg.get("role", "user"),
                    "content": msg.get("content", ""),
                })
        
        # Build user prompt with current code context
        user_prompt = f"Request: {intent_message}\n\n"
        if current_files:
            user_prompt += "Current files:\n"
            for path, content in current_files.items():
                user_prompt += f"\n--- {path} ---\n{content[:2000]}...\n"
        
        user_prompt += "\nGenerate the requested code changes. Wrap each file in ```filepath:path/to/file.tsx blocks."
        messages.append({"role": "user", "content": user_prompt})
        
        try:
            with httpx.Client(timeout=180.0) as client:
                with client.stream(
                    "POST",
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json={
                        "model": model,
                        "messages": messages,
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
                                    yield StreamChunk(type="content", content=content)
                            except json.JSONDecodeError:
                                continue
                    
                    # Parse files from response
                    files = self._parse_code_blocks(full_content)
                    yield StreamChunk(
                        type="done",
                        content="",
                        metadata={"files": files},
                    )
                        
        except Exception as e:
            logger.error(f"Code generation error: {e}")
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

