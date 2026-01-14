"""
LLMClient for executing prompts via OpenRouter.
"""
import json
import logging
from typing import Any, Dict, Iterator, List, Optional

import httpx
from django.conf import settings

from vector_app.ai.models import AIModel
from vector_app.ai.exceptions import APIError, ChatFunctionError
from vector_app.ai.types import ChatResult, StreamChunk, LLMSettings, DEFAULT_LLM_SETTINGS
from vector_app.utils.enum_utils import safe_str_enum

logger = logging.getLogger(__name__)


class LLMClient:
    """
    Client for executing prompts via OpenRouter.

    Usage:
        client = get_llm_client()

        llm_settings = LLMSettings(temperature=0.3)

        # Non-streaming
        result = client.run(
            system_prompt="Be concise.",
            user_prompt="Summarize: Hello world",
            llm_settings=llm_settings,
        )

        # Streaming
        for chunk in client.stream(
            system_prompt="Be concise.",
            user_prompt="Summarize: Hello world",
            llm_settings=llm_settings,
        ):
            print(chunk.content, end="")
    """

    OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions"

    def __init__(self):
        self.api_key = getattr(settings, "OPENROUTER_API_KEY", None)
        self.app_name = getattr(settings, "OPENROUTER_APP_NAME", "Internal Apps Builder")
        self.site_url = getattr(settings, "BASE_URL", "http://localhost:8001")

    def _build_headers(self) -> Dict[str, str]:
        return {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json",
            "HTTP-Referer": self.site_url,
            "X-Title": self.app_name,
        }

    def _build_messages(
        self,
        system_prompt: str,
        user_prompt: str,
        chat_history: Optional[List[Dict[str, str]]] = None,
    ) -> List[Dict[str, str]]:
        """Build messages list for the API request."""
        messages = [{"role": "system", "content": system_prompt}]

        if chat_history:
            for msg in chat_history:
                messages.append(
                    {
                        "role": msg.get("role", "user"),
                        "content": msg.get("content", ""),
                    }
                )

        messages.append({"role": "user", "content": user_prompt})
        return messages

    def _build_body(
        self,
        messages: List[Dict[str, str]],
        llm_settings: LLMSettings,
        *,
        stream: bool = False,
        json_mode: bool = False,
    ) -> Dict[str, Any]:
        """Build the request body for the API."""
        body = {
            "model": llm_settings.model,
            "messages": messages,
            "temperature": llm_settings.temperature,
            "stream": stream,
        }
        if llm_settings.max_tokens:
            body["max_tokens"] = llm_settings.max_tokens
        if json_mode:
            body["response_format"] = {"type": "json_object"}
        return body

    def run(
        self,
        *,
        system_prompt: str,
        user_prompt: str,
        llm_settings: LLMSettings = DEFAULT_LLM_SETTINGS,
        json_mode: bool = False,
    ) -> ChatResult:
        """Execute a prompt and return the result."""
        if not self.api_key:
            raise ChatFunctionError("OpenRouter API key not configured")

        messages = self._build_messages(
            system_prompt=system_prompt,
            user_prompt=user_prompt,
        )
        
        body = self._build_body(
            messages=messages,
            llm_settings=llm_settings,
            stream=False,
            json_mode=json_mode,
        )

        logger.debug(
            "[LLMClient] Running prompt - settings: %s",
            llm_settings,
        )

        try:
            with httpx.Client(timeout=llm_settings.timeout) as client:
                response = client.post(
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json=body,
                )
                response.raise_for_status()

                result = response.json()
                choice = result.get("choices", [{}])[0]

                model = safe_str_enum(result.get("model"), llm_settings.model, AIModel)
                return ChatResult(
                    content=choice.get("message", {}).get("content", ""),
                    model=model,
                    finish_reason=choice.get("finish_reason", "stop"),
                    usage=result.get("usage"),
                    raw=result,
                )

        except httpx.HTTPStatusError as e:
            logger.error("API error: %s with response: %s", e.response.status_code, e.response.text)
            raise APIError(
                f"API request failed: {e.response.status_code} with response: {e.response.text}",
                status_code=e.response.status_code,
                response_body=e.response.text,
            ) from e
        except httpx.HTTPError as e:
            logger.error("HTTP error: %s", e)
            raise APIError(f"HTTP error: {str(e)}") from e
        except Exception as e:
            logger.error("Unexpected error: %s", e)
            raise ChatFunctionError(f"Unexpected error: {str(e)}") from e

    def stream(
        self,
        *,
        system_prompt: str,
        user_prompt: str,
        llm_settings: LLMSettings = DEFAULT_LLM_SETTINGS,
        chat_history: Optional[List[Dict[str, str]]] = None,
        json_mode: bool = False,
    ) -> Iterator[StreamChunk]:
        """Execute a prompt with streaming."""
        if not self.api_key:
            yield StreamChunk(content="", error="OpenRouter API key not configured")
            return

        messages = self._build_messages(
            system_prompt=system_prompt,
            user_prompt=user_prompt,
            chat_history=chat_history,
        )
        body = self._build_body(
            messages=messages,
            llm_settings=llm_settings,
            stream=True,
            json_mode=json_mode,
        )

        try:
            logger.info("Streaming LLM with settings: %s", llm_settings)
            with httpx.Client(timeout=llm_settings.timeout) as client:
                with client.stream(
                    "POST",
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json=body,
                ) as response:
                    response.raise_for_status()

                    for line in response.iter_lines():
                        if not line or not line.startswith("data: "):
                            continue

                        data = line[6:]
                        if data == "[DONE]":
                            break

                        try:
                            chunk = json.loads(data)
                            content = chunk.get("choices", [{}])[0].get("delta", {}).get("content", "")
                            if content:
                                yield StreamChunk(content=content)
                        except json.JSONDecodeError:
                            continue

            yield StreamChunk(content="", done=True)

        except httpx.HTTPStatusError as e:
            logger.error("API error: %s with response: %s", e.response.status_code, e.response.text)
            yield StreamChunk(content="", error=f"API error: {e.response.status_code} with response: {e.response.text}")
        except Exception as e:
            logger.error("Stream error: %s", e)
            yield StreamChunk(content="", error=str(e))


_llm_client: Optional[LLMClient] = None


def get_llm_client() -> LLMClient:
    """Get the singleton LLMClient instance."""
    global _llm_client
    if _llm_client is None:
        _llm_client = LLMClient()
    return _llm_client
