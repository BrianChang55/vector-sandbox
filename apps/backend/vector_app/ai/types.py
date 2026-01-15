"""
Core types for the ai module.
"""
from dataclasses import dataclass
from typing import Any, Dict, Optional

from vector_app.ai.models import AIModel


@dataclass
class LLMSettings:
    """Runtime settings for LLM calls."""
    model: AIModel = AIModel.CLAUDE_SONNET_4_5
    temperature: float = 0.7
    max_tokens: Optional[int] = None
    timeout: float = 120.0


DEFAULT_LLM_SETTINGS = LLMSettings()


@dataclass
class ChatResult:
    """Result from a chat completion."""
    content: str
    model: AIModel
    finish_reason: str = "stop"
    usage: Optional[Dict[str, int]] = None
    raw: Optional[Dict[str, Any]] = None
    error: Optional[Exception] = None

    def validated(self, formatter=None, default=None):
        """
        Get the validated content, optionally applying a formatter.

        Args:
            formatter: Optional callable to transform the content (e.g., json.loads).
                       If None, returns content as-is.
            default: Value to return if content is empty or formatter raises an exception.
                     If None and content is empty, raises the stored error.

        Returns:
            The content (optionally formatted), or the default value.
        """
        if not self.content:
            if default is not None:
                return default
            if self.error:
                raise self.error
            raise AssertionError("ChatResult returned no value but no error was captured")

        formatter = formatter or (lambda x: x)
        try:
            return formatter(self.content)
        except Exception:
            return default


@dataclass
class StreamChunk:
    """A chunk from streaming response."""
    content: str
    done: bool = False
    error: Optional[str] = None

