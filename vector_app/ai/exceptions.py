"""
Custom exceptions for the ai module.
"""


class ChatFunctionError(Exception):
    """Base exception for LLM client errors."""


class PromptFormattingError(ChatFunctionError):
    """Raised when prompt formatting fails."""


class APIError(ChatFunctionError):
    """Raised when the API request fails."""

    def __init__(self, message: str, status_code: int = None, response_body: str = None):
        super().__init__(message)
        self.status_code = status_code
        self.response_body = response_body

