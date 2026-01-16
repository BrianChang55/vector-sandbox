"""
Type definitions for the chat app.
"""
from enum import StrEnum


class ChatMessageRole(StrEnum):
    USER = "user"
    ASSISTANT = "assistant"
    SYSTEM = "system"


class ChatMessageStatus(StrEnum):
    PENDING = "pending"
    STREAMING = "streaming"
    COMPLETE = "complete"
    ERROR = "error"


class CodeGenerationJobStatus(StrEnum):
    QUEUED = "queued"
    PROCESSING = "processing"
    STREAMING = "streaming"
    COMPLETE = "complete"
    FAILED = "failed"
    CANCELLED = "cancelled"


class QuestioningStatus(StrEnum):
    IN_PROGRESS = "in_progress"
    COMPLETE = "complete"
    SKIPPED = "skipped"

class QuestioningJobStatus(StrEnum):
   QUEUED = "queued"
   PROCESSING = "processing"
   WAITING_FOR_ANSWER = "waiting_for_answer"
   COMPLETE = "complete"
   SKIPPED = "skipped"
   FAILED = "failed"


class ChatMessageContext(StrEnum):
    """Context in which a chat message was created (requirements vs build phase)."""
    REQUIREMENTS = "requirements"
    BUILD = "build"