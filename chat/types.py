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


class VerificationStatus(StrEnum):
    """Status of a file verification."""

    PENDING = "pending"  # Not yet verified
    PASSED = "passed"  # Verification succeeded
    FAILED = "failed"  # Verification failed (code has errors)
    SKIPPED = "skipped"  # No verifier available for file type
    ERROR = "error"  # Verifier service error (tsc not found, timeout)
