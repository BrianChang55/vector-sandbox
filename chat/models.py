"""
Chat app models - AI chat sessions and messages.
"""
import time

from django.conf import settings
from django.db import models

from internal_apps.utils.base_model import DjangoBaseModel
from internal_apps.utils.enum import choices
import uuid

from chat.types import ChatMessageRole, ChatMessageStatus, CodeGenerationJobStatus, QuestioningStatus, VerificationStatus


class ChatSession(DjangoBaseModel):
    """
    A chat session for building an internal app.
    Each app can have multiple chat sessions (e.g., different features).
    """

    internal_app = models.ForeignKey('apps.InternalApp', on_delete=models.CASCADE, related_name="chat_sessions")
    title = models.CharField(max_length=255, default="New Chat")
    model_id = models.CharField(
        max_length=100, default="anthropic/claude-sonnet-4.5", help_text="AI model used for this session"
    )
    is_active = models.BooleanField(default=True)
    created_by = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.SET_NULL, null=True, related_name="chat_sessions")

    class Meta:
        ordering = ["-created_at"]
        indexes = [
            models.Index(fields=["internal_app", "-created_at"]),
        ]

    def __str__(self):
        return f"{self.title} - {self.internal_app.name}"


class ChatMessage(DjangoBaseModel):
    """
    Individual message in a chat session.
    Supports user messages, AI responses, and system messages.
    """

    session = models.ForeignKey(ChatSession, on_delete=models.CASCADE, related_name="messages")
    role = models.CharField(max_length=20, choices=choices(ChatMessageRole))
    content = models.TextField()
    status = models.CharField(
        max_length=20,
        choices=choices(ChatMessageStatus),
        default=ChatMessageStatus.COMPLETE,
    )

    # Metadata
    model_id = models.CharField(max_length=100, blank=True, null=True)
    token_count_input = models.IntegerField(null=True, blank=True)
    token_count_output = models.IntegerField(null=True, blank=True)
    duration_ms = models.IntegerField(null=True, blank=True)

    # For AI responses that generate code
    generated_spec_json = models.JSONField(
        null=True, blank=True, help_text="Generated AppSpec JSON if this was a spec generation"
    )
    generated_files = models.JSONField(
        null=True, blank=True, help_text="Generated code files {path: content}"
    )
    version_created = models.ForeignKey(
        'apps.AppVersion',
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name="source_messages",
        help_text="Version created from this message",
    )

    # Error handling
    error_message = models.TextField(null=True, blank=True)

    class Meta:
        ordering = ["created_at"]
        indexes = [
            models.Index(fields=["session", "created_at"]),
        ]

    def __str__(self):
        preview = self.content[:50] + "..." if len(self.content) > 50 else self.content
        return f"{self.role}: {preview}"


class QuestioningSession(DjangoBaseModel):
    """
    Tracks a multi-turn questioning phase for gathering requirements.
    Links to a ChatSession where Q&A happens via normal ChatMessages.
    Stores synthesized requirements after questioning completes.
    """

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    chat_session = models.OneToOneField(
        ChatSession, on_delete=models.CASCADE, related_name="questioning_session"
    )
    status = models.CharField(
        max_length=20,
        choices=choices(QuestioningStatus),
        default=QuestioningStatus.IN_PROGRESS,
    )
    synthesized_requirements = models.JSONField(
        default=dict, help_text="Final requirements document after questioning completes"
    )
    question_count = models.IntegerField(default=0, help_text="Number of questions asked")
    initial_request = models.TextField(help_text="User's original request that triggered questioning")

    class Meta:
        ordering = ["-created_at"]

    def __str__(self):
        return f"QuestioningSession({self.status}) - {self.chat_session}"


class CodeGenerationJob(DjangoBaseModel):
    """
    Tracks code generation jobs for async processing and streaming.

    Events are stored in events_json for replay on reconnection.
    The Celery worker appends events as they're generated, and the
    SSE endpoint streams them to the client.
    """

    chat_message = models.OneToOneField(
        ChatMessage,
        on_delete=models.CASCADE,
        related_name="generation_job",
        null=True,
        blank=True,
    )
    internal_app = models.ForeignKey('apps.InternalApp', on_delete=models.CASCADE, related_name="generation_jobs")
    version = models.ForeignKey(
        'apps.AppVersion',
        on_delete=models.CASCADE,
        related_name="generation_job",
        null=True,
        blank=True,
    )
    session = models.ForeignKey(
        ChatSession,
        on_delete=models.CASCADE,
        related_name="generation_jobs",
        null=True,
        blank=True,
    )
    questioning_session = models.ForeignKey(
        QuestioningSession,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name="generation_jobs",
        help_text="QuestioningSession that gathered requirements for this job",
    )

    # Generation parameters
    user_message = models.TextField(blank=True)
    model_id = models.CharField(max_length=100, default="anthropic/claude-sonnet-4.5")

    status = models.CharField(
        max_length=20,
        choices=choices(CodeGenerationJobStatus),
        default=CodeGenerationJobStatus.QUEUED,
    )

    # Streaming state - events stored for replay
    chunk_count = models.IntegerField(default=0)
    events_json = models.JSONField(default=list, blank=True)  # List of {type, data, timestamp}

    # Timing
    started_at = models.DateTimeField(null=True, blank=True)
    completed_at = models.DateTimeField(null=True, blank=True)

    # Error tracking
    error_message = models.TextField(null=True, blank=True)

    # User who initiated the job
    created_by = models.ForeignKey(
        settings.AUTH_USER_MODEL, on_delete=models.SET_NULL, null=True, blank=True, related_name="generation_jobs"
    )

    class Meta:
        ordering = ["-created_at"]
        indexes = [
            models.Index(fields=["internal_app", "status"]),
            models.Index(fields=["status", "-created_at"]),
        ]

    def __str__(self):
        return f"Job {self.id} - {self.status}"

    def append_event(self, event_type: str, data: dict):
        """Append an event to events_json and save."""
        event = {
            "type": event_type,
            "data": data,
            "timestamp": time.time(),
            "index": len(self.events_json),
        }
        self.events_json.append(event)
        self.chunk_count = len(self.events_json)
        self.save(update_fields=["events_json", "chunk_count", "updated_at"])

# ============================================================================
# File Verification Models (Handler Validation Service)
# ============================================================================


class FileVerificationRecord(DjangoBaseModel):
    """
    Record of verifying a single file during code generation.

    Stores the complete audit trail including all retry attempts,
    enabling analysis of verification patterns and LLM fix effectiveness.
    """

    # Optional link to the code generation job that triggered this verification
    code_generation_job = models.ForeignKey(
        CodeGenerationJob,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name="verification_records",
        help_text="The code generation job that triggered this verification",
    )

    # File identification
    file_path = models.CharField(max_length=500, help_text="Path of the verified file")
    file_type = models.CharField(max_length=20, help_text='File extension (e.g., ".ts", ".tsx")')

    # Verification result
    status = models.CharField(
        max_length=20,
        choices=choices(VerificationStatus),
        default=VerificationStatus.PENDING,
        help_text="Final verification status",
    )
    verifier_name = models.CharField(
        max_length=100,
        null=True,
        blank=True,
        help_text="Name of the verifier used (e.g., 'typescript')",
    )

    # Content and errors
    final_content = models.TextField(
        null=True,
        blank=True,
        help_text="The file content after all retries",
    )
    error_message = models.TextField(
        null=True,
        blank=True,
        help_text="Final error message if verification failed",
    )

    # Timing
    started_at = models.DateTimeField(null=True, blank=True, help_text="When verification started")
    completed_at = models.DateTimeField(null=True, blank=True, help_text="When verification completed")

    class Meta:
        ordering = ["-created_at"]
        indexes = [
            models.Index(fields=["code_generation_job", "-created_at"]),
            models.Index(fields=["status", "-created_at"]),
            models.Index(fields=["file_type", "-created_at"]),
        ]

    def __str__(self):
        return f"{self.file_path} ({self.status})"

    @property
    def attempt_count(self) -> int:
        """Number of verification attempts made."""
        return self.attempts.count()  # pylint: disable=no-member

    @property
    def passed(self) -> bool:
        """Whether the verification passed."""
        return self.status == VerificationStatus.PASSED

    @property
    def duration_seconds(self) -> float | None:
        """Duration of verification in seconds."""
        if self.started_at and self.completed_at:
            return (self.completed_at - self.started_at).total_seconds()
        return None


class VerificationAttemptRecord(DjangoBaseModel):
    """
    Record of a single verification attempt (for retry tracking).

    Each attempt captures the code content and result at that point,
    enabling analysis of how errors evolve across retries.
    """

    # Parent verification record
    verification_record = models.ForeignKey(
        FileVerificationRecord,
        on_delete=models.CASCADE,
        related_name="attempts",
        help_text="The verification record this attempt belongs to",
    )

    # Attempt metadata
    attempt_number = models.PositiveIntegerField(help_text="Attempt number (1-based)")

    # Content at this attempt
    content = models.TextField(help_text="The code content that was verified in this attempt")

    # Result of this attempt
    status = models.CharField(
        max_length=20,
        choices=choices(VerificationStatus),
        help_text="Status of this specific attempt",
    )
    error_message = models.TextField(
        null=True,
        blank=True,
        help_text="Error message if this attempt failed",
    )

    # Timing
    timestamp = models.DateTimeField(null=True, blank=True, help_text="When this attempt occurred")

    class Meta:
        ordering = ["attempt_number"]
        indexes = [
            models.Index(fields=["verification_record", "attempt_number"]),
        ]

    def __str__(self):
        return f"Attempt {self.attempt_number} for {self.verification_record.file_path}"
