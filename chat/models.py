"""
Chat app models - AI chat sessions and messages.
"""
import time

from django.conf import settings
from django.db import models

from internal_apps.utils.base_model import DjangoBaseModel
from internal_apps.utils.enum import choices
import uuid

from chat.types import ChatMessageRole, ChatMessageStatus, ChatMessageContext, CodeGenerationJobStatus, QuestioningStatus, QuestioningJobStatus



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
    context = models.CharField(
        max_length=20,
        choices=choices(ChatMessageContext),
        default=ChatMessageContext.BUILD,
        help_text="Which phase/tab this message belongs to (requirements or build)",
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


class QuestioningJob(DjangoBaseModel):
    """
    Tracks async questioning phase jobs with SSE event streaming.
    """

    questioning_session = models.ForeignKey(
        QuestioningSession,
        on_delete=models.CASCADE,
        related_name="jobs",
    )
    status = models.CharField(
        max_length=20,
        choices=choices(QuestioningJobStatus),
        default=QuestioningJobStatus.QUEUED,
    )
    events_json = models.JSONField(default=list, blank=True)  # List of {type, data, timestamp}
    current_question = models.TextField(blank=True)
    user_answer = models.TextField(blank=True)
    created_by = models.ForeignKey(
        settings.AUTH_USER_MODEL, on_delete=models.SET_NULL, null=True, blank=True, related_name="questioning_jobs"
    )

    class Meta:
        ordering = ["-created_at"]
        indexes = [
            models.Index(fields=["status", "-created_at"]),
            models.Index(fields=["questioning_session", "status"]),
        ]

    def __str__(self):
        return f"QuestioningJob {self.id} - {self.status}"

    def append_event(self, event_type: str, data: dict):
        """Append an event to events_json and save."""
        event = {
            "type": event_type,
            "data": data,
            "timestamp": time.time(),
            "index": len(self.events_json),
        }
        self.events_json.append(event)
        self.save(update_fields=["events_json", "updated_at"])
