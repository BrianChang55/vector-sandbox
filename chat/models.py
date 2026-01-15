"""
Chat app models - AI chat sessions and messages.
"""
import time

from django.conf import settings
from django.db import models

from internal_apps.utils.base_model import DjangoBaseModel
from internal_apps.utils.enum import choices
from chat.types import ChatMessageRole, ChatMessageStatus, CodeGenerationJobStatus


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
