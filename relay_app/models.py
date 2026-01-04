"""
Relay Internal Apps - Data Models

This module contains all Django models for the Relay Internal Apps platform.
"""
import uuid
import json
from django.contrib.auth.models import AbstractUser
from django.db import models
from django.core.exceptions import ValidationError

from internal_apps.utils.base_model import BaseModel
from .utils.encryption import encrypt_json, decrypt_json, encrypt_string, decrypt_string


# ============================================================================
# Base User and Organization Models
# ============================================================================

class User(AbstractUser, BaseModel):
    """
    User model that extends Django's AbstractUser.
    Users can belong to multiple organizations.
    
    Supports:
    - Email/password authentication
    - Google OAuth authentication
    - JWT token-based API authentication
    """
    email = models.EmailField(unique=True)
    username = models.CharField(max_length=150, unique=True, null=True, blank=True)
    
    # OAuth integration
    google_id = models.CharField(
        max_length=255,
        unique=True,
        null=True,
        blank=True,
        help_text='Google user ID for OAuth'
    )
    profile_image_url = models.URLField(
        max_length=500,
        blank=True,
        null=True,
        help_text='Profile image URL from OAuth provider'
    )
    
    USERNAME_FIELD = 'email'
    REQUIRED_FIELDS = []
    
    def __str__(self):
        return self.email


class Organization(BaseModel):
    """
    Organization model representing a company/team (Project container).
    """
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    name = models.CharField(max_length=255)
    slug = models.SlugField(max_length=255, unique=True)
    logo = models.ImageField(
        upload_to='org_logos/',
        null=True,
        blank=True,
        help_text='Organization logo image'
    )
    
    def __str__(self):
        return self.name


class UserOrganization(BaseModel):
    """
    Junction table for User-Organization many-to-many relationship.
    Includes role field for membership permissions.
    """
    ROLE_ADMIN = 'admin'
    ROLE_MEMBER = 'member'
    
    ROLE_CHOICES = [
        (ROLE_ADMIN, 'Admin'),
        (ROLE_MEMBER, 'Member'),
    ]
    
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name='user_organizations')
    organization = models.ForeignKey(Organization, on_delete=models.CASCADE, related_name='user_organizations')
    role = models.CharField(max_length=20, choices=ROLE_CHOICES, default=ROLE_MEMBER)
    
    class Meta:
        unique_together = ['user', 'organization']
    
    def __str__(self):
        return f"{self.user.email} - {self.organization.name} ({self.role})"


# ============================================================================
# Backend Connection Models
# ============================================================================

class BackendConnection(BaseModel):
    """
    Represents a connected backend configured for an organization.
    Stores encrypted configuration (service role keys, etc.).
    """
    ADAPTER_SUPABASE = 'supabase'
    ADAPTER_POSTGRESQL = 'postgresql'
    ADAPTER_MYSQL = 'mysql'
    
    ADAPTER_CHOICES = [
        (ADAPTER_SUPABASE, 'Supabase'),
        (ADAPTER_POSTGRESQL, 'PostgreSQL'),
        (ADAPTER_MYSQL, 'MySQL'),
    ]
    
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    organization = models.ForeignKey(Organization, on_delete=models.CASCADE, related_name='backend_connections')
    adapter_type = models.CharField(max_length=50, choices=ADAPTER_CHOICES, default=ADAPTER_SUPABASE)
    display_name = models.CharField(max_length=255)
    config_encrypted = models.TextField(help_text='Encrypted JSON configuration')
    
    def set_config(self, config: dict):
        """Set encrypted configuration."""
        self.config_encrypted = encrypt_json(config)
    
    def get_config(self) -> dict:
        """Get decrypted configuration."""
        if not self.config_encrypted:
            return {}
        return decrypt_json(self.config_encrypted)


class ProjectUserBackendAuth(BaseModel):
    """
    Stores user-specific JWT tokens for backend connections (encrypted).
    """
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name='backend_auths')
    backend_connection = models.ForeignKey(BackendConnection, on_delete=models.CASCADE, related_name='user_auths')
    user_jwt_encrypted = models.TextField(help_text='Encrypted user JWT')
    
    class Meta:
        unique_together = ['user', 'backend_connection']
    
    def set_jwt(self, jwt: str):
        """Set encrypted JWT."""
        self.user_jwt_encrypted = encrypt_string(jwt)
    
    def get_jwt(self) -> str:
        """Get decrypted JWT."""
        if not self.user_jwt_encrypted:
            return ""
        return decrypt_string(self.user_jwt_encrypted)


# ============================================================================
# Internal App Models
# ============================================================================

class InternalApp(BaseModel):
    """
    Internal application created by users.
    """
    STATUS_DRAFT = 'draft'
    STATUS_PUBLISHED = 'published'
    
    STATUS_CHOICES = [
        (STATUS_DRAFT, 'Draft'),
        (STATUS_PUBLISHED, 'Published'),
    ]
    
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    organization = models.ForeignKey(Organization, on_delete=models.CASCADE, related_name='internal_apps')
    name = models.CharField(max_length=255)
    description = models.TextField(blank=True)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default=STATUS_DRAFT)
    backend_connection = models.ForeignKey(
        BackendConnection, 
        on_delete=models.PROTECT, 
        related_name='internal_apps',
        null=True,
        blank=True,
        help_text='Optional backend connection for data access'
    )
    allow_actions_in_preview = models.BooleanField(default=False)
    created_by = models.ForeignKey(User, on_delete=models.SET_NULL, null=True, related_name='created_apps')
    
    def __str__(self):
        return f"{self.name} ({self.organization.name})"


class ResourceRegistryEntry(BaseModel):
    """
    Registry entry for a resource available from a backend connection.
    Used for safety gates and validation.
    """
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    organization = models.ForeignKey(Organization, on_delete=models.CASCADE, related_name='resource_registry_entries')
    backend_connection = models.ForeignKey(BackendConnection, on_delete=models.CASCADE, related_name='resource_registry_entries')
    resource_id = models.CharField(max_length=255)
    resource_name = models.CharField(max_length=255)
    schema_json = models.JSONField(help_text='JSON schema for this resource')
    enabled = models.BooleanField(default=True)
    exposed_fields_json = models.JSONField(default=list, help_text='List of field names that are exposed')
    ui_constraints_json = models.JSONField(default=dict, help_text='UI constraints (read_only, etc.)')
    allowed_actions_json = models.JSONField(default=list, help_text='List of allowed ActionDef JSON objects')
    
    class Meta:
        unique_together = ['backend_connection', 'resource_id']
        indexes = [
            models.Index(fields=['backend_connection', 'resource_id']),
        ]
    
    def __str__(self):
        return f"{self.resource_id} ({self.backend_connection.display_name})"


class AppVersion(BaseModel):
    """
    Version of an internal app (immutable snapshot).
    """
    SOURCE_AI = 'ai'
    SOURCE_CODE = 'code'
    SOURCE_PUBLISH = 'publish'
    
    SOURCE_CHOICES = [
        (SOURCE_AI, 'AI'),
        (SOURCE_CODE, 'Code'),
        (SOURCE_PUBLISH, 'Publish'),
    ]
    
    # Generation status for agentic workflow
    GEN_STATUS_PENDING = 'pending'
    GEN_STATUS_GENERATING = 'generating'
    GEN_STATUS_COMPLETE = 'complete'
    GEN_STATUS_ERROR = 'error'
    
    GEN_STATUS_CHOICES = [
        (GEN_STATUS_PENDING, 'Pending'),
        (GEN_STATUS_GENERATING, 'Generating'),
        (GEN_STATUS_COMPLETE, 'Complete'),
        (GEN_STATUS_ERROR, 'Error'),
    ]
    
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    internal_app = models.ForeignKey(InternalApp, on_delete=models.CASCADE, related_name='versions')
    version_number = models.PositiveIntegerField()
    spec_json = models.JSONField(help_text='AppSpec JSON for this version')
    scope_snapshot_json = models.JSONField(null=True, blank=True, help_text='Snapshot of resource registry scope for published versions')
    source = models.CharField(max_length=20, choices=SOURCE_CHOICES, default=SOURCE_AI)
    created_by = models.ForeignKey(User, on_delete=models.SET_NULL, null=True)
    
    # Agentic generation tracking
    generation_status = models.CharField(
        max_length=20,
        choices=GEN_STATUS_CHOICES,
        default=GEN_STATUS_COMPLETE,
        help_text='Status of agentic code generation'
    )
    generation_plan_json = models.JSONField(
        null=True,
        blank=True,
        help_text='Current plan for agentic generation'
    )
    generation_current_step = models.IntegerField(
        default=0,
        help_text='Current step index in generation plan'
    )
    generation_error = models.TextField(
        null=True,
        blank=True,
        help_text='Error message if generation failed'
    )
    
    class Meta:
        unique_together = ['internal_app', 'version_number']
        ordering = ['-version_number']
        indexes = [
            models.Index(fields=['internal_app', '-version_number']),
        ]
    
    def __str__(self):
        return f"{self.internal_app.name} v{self.version_number}"


class VersionFile(BaseModel):
    """
    File content for an app version.
    """
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    app_version = models.ForeignKey(AppVersion, on_delete=models.CASCADE, related_name='files')
    path = models.CharField(max_length=500)
    content = models.TextField()
    
    class Meta:
        unique_together = ['app_version', 'path']
        indexes = [
            models.Index(fields=['app_version', 'path']),
        ]
    
    def __str__(self):
        return f"{self.app_version} - {self.path}"


class ActionExecutionLog(BaseModel):
    """
    Log of action executions for audit and debugging.
    """
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    app_version = models.ForeignKey(AppVersion, on_delete=models.CASCADE, related_name='action_logs')
    action_id = models.CharField(max_length=255)
    input_json = models.JSONField(default=dict)
    output_json = models.JSONField(null=True, blank=True)
    error_message = models.TextField(null=True, blank=True)
    executed_by = models.ForeignKey(User, on_delete=models.SET_NULL, null=True)
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['app_version', '-created_at']),
        ]
    
    def __str__(self):
        return f"{self.action_id} - {self.created_at}"


# ============================================================================
# Magic Link Authentication Model
# ============================================================================

class MagicLinkToken(BaseModel):
    """
    Secure storage for magic link authentication tokens.
    
    Security features:
    - Tokens are hashed (SHA256) before storage - raw token never saved
    - Short expiry time (15 minutes default)
    - Single-use: token is marked as used after successful verification
    - Rate limiting tracked via created_at timestamps
    """
    email = models.EmailField(
        db_index=True,
        help_text='Email address this magic link is for'
    )
    token_hash = models.CharField(
        max_length=64,
        unique=True,
        help_text='SHA256 hash of the magic link token'
    )
    expires_at = models.DateTimeField(
        help_text='When this magic link expires'
    )
    is_used = models.BooleanField(
        default=False,
        help_text='Whether this token has been used'
    )
    # For new user signup flow
    first_name = models.CharField(
        max_length=150,
        blank=True,
        null=True,
        help_text='First name for new user registration'
    )
    last_name = models.CharField(
        max_length=150,
        blank=True,
        null=True,
        help_text='Last name for new user registration'
    )
    
    # Security tracking
    ip_address = models.GenericIPAddressField(
        null=True,
        blank=True,
        help_text='IP address that requested this magic link'
    )
    user_agent = models.TextField(
        blank=True,
        null=True,
        help_text='User agent that requested this magic link'
    )
    
    class Meta:
        indexes = [
            models.Index(fields=['email', 'created_at']),
            models.Index(fields=['expires_at']),
        ]
        ordering = ['-created_at']
    
    def __str__(self):
        return f"MagicLink for {self.email} (expires: {self.expires_at})"
    
    @property
    def is_expired(self):
        """Check if the token has expired."""
        from django.utils import timezone
        return timezone.now() > self.expires_at
    
    @property
    def is_valid(self):
        """Check if the token is still valid (not expired and not used)."""
        return not self.is_expired and not self.is_used
    
    @classmethod
    def hash_token(cls, raw_token: str) -> str:
        """Hash a raw token using SHA256."""
        import hashlib
        return hashlib.sha256(raw_token.encode()).hexdigest()
    
    @classmethod
    def create_token(cls, email: str, first_name: str = None, last_name: str = None,
                     ip_address: str = None, user_agent: str = None, expiry_minutes: int = 15):
        """
        Create a new magic link token.
        
        Note: Old unused tokens are marked as used (invalidated) rather than deleted
        to preserve rate limiting history.
        
        Returns:
            tuple: (MagicLinkToken instance, raw_token string)
        """
        import secrets
        from django.utils import timezone
        from datetime import timedelta
        
        # Generate a secure random token (32 bytes = 256 bits)
        raw_token = secrets.token_urlsafe(32)
        token_hash = cls.hash_token(raw_token)
        
        # Mark any existing unused tokens as used (invalidated) for this email
        # Don't delete them to preserve rate limiting history
        cls.objects.filter(email=email, is_used=False).update(is_used=True)
        
        # Create the token record
        magic_link = cls.objects.create(
            email=email,
            token_hash=token_hash,
            expires_at=timezone.now() + timedelta(minutes=expiry_minutes),
            first_name=first_name,
            last_name=last_name,
            ip_address=ip_address,
            user_agent=user_agent,
        )
        
        return magic_link, raw_token
    
    @classmethod
    def verify_token(cls, raw_token: str):
        """
        Verify a magic link token.
        
        Returns:
            MagicLinkToken or None if invalid/expired/used
        """
        token_hash = cls.hash_token(raw_token)
        
        try:
            magic_link = cls.objects.get(token_hash=token_hash)
            if magic_link.is_valid:
                return magic_link
            return None
        except cls.DoesNotExist:
            return None
    
    @classmethod
    def get_recent_requests_count(cls, email: str, minutes: int = 5) -> int:
        """
        Count recent magic link requests for rate limiting.
        
        Args:
            email: Email address to check
            minutes: Time window in minutes
            
        Returns:
            Number of requests in the time window
        """
        from django.utils import timezone
        from datetime import timedelta
        
        cutoff = timezone.now() - timedelta(minutes=minutes)
        return cls.objects.filter(
            email=email,
            created_at__gte=cutoff
        ).count()
    
    @classmethod
    def cleanup_expired(cls):
        """Delete all expired tokens."""
        from django.utils import timezone
        return cls.objects.filter(expires_at__lt=timezone.now()).delete()


# ============================================================================
# Chat Models for Vibe Coding Interface
# ============================================================================

class ChatSession(BaseModel):
    """
    A chat session for building an internal app.
    Each app can have multiple chat sessions (e.g., different features).
    """
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    internal_app = models.ForeignKey(
        InternalApp,
        on_delete=models.CASCADE,
        related_name='chat_sessions'
    )
    title = models.CharField(max_length=255, default="New Chat")
    model_id = models.CharField(
        max_length=100,
        default="anthropic/claude-sonnet-4",
        help_text="AI model used for this session"
    )
    is_active = models.BooleanField(default=True)
    created_by = models.ForeignKey(
        User,
        on_delete=models.SET_NULL,
        null=True,
        related_name='chat_sessions'
    )
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['internal_app', '-created_at']),
        ]
    
    def __str__(self):
        return f"{self.title} - {self.internal_app.name}"


class ChatMessage(BaseModel):
    """
    Individual message in a chat session.
    Supports user messages, AI responses, and system messages.
    """
    ROLE_USER = 'user'
    ROLE_ASSISTANT = 'assistant'
    ROLE_SYSTEM = 'system'
    
    ROLE_CHOICES = [
        (ROLE_USER, 'User'),
        (ROLE_ASSISTANT, 'Assistant'),
        (ROLE_SYSTEM, 'System'),
    ]
    
    STATUS_PENDING = 'pending'
    STATUS_STREAMING = 'streaming'
    STATUS_COMPLETE = 'complete'
    STATUS_ERROR = 'error'
    
    STATUS_CHOICES = [
        (STATUS_PENDING, 'Pending'),
        (STATUS_STREAMING, 'Streaming'),
        (STATUS_COMPLETE, 'Complete'),
        (STATUS_ERROR, 'Error'),
    ]
    
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    session = models.ForeignKey(
        ChatSession,
        on_delete=models.CASCADE,
        related_name='messages'
    )
    role = models.CharField(max_length=20, choices=ROLE_CHOICES)
    content = models.TextField()
    status = models.CharField(
        max_length=20,
        choices=STATUS_CHOICES,
        default=STATUS_COMPLETE
    )
    
    # Metadata
    model_id = models.CharField(max_length=100, blank=True, null=True)
    token_count_input = models.IntegerField(null=True, blank=True)
    token_count_output = models.IntegerField(null=True, blank=True)
    duration_ms = models.IntegerField(null=True, blank=True)
    
    # For AI responses that generate code
    generated_spec_json = models.JSONField(
        null=True,
        blank=True,
        help_text="Generated AppSpec JSON if this was a spec generation"
    )
    generated_files = models.JSONField(
        null=True,
        blank=True,
        help_text="Generated code files {path: content}"
    )
    version_created = models.ForeignKey(
        AppVersion,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name='source_messages',
        help_text="Version created from this message"
    )
    
    # Error handling
    error_message = models.TextField(null=True, blank=True)
    
    class Meta:
        ordering = ['created_at']
        indexes = [
            models.Index(fields=['session', 'created_at']),
        ]
    
    def __str__(self):
        preview = self.content[:50] + "..." if len(self.content) > 50 else self.content
        return f"{self.role}: {preview}"


class AgentConfiguration(BaseModel):
    """
    User-configurable agent settings for code generation.
    """
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    organization = models.ForeignKey(
        Organization,
        on_delete=models.CASCADE,
        related_name='agent_configs'
    )
    name = models.CharField(max_length=100)
    description = models.TextField(blank=True)
    
    # Model configuration
    default_model = models.CharField(
        max_length=100,
        default="anthropic/claude-sonnet-4"
    )
    fallback_model = models.CharField(
        max_length=100,
        default="openai/gpt-4o-mini",
        help_text="Model to use if primary fails"
    )
    
    # Generation settings
    temperature = models.FloatField(default=0.3)
    max_tokens = models.IntegerField(default=8192)
    
    # Custom instructions
    system_prompt_override = models.TextField(
        blank=True,
        null=True,
        help_text="Custom system prompt to prepend"
    )
    coding_guidelines = models.TextField(
        blank=True,
        null=True,
        help_text="Custom coding guidelines for generation"
    )
    
    # Feature flags
    enable_streaming = models.BooleanField(default=True)
    enable_auto_apply = models.BooleanField(
        default=False,
        help_text="Auto-apply generated code without confirmation"
    )
    enable_thinking_display = models.BooleanField(
        default=True,
        help_text="Show AI thinking process"
    )
    
    is_default = models.BooleanField(default=False)
    
    class Meta:
        unique_together = ['organization', 'name']
    
    def __str__(self):
        return f"{self.name} ({self.organization.name})"
    
    def save(self, *args, **kwargs):
        # Ensure only one default per org
        if self.is_default:
            AgentConfiguration.objects.filter(
                organization=self.organization,
                is_default=True
            ).exclude(pk=self.pk).update(is_default=False)
        super().save(*args, **kwargs)


class CodeGenerationJob(BaseModel):
    """
    Tracks code generation jobs for async processing and streaming.
    """
    STATUS_QUEUED = 'queued'
    STATUS_PROCESSING = 'processing'
    STATUS_STREAMING = 'streaming'
    STATUS_COMPLETE = 'complete'
    STATUS_FAILED = 'failed'
    STATUS_CANCELLED = 'cancelled'
    
    STATUS_CHOICES = [
        (STATUS_QUEUED, 'Queued'),
        (STATUS_PROCESSING, 'Processing'),
        (STATUS_STREAMING, 'Streaming'),
        (STATUS_COMPLETE, 'Complete'),
        (STATUS_FAILED, 'Failed'),
        (STATUS_CANCELLED, 'Cancelled'),
    ]
    
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    chat_message = models.OneToOneField(
        ChatMessage,
        on_delete=models.CASCADE,
        related_name='generation_job'
    )
    internal_app = models.ForeignKey(
        InternalApp,
        on_delete=models.CASCADE,
        related_name='generation_jobs'
    )
    
    status = models.CharField(
        max_length=20,
        choices=STATUS_CHOICES,
        default=STATUS_QUEUED
    )
    
    # Progress tracking
    progress_percentage = models.IntegerField(default=0)
    progress_message = models.CharField(max_length=255, blank=True)
    
    # Streaming state
    accumulated_content = models.TextField(blank=True)
    chunk_count = models.IntegerField(default=0)
    
    # Timing
    started_at = models.DateTimeField(null=True, blank=True)
    completed_at = models.DateTimeField(null=True, blank=True)
    
    # Error tracking
    error_message = models.TextField(null=True, blank=True)
    retry_count = models.IntegerField(default=0)
    max_retries = models.IntegerField(default=3)
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['internal_app', 'status']),
            models.Index(fields=['status', '-created_at']),
        ]
    
    def __str__(self):
        return f"Job {self.id} - {self.status}"


class AIUsageLog(BaseModel):
    """
    Tracks AI API usage for cost monitoring and analytics.
    """
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    organization = models.ForeignKey(
        Organization,
        on_delete=models.CASCADE,
        related_name='ai_usage_logs'
    )
    user = models.ForeignKey(
        User,
        on_delete=models.SET_NULL,
        null=True,
        related_name='ai_usage_logs'
    )
    
    # Request details
    model_id = models.CharField(max_length=100)
    request_type = models.CharField(max_length=50)  # 'appspec', 'code', 'chat'
    
    # Usage metrics
    input_tokens = models.IntegerField(default=0)
    output_tokens = models.IntegerField(default=0)
    total_tokens = models.IntegerField(default=0)
    
    # Cost tracking (in USD cents for precision)
    cost_input_cents = models.IntegerField(default=0)
    cost_output_cents = models.IntegerField(default=0)
    total_cost_cents = models.IntegerField(default=0)
    
    # Timing
    duration_ms = models.IntegerField(default=0)
    
    # Status
    success = models.BooleanField(default=True)
    error_message = models.TextField(null=True, blank=True)
    
    # Context
    internal_app = models.ForeignKey(
        InternalApp,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name='ai_usage_logs'
    )
    chat_message = models.ForeignKey(
        ChatMessage,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name='usage_logs'
    )
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['organization', '-created_at']),
            models.Index(fields=['user', '-created_at']),
            models.Index(fields=['model_id', '-created_at']),
        ]
    
    def __str__(self):
        return f"{self.model_id} - {self.total_tokens} tokens - ${self.total_cost_cents / 100:.4f}"
    
    @classmethod
    def log_usage(
        cls,
        organization,
        model_id: str,
        request_type: str,
        input_tokens: int,
        output_tokens: int,
        duration_ms: int,
        user=None,
        internal_app=None,
        chat_message=None,
        success: bool = True,
        error_message: str = None
    ):
        """Create a usage log entry with cost calculation."""
        from .services.openrouter_service import MODEL_CONFIGS
        
        # Get model cost config
        config = MODEL_CONFIGS.get(model_id)
        if config:
            cost_input = int((input_tokens / 1_000_000) * config.cost_per_million_input * 100)
            cost_output = int((output_tokens / 1_000_000) * config.cost_per_million_output * 100)
        else:
            # Default fallback pricing
            cost_input = int((input_tokens / 1_000_000) * 1.0 * 100)
            cost_output = int((output_tokens / 1_000_000) * 3.0 * 100)
        
        return cls.objects.create(
            organization=organization,
            user=user,
            model_id=model_id,
            request_type=request_type,
            input_tokens=input_tokens,
            output_tokens=output_tokens,
            total_tokens=input_tokens + output_tokens,
            cost_input_cents=cost_input,
            cost_output_cents=cost_output,
            total_cost_cents=cost_input + cost_output,
            duration_ms=duration_ms,
            success=success,
            error_message=error_message,
            internal_app=internal_app,
            chat_message=chat_message,
        )
    
    @classmethod
    def get_org_usage_summary(cls, organization, days: int = 30):
        """Get usage summary for an organization."""
        from django.utils import timezone
        from datetime import timedelta
        from django.db.models import Sum, Count, Avg
        
        cutoff = timezone.now() - timedelta(days=days)
        
        return cls.objects.filter(
            organization=organization,
            created_at__gte=cutoff
        ).aggregate(
            total_requests=Count('id'),
            total_tokens=Sum('total_tokens'),
            total_cost_cents=Sum('total_cost_cents'),
            avg_duration_ms=Avg('duration_ms'),
        )
