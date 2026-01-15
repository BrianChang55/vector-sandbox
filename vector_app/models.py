"""
Vector Internal Apps - Data Models

This module contains all Django models for the Vector Internal Apps platform.
"""

import uuid
import json
import hashlib
from django.contrib.auth.models import AbstractUser
from django.db import models
from django.core.exceptions import ValidationError
from vector_app.action_classification.types import ActionType
from internal_apps.utils.base_model import BaseModel
from .utils.encryption import encrypt_string, decrypt_string
from internal_apps.utils.enum import choices

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
        max_length=255, unique=True, null=True, blank=True, help_text="Google user ID for OAuth"
    )
    
    # Profile image - supports multiple sources:
    # 1. OAuth provider URL (stored in profile_image_url)
    # 2. Uploaded image (stored in profile_image_storage_key)
    profile_image_url = models.URLField(
        max_length=500, blank=True, null=True, help_text="Profile image URL from OAuth provider (legacy)"
    )
    
    # New storage key field for uploaded profile images (cloud/local unified storage)
    # Format: 'r2://folder/filename.png' or 'local://folder/filename.png'
    profile_image_storage_key = models.CharField(
        max_length=500,
        null=True,
        blank=True,
        help_text='Storage key for uploaded profile image (supports cloud and local storage)'
    )
    
    USERNAME_FIELD = 'email'
    REQUIRED_FIELDS = []

    def __str__(self):
        return self.email
    
    def get_profile_image_url(self):
        """
        Get the profile image URL, preferring uploaded image over OAuth provider URL.
        
        Priority:
        1. Uploaded image (profile_image_storage_key) - if set
        2. OAuth provider URL (profile_image_url) - fallback
        
        Returns:
            str or None: URL to the profile image
        """
        from .services.image_upload_service import ImageUploadService
        
        # Prefer uploaded image if set
        if self.profile_image_storage_key:
            return ImageUploadService.get_image_url(self.profile_image_storage_key)
        
        # Fall back to OAuth provider URL
        if self.profile_image_url:
            return self.profile_image_url
        
        return None
    
    def delete_profile_image(self):
        """Delete the uploaded profile image and clear the storage key."""
        from .services.image_upload_service import ImageUploadService
        
        if self.profile_image_storage_key:
            ImageUploadService.delete_image(self.profile_image_storage_key)
            self.profile_image_storage_key = None
            self.save(update_fields=['profile_image_storage_key', 'updated_at'])


class Organization(BaseModel):
    """
    Organization model representing a company/team (Project container).
    """

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    name = models.CharField(max_length=255)
    slug = models.SlugField(max_length=255, unique=True)
    
    # Legacy logo field (Django ImageField for local storage)
    logo = models.ImageField(
        upload_to='org_logos/',
        null=True,
        blank=True,
        help_text='Organization logo image (legacy - use logo_storage_key for new uploads)'
    )
    
    # New storage key field for cloud/local unified storage
    # Format: 'r2://folder/filename.png' or 'local://folder/filename.png'
    logo_storage_key = models.CharField(
        max_length=500,
        null=True,
        blank=True,
        help_text='Storage key for logo image (supports cloud and local storage)'
    )

    def __str__(self):
        return self.name
    
    def get_logo_url(self):
        """
        Get the logo URL, preferring the new storage key over legacy ImageField.
        
        Returns:
            str or None: URL to the logo image
        """
        from .services.image_upload_service import ImageUploadService
        
        # Prefer new storage key if set
        if self.logo_storage_key:
            return ImageUploadService.get_image_url(self.logo_storage_key)
        
        # Fall back to legacy logo field
        if self.logo:
            return self.logo.url
        
        return None
    
    def delete_logo(self):
        """Delete the logo from storage and clear both fields."""
        from .services.image_upload_service import ImageUploadService
        
        # Delete from new storage if set
        if self.logo_storage_key:
            ImageUploadService.delete_image(self.logo_storage_key)
            self.logo_storage_key = None
        
        # Delete legacy logo if set
        if self.logo:
            self.logo.delete(save=False)
        
        self.save(update_fields=['logo', 'logo_storage_key', 'updated_at'])


class UserOrganization(BaseModel):
    """
    Junction table for User-Organization many-to-many relationship.
    Includes role field for membership permissions.

    Roles:
    - Admin: Full access - can edit apps, manage integrations, manage members/roles
    - Editor: Can edit apps - cannot modify integrations or members
    - Viewer: View/run only - sees published apps, redirected to published view
    """

    ROLE_ADMIN = "admin"
    ROLE_EDITOR = "editor"
    ROLE_VIEWER = "viewer"

    # Legacy role alias for backwards compatibility during migration
    ROLE_MEMBER = "editor"

    ROLE_CHOICES = [
        (ROLE_ADMIN, "Admin"),
        (ROLE_EDITOR, "Editor"),
        (ROLE_VIEWER, "Viewer"),
    ]

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name="user_organizations")
    organization = models.ForeignKey(
        Organization, on_delete=models.CASCADE, related_name="user_organizations"
    )
    role = models.CharField(max_length=20, choices=ROLE_CHOICES, default=ROLE_EDITOR)

    class Meta:
        unique_together = ["user", "organization"]

    def __str__(self):
        return f"{self.user.email} - {self.organization.name} ({self.role})"

    def is_admin(self):
        """Check if user has admin role."""
        return self.role == self.ROLE_ADMIN

    def is_editor_or_above(self):
        """Check if user has editor or admin role."""
        return self.role in [self.ROLE_ADMIN, self.ROLE_EDITOR]

    def can_edit_apps(self):
        """Check if user can edit apps."""
        return self.is_editor_or_above()

    def can_manage_integrations(self):
        """Check if user can manage integrations."""
        return self.is_admin()

    def can_manage_members(self):
        """Check if user can manage organization members."""
        return self.is_admin()

    def can_update_org_settings(self):
        """Check if user can update organization settings."""
        return self.is_admin()


class OrganizationInvite(BaseModel):
    """
    Pending invitation to join an organization.

    Security features:
    - Tokens are hashed (SHA256) before storage - raw token never saved
    - Short expiry time (7 days default)
    - Single-use: invite is marked as accepted after successful verification
    - Rate limiting can be tracked via created_at timestamps
    """

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    organization = models.ForeignKey(Organization, on_delete=models.CASCADE, related_name="invites")
    email = models.EmailField(db_index=True, help_text="Email address this invitation is for")
    role = models.CharField(
        max_length=20,
        choices=UserOrganization.ROLE_CHOICES,
        default=UserOrganization.ROLE_EDITOR,
        help_text="Role the user will have when they accept",
    )
    invited_by = models.ForeignKey(
        User,
        on_delete=models.SET_NULL,
        null=True,
        related_name="sent_invites",
        help_text="User who sent the invitation",
    )
    token_hash = models.CharField(max_length=64, unique=True, help_text="SHA256 hash of the invitation token")
    expires_at = models.DateTimeField(help_text="When this invitation expires")
    accepted_at = models.DateTimeField(null=True, blank=True, help_text="When the invitation was accepted")
    is_accepted = models.BooleanField(default=False, help_text="Whether this invitation has been accepted")

    class Meta:
        indexes = [
            models.Index(fields=["organization", "email"]),
            models.Index(fields=["expires_at"]),
            models.Index(fields=["token_hash"]),
        ]
        ordering = ["-created_at"]
        # Allow resending invites, but track unique active invites
        unique_together = []

    def __str__(self):
        status = "accepted" if self.is_accepted else ("expired" if self.is_expired else "pending")
        return f"Invite for {self.email} to {self.organization.name} ({status})"

    @property
    def is_expired(self):
        """Check if the invitation has expired."""
        from django.utils import timezone

        return timezone.now() > self.expires_at

    @property
    def is_valid(self):
        """Check if the invitation is still valid (not expired and not accepted)."""
        return not self.is_expired and not self.is_accepted

    @classmethod
    def hash_token(cls, raw_token: str) -> str:
        """Hash a raw token using SHA256."""
        return hashlib.sha256(raw_token.encode()).hexdigest()

    @classmethod
    def create_invite(cls, organization, email: str, role: str, invited_by, expiry_days: int = 7):
        """
        Create a new organization invitation.

        Invalidates any existing pending invites for the same email/org combo.

        Returns:
            tuple: (OrganizationInvite instance, raw_token string)
        """
        import secrets
        from django.utils import timezone
        from datetime import timedelta

        # Generate a secure random token (32 bytes = 256 bits)
        raw_token = secrets.token_urlsafe(32)
        token_hash = cls.hash_token(raw_token)

        # Invalidate any existing pending invites for this email/org
        cls.objects.filter(organization=organization, email=email, is_accepted=False).delete()

        # Create the invite record
        invite = cls.objects.create(
            organization=organization,
            email=email,
            role=role,
            invited_by=invited_by,
            token_hash=token_hash,
            expires_at=timezone.now() + timedelta(days=expiry_days),
        )

        return invite, raw_token

    @classmethod
    def verify_token(cls, raw_token: str):
        """
        Verify an invitation token.

        Returns:
            OrganizationInvite or None if invalid/expired/accepted
        """
        token_hash = cls.hash_token(raw_token)

        try:
            invite = cls.objects.select_related("organization", "invited_by").get(token_hash=token_hash)
            if invite.is_valid:
                return invite
            return None
        except cls.DoesNotExist:
            return None

    def accept(self, user):
        """
        Accept this invitation and add user to organization.

        Args:
            user: The User accepting the invitation

        Returns:
            UserOrganization: The created membership
        """
        from django.utils import timezone

        # Create the membership
        membership, created = UserOrganization.objects.get_or_create(
            user=user, organization=self.organization, defaults={"role": self.role}
        )

        # If already a member, update role if needed
        if not created and membership.role != self.role:
            membership.role = self.role
            membership.save(update_fields=["role", "updated_at"])

        # Mark invite as accepted
        self.is_accepted = True
        self.accepted_at = timezone.now()
        self.save(update_fields=["is_accepted", "accepted_at", "updated_at"])

        return membership

    @classmethod
    def cleanup_expired(cls):
        """Delete all expired and unaccepted invitations."""
        from django.utils import timezone

        return cls.objects.filter(expires_at__lt=timezone.now(), is_accepted=False).delete()

    @classmethod
    def get_pending_for_org(cls, organization):
        """Get all pending invitations for an organization."""
        from django.utils import timezone

        return cls.objects.filter(
            organization=organization, is_accepted=False, expires_at__gt=timezone.now()
        ).select_related("invited_by")


# ============================================================================
# Internal App Models
# ============================================================================


class InternalApp(BaseModel):
    """
    Internal application created by users.
    """

    STATUS_DRAFT = "draft"
    STATUS_PUBLISHED = "published"

    STATUS_CHOICES = [
        (STATUS_DRAFT, "Draft"),
        (STATUS_PUBLISHED, "Published"),
    ]

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    organization = models.ForeignKey(Organization, on_delete=models.CASCADE, related_name="internal_apps")
    name = models.CharField(max_length=255)
    slug = models.SlugField(
        max_length=255,
        blank=True,
        null=True,
        help_text="URL-friendly identifier for the app (auto-generated from name if not set)",
    )
    description = models.TextField(blank=True)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default=STATUS_DRAFT)
    published_version = models.ForeignKey(
        "AppVersion",
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name="published_for_app",
        help_text="The currently active published version",
    )
    allow_actions_in_preview = models.BooleanField(default=False)
    created_by = models.ForeignKey(User, on_delete=models.SET_NULL, null=True, related_name="created_apps")

    class Meta:
        unique_together = ["organization", "slug"]
        indexes = [
            models.Index(fields=["organization", "slug"]),
        ]

    def __str__(self):
        return f"{self.name} ({self.organization.name})"

    def generate_slug(self):
        """Generate a URL-friendly slug from the app name."""
        from django.utils.text import slugify

        base_slug = slugify(self.name)
        if not base_slug:
            base_slug = "app"

        # Check for uniqueness within organization
        slug = base_slug
        counter = 1
        while (
            InternalApp.objects.filter(organization=self.organization, slug=slug).exclude(pk=self.pk).exists()
        ):
            slug = f"{base_slug}-{counter}"
            counter += 1

        return slug

    def save(self, *args, **kwargs):
        # Auto-generate slug if not set
        if not self.slug:
            self.slug = self.generate_slug()
        super().save(*args, **kwargs)


class AppFavorite(BaseModel):
    """
    Tracks which apps a user has favorited within an organization.
    """

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name="app_favorites")
    organization = models.ForeignKey(Organization, on_delete=models.CASCADE, related_name="app_favorites")
    app = models.ForeignKey(InternalApp, on_delete=models.CASCADE, related_name="favorites")

    class Meta:
        unique_together = ["user", "organization", "app"]
        indexes = [
            models.Index(fields=["user", "organization"]),
        ]

    def __str__(self):
        return f"{self.user.email} - {self.app.name}"


class AppVersion(BaseModel):
    """
    Version of an internal app (immutable snapshot).
    """

    SOURCE_AI_EDIT = "ai_edit"
    SOURCE_CODE_EDIT = "code_edit"
    SOURCE_ROLLBACK = "rollback"
    SOURCE_PUBLISH = "publish"
    # Legacy aliases (kept for compatibility with existing data/choices)
    SOURCE_AI = "ai"
    SOURCE_CODE = "code"

    SOURCE_CHOICES = [
        (SOURCE_AI_EDIT, "AI Edit"),
        (SOURCE_CODE_EDIT, "Code Edit"),
        (SOURCE_ROLLBACK, "Rollback"),
        (SOURCE_PUBLISH, "Publish"),
        (SOURCE_AI, "AI (legacy)"),
        (SOURCE_CODE, "Code (legacy)"),
    ]

    # Generation status for agentic workflow
    GEN_STATUS_PENDING = "pending"
    GEN_STATUS_GENERATING = "generating"
    GEN_STATUS_COMPLETE = "complete"
    GEN_STATUS_ERROR = "error"

    GEN_STATUS_CHOICES = [
        (GEN_STATUS_PENDING, "Pending"),
        (GEN_STATUS_GENERATING, "Generating"),
        (GEN_STATUS_COMPLETE, "Complete"),
        (GEN_STATUS_ERROR, "Error"),
    ]

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    internal_app = models.ForeignKey(InternalApp, on_delete=models.CASCADE, related_name="versions")
    version_number = models.PositiveIntegerField()
    parent_version = models.ForeignKey(
        "self",
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name="child_versions",
        help_text="Parent version used as the base for this version",
    )
    spec_json = models.JSONField(help_text="AppSpec JSON for this version")
    intent_message = models.TextField(
        null=True, blank=True, help_text="User intent that generated this version (AI edits)"
    )
    source = models.CharField(max_length=20, choices=SOURCE_CHOICES, default=SOURCE_AI_EDIT)
    created_by = models.ForeignKey(User, on_delete=models.SET_NULL, null=True)

    # Version activation status
    is_active = models.BooleanField(
        default=False,
        help_text="Whether this version is active and should be returned by API. "
        "Only set True when generation completes successfully.",
    )

    # Agentic generation tracking
    generation_status = models.CharField(
        max_length=20,
        choices=GEN_STATUS_CHOICES,
        default=GEN_STATUS_COMPLETE,
        help_text="Status of agentic code generation",
    )
    generation_plan_json = models.JSONField(
        null=True, blank=True, help_text="Current plan for agentic generation"
    )
    generation_current_step = models.IntegerField(
        default=0, help_text="Current step index in generation plan"
    )
    generation_error = models.TextField(null=True, blank=True, help_text="Error message if generation failed")

    # Validation status for TypeScript compilation
    VALIDATION_PENDING = "pending"
    VALIDATION_PASSED = "passed"
    VALIDATION_FAILED = "failed"
    VALIDATION_SKIPPED = "skipped"

    VALIDATION_STATUS_CHOICES = [
        (VALIDATION_PENDING, "Pending"),
        (VALIDATION_PASSED, "Passed"),
        (VALIDATION_FAILED, "Failed"),
        (VALIDATION_SKIPPED, "Skipped"),
    ]

    validation_status = models.CharField(
        max_length=20,
        choices=VALIDATION_STATUS_CHOICES,
        default=VALIDATION_PENDING,
        help_text="TypeScript compilation validation status",
    )
    validation_errors_json = models.JSONField(
        null=True, blank=True, help_text="JSON list of validation errors if failed"
    )
    fix_attempts = models.IntegerField(default=0, help_text="Number of auto-fix attempts made")

    class Meta:
        unique_together = ["internal_app", "version_number"]
        ordering = ["-version_number"]
        indexes = [
            models.Index(fields=["internal_app", "-version_number"]),
        ]

    def __str__(self):
        return f"{self.internal_app.name} v{self.version_number}"


class VersionFile(BaseModel):
    """
    File content for an app version.
    """

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    app_version = models.ForeignKey(AppVersion, on_delete=models.CASCADE, related_name="files")
    path = models.CharField(max_length=500)
    content = models.TextField()
    content_hash = models.CharField(
        max_length=64,
        blank=True,
        default="",
        help_text="Deterministic hash of the file content for integrity checks",
    )

    class Meta:
        unique_together = ["app_version", "path"]
        indexes = [
            models.Index(fields=["app_version", "path"]),
        ]

    @staticmethod
    def compute_hash(content: str) -> str:
        """Compute a stable hash for file content."""
        return hashlib.md5((content or "").encode("utf-8")).hexdigest()

    def save(self, *args, **kwargs):
        """Ensure content_hash is always populated before save."""
        if not self.content_hash:
            self.content_hash = self.compute_hash(self.content)
        super().save(*args, **kwargs)

    def __str__(self):
        return f"{self.app_version} - {self.path}"


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

    email = models.EmailField(db_index=True, help_text="Email address this magic link is for")
    token_hash = models.CharField(max_length=64, unique=True, help_text="SHA256 hash of the magic link token")
    expires_at = models.DateTimeField(help_text="When this magic link expires")
    is_used = models.BooleanField(default=False, help_text="Whether this token has been used")
    # For new user signup flow
    first_name = models.CharField(
        max_length=150, blank=True, null=True, help_text="First name for new user registration"
    )
    last_name = models.CharField(
        max_length=150, blank=True, null=True, help_text="Last name for new user registration"
    )

    # Security tracking
    ip_address = models.GenericIPAddressField(
        null=True, blank=True, help_text="IP address that requested this magic link"
    )
    user_agent = models.TextField(
        blank=True, null=True, help_text="User agent that requested this magic link"
    )

    class Meta:
        indexes = [
            models.Index(fields=["email", "created_at"]),
            models.Index(fields=["expires_at"]),
        ]
        ordering = ["-created_at"]

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
    def create_token(
        cls,
        email: str,
        first_name: str = None,
        last_name: str = None,
        ip_address: str = None,
        user_agent: str = None,
        expiry_minutes: int = 15,
    ):
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
        return cls.objects.filter(email=email, created_at__gte=cutoff).count()

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
    internal_app = models.ForeignKey(InternalApp, on_delete=models.CASCADE, related_name="chat_sessions")
    title = models.CharField(max_length=255, default="New Chat")
    model_id = models.CharField(
        max_length=100, default="anthropic/claude-sonnet-4.5", help_text="AI model used for this session"
    )
    is_active = models.BooleanField(default=True)
    created_by = models.ForeignKey(User, on_delete=models.SET_NULL, null=True, related_name="chat_sessions")

    class Meta:
        ordering = ["-created_at"]
        indexes = [
            models.Index(fields=["internal_app", "-created_at"]),
        ]

    def __str__(self):
        return f"{self.title} - {self.internal_app.name}"


class ChatMessage(BaseModel):
    """
    Individual message in a chat session.
    Supports user messages, AI responses, and system messages.
    """

    ROLE_USER = "user"
    ROLE_ASSISTANT = "assistant"
    ROLE_SYSTEM = "system"

    ROLE_CHOICES = [
        (ROLE_USER, "User"),
        (ROLE_ASSISTANT, "Assistant"),
        (ROLE_SYSTEM, "System"),
    ]

    STATUS_PENDING = "pending"
    STATUS_STREAMING = "streaming"
    STATUS_COMPLETE = "complete"
    STATUS_ERROR = "error"

    STATUS_CHOICES = [
        (STATUS_PENDING, "Pending"),
        (STATUS_STREAMING, "Streaming"),
        (STATUS_COMPLETE, "Complete"),
        (STATUS_ERROR, "Error"),
    ]

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    session = models.ForeignKey(ChatSession, on_delete=models.CASCADE, related_name="messages")
    role = models.CharField(max_length=20, choices=ROLE_CHOICES)
    content = models.TextField()
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default=STATUS_COMPLETE)

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
        AppVersion,
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


class CodeGenerationJob(BaseModel):
    """
    Tracks code generation jobs for async processing and streaming.

    Events are stored in events_json for replay on reconnection.
    The Celery worker appends events as they're generated, and the
    SSE endpoint streams them to the client.
    """

    STATUS_QUEUED = "queued"
    STATUS_PROCESSING = "processing"
    STATUS_STREAMING = "streaming"
    STATUS_COMPLETE = "complete"
    STATUS_FAILED = "failed"
    STATUS_CANCELLED = "cancelled"

    STATUS_CHOICES = [
        (STATUS_QUEUED, "Queued"),
        (STATUS_PROCESSING, "Processing"),
        (STATUS_STREAMING, "Streaming"),
        (STATUS_COMPLETE, "Complete"),
        (STATUS_FAILED, "Failed"),
        (STATUS_CANCELLED, "Cancelled"),
    ]

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    chat_message = models.OneToOneField(
        ChatMessage,
        on_delete=models.CASCADE,
        related_name="generation_job",
        null=True,
        blank=True,
    )
    internal_app = models.ForeignKey(InternalApp, on_delete=models.CASCADE, related_name="generation_jobs")
    version = models.ForeignKey(
        "AppVersion",
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

    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default=STATUS_QUEUED)

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
        User, on_delete=models.SET_NULL, null=True, blank=True, related_name="generation_jobs"
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
        import time

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
# App Data Store Models
# ============================================================================


class AppDataTable(BaseModel):
    """
    Represents a data table within an Internal App's data store.
    Each app can have multiple tables, each with its own schema.

    The schema is stored as JSON and defines columns, types, and constraints.
    This allows apps to store structured data without external database connections.
    """

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    internal_app = models.ForeignKey(InternalApp, on_delete=models.CASCADE, related_name="data_tables")
    name = models.CharField(max_length=255)
    slug = models.SlugField(max_length=255)
    description = models.TextField(blank=True)

    # Schema definition stored as JSON
    # Structure: {"columns": [...], "indexes": [...]}
    schema_json = models.JSONField(default=dict, help_text="Table schema: columns, types, constraints")

    # Cached row count for performance
    row_count = models.PositiveIntegerField(default=0)

    class Meta:
        unique_together = ["internal_app", "slug"]
        indexes = [
            models.Index(fields=["internal_app", "slug"]),
        ]

    def __str__(self):
        return f"{self.name} ({self.internal_app.name})"

    def get_columns(self):
        """Get column definitions from schema."""
        return self.schema_json.get("columns", [])

    def get_column_names(self):
        """Get list of column names."""
        return [col["name"] for col in self.get_columns()]

    def get_primary_key_column(self):
        """Get the primary key column definition."""
        for col in self.get_columns():
            if col.get("primary_key"):
                return col
        return None

    def increment_row_count(self, delta: int = 1):
        """Increment the cached row count."""
        self.row_count = models.F("row_count") + delta
        self.save(update_fields=["row_count", "updated_at"])
        self.refresh_from_db(fields=["row_count"])

    def decrement_row_count(self, delta: int = 1):
        """Decrement the cached row count."""
        from django.db.models.functions import Greatest

        self.row_count = Greatest(models.F("row_count") - delta, 0)
        self.save(update_fields=["row_count", "updated_at"])
        self.refresh_from_db(fields=["row_count"])


class AppDataRow(BaseModel):
    """
    Represents a single row of data in an AppDataTable.
    Data is stored as JSON and validated against the table schema.

    Each row has a sequential index within its table for ordering.
    """

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    table = models.ForeignKey(AppDataTable, on_delete=models.CASCADE, related_name="rows")

    # Row data stored as JSON - keys match column names from schema
    data = models.JSONField(default=dict, help_text="Row data matching table schema")

    # Row ordering/identifier within table
    row_index = models.PositiveIntegerField(help_text="Sequential index within table")

    class Meta:
        indexes = [
            models.Index(fields=["table", "row_index"]),
            models.Index(fields=["table", "created_at"]),
        ]
        ordering = ["row_index"]

    def __str__(self):
        return f"Row {self.row_index} in {self.table.name}"

    def get_value(self, column_name: str):
        """Get value for a specific column."""
        return self.data.get(column_name)

    def set_value(self, column_name: str, value):
        """Set value for a specific column."""
        self.data[column_name] = value

    def save(self, *args, **kwargs):
        """Assign row_index if not set."""
        if self.row_index is None:
            # Get the next row index for this table
            max_index = AppDataRow.objects.filter(table=self.table).aggregate(
                max_index=models.Max("row_index")
            )["max_index"]
            self.row_index = (max_index or 0) + 1
        super().save(*args, **kwargs)


class AppDataTableSnapshot(BaseModel):
    """
    Snapshot of a table's schema at a specific app version.

    Enables schema versioning and rollback. Each time a table is created,
    updated, or deleted during app generation, a snapshot is recorded
    linking the schema state to the AppVersion being created.
    """

    OPERATION_CHOICES = [
        ("create", "Create"),
        ("update", "Update"),
        ("delete", "Delete"),
    ]

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)

    app_version = models.ForeignKey(
        AppVersion,
        on_delete=models.CASCADE,
        related_name="table_snapshots",
        help_text="The app version this snapshot belongs to",
    )

    table = models.ForeignKey(
        AppDataTable,
        on_delete=models.CASCADE,
        related_name="snapshots",
        help_text="The table this snapshot is for",
    )

    # Frozen schema at this version
    schema_json = models.JSONField(help_text="Table schema frozen at this version")

    # Table metadata frozen at this version
    table_name = models.CharField(max_length=255, help_text="Table name at this version")
    table_slug = models.SlugField(max_length=255, help_text="Table slug at this version")
    table_description = models.TextField(blank=True, help_text="Table description at this version")

    # What operation created this snapshot
    operation = models.CharField(
        max_length=20, choices=OPERATION_CHOICES, help_text="Operation that created this snapshot"
    )

    # For tracking changes
    previous_schema_json = models.JSONField(
        null=True, blank=True, help_text="Previous schema (for update operations)"
    )

    class Meta:
        indexes = [
            models.Index(fields=["app_version", "table"]),
            models.Index(fields=["table", "created_at"]),
        ]
        # Only one snapshot per table per version
        unique_together = ["app_version", "table"]

    def __str__(self):
        return f"{self.table_name} @ v{self.app_version.version_number} ({self.operation})"

    @classmethod
    def create_snapshot(
        cls, app_version: "AppVersion", table: "AppDataTable", operation: str, previous_schema: dict = None
    ) -> "AppDataTableSnapshot":
        """
        Create a snapshot of a table's current state.

        Args:
            app_version: The version this snapshot is associated with
            table: The table to snapshot
            operation: One of 'create', 'update', 'delete'
            previous_schema: For update operations, the schema before changes

        Returns:
            The created snapshot
        """
        return cls.objects.create(
            app_version=app_version,
            table=table,
            schema_json=table.schema_json,
            table_name=table.name,
            table_slug=table.slug,
            table_description=table.description or "",
            operation=operation,
            previous_schema_json=previous_schema,
        )

    def get_column_changes(self) -> dict:
        """
        For update operations, compute what columns changed.

        Returns:
            Dict with 'added', 'removed', 'modified' column lists
        """
        if self.operation != "update" or not self.previous_schema_json:
            return {"added": [], "removed": [], "modified": []}

        old_cols = {c["name"]: c for c in self.previous_schema_json.get("columns", [])}
        new_cols = {c["name"]: c for c in self.schema_json.get("columns", [])}

        added = [name for name in new_cols if name not in old_cols]
        removed = [name for name in old_cols if name not in new_cols]
        modified = [name for name in new_cols if name in old_cols and new_cols[name] != old_cols[name]]

        return {"added": added, "removed": removed, "modified": modified}


# ============================================================================
# Version State Snapshot Models (Enterprise Version Management)
# ============================================================================


class VersionStateSnapshot(BaseModel):
    """
    Complete application state snapshot at a specific version.
    Created automatically on every version to enable full revert.

    This captures the entire state of an app's data tables at the moment
    a version is created, enabling enterprise-grade version management
    with schema-only rollback while preserving data.
    """

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)

    app_version = models.OneToOneField(
        AppVersion,
        on_delete=models.CASCADE,
        related_name="state_snapshot",
        help_text="The version this snapshot belongs to",
    )

    # All table schemas frozen at this version
    tables_json = models.JSONField(
        default=list, help_text="Array of {id, slug, name, description, schema, row_count} for all tables"
    )

    # Metadata
    total_tables = models.IntegerField(default=0, help_text="Number of data tables at this version")
    total_rows = models.IntegerField(default=0, help_text="Total row count across all tables at this version")
    file_count = models.IntegerField(default=0, help_text="Number of code files at this version")

    class Meta:
        indexes = [
            models.Index(fields=["app_version"]),
        ]

    def __str__(self):
        return f"Snapshot for {self.app_version}"

    @classmethod
    def create_for_version(cls, app_version: "AppVersion") -> "VersionStateSnapshot":
        """
        Create a complete state snapshot for an app version.

        Captures:
        - All data table schemas
        - File counts and metadata
        """
        from .models import AppDataTable

        app = app_version.internal_app

        # Capture all tables for this app
        tables = AppDataTable.objects.filter(internal_app=app)
        tables_data = []
        total_rows = 0

        for table in tables:
            tables_data.append(
                {
                    "id": str(table.id),
                    "slug": table.slug,
                    "name": table.name,
                    "description": table.description or "",
                    "schema": table.schema_json,
                    "row_count": table.row_count,
                }
            )
            total_rows += table.row_count

        # Get file count
        file_count = app_version.files.count()

        return cls.objects.create(
            app_version=app_version,
            tables_json=tables_data,
            total_tables=len(tables_data),
            total_rows=total_rows,
            file_count=file_count,
        )

    def get_table_by_slug(self, slug: str) -> dict:
        """Get a table's snapshot data by slug."""
        for table in self.tables_json:
            if table.get("slug") == slug:
                return table
        return None

    def compare_to(self, other: "VersionStateSnapshot") -> dict:
        """
        Compare this snapshot to another snapshot.

        Returns:
            Dict with 'tables' and 'files' changes
        """
        if not other:
            return {
                "tables": {"added": self.tables_json, "removed": [], "modified": []},
                "files": {"from_count": 0, "to_count": self.file_count},
            }

        # Compare tables
        self_tables = {t["slug"]: t for t in self.tables_json}
        other_tables = {t["slug"]: t for t in other.tables_json}

        tables_added = [t for slug, t in self_tables.items() if slug not in other_tables]
        tables_removed = [t for slug, t in other_tables.items() if slug not in self_tables]
        tables_modified = []

        for slug in self_tables:
            if slug in other_tables:
                if self_tables[slug]["schema"] != other_tables[slug]["schema"]:
                    tables_modified.append(
                        {
                            "slug": slug,
                            "from": other_tables[slug],
                            "to": self_tables[slug],
                        }
                    )

        return {
            "tables": {
                "added": tables_added,
                "removed": tables_removed,
                "modified": tables_modified,
            },
            "files": {
                "from_count": other.file_count,
                "to_count": self.file_count,
            },
        }


class VersionAuditLog(BaseModel):
    """
    Audit trail for version operations.

    Tracks all version-related operations for enterprise compliance:
    - Version creation (AI edit, code edit)
    - Rollback operations
    - Publish operations
    - Schema reverts

    This is an append-only log that provides full traceability.
    """

    OPERATION_CREATE = "create"
    OPERATION_ROLLBACK = "rollback"
    OPERATION_PUBLISH = "publish"
    OPERATION_SCHEMA_REVERT = "schema_revert"
    OPERATION_PREVIEW = "preview"

    OPERATION_CHOICES = [
        (OPERATION_CREATE, "Create"),
        (OPERATION_ROLLBACK, "Rollback"),
        (OPERATION_PUBLISH, "Publish"),
        (OPERATION_SCHEMA_REVERT, "Schema Revert"),
        (OPERATION_PREVIEW, "Preview"),
    ]

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)

    # The app this operation belongs to
    internal_app = models.ForeignKey(
        InternalApp,
        on_delete=models.CASCADE,
        related_name="version_audit_logs",
        help_text="The app this audit log belongs to",
    )

    # The version affected by this operation
    app_version = models.ForeignKey(
        AppVersion,
        on_delete=models.CASCADE,
        related_name="audit_logs",
        help_text="The version affected by this operation",
    )

    # For rollback operations, the source version
    source_version = models.ForeignKey(
        AppVersion,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name="rollback_audit_logs",
        help_text="For rollback operations, the version being rolled back from",
    )

    # Operation details
    operation = models.CharField(
        max_length=20, choices=OPERATION_CHOICES, help_text="The type of operation performed"
    )

    # User who performed the operation
    user = models.ForeignKey(
        User,
        on_delete=models.SET_NULL,
        null=True,
        related_name="version_audit_logs",
        help_text="User who performed this operation",
    )

    # Detailed operation context
    details_json = models.JSONField(default=dict, help_text="Detailed operation context and parameters")

    # Schema changes summary (for rollback/revert operations)
    schema_changes_json = models.JSONField(
        null=True, blank=True, help_text="Summary of schema changes for rollback operations"
    )

    # Security/compliance tracking
    ip_address = models.GenericIPAddressField(
        null=True, blank=True, help_text="IP address of the user performing the operation"
    )
    user_agent = models.TextField(blank=True, null=True, help_text="User agent string of the client")

    # Operation result
    success = models.BooleanField(default=True, help_text="Whether the operation completed successfully")
    error_message = models.TextField(null=True, blank=True, help_text="Error message if operation failed")

    class Meta:
        ordering = ["-created_at"]
        indexes = [
            models.Index(fields=["internal_app", "-created_at"]),
            models.Index(fields=["app_version", "-created_at"]),
            models.Index(fields=["user", "-created_at"]),
            models.Index(fields=["operation", "-created_at"]),
        ]

    def __str__(self):
        return f"{self.operation} on {self.app_version} by {self.user}"

    @classmethod
    def log_operation(
        cls,
        internal_app: "InternalApp",
        app_version: "AppVersion",
        operation: str,
        user: "User" = None,
        source_version: "AppVersion" = None,
        details: dict = None,
        schema_changes: dict = None,
        ip_address: str = None,
        user_agent: str = None,
        success: bool = True,
        error_message: str = None,
    ) -> "VersionAuditLog":
        """
        Create an audit log entry for a version operation.

        Args:
            internal_app: The app this operation belongs to
            app_version: The version affected by this operation
            operation: One of OPERATION_* constants
            user: User performing the operation
            source_version: For rollback, the version being rolled back from
            details: Additional operation context
            schema_changes: Summary of schema changes
            ip_address: Client IP address
            user_agent: Client user agent
            success: Whether operation succeeded
            error_message: Error message if failed

        Returns:
            The created audit log entry
        """
        return cls.objects.create(
            internal_app=internal_app,
            app_version=app_version,
            operation=operation,
            user=user,
            source_version=source_version,
            details_json=details or {},
            schema_changes_json=schema_changes,
            ip_address=ip_address,
            user_agent=user_agent,
            success=success,
            error_message=error_message,
        )

    @classmethod
    def get_app_history(cls, internal_app: "InternalApp", limit: int = 50) -> list:
        """Get recent audit history for an app."""
        return list(
            cls.objects.filter(internal_app=internal_app).select_related(
                "app_version", "user", "source_version"
            )[:limit]
        )

    @classmethod
    def get_user_operations(cls, user: "User", days: int = 30) -> list:
        """Get recent operations by a user."""
        from django.utils import timezone
        from datetime import timedelta

        cutoff = timezone.now() - timedelta(days=days)
        return list(
            cls.objects.filter(user=user, created_at__gte=cutoff).select_related(
                "internal_app", "app_version"
            )
        )


# ============================================================================
# Connectors & Integrations Models (Powered by Merge Agent Handler)
# ============================================================================


class MergeIntegrationProvider(BaseModel):
    """
    Organization's integration provider configuration.
    Internally uses Merge Agent Handler API.
    User-facing: shown as 'Integration Provider' or just 'Integrations'.

    The organization is registered as a single "user" with Merge, and all
    organization members share the connected integrations.

    Note: Merge API credentials (tool_pack_id, access_key) are now read from
    environment variables (settings.py) instead of being stored per-provider.
    The legacy fields are kept for backward compatibility but are no longer used.
    """

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    organization = models.ForeignKey(
        Organization, on_delete=models.CASCADE, related_name="integration_providers"
    )

    # Legacy Merge-specific config - no longer used, credentials come from settings
    # Kept for backward compatibility with existing database rows
    merge_tool_pack_id = models.CharField(
        max_length=255,
        blank=True,
        default="",
        help_text="DEPRECATED: Tool Pack ID now comes from environment variables",
    )
    merge_access_key_encrypted = models.TextField(
        blank=True, default="", help_text="DEPRECATED: Access key now comes from environment variables"
    )
    merge_registered_user_id = models.CharField(
        max_length=255,
        blank=True,
        default="",
        help_text="Organization registered user ID from Merge (set after first link)",
    )

    # User-facing fields
    display_name = models.CharField(max_length=255, default="Integrations")
    is_active = models.BooleanField(default=True)

    class Meta:
        # One integration provider per organization for now
        unique_together = ["organization"]

    def __str__(self):
        return f"{self.display_name} ({self.organization.name})"

    def set_access_key(self, access_key: str):
        """Set encrypted access key. DEPRECATED - use environment variables instead."""
        self.merge_access_key_encrypted = encrypt_string(access_key)

    def get_access_key(self) -> str:
        """Get decrypted access key. DEPRECATED - use environment variables instead."""
        if not self.merge_access_key_encrypted:
            return ""
        return decrypt_string(self.merge_access_key_encrypted)

    def is_registered_with_merge(self) -> bool:
        """Check if the organization is registered with Merge."""
        return bool(self.merge_registered_user_id)


class ConnectorCache(BaseModel):
    """
    Cached connector and tool metadata from the integration provider.
    Used for LLM context generation and UI display.

    This cache is synced periodically from Merge Agent Handler to avoid
    making API calls on every request.
    """

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    provider = models.ForeignKey(
        MergeIntegrationProvider, on_delete=models.CASCADE, related_name="connectors"
    )

    # Connector identity
    connector_id = models.CharField(
        max_length=255, help_text='Connector ID from Merge (e.g., "jira", "linear", "slack")'
    )
    connector_name = models.CharField(
        max_length=255, help_text='Display name (e.g., "Jira", "Linear", "Slack")'
    )
    #move to json
    category = models.CharField(
        max_length=100, help_text='Category (e.g., "project_management", "communication")'
    )
    #deprecate
    logo_url = models.URLField(blank=True, null=True, help_text="URL to connector logo image")
    
    source_url = models.URLField(
        blank=True, null=True, help_text='Source website URL (e.g., "https://linear.app")'
    )
    categories_json = models.JSONField(
        default=list, help_text='List of category tags (e.g., ["Project Management", "Ticketing"])'
    )
    description = models.TextField(blank=True, help_text="Description of the connector")

    # Legacy field - kept for backwards compatibility
    icon_url = models.URLField(blank=True, null=True, help_text="Deprecated: Use logo_url instead")

    # Available tools/actions - cached from Merge
    tools_json = models.JSONField(
        default=list, help_text="List of available tools: [{id, name, description, parameters}]"
    )

    # Status
    is_enabled = models.BooleanField(
        default=True, help_text="Whether this connector is enabled for the organization"
    )
    last_synced_at = models.DateTimeField(
        auto_now=True, help_text="Last time this connector was synced from Merge"
    )

    class Meta:
        unique_together = ["provider", "connector_id"]
        ordering = ["connector_name"]

    def __str__(self):
        return f"{self.connector_name} ({self.provider.organization.name})"

    def get_tools(self) -> list:
        """Get the list of available tools for this connector."""
        return self.tools_json or []

    def get_tool_by_id(self, tool_id: str) -> dict:
        """Get a specific tool by ID."""
        for tool in self.get_tools():
            if tool.get("id") == tool_id:
                return tool
        return None


class OrganizationConnectorLink(BaseModel):
    """
    Tracks organization-level OAuth connections to specific connectors.
    When an org member completes the OAuth flow, the connection is shared
    by all members of the organization.
    """

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    provider = models.ForeignKey(
        MergeIntegrationProvider, on_delete=models.CASCADE, related_name="connector_links"
    )
    connector = models.ForeignKey(ConnectorCache, on_delete=models.CASCADE, related_name="org_links")
    is_connected = models.BooleanField(
        default=False, help_text="Whether the organization has connected this connector"
    )
    connected_at = models.DateTimeField(null=True, blank=True, help_text="When the connector was connected")
    connected_by = models.ForeignKey(
        User,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name="connected_integrations",
        help_text="User who connected this integration",
    )
    connection_metadata = models.JSONField(
        default=dict, blank=True, help_text="Additional metadata about the connection"
    )

    class Meta:
        unique_together = ["provider", "connector"]

    def __str__(self):
        status = "connected" if self.is_connected else "not connected"
        return f"{self.provider.organization.name} - {self.connector.connector_name} ({status})"

    def mark_connected(self, user: User = None):
        """Mark this connector as connected."""
        from django.utils import timezone

        self.is_connected = True
        self.connected_at = timezone.now()
        if user:
            self.connected_by = user
        self.save(update_fields=["is_connected", "connected_at", "connected_by", "updated_at"])

    def mark_disconnected(self):
        """Mark this connector as disconnected."""
        self.is_connected = False
        self.connected_at = None
        self.connected_by = None
        self.save(update_fields=["is_connected", "connected_at", "connected_by", "updated_at"])


class ConnectorToolAction(BaseModel):
    """
    Individual tool with action categorization.
    Groups MCP tools from connectors into action types for easier filtering
    and organization.
    """

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)

    # Foreign key to parent connector cache
    connector_cache = models.ForeignKey(
        ConnectorCache, on_delete=models.CASCADE, related_name="categorized_tools"
    )

    # Tool identity (same fields as tools_json)
    tool_id = models.CharField(max_length=255, help_text='Tool identifier (e.g., "create_issue")')
    tool_name = models.CharField(max_length=255, help_text="Display name for the tool")
    description = models.TextField(blank=True, help_text="Human-readable description of the tool")
    input_schema = models.JSONField(default=dict, help_text="Full JSON Schema for tool parameters")

    # Action categorization
    action_type = models.CharField(
        max_length=20,
        choices=choices(ActionType),
        default=ActionType.OTHER,
        help_text="Categorized action type (query, create, update, delete, send, other)",
    )

    class Meta:
        unique_together = ["connector_cache", "tool_id"]
        indexes = [
            models.Index(fields=["connector_cache", "action_type"]),
            models.Index(fields=["action_type"]),
        ]
        ordering = ["action_type", "tool_name"]

    def __str__(self):
        return f"{self.connector_cache.connector_name} - {self.tool_name} ({self.action_type})"


class ConnectorExecutionLog(BaseModel):
    """
    Log of connector tool executions for audit and debugging.
    Uses the same audit-log pattern for connector operations.
    """

    STATUS_SUCCESS = "success"
    STATUS_ERROR = "error"

    STATUS_CHOICES = [
        (STATUS_SUCCESS, "Success"),
        (STATUS_ERROR, "Error"),
    ]

    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)

    # Context
    internal_app = models.ForeignKey(
        InternalApp,
        on_delete=models.CASCADE,
        related_name="connector_logs",
        null=True,
        blank=True,
    )
    user = models.ForeignKey(
        User, on_delete=models.SET_NULL, null=True, blank=True, related_name="connector_logs"
    )
    connector = models.ForeignKey(
        ConnectorCache, on_delete=models.SET_NULL, null=True, blank=True, related_name="execution_logs"
    )

    # Execution details (denormalized for quick access even if connector deleted)
    connector_slug = models.CharField(max_length=255, help_text='Connector ID slug (e.g., "jira")')
    tool_id = models.CharField(max_length=255)
    params_json = models.JSONField(default=dict, help_text="Input parameters")
    result_json = models.JSONField(null=True, blank=True, help_text="Execution result")

    # Status
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default=STATUS_SUCCESS)
    error_message = models.TextField(null=True, blank=True)
    duration_ms = models.IntegerField(null=True, blank=True, help_text="Execution duration in milliseconds")

    class Meta:
        ordering = ["-created_at"]
        indexes = [
            models.Index(fields=["internal_app", "-created_at"]),
            models.Index(fields=["user", "-created_at"]),
            models.Index(fields=["connector_slug", "-created_at"]),
        ]

    def __str__(self):
        return f"{self.connector_slug}.{self.tool_id} - {self.status}"
