"""
Apps app models - Internal application and version management.
"""
import hashlib

from django.conf import settings
from django.db import models
from django.utils.text import slugify

from internal_apps.utils.base_model import DjangoBaseModel
from internal_apps.utils.enum import choices
from apps.types import (
    InternalAppStatus,
    AppVersionSource,
    AppVersionGenerationStatus,
    AppVersionValidationStatus,
)


class InternalApp(DjangoBaseModel):
    """
    Internal application created by users.
    """
    organization = models.ForeignKey('accounts.Organization', on_delete=models.CASCADE, related_name="internal_apps")
    name = models.CharField(max_length=255)
    slug = models.SlugField(
        max_length=255,
        blank=True,
        null=True,
        help_text="URL-friendly identifier for the app (auto-generated from name if not set)",
    )
    description = models.TextField(blank=True)
    status = models.CharField(
        max_length=20,
        choices=choices(InternalAppStatus),
        default=InternalAppStatus.DRAFT,
    )
    published_version = models.ForeignKey(
        "AppVersion",
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name="published_for_app",
        help_text="The currently active published version",
    )
    allow_actions_in_preview = models.BooleanField(default=False)
    created_by = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.SET_NULL, null=True, related_name="created_apps")

    class Meta:
        unique_together = ["organization", "slug"]
        indexes = [
            models.Index(fields=["organization", "slug"]),
        ]

    def __str__(self):
        return f"{self.name} ({self.organization.name})"

    def generate_slug(self):
        """Generate a URL-friendly slug from the app name."""
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


class AppFavorite(DjangoBaseModel):
    """
    Tracks which apps a user has favorited within an organization.
    """

    user = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.CASCADE, related_name="app_favorites")
    organization = models.ForeignKey('accounts.Organization', on_delete=models.CASCADE, related_name="app_favorites")
    app = models.ForeignKey(InternalApp, on_delete=models.CASCADE, related_name="favorites")

    class Meta:
        unique_together = ["user", "organization", "app"]
        indexes = [
            models.Index(fields=["user", "organization"]),
        ]

    def __str__(self):
        return f"{self.user.email} - {self.app.name}"


class AppVersion(DjangoBaseModel):
    """
    Version of an internal app (immutable snapshot).
    """
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
    source = models.CharField(
        max_length=20,
        choices=choices(AppVersionSource),
        default=AppVersionSource.AI_EDIT,
    )
    created_by = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.SET_NULL, null=True)

    # Version activation status
    is_active = models.BooleanField(
        default=False,
        help_text="Whether this version is active and should be returned by API. "
        "Only set True when generation completes successfully.",
    )

    # Agentic generation tracking
    generation_status = models.CharField(
        max_length=20,
        choices=choices(AppVersionGenerationStatus),
        default=AppVersionGenerationStatus.COMPLETE,
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
    validation_status = models.CharField(
        max_length=20,
        choices=choices(AppVersionValidationStatus),
        default=AppVersionValidationStatus.PENDING,
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


class VersionFile(DjangoBaseModel):
    """
    File content for an app version.
    """

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
