"""
Accounts app models - User identity and organization management.
"""
import hashlib
import secrets
from datetime import timedelta

from django.contrib.auth.models import AbstractUser
from django.db import models
from django.utils import timezone

from internal_apps.utils.base_model import DjangoBaseModel
from internal_apps.utils.enum import choices
from accounts.types import UserOrganizationRole


class User(AbstractUser, DjangoBaseModel):
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
        # Import here to avoid circular import between models and services
        from vector_app.services.image_upload_service import ImageUploadService

        # Prefer uploaded image if set
        if self.profile_image_storage_key:
            return ImageUploadService.get_image_url(self.profile_image_storage_key)

        # Fall back to OAuth provider URL
        if self.profile_image_url:
            return self.profile_image_url

        return None

    def delete_profile_image(self):
        """Delete the uploaded profile image and clear the storage key."""
        # Import here to avoid circular import between models and services
        from vector_app.services.image_upload_service import ImageUploadService

        if self.profile_image_storage_key:
            ImageUploadService.delete_image(self.profile_image_storage_key)
            self.profile_image_storage_key = None
            self.save(update_fields=['profile_image_storage_key', 'updated_at'])


class Organization(DjangoBaseModel):
    """
    Organization model representing a company/team (Project container).
    """

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
        # Import here to avoid circular import between models and services
        from vector_app.services.image_upload_service import ImageUploadService

        # Prefer new storage key if set
        if self.logo_storage_key:
            return ImageUploadService.get_image_url(self.logo_storage_key)

        # Fall back to legacy logo field
        if self.logo:
            return self.logo.url

        return None

    def delete_logo(self):
        """Delete the logo from storage and clear both fields."""
        # Import here to avoid circular import between models and services
        from vector_app.services.image_upload_service import ImageUploadService

        # Delete from new storage if set
        if self.logo_storage_key:
            ImageUploadService.delete_image(self.logo_storage_key)
            self.logo_storage_key = None

        # Delete legacy logo if set
        if self.logo:
            self.logo.delete(save=False)

        self.save(update_fields=['logo', 'logo_storage_key', 'updated_at'])


class UserOrganization(DjangoBaseModel):
    """
    Junction table for User-Organization many-to-many relationship.
    Includes role field for membership permissions.

    Roles:
    - Admin: Full access - can edit apps, manage integrations, manage members/roles
    - Editor: Can edit apps - cannot modify integrations or members
    - Viewer: View/run only - sees published apps, redirected to published view
    """

    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name="user_organizations")
    organization = models.ForeignKey(
        Organization, on_delete=models.CASCADE, related_name="user_organizations"
    )
    role = models.CharField(
        max_length=20,
        choices=choices(UserOrganizationRole),
        default=UserOrganizationRole.EDITOR,
    )

    class Meta:
        unique_together = ["user", "organization"]

    def __str__(self):
        return f"{self.user.email} - {self.organization.name} ({self.role})"

    def is_admin(self):
        """Check if user has admin role."""
        return self.role == UserOrganizationRole.ADMIN

    def is_editor_or_above(self):
        """Check if user has editor or admin role."""
        return self.role in [UserOrganizationRole.ADMIN, UserOrganizationRole.EDITOR]

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


class OrganizationInvite(DjangoBaseModel):
    """
    Pending invitation to join an organization.

    Security features:
    - Tokens are hashed (SHA256) before storage - raw token never saved
    - Short expiry time (7 days default)
    - Single-use: invite is marked as accepted after successful verification
    - Rate limiting can be tracked via created_at timestamps
    """

    organization = models.ForeignKey(Organization, on_delete=models.CASCADE, related_name="invites")
    email = models.EmailField(db_index=True, help_text="Email address this invitation is for")
    role = models.CharField(
        max_length=20,
        choices=choices(UserOrganizationRole),
        default=UserOrganizationRole.EDITOR,
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
        return cls.objects.filter(expires_at__lt=timezone.now(), is_accepted=False).delete()

    @classmethod
    def get_pending_for_org(cls, organization):
        """Get all pending invitations for an organization."""
        return cls.objects.filter(
            organization=organization, is_accepted=False, expires_at__gt=timezone.now()
        ).select_related("invited_by")


class MagicLinkToken(DjangoBaseModel):
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
        return timezone.now() > self.expires_at

    @property
    def is_valid(self):
        """Check if the token is still valid (not expired and not used)."""
        return not self.is_expired and not self.is_used

    @classmethod
    def hash_token(cls, raw_token: str) -> str:
        """Hash a raw token using SHA256."""
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
        cutoff = timezone.now() - timedelta(minutes=minutes)
        return cls.objects.filter(email=email, created_at__gte=cutoff).count()

    @classmethod
    def cleanup_expired(cls):
        """Delete all expired tokens."""
        return cls.objects.filter(expires_at__lt=timezone.now()).delete()
