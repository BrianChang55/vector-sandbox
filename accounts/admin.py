from django.contrib import admin
from django.contrib.auth.admin import UserAdmin as BaseUserAdmin

from accounts.models import (
    User,
    Organization,
    UserOrganization,
    OrganizationInvite,
    MagicLinkToken,
)


@admin.register(User)
class UserAdmin(BaseUserAdmin):
    list_display = ("email", "first_name", "last_name", "is_staff", "created_at")
    list_filter = ("is_staff", "is_superuser", "is_active")
    search_fields = ("email", "first_name", "last_name")
    ordering = ("-created_at",)
    
    fieldsets = BaseUserAdmin.fieldsets + (
        ("OAuth", {"fields": ("google_id",)}),
        ("Profile Image", {"fields": ("profile_image_url", "profile_image_storage_key")}),
    )


@admin.register(Organization)
class OrganizationAdmin(admin.ModelAdmin):
    list_display = ("name", "slug", "created_at")
    search_fields = ("name", "slug")
    prepopulated_fields = {"slug": ("name",)}


@admin.register(UserOrganization)
class UserOrganizationAdmin(admin.ModelAdmin):
    list_display = ("user", "organization", "role", "created_at")
    list_filter = ("role",)
    search_fields = ("user__email", "organization__name")


@admin.register(OrganizationInvite)
class OrganizationInviteAdmin(admin.ModelAdmin):
    list_display = ("email", "organization", "role", "is_accepted", "expires_at", "created_at")
    list_filter = ("is_accepted", "role")
    search_fields = ("email", "organization__name")


@admin.register(MagicLinkToken)
class MagicLinkTokenAdmin(admin.ModelAdmin):
    list_display = ("email", "is_used", "expires_at", "created_at")
    list_filter = ("is_used",)
    search_fields = ("email",)
