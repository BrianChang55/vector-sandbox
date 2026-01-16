from django.contrib import admin

from apps.models import InternalApp, AppFavorite, AppVersion, VersionFile


@admin.register(InternalApp)
class InternalAppAdmin(admin.ModelAdmin):
    list_display = ("name", "organization", "status", "created_at")
    list_filter = ("status",)
    search_fields = ("name", "organization__name")
    prepopulated_fields = {"slug": ("name",)}


@admin.register(AppFavorite)
class AppFavoriteAdmin(admin.ModelAdmin):
    list_display = ("user", "app", "organization", "created_at")
    search_fields = ("user__email", "app__name")


@admin.register(AppVersion)
class AppVersionAdmin(admin.ModelAdmin):
    list_display = ("internal_app", "version_number", "source", "generation_status", "is_active", "created_at")
    list_filter = ("source", "generation_status", "is_active")
    search_fields = ("internal_app__name",)


@admin.register(VersionFile)
class VersionFileAdmin(admin.ModelAdmin):
    list_display = ("app_version", "path", "created_at")
    search_fields = ("app_version__internal_app__name", "path")
