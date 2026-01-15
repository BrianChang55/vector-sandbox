from django.contrib import admin

from audit.models import VersionStateSnapshot, VersionAuditLog


@admin.register(VersionStateSnapshot)
class VersionStateSnapshotAdmin(admin.ModelAdmin):
    list_display = ("app_version", "total_tables", "total_rows", "file_count", "created_at")
    search_fields = ("app_version__internal_app__name",)


@admin.register(VersionAuditLog)
class VersionAuditLogAdmin(admin.ModelAdmin):
    list_display = ("internal_app", "app_version", "operation", "user", "success", "created_at")
    list_filter = ("operation", "success")
    search_fields = ("internal_app__name", "user__email")
