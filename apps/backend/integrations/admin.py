from django.contrib import admin

from integrations.models import (
    MergeIntegrationProvider,
    ConnectorCache,
    OrganizationConnectorLink,
    ConnectorToolAction,
    ConnectorExecutionLog,
)


@admin.register(MergeIntegrationProvider)
class MergeIntegrationProviderAdmin(admin.ModelAdmin):
    list_display = ("display_name", "organization", "is_active", "created_at")
    list_filter = ("is_active",)
    search_fields = ("display_name", "organization__name")


@admin.register(ConnectorCache)
class ConnectorCacheAdmin(admin.ModelAdmin):
    list_display = ("connector_name", "provider", "connector_id", "is_enabled", "last_synced_at")
    list_filter = ("is_enabled",)
    search_fields = ("connector_name", "connector_id")


@admin.register(OrganizationConnectorLink)
class OrganizationConnectorLinkAdmin(admin.ModelAdmin):
    list_display = ("provider", "connector", "is_connected", "connected_at", "connected_by")
    list_filter = ("is_connected",)


@admin.register(ConnectorToolAction)
class ConnectorToolActionAdmin(admin.ModelAdmin):
    list_display = ("tool_name", "connector_cache", "action_type", "created_at")
    list_filter = ("action_type",)
    search_fields = ("tool_name", "tool_id")


@admin.register(ConnectorExecutionLog)
class ConnectorExecutionLogAdmin(admin.ModelAdmin):
    list_display = ("connector_slug", "tool_id", "status", "user", "duration_ms", "created_at")
    list_filter = ("status",)
    search_fields = ("connector_slug", "tool_id")
