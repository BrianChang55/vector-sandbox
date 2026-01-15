"""
Integrations app models - Merge/MCP connector management.
"""
from django.conf import settings
from django.db import models
from django.utils import timezone

from internal_apps.utils.base_model import DjangoBaseModel
from internal_apps.utils.enum import choices
from internal_apps.utils.encryption import encrypt_string, decrypt_string
from integrations.types import ConnectorExecutionStatus
from apps.action_classification.types import ActionType


class MergeIntegrationProvider(DjangoBaseModel):
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

    organization = models.ForeignKey(
        'accounts.Organization', on_delete=models.CASCADE, related_name="integration_providers"
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


class ConnectorCache(DjangoBaseModel):
    """
    Cached connector and tool metadata from the integration provider.
    Used for LLM context generation and UI display.

    This cache is synced periodically from Merge Agent Handler to avoid
    making API calls on every request.
    """

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


class OrganizationConnectorLink(DjangoBaseModel):
    """
    Tracks organization-level OAuth connections to specific connectors.
    When an org member completes the OAuth flow, the connection is shared
    by all members of the organization.
    """

    provider = models.ForeignKey(
        MergeIntegrationProvider, on_delete=models.CASCADE, related_name="connector_links"
    )
    connector = models.ForeignKey(ConnectorCache, on_delete=models.CASCADE, related_name="org_links")
    is_connected = models.BooleanField(
        default=False, help_text="Whether the organization has connected this connector"
    )
    connected_at = models.DateTimeField(null=True, blank=True, help_text="When the connector was connected")
    connected_by = models.ForeignKey(
        settings.AUTH_USER_MODEL,
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

    def mark_connected(self, user=None):
        """Mark this connector as connected."""
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


class ConnectorToolAction(DjangoBaseModel):
    """
    Individual tool with action categorization.
    Groups MCP tools from connectors into action types for easier filtering
    and organization.
    """

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


class ConnectorExecutionLog(DjangoBaseModel):
    """
    Log of connector tool executions for audit and debugging.
    Uses the same audit-log pattern for connector operations.
    """

    # Context
    internal_app = models.ForeignKey(
        'apps.InternalApp',
        on_delete=models.CASCADE,
        related_name="connector_logs",
        null=True,
        blank=True,
    )
    user = models.ForeignKey(
        settings.AUTH_USER_MODEL, on_delete=models.SET_NULL, null=True, blank=True, related_name="connector_logs"
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
    status = models.CharField(
        max_length=20,
        choices=choices(ConnectorExecutionStatus),
        default=ConnectorExecutionStatus.SUCCESS,
    )
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
