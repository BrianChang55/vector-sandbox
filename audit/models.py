"""
Audit app models - Version snapshots and audit logs.
"""
from datetime import timedelta

from django.conf import settings
from django.db import models
from django.utils import timezone

from internal_apps.utils.base_model import DjangoBaseModel
from internal_apps.utils.enum import choices
from audit.types import VersionAuditOperation


class VersionStateSnapshot(DjangoBaseModel):
    """
    Complete application state snapshot at a specific version.
    Created automatically on every version to enable full revert.

    This captures the entire state of an app's data tables at the moment
    a version is created, enabling enterprise-grade version management
    with schema-only rollback while preserving data.
    """

    app_version = models.OneToOneField(
        'apps.AppVersion',
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
    def create_for_version(cls, app_version):
        """
        Create a complete state snapshot for an app version.

        Captures:
        - All data table schemas
        - File counts and metadata
        """
        from data_store.models import AppDataTable
        
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

    def compare_to(self, other):
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


class VersionAuditLog(DjangoBaseModel):
    """
    Audit trail for version operations.

    Tracks all version-related operations for enterprise compliance:
    - Version creation (AI edit, code edit)
    - Rollback operations
    - Publish operations
    - Schema reverts

    This is an append-only log that provides full traceability.
    """

    # The app this operation belongs to
    internal_app = models.ForeignKey(
        'apps.InternalApp',
        on_delete=models.CASCADE,
        related_name="version_audit_logs",
        help_text="The app this audit log belongs to",
    )

    # The version affected by this operation
    app_version = models.ForeignKey(
        'apps.AppVersion',
        on_delete=models.CASCADE,
        related_name="audit_logs",
        help_text="The version affected by this operation",
    )

    # For rollback operations, the source version
    source_version = models.ForeignKey(
        'apps.AppVersion',
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name="rollback_audit_logs",
        help_text="For rollback operations, the version being rolled back from",
    )

    # Operation details
    operation = models.CharField(
        max_length=20,
        choices=choices(VersionAuditOperation),
        help_text="The type of operation performed",
    )

    # User who performed the operation
    user = models.ForeignKey(
        settings.AUTH_USER_MODEL,
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
        internal_app,
        app_version,
        operation: str,
        user=None,
        source_version=None,
        details: dict = None,
        schema_changes: dict = None,
        ip_address: str = None,
        user_agent: str = None,
        success: bool = True,
        error_message: str = None,
    ):
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
    def get_app_history(cls, internal_app, limit: int = 50) -> list:
        """Get recent audit history for an app."""
        return list(
            cls.objects.filter(internal_app=internal_app).select_related(
                "app_version", "user", "source_version"
            )[:limit]
        )

    @classmethod
    def get_user_operations(cls, user, days: int = 30) -> list:
        """Get recent operations by a user."""
        cutoff = timezone.now() - timedelta(days=days)
        return list(
            cls.objects.filter(user=user, created_at__gte=cutoff).select_related(
                "internal_app", "app_version"
            )
        )
