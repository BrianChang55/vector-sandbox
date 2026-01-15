"""
Data Store app models - App data tables and rows.
"""
from django.db import models
from django.db.models.functions import Greatest

from internal_apps.utils.base_model import DjangoBaseModel
from internal_apps.utils.enum import choices
from data_store.types import AppDataTableSnapshotOperation


class AppDataTable(DjangoBaseModel):
    """
    Represents a data table within an Internal App's data store.
    Each app can have multiple tables, each with its own schema.

    The schema is stored as JSON and defines columns, types, and constraints.
    This allows apps to store structured data without external database connections.
    """

    internal_app = models.ForeignKey('apps.InternalApp', on_delete=models.CASCADE, related_name="data_tables")
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
        self.row_count = Greatest(models.F("row_count") - delta, 0)
        self.save(update_fields=["row_count", "updated_at"])
        self.refresh_from_db(fields=["row_count"])


class AppDataRow(DjangoBaseModel):
    """
    Represents a single row of data in an AppDataTable.
    Data is stored as JSON and validated against the table schema.

    Each row has a sequential index within its table for ordering.
    """

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


class AppDataTableSnapshot(DjangoBaseModel):
    """
    Snapshot of a table's schema at a specific app version.

    Enables schema versioning and rollback. Each time a table is created,
    updated, or deleted during app generation, a snapshot is recorded
    linking the schema state to the AppVersion being created.
    """

    app_version = models.ForeignKey(
        'apps.AppVersion',
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
        max_length=20,
        choices=choices(AppDataTableSnapshotOperation),
        help_text="Operation that created this snapshot",
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
    def create_snapshot(cls, app_version, table, operation: str, previous_schema: dict = None):
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
