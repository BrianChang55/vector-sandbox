"""
Snapshot Service for Enterprise Version Management.

Provides automatic snapshot creation, comparison, and revert functionality
for app versions. This enables enterprise-grade version control with
schema-only rollback while preserving data.
"""

import logging
from typing import Optional
from django.db import transaction

from ..models import (
    AppVersion,
    InternalApp,
    VersionFile,
    VersionStateSnapshot,
    VersionAuditLog,
    AppDataTable,
    AppDataTableSnapshot,
)

logger = logging.getLogger(__name__)


class SnapshotService:
    """
    Service for managing version snapshots and reverts.

    Provides:
    - Automatic snapshot creation on version creation
    - Snapshot comparison for diff views
    - Preview of what a revert would change
    - Schema-only revert with data preservation
    - Full audit logging of all operations
    """

    # =========================================================================
    # Snapshot Creation
    # =========================================================================

    @staticmethod
    def create_version_snapshot(version: AppVersion) -> VersionStateSnapshot:
        """
        Create a complete state snapshot for an app version.

        This should be called after every version creation to ensure
        we can always revert to any point in time.

        Args:
            version: The AppVersion to snapshot

        Returns:
            The created VersionStateSnapshot
        """
        # Check if snapshot already exists
        existing = VersionStateSnapshot.objects.filter(app_version=version).first()
        if existing:
            logger.info(f"Snapshot already exists for version {version.id}")
            return existing

        # Create new snapshot
        snapshot = VersionStateSnapshot.create_for_version(version)
        logger.info(
            f"Created snapshot for version {version.id}: "
            f"{snapshot.total_tables} tables, {snapshot.total_rows} rows, "
            f"{snapshot.file_count} files"
        )

        return snapshot

    @staticmethod
    def ensure_snapshot_exists(version: AppVersion) -> VersionStateSnapshot:
        """
        Ensure a snapshot exists for a version, creating one if needed.

        This is useful for backfilling snapshots for existing versions.

        Args:
            version: The AppVersion to ensure has a snapshot

        Returns:
            The existing or newly created snapshot
        """
        try:
            return version.state_snapshot
        except VersionStateSnapshot.DoesNotExist:
            return SnapshotService.create_version_snapshot(version)

    # =========================================================================
    # Snapshot Comparison
    # =========================================================================

    @staticmethod
    def get_snapshot_diff(from_version: AppVersion, to_version: AppVersion) -> dict:
        """
        Compare two version snapshots and return the differences.

        Args:
            from_version: The source version (e.g., current)
            to_version: The target version (e.g., rollback target)

        Returns:
            Dict containing all differences between versions
        """
        # Ensure both versions have snapshots
        from_snapshot = SnapshotService.ensure_snapshot_exists(from_version)
        to_snapshot = SnapshotService.ensure_snapshot_exists(to_version)

        # Get base comparison from snapshot model
        comparison = to_snapshot.compare_to(from_snapshot)

        # Add file differences
        from_files = {f.path: f.content_hash for f in from_version.files.all()}
        to_files = {f.path: f.content_hash for f in to_version.files.all()}

        files_added = [p for p in to_files if p not in from_files]
        files_removed = [p for p in from_files if p not in to_files]
        files_modified = [p for p in to_files if p in from_files and to_files[p] != from_files[p]]

        comparison["files"] = {
            "added": files_added,
            "removed": files_removed,
            "modified": files_modified,
            "from_count": len(from_files),
            "to_count": len(to_files),
        }

        # Add version metadata
        comparison["versions"] = {
            "from": {
                "id": str(from_version.id),
                "version_number": from_version.version_number,
                "source": from_version.source,
                "created_at": from_version.created_at.isoformat(),
            },
            "to": {
                "id": str(to_version.id),
                "version_number": to_version.version_number,
                "source": to_version.source,
                "created_at": to_version.created_at.isoformat(),
            },
        }

        return comparison

    # =========================================================================
    # Revert Preview
    # =========================================================================

    @staticmethod
    def preview_revert(current_version: AppVersion, target_version: AppVersion) -> dict:
        """
        Preview what a revert operation would change.

        This allows users to see the impact before committing to a revert.

        Args:
            current_version: The current version
            target_version: The version to revert to

        Returns:
            Dict with preview information including warnings
        """
        diff = SnapshotService.get_snapshot_diff(current_version, target_version)

        # Generate warnings
        warnings = []

        # Warn about tables that would be removed
        if diff["tables"]["removed"]:
            table_names = [t["name"] for t in diff["tables"]["removed"]]
            warnings.append(
                {
                    "type": "tables_removed",
                    "severity": "warning",
                    "message": f"Tables will be removed from schema: {', '.join(table_names)}",
                    "details": diff["tables"]["removed"],
                }
            )

        # Warn about schema changes
        if diff["tables"]["modified"]:
            modified_names = [t["slug"] for t in diff["tables"]["modified"]]
            warnings.append(
                {
                    "type": "tables_modified",
                    "severity": "info",
                    "message": f"Table schemas will be modified: {', '.join(modified_names)}",
                    "details": diff["tables"]["modified"],
                }
            )

        # Warn about files being replaced
        if diff["files"]["modified"]:
            warnings.append(
                {
                    "type": "files_modified",
                    "severity": "info",
                    "message": f"{len(diff['files']['modified'])} files will be replaced with older versions",
                    "details": diff["files"]["modified"],
                }
            )

        return {
            "diff": diff,
            "warnings": warnings,
            "can_revert": True,  # Schema-only revert is always safe
            "target_version": {
                "id": str(target_version.id),
                "version_number": target_version.version_number,
                "source": target_version.source,
                "created_at": target_version.created_at.isoformat(),
                "intent_message": target_version.intent_message,
            },
        }

    # =========================================================================
    # Revert Execution
    # =========================================================================

    @staticmethod
    def execute_rollback(
        target_version: AppVersion,
        user=None,
        include_schema: bool = True,
        ip_address: str = None,
        user_agent: str = None,
    ) -> AppVersion:
        """
        Execute a rollback to a target version.

        This creates a new version (preserving history) with:
        - Code files from the target version
        - Schema from the target version (if include_schema=True)
        - Current data is preserved (schema-only rollback)

        Args:
            target_version: The version to rollback to
            user: The user performing the rollback
            include_schema: Whether to revert schema changes
            ip_address: Client IP for audit logging
            user_agent: Client user agent for audit logging

        Returns:
            The newly created rollback version
        """
        from .version_service import VersionService

        app = target_version.internal_app
        current_version = VersionService.get_latest_stable_version(app)

        # Get the preview/diff for audit logging
        diff = None
        if current_version:
            diff = SnapshotService.get_snapshot_diff(current_version, target_version)

        with transaction.atomic():
            # Get next version number
            next_version_number = VersionService.get_next_version_number(app)

            # Create new rollback version
            rollback_version = AppVersion.objects.create(
                internal_app=app,
                version_number=next_version_number,
                parent_version=target_version,
                source=AppVersion.SOURCE_ROLLBACK,
                spec_json=target_version.spec_json,
                intent_message=f"Rollback to v{target_version.version_number}",
                created_by=user,
                generation_status=AppVersion.GEN_STATUS_COMPLETE,
                is_active=False,  # Start inactive until files are copied
            )

            # Copy files from target version
            for source_file in target_version.files.all():
                VersionFile.objects.create(
                    app_version=rollback_version,
                    path=source_file.path,
                    content=source_file.content,
                    content_hash=source_file.content_hash or VersionFile.compute_hash(source_file.content),
                )

            # Revert schemas if requested
            schema_changes = None
            if include_schema:
                schema_changes = SnapshotService._revert_schemas(app, target_version, rollback_version)

            # Create snapshot for the new rollback version
            SnapshotService.create_version_snapshot(rollback_version)

            # Mark as active after successful file copy and schema revert
            rollback_version.is_active = True
            rollback_version.save(update_fields=["is_active", "updated_at"])

            # Log the operation
            VersionAuditLog.log_operation(
                internal_app=app,
                app_version=rollback_version,
                operation=VersionAuditLog.OPERATION_ROLLBACK,
                user=user,
                source_version=current_version,
                details={
                    "target_version_id": str(target_version.id),
                    "target_version_number": target_version.version_number,
                    "include_schema": include_schema,
                    "files_copied": rollback_version.files.count(),
                },
                schema_changes=schema_changes,
                ip_address=ip_address,
                user_agent=user_agent,
            )

            logger.info(
                f"Rollback executed: v{current_version.version_number if current_version else 0} -> "
                f"v{target_version.version_number} (created v{rollback_version.version_number})"
            )

        return rollback_version

    @staticmethod
    def _revert_schemas(app: InternalApp, target_version: AppVersion, rollback_version: AppVersion) -> dict:
        """
        Revert table schemas to match the target version.

        This is a schema-only operation - data is preserved.

        Args:
            app: The internal app
            target_version: Version to get schemas from
            rollback_version: The new rollback version being created

        Returns:
            Summary of schema changes made
        """
        # Get target version's snapshot
        try:
            target_snapshot = target_version.state_snapshot
        except VersionStateSnapshot.DoesNotExist:
            # Create snapshot if it doesn't exist (backfill case)
            target_snapshot = SnapshotService.create_version_snapshot(target_version)

        changes = {
            "tables_reverted": [],
            "tables_created": [],
            "tables_unchanged": [],
        }

        # Get current tables
        current_tables = {t.slug: t for t in AppDataTable.objects.filter(internal_app=app)}

        # Get target table schemas from snapshot
        target_tables = {t["slug"]: t for t in target_snapshot.tables_json}

        # Revert existing tables that exist in target
        for slug, table in current_tables.items():
            if slug in target_tables:
                target_schema = target_tables[slug]

                # Check if schema needs updating
                if table.schema_json != target_schema["schema"]:
                    # Store previous schema for the snapshot
                    previous_schema = table.schema_json.copy()

                    # Update table schema
                    table.schema_json = target_schema["schema"]
                    table.name = target_schema["name"]
                    table.description = target_schema.get("description", "")
                    table.save()

                    # Create table snapshot for the rollback version
                    AppDataTableSnapshot.create_snapshot(
                        app_version=rollback_version,
                        table=table,
                        operation="update",
                        previous_schema=previous_schema,
                    )

                    changes["tables_reverted"].append(
                        {
                            "slug": slug,
                            "name": table.name,
                        }
                    )
                else:
                    changes["tables_unchanged"].append(slug)
            # Note: We don't delete tables that don't exist in target
            # This preserves data even when reverting to older versions

        # Create tables that exist in target but not currently
        for slug, target_schema in target_tables.items():
            if slug not in current_tables:
                # Create the table
                table = AppDataTable.objects.create(
                    internal_app=app,
                    slug=slug,
                    name=target_schema["name"],
                    description=target_schema.get("description", ""),
                    schema_json=target_schema["schema"],
                    row_count=0,
                )

                # Create table snapshot
                AppDataTableSnapshot.create_snapshot(
                    app_version=rollback_version,
                    table=table,
                    operation="create",
                )

                changes["tables_created"].append(
                    {
                        "slug": slug,
                        "name": target_schema["name"],
                    }
                )

        return changes

    # =========================================================================
    # Utility Methods
    # =========================================================================

    @staticmethod
    def get_version_history(app: InternalApp, limit: int = 50, include_snapshots: bool = False) -> list:
        """
        Get version history for an app with optional snapshot data.

        Args:
            app: The internal app
            limit: Maximum number of versions to return
            include_snapshots: Whether to include full snapshot data

        Returns:
            List of version data dicts
        """
        versions = (
            AppVersion.objects.filter(internal_app=app, generation_status=AppVersion.GEN_STATUS_COMPLETE)
            .select_related("created_by")
            .order_by("-version_number")[:limit]
        )

        result = []
        for version in versions:
            version_data = {
                "id": str(version.id),
                "version_number": version.version_number,
                "source": version.source,
                "intent_message": version.intent_message,
                "created_by_email": version.created_by.email if version.created_by else None,
                "created_at": version.created_at.isoformat(),
                "file_count": version.files.count(),
            }

            if include_snapshots:
                try:
                    snapshot = version.state_snapshot
                    version_data["snapshot"] = {
                        "total_tables": snapshot.total_tables,
                        "total_rows": snapshot.total_rows,
                        "tables": snapshot.tables_json,
                    }
                except VersionStateSnapshot.DoesNotExist:
                    version_data["snapshot"] = None

            result.append(version_data)

        return result

    @staticmethod
    def get_audit_trail(app: InternalApp, limit: int = 50) -> list:
        """
        Get audit trail for an app.

        Args:
            app: The internal app
            limit: Maximum number of entries to return

        Returns:
            List of audit log entries
        """
        logs = VersionAuditLog.get_app_history(app, limit)

        return [
            {
                "id": str(log.id),
                "operation": log.operation,
                "version_number": log.app_version.version_number,
                "source_version_number": log.source_version.version_number if log.source_version else None,
                "user_email": log.user.email if log.user else None,
                "details": log.details_json,
                "schema_changes": log.schema_changes_json,
                "success": log.success,
                "error_message": log.error_message,
                "created_at": log.created_at.isoformat(),
            }
            for log in logs
        ]
