"""
Type definitions for the audit app.
"""
from enum import StrEnum


class VersionAuditOperation(StrEnum):
    CREATE = "create"
    ROLLBACK = "rollback"
    PUBLISH = "publish"
    SCHEMA_REVERT = "schema_revert"
    PREVIEW = "preview"
