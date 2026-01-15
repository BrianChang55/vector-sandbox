"""
Type definitions for the data_store app.
"""
from enum import StrEnum


class AppDataTableSnapshotOperation(StrEnum):
    CREATE = "create"
    UPDATE = "update"
    DELETE = "delete"
