"""
Type definitions for the apps app.
"""
from enum import StrEnum


class InternalAppStatus(StrEnum):
    DRAFT = "draft"
    PUBLISHED = "published"


class AppVersionSource(StrEnum):
    AI_EDIT = "ai_edit"
    CODE_EDIT = "code_edit"
    ROLLBACK = "rollback"
    PUBLISH = "publish"
    # Legacy aliases (kept for compatibility with existing data/choices)
    AI = "ai"
    CODE = "code"


class AppVersionGenerationStatus(StrEnum):
    PENDING = "pending"
    GENERATING = "generating"
    COMPLETE = "complete"
    ERROR = "error"


class AppVersionValidationStatus(StrEnum):
    PENDING = "pending"
    PASSED = "passed"
    FAILED = "failed"
    SKIPPED = "skipped"
