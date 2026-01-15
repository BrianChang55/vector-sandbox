"""
Shared Types for Services

Common dataclasses used across validation, error fixing, and code generation services.
"""

import json
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional


class VerificationStatus(str, Enum):
    """Status of a file verification."""

    PENDING = "pending"  # Not yet verified
    PASSED = "passed"  # Verification succeeded
    FAILED = "failed"  # Verification failed (code has errors)
    SKIPPED = "skipped"  # No verifier available for file type
    ERROR = "error"  # Verifier service error (tsc not found, timeout)


@dataclass
class FileChange:
    """A file change during execution."""

    path: str
    action: str  # create, modify, delete
    language: str
    content: str
    previous_content: str = ""
    lines_added: int = 0
    lines_removed: int = 0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "path": self.path,
            "action": self.action,
            "language": self.language,
            "content": self.content,
            "previous_content": self.previous_content,
            "lines_added": self.lines_added,
            "lines_removed": self.lines_removed,
        }


@dataclass
class CompilationError:
    """A compilation error from TypeScript validation."""

    file: str
    line: int
    column: int
    message: str
    code: Optional[str] = None  # TypeScript error code like TS2304

    def to_dict(self) -> Dict[str, Any]:
        return {
            "file": self.file,
            "line": self.line,
            "column": self.column,
            "message": self.message,
            "code": self.code,
        }


@dataclass
class ValidationResult:
    """Result of TypeScript validation."""

    passed: bool
    errors: List[CompilationError] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "passed": self.passed,
            "errors": [e.to_dict() for e in self.errors],
            "warnings": self.warnings,
        }


@dataclass
class AgentEvent:
    """An event emitted during agent execution."""

    type: str
    data: Dict[str, Any]

    def to_sse(self) -> str:
        """Format as Server-Sent Event."""
        return f"event: {self.type}\ndata: {json.dumps(self.data)}\n\n"


@dataclass
class VerificationAttempt:
    """A single verification attempt (for retry tracking)."""

    attempt_number: int
    content: str  # The code content that was verified
    status: VerificationStatus
    error_message: Optional[str] = None  # Compilation/syntax error if failed
    timestamp: Optional[datetime] = None  # When this attempt occurred

    def to_dict(self) -> Dict[str, Any]:
        return {
            "attempt_number": self.attempt_number,
            "content": self.content,
            "status": self.status.value,
            "error_message": self.error_message,
            "timestamp": self.timestamp.isoformat() if self.timestamp else None,
        }


@dataclass
class VerifierConfig:
    """Configuration for a verifier."""

    name: str  # e.g., "typescript_syntax"
    is_blocking: bool = True  # If True, failures trigger retry; if False, just track
    timeout_seconds: int = 30  # Max time for verification

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "is_blocking": self.is_blocking,
            "timeout_seconds": self.timeout_seconds,
        }


@dataclass
class FileVerificationResult:
    """Result of verifying a single file (with retry tracking)."""

    file_path: str
    file_type: str  # e.g., ".ts", ".tsx", ".py"
    status: VerificationStatus
    verifier_name: Optional[str] = None  # Which verifier was used
    attempts: List[VerificationAttempt] = field(default_factory=list)
    final_content: Optional[str] = None  # The content after all retries
    error_message: Optional[str] = None  # Final error if failed
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None

    @property
    def attempt_count(self) -> int:
        return len(self.attempts)

    @property
    def passed(self) -> bool:
        return self.status == VerificationStatus.PASSED

    def to_dict(self) -> Dict[str, Any]:
        return {
            "file_path": self.file_path,
            "file_type": self.file_type,
            "status": self.status.value,
            "verifier_name": self.verifier_name,
            "attempts": [a.to_dict() for a in self.attempts],
            "attempt_count": self.attempt_count,
            "final_content": self.final_content,
            "error_message": self.error_message,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
        }
