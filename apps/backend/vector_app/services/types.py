"""
Shared Types for Services

Common dataclasses used across validation, error fixing, and code generation services.
"""
import json
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional


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
