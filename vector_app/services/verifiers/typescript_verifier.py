"""
TypeScript Verifier

Wraps existing ValidationService to fit the BaseVerifier framework.
Enables per-file tracking, events, and extensibility while leveraging
the battle-tested TypeScript validation with React/lucide/dataStore stubs.
"""

import logging
from datetime import datetime
from typing import List, Optional

from vector_app.services.types import (
    FileChange,
    FileVerificationResult,
    VerificationAttempt,
    VerifierConfig,
    CompilationError,
)
from vector_app.types import VerificationStatus
from vector_app.services.validation_service import get_validation_service
from vector_app.services.verifiers.base_verifier import BaseVerifier

logger = logging.getLogger(__name__)


class TypeScriptVerifier(BaseVerifier):
    """
    Verifier for TypeScript and TSX files.

    Wraps the existing ValidationService to provide per-file verification
    results while leveraging its comprehensive type stubs and error filtering.

    Usage:
        verifier = get_typescript_verifier()
        result = verifier(content="const x: number = 1;", file_path="test.ts")
    """

    def _default_config(self) -> VerifierConfig:
        """Return default configuration for TypeScript verification."""
        return VerifierConfig(
            name="typescript",
            is_blocking=True,
            timeout_seconds=30,
        )

    def __call__(
        self,
        content: str,
        file_path: str,
    ) -> FileVerificationResult:
        """
        Verify TypeScript/TSX file content.

        Args:
            content: The file content to verify
            file_path: Path to the file (used for language detection)

        Returns:
            FileVerificationResult with validation status and any errors
        """
        logger.info("[TS_VERIFIER] Starting verification for: %s", file_path)
        started_at = datetime.now()
        file_type = self._get_file_type(file_path)

        # Determine language from extension
        language = self._get_language(file_path)
        logger.debug("[TS_VERIFIER] Detected language: %s for %s", language, file_path)

        # Create FileChange for ValidationService
        file_change = FileChange(
            path=file_path,
            action="create",
            language=language,
            content=content,
        )

        # Get validation service and validate
        logger.info("[TS_VERIFIER] Calling ValidationService.validate_typescript for %s", file_path)
        validation_service = get_validation_service()
        result = validation_service.validate_typescript([file_change])
        logger.info("[TS_VERIFIER] ValidationService returned passed=%s for %s", result.passed, file_path)

        # Map ValidationResult to FileVerificationResult
        if result.passed:
            status = VerificationStatus.PASSED
            error_message = None
            logger.info("[TS_VERIFIER] Verification PASSED for %s", file_path)
        else:
            status = VerificationStatus.FAILED
            error_message = self._format_errors(result.errors)
            logger.warning("[TS_VERIFIER] Verification FAILED for %s: %s", file_path, error_message)

        completed_at = datetime.now()

        return FileVerificationResult(
            file_path=file_path,
            file_type=file_type,
            status=status,
            verifier_name=self.name,
            attempts=[
                VerificationAttempt(
                    attempt_number=1,
                    content=content,
                    status=status,
                    error_message=error_message,
                    timestamp=started_at,
                )
            ],
            final_content=content,
            error_message=error_message,
            started_at=started_at,
            completed_at=completed_at,
        )

    def _get_language(self, file_path: str) -> str:
        """Determine TypeScript language variant from file path."""
        if file_path.endswith(".tsx"):
            return "tsx"
        if file_path.endswith(".ts"):
            return "typescript"
        # Default to typescript for unknown
        return "typescript"

    def _format_errors(self, errors: List[CompilationError]) -> Optional[str]:
        """
        Format compilation errors into a readable string.

        Args:
            errors: List of CompilationError objects

        Returns:
            Formatted error string or None if no errors
        """
        if not errors:
            return None

        formatted = []
        for error in errors:
            # Format: file:line:col - TS1234: message
            code_str = f"{error.code}: " if error.code else ""
            formatted.append(f"{error.file}:{error.line}:{error.column} - {code_str}{error.message}")

        return "\n".join(formatted)


# Singleton instance
_typescript_verifier: Optional[TypeScriptVerifier] = None


def get_typescript_verifier() -> TypeScriptVerifier:
    """Get singleton TypeScript verifier instance."""
    global _typescript_verifier
    if _typescript_verifier is None:
        _typescript_verifier = TypeScriptVerifier()
    return _typescript_verifier
