"""
Base Verifier

Abstract base class for all file verifiers.
"""
import logging
from abc import ABC, abstractmethod
from typing import Optional

from vector_app.services.types import (
    FileVerificationResult,
    VerificationStatus,
    VerifierConfig,
)

logger = logging.getLogger(__name__)


class BaseVerifier(ABC):
    """
    Abstract base class for file verifiers.

    All verifiers must implement the __call__ method which takes
    file content and returns a FileVerificationResult.

    Usage:
        verifier = get_typescript_syntax_verifier()
        result = verifier(content="const x: number = 1;", file_path="test.ts")
    """

    def __init__(self, config: Optional[VerifierConfig] = None):
        """
        Initialize verifier with optional config.

        Args:
            config: Verifier configuration. If None, uses default config.
        """
        self._config = config or self._default_config()

    @property
    def config(self) -> VerifierConfig:
        """Get verifier configuration."""
        return self._config

    @property
    def name(self) -> str:
        """Get verifier name from config."""
        return self._config.name

    @property
    def is_blocking(self) -> bool:
        """Whether failures should trigger retry (blocking) or just track (advisory)."""
        return self._config.is_blocking

    @property
    def timeout_seconds(self) -> int:
        """Maximum time for verification."""
        return self._config.timeout_seconds

    @abstractmethod
    def _default_config(self) -> VerifierConfig:
        """
        Return default configuration for this verifier.

        Subclasses must implement to provide their default config.
        """
        pass

    @abstractmethod
    def __call__(
        self,
        content: str,
        file_path: str,
    ) -> FileVerificationResult:
        """
        Verify file content.

        Args:
            content: The file content to verify
            file_path: Path to the file (for error messages and file_type detection)

        Returns:
            FileVerificationResult with status, error messages, and metadata

        Note:
            - Return VerificationStatus.PASSED if verification succeeds
            - Return VerificationStatus.FAILED if code has errors (syntax, type, etc.)
            - Return VerificationStatus.ERROR if verifier itself fails (timeout, not installed)
            - Return VerificationStatus.SKIPPED if file type not supported
        """
        pass

    def _get_file_type(self, file_path: str) -> str:
        """Extract file extension from path."""
        if "." in file_path:
            return "." + file_path.rsplit(".", 1)[-1]
        return ""
