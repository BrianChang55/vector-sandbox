"""
Verifier Registry

Maps file extensions to appropriate verifier(s).
Provides clean interface for handlers to ask "what verifiers should I run for this file?"
"""

import logging
from typing import Dict, List, Optional

from vector_app.services.verifiers.base_verifier import BaseVerifier
from vector_app.services.verifiers.typescript_verifier import get_typescript_verifier

logger = logging.getLogger(__name__)


class VerifierRegistry:
    """
    Registry that maps file extensions to verifier(s).

    Provides extension-based routing to determine which verifiers
    should be run for a given file. Supports multiple verifiers
    per extension for future extensibility (e.g., syntax + style).

    Usage:
        registry = get_verifier_registry()
        verifiers = registry.get_verifiers("src/App.tsx")
        for verifier in verifiers:
            result = verifier(content, file_path)
    """

    def __init__(self) -> None:
        """Initialize registry with default verifier registrations."""
        self._extension_map: Dict[str, List[BaseVerifier]] = {}
        self._register_defaults()

    def _register_defaults(self) -> None:
        """Register built-in verifiers for known file types."""
        typescript_verifier = get_typescript_verifier()
        self.register(".ts", typescript_verifier)
        self.register(".tsx", typescript_verifier)

    def register(self, extension: str, verifier: BaseVerifier) -> None:
        """
        Register a verifier for a file extension.

        Args:
            extension: File extension (e.g., ".ts", "ts", ".tsx")
            verifier: Verifier instance to register
        """
        # Normalize extension to lowercase with leading dot
        normalized = self._normalize_extension(extension)

        if normalized not in self._extension_map:
            self._extension_map[normalized] = []

        self._extension_map[normalized].append(verifier)
        logger.debug("Registered %s for %s", verifier.name, normalized)

    def get_verifiers(self, file_path: str) -> List[BaseVerifier]:
        """
        Get all verifiers for a file based on its extension.

        Args:
            file_path: Path to the file

        Returns:
            List of verifiers for the file's extension, or empty list if none
        """
        extension = self._get_extension(file_path)
        return self.get_verifiers_for_extension(extension)

    def get_verifiers_for_extension(self, extension: str) -> List[BaseVerifier]:
        """
        Get all verifiers for a specific extension.

        Args:
            extension: File extension (e.g., ".ts", "ts")

        Returns:
            List of verifiers for the extension, or empty list if none
        """
        normalized = self._normalize_extension(extension)
        return self._extension_map.get(normalized, [])

    def supports(self, file_path: str) -> bool:
        """
        Check if any verifiers are registered for the file's extension.

        Args:
            file_path: Path to the file

        Returns:
            True if at least one verifier is registered for the extension
        """
        return len(self.get_verifiers(file_path)) > 0

    def _normalize_extension(self, extension: str) -> str:
        """Normalize extension to lowercase with leading dot."""
        ext = extension.lower()
        if not ext.startswith("."):
            ext = "." + ext
        return ext

    def _get_extension(self, file_path: str) -> str:
        """Extract file extension from path."""
        if "." in file_path:
            return "." + file_path.rsplit(".", 1)[-1].lower()
        return ""


# Singleton instance
_verifier_registry: Optional[VerifierRegistry] = None


def get_verifier_registry() -> VerifierRegistry:
    """Get singleton verifier registry instance."""
    global _verifier_registry
    if _verifier_registry is None:
        _verifier_registry = VerifierRegistry()
    return _verifier_registry
