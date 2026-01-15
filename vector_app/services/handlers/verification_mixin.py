"""
Verification Mixin for Handlers

Provides automatic file verification for generated code.
"""

import logging
from datetime import datetime
from typing import Dict, List, Optional

from vector_app.services.types import FileChange, FileVerificationResult
from vector_app.services.verifiers import get_verifier_registry
from vector_app.types import VerificationStatus

logger = logging.getLogger(__name__)


class VerificationMixin:
    """
    Mixin that provides file verification capabilities to handlers.

    Add to handlers to enable automatic verification of generated files.
    Tracks verification results for all files processed during execution.

    Usage:
        class MyHandler(VerificationMixin, BaseHandler):
            def execute(self, ...):
                # Generate file
                file_change = FileChange(...)

                # Verify it
                result = self.verify_file(file_change)

                # Check if blocking failure
                if result.status == VerificationStatus.FAILED and self.has_blocking_failures():
                    # Phase 7 will handle retry
                    pass
    """

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._verification_results: Dict[str, FileVerificationResult] = {}

    def verify_file(self, file_change: FileChange) -> FileVerificationResult:
        """
        Verify a single FileChange using appropriate verifiers.

        Args:
            file_change: The file to verify

        Returns:
            FileVerificationResult with verification status and any errors
        """
        registry = get_verifier_registry()
        verifiers = registry.get_verifiers(file_change.path)

        # No verifiers registered - skip verification
        if not verifiers:
            result = FileVerificationResult(
                file_path=file_change.path,
                file_type=self._get_file_extension(file_change.path),
                status=VerificationStatus.SKIPPED,
                verifier_name=None,
                started_at=datetime.now(),
                completed_at=datetime.now(),
            )
            self._verification_results[file_change.path] = result
            logger.debug("No verifiers for %s, skipping verification", file_change.path)
            return result

        # Run all verifiers (currently just one per extension, but supports multiple)
        started_at = datetime.now()
        combined_errors: List[str] = []
        final_status = VerificationStatus.PASSED
        verifier_names: List[str] = []

        for verifier in verifiers:
            verifier_names.append(verifier.name)

            try:
                verifier_result = verifier(
                    content=file_change.content,
                    file_path=file_change.path,
                )

                # Track failures
                if verifier_result.status == VerificationStatus.FAILED:
                    if verifier.is_blocking:
                        final_status = VerificationStatus.FAILED
                    if verifier_result.error_message:
                        combined_errors.append(verifier_result.error_message)

                elif verifier_result.status == VerificationStatus.ERROR:
                    # Verifier error (e.g., timeout) - treat as pass per project decision
                    logger.warning(
                        "Verifier %s encountered error for %s: %s",
                        verifier.name,
                        file_change.path,
                        verifier_result.error_message,
                    )

            except Exception as e:
                # Unexpected verifier exception - treat as pass, log error
                logger.error(
                    "Verifier %s raised exception for %s: %s",
                    verifier.name,
                    file_change.path,
                    str(e),
                )

        completed_at = datetime.now()

        result = FileVerificationResult(
            file_path=file_change.path,
            file_type=self._get_file_extension(file_change.path),
            status=final_status,
            verifier_name=", ".join(verifier_names) if verifier_names else None,
            final_content=file_change.content,
            error_message="\n".join(combined_errors) if combined_errors else None,
            started_at=started_at,
            completed_at=completed_at,
        )

        self._verification_results[file_change.path] = result

        if final_status == VerificationStatus.PASSED:
            logger.info("Verification passed for %s", file_change.path)
        else:
            logger.warning(
                "Verification failed for %s: %s",
                file_change.path,
                result.error_message,
            )

        return result

    def get_verification_results(self) -> Dict[str, FileVerificationResult]:
        """Get all verification results from this execution."""
        return self._verification_results.copy()

    def get_verification_result(self, file_path: str) -> Optional[FileVerificationResult]:
        """Get verification result for a specific file."""
        return self._verification_results.get(file_path)

    def has_blocking_failures(self) -> bool:
        """Check if any verification results have blocking failures."""
        for result in self._verification_results.values():
            if result.status == VerificationStatus.FAILED:
                # Check if the verifier was blocking
                registry = get_verifier_registry()
                verifiers = registry.get_verifiers(result.file_path)
                for verifier in verifiers:
                    if verifier.is_blocking:
                        return True
        return False

    def clear_verification_results(self) -> None:
        """Clear all verification results (useful between retries)."""
        self._verification_results.clear()

    def _get_file_extension(self, file_path: str) -> str:
        """Extract file extension from path."""
        if "." in file_path:
            return "." + file_path.rsplit(".", 1)[-1]
        return ""
