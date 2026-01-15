"""
Verification Mixin for Handlers

Provides automatic file verification for generated code.
"""

import logging
from datetime import datetime
from typing import Callable, Dict, List, Optional, Tuple

from vector_app.services.types import (
    AgentEvent,
    FileChange,
    FileVerificationResult,
    VerificationAttempt,
)
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

    def verify_and_retry(
        self,
        file_change: FileChange,
        regenerate_callback: Callable[[FileChange, str], FileChange],
        max_attempts: int = 3,
    ) -> Tuple[FileChange, FileVerificationResult]:
        """
        Verify a file and retry with regeneration if blocking verification fails.

        Args:
            file_change: The file to verify
            regenerate_callback: Function that takes (file_change, error_message) and returns
                               a new FileChange with regenerated content. This callback
                               should call the LLM with error context (Phase 8 provides prompts).
            max_attempts: Maximum verification attempts (default 3 per project requirements)

        Returns:
            Tuple of (final_file_change, final_verification_result)
            - If verification passes, returns the successful file and result
            - If all retries exhausted, returns the last attempt's file and result

        Usage:
            def my_regenerate(file: FileChange, error: str) -> FileChange:
                # Call LLM with error context to regenerate
                new_content = llm_call(original_prompt + f"\\nFix this error: {error}")
                return FileChange(..., content=new_content)

            final_file, result = self.verify_and_retry(file_change, my_regenerate)
        """
        current_file = file_change
        final_result: Optional[FileVerificationResult] = None
        all_attempts: List[VerificationAttempt] = []

        for attempt_num in range(1, max_attempts + 1):
            # Clear previous verification results before each attempt
            # This is safe because we're tracking attempts in all_attempts list
            self.clear_verification_results()

            # Verify the current file
            result = self.verify_file(current_file)

            # Record this attempt
            attempt = VerificationAttempt(
                attempt_number=attempt_num,
                content=current_file.content,
                status=result.status,
                error_message=result.error_message,
                timestamp=datetime.now(),
            )
            all_attempts.append(attempt)

            logger.info(
                "Verification attempt %d/%d for %s: %s",
                attempt_num,
                max_attempts,
                current_file.path,
                result.status.value,
            )

            # Check if we should continue
            if result.status != VerificationStatus.FAILED:
                # Passed, skipped, or error (treated as pass) - we're done
                final_result = result
                break

            # Check if the failure is blocking
            registry = get_verifier_registry()
            verifiers = registry.get_verifiers(current_file.path)
            is_blocking = any(v.is_blocking for v in verifiers)

            if not is_blocking:
                # Advisory verifier failed - don't retry, just track
                logger.info(
                    "Advisory verification failed for %s, not retrying",
                    current_file.path,
                )
                final_result = result
                break

            # Blocking failure - check if we have retries left
            if attempt_num >= max_attempts:
                logger.warning(
                    "All %d verification attempts exhausted for %s",
                    max_attempts,
                    current_file.path,
                )
                final_result = result
                break

            # Call regenerate callback for retry
            logger.info(
                "Blocking verification failed for %s, calling regenerate callback",
                current_file.path,
            )
            try:
                current_file = regenerate_callback(current_file, result.error_message or "")
            except Exception as e:
                logger.error(
                    "Regenerate callback failed for %s: %s",
                    current_file.path,
                    str(e),
                )
                final_result = result
                break

        # Update final result with all attempts
        if final_result is not None:
            final_result.attempts = all_attempts
            final_result.final_content = current_file.content

            # Update the stored result
            self._verification_results[current_file.path] = final_result

        return current_file, final_result

    def _get_file_extension(self, file_path: str) -> str:
        """Extract file extension from path."""
        if "." in file_path:
            return "." + file_path.rsplit(".", 1)[-1]
        return ""

    # Verification Event Emission Methods
    # These return AgentEvent objects - handlers yield them at appropriate times

    def emit_verification_started(self, file_path: str, verifier_name: str) -> AgentEvent:
        """
        Emit event when verification starts for a file.

        Usage in handler:
            yield self.emit_verification_started(file.path, "typescript")
            result = self.verify_file(file)
        """
        return AgentEvent(
            "verification_started",
            {
                "file_path": file_path,
                "verifier": verifier_name,
            },
        )

    def emit_verification_passed(self, file_path: str, verifier_name: str) -> AgentEvent:
        """
        Emit event when verification passes for a file.

        Usage in handler:
            result = self.verify_file(file)
            if result.status == VerificationStatus.PASSED:
                yield self.emit_verification_passed(file.path, result.verifier_name)
        """
        return AgentEvent(
            "verification_passed",
            {
                "file_path": file_path,
                "verifier": verifier_name,
            },
        )

    def emit_verification_failed(
        self,
        file_path: str,
        verifier_name: str,
        error_message: str,
        is_blocking: bool = True,
    ) -> AgentEvent:
        """
        Emit event when verification fails for a file.

        Usage in handler:
            result = self.verify_file(file)
            if result.status == VerificationStatus.FAILED:
                yield self.emit_verification_failed(
                    file.path, result.verifier_name, result.error_message, is_blocking=True
                )
        """
        return AgentEvent(
            "verification_failed",
            {
                "file_path": file_path,
                "verifier": verifier_name,
                "error_message": error_message,
                "is_blocking": is_blocking,
            },
        )

    def emit_verification_retry_started(
        self,
        file_path: str,
        attempt_number: int,
        max_attempts: int,
        previous_error: str,
    ) -> AgentEvent:
        """
        Emit event when a verification retry is starting.

        Usage in handler:
            yield self.emit_verification_retry_started(
                file.path, attempt_num, max_attempts, error_message
            )
        """
        return AgentEvent(
            "verification_retry_started",
            {
                "file_path": file_path,
                "attempt_number": attempt_number,
                "max_attempts": max_attempts,
                "previous_error": previous_error,
            },
        )

    def emit_verification_skipped(self, file_path: str, reason: str = "no verifier") -> AgentEvent:
        """
        Emit event when verification is skipped for a file.

        Usage in handler:
            result = self.verify_file(file)
            if result.status == VerificationStatus.SKIPPED:
                yield self.emit_verification_skipped(file.path)
        """
        return AgentEvent(
            "verification_skipped",
            {
                "file_path": file_path,
                "reason": reason,
            },
        )
