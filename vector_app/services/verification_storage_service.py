"""
Verification Storage Service

Persists verification results to the database for audit trail and analysis.
"""

import logging
from typing import Dict, List, Optional

from chat.models import FileVerificationRecord, VerificationAttemptRecord
from vector_app.models import CodeGenerationJob
from vector_app.services.types import FileVerificationResult

logger = logging.getLogger(__name__)


class VerificationStorageService:
    """
    Service for storing verification results in the database.

    Creates FileVerificationRecord and associated VerificationAttemptRecord
    entries for complete audit trail.

    Usage:
        storage = VerificationStorageService()
        record = storage.save_verification_result(result, job=code_gen_job)
    """

    def save_verification_result(
        self,
        result: FileVerificationResult,
        job: Optional[CodeGenerationJob] = None,
    ) -> FileVerificationRecord:
        """
        Save a verification result to the database.

        Args:
            result: The FileVerificationResult from verification
            job: Optional CodeGenerationJob to link the verification to

        Returns:
            The created FileVerificationRecord

        Creates:
            - One FileVerificationRecord with final status
            - One VerificationAttemptRecord per attempt in result.attempts
        """
        # Create the main verification record
        record = FileVerificationRecord.objects.create( # pylint: disable=no-member
            code_generation_job=job,
            file_path=result.file_path,
            file_type=result.file_type,
            status=result.status.value,
            verifier_name=result.verifier_name,
            final_content=result.final_content,
            error_message=result.error_message,
            started_at=result.started_at,
            completed_at=result.completed_at,
        )

        # Create attempt records
        for attempt in result.attempts:
            VerificationAttemptRecord.objects.create( # pylint: disable=no-member
                verification_record=record,
                attempt_number=attempt.attempt_number,
                content=attempt.content,
                status=attempt.status.value,
                error_message=attempt.error_message,
                timestamp=attempt.timestamp,
            )

        logger.info(
            "Saved verification result for %s: %s (%d attempts)",
            result.file_path,
            result.status.value,
            len(result.attempts),
        )

        return record

    def save_verification_results(
        self,
        results: Dict[str, FileVerificationResult],
        job: Optional[CodeGenerationJob] = None,
    ) -> List[FileVerificationRecord]:
        """
        Save multiple verification results (e.g., from get_verification_results()).

        Args:
            results: Dict of file_path -> FileVerificationResult
            job: Optional CodeGenerationJob to link all verifications to

        Returns:
            List of created FileVerificationRecord objects
        """
        records = []
        for result in results.values():
            record = self.save_verification_result(result, job=job)
            records.append(record)
        return records


# Factory getter for consistency with other services
_storage_service: Optional[VerificationStorageService] = None


def get_verification_storage_service() -> VerificationStorageService:
    """Get or create the verification storage service instance."""
    global _storage_service # pylint: disable=global-statement
    if _storage_service is None:
        _storage_service = VerificationStorageService()
    return _storage_service
