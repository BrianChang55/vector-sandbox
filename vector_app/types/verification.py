"""
Verification Status Types

Types related to file verification status.
"""

from enum import Enum


class VerificationStatus(str, Enum):
    """Status of a file verification."""

    PENDING = "pending"  # Not yet verified
    PASSED = "passed"  # Verification succeeded
    FAILED = "failed"  # Verification failed (code has errors)
    SKIPPED = "skipped"  # No verifier available for file type
    ERROR = "error"  # Verifier service error (tsc not found, timeout)
