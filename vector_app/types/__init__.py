"""
Shared Types for Vector App

Common types used across models, services, and other modules.
These types are placed here to avoid circular imports.
"""

from .verification import VerificationStatus

__all__ = [
    "VerificationStatus",
]
