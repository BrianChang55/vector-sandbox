"""
Type definitions for the integrations app.
"""
from enum import StrEnum


class ConnectorExecutionStatus(StrEnum):
    SUCCESS = "success"
    ERROR = "error"
