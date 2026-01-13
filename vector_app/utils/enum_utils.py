"""
Utility functions for working with enums.
"""
from typing import TypeVar, Type
from enum import StrEnum

T = TypeVar('T', bound=StrEnum)


def safe_str_enum(val: str, default: StrEnum, enum_type: Type[T]) -> T:
    """
    Safely convert a string value to a StrEnum, returning default if conversion fails.
    
    Args:
        val: String value to convert to enum
        default: Default enum value to return if conversion fails
        enum_type: The StrEnum type to convert to
        
    Returns:
        Enum value of type enum_type, or default if val doesn't match any enum value
        
    Example:
        >>> from vector_app.services.planning_service import PlanOperationType
        >>> safe_str_enum("generate", PlanOperationType.GENERATE, PlanOperationType)
        PlanOperationType.GENERATE
        >>> safe_str_enum("invalid", PlanOperationType.GENERATE, PlanOperationType)
        PlanOperationType.GENERATE
    """
    if not val:
        return default
    
    val = val.lower().strip()
    try:
        return enum_type(val)
    except (ValueError, KeyError):
        return default
