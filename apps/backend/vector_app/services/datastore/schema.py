"""
Schema Definitions

Single source of truth for all database schema constants, types, and defaults.
"""
from typing import Dict, List, Any, Set


# System-managed columns that are auto-added to every table
SYSTEM_COLUMNS = [
    {
        'name': 'id',
        'type': 'uuid',
        'primary_key': True,
        'auto_generate': True,
        'nullable': False,
    },
    {
        'name': 'created_at',
        'type': 'datetime',
        'auto_now_add': True,
        'nullable': False,
    },
    {
        'name': 'updated_at',
        'type': 'datetime',
        'auto_now': True,
        'nullable': False,
    },
]

# Reserved field names that users cannot define
RESERVED_FIELD_NAMES: Set[str] = {'id', 'created_at', 'updated_at'}


# Column type specifications
COLUMN_TYPES: Dict[str, Dict[str, Any]] = {
    'uuid': {
        'description': 'UUID identifier',
        'python_type': str,
        'required_fields': [],
        'optional_fields': {},
    },
    'string': {
        'description': 'Short text (max 255 chars by default)',
        'python_type': str,
        'required_fields': [],
        'optional_fields': {'max_length': 255},
    },
    'text': {
        'description': 'Long text (unlimited)',
        'python_type': str,
        'required_fields': [],
        'optional_fields': {},
    },
    'integer': {
        'description': 'Whole numbers',
        'python_type': int,
        'required_fields': [],
        'optional_fields': {'min': None, 'max': None},
    },
    'float': {
        'description': 'Decimal numbers',
        'python_type': float,
        'required_fields': [],
        'optional_fields': {'min': None, 'max': None},
    },
    'boolean': {
        'description': 'True/false',
        'python_type': bool,
        'required_fields': [],
        'optional_fields': {},
    },
    'datetime': {
        'description': 'Date and time with timezone',
        'python_type': str,  # ISO format string
        'required_fields': [],
        'optional_fields': {},
    },
    'date': {
        'description': 'Date only',
        'python_type': str,  # ISO format string
        'required_fields': [],
        'optional_fields': {},
    },
    'enum': {
        'description': 'Fixed set of values',
        'python_type': str,
        'required_fields': ['enum_values'],
        'optional_fields': {},
    },
    'json': {
        'description': 'Arbitrary JSON data',
        'python_type': (dict, list),
        'required_fields': [],
        'optional_fields': {},
    },
    'reference': {
        'description': 'Foreign key to another table',
        'python_type': str,  # UUID string
        'required_fields': ['reference_table'],
        'optional_fields': {'reference_column': 'id'},  # Defaults to 'id'
    },
}


# Column options that apply to all types
COLUMN_OPTIONS = {
    'nullable': {
        'type': bool,
        'default': True,
        'description': 'Allow null values',
    },
    'unique': {
        'type': bool,
        'default': False,
        'description': 'Enforce unique values',
    },
    'default': {
        'type': Any,
        'default': None,
        'description': 'Default value for new rows',
    },
    'primary_key': {
        'type': bool,
        'default': False,
        'description': 'Mark as primary key',
    },
    'auto_generate': {
        'type': bool,
        'default': False,
        'description': 'Auto-generate UUID values',
    },
    'auto_now_add': {
        'type': bool,
        'default': False,
        'description': 'Set to current datetime on create',
    },
    'auto_now': {
        'type': bool,
        'default': False,
        'description': 'Update to current datetime on every save',
    },
    'max_length': {
        'type': int,
        'default': None,
        'description': 'Maximum string length',
    },
    'enum_values': {
        'type': list,
        'default': None,
        'description': 'Allowed values for enum type',
    },
    'reference_table': {
        'type': str,
        'default': None,
        'description': 'Target table for reference type',
    },
    'reference_column': {
        'type': str,
        'default': 'id',
        'description': 'Target column in referenced table (defaults to \'id\')',
    },
}


def get_system_columns() -> List[Dict[str, Any]]:
    """
    Get a copy of system columns that are auto-added to every table.

    Returns:
        List of column definitions for id, created_at, updated_at
    """
    import copy
    return copy.deepcopy(SYSTEM_COLUMNS)


def is_reserved_field(field_name: str) -> bool:
    """Check if a field name is reserved (system-managed)."""
    return field_name.lower() in RESERVED_FIELD_NAMES


def get_column_type_spec(col_type: str) -> Dict[str, Any]:
    """
    Get the specification for a column type.

    Args:
        col_type: The column type name

    Returns:
        Column type specification dict

    Raises:
        ValueError: If column type is not supported
    """
    if col_type not in COLUMN_TYPES:
        raise ValueError(f"Unsupported column type: {col_type}")
    return COLUMN_TYPES[col_type]


def apply_column_defaults(column: Dict[str, Any]) -> Dict[str, Any]:
    """
    Apply default values for optional fields based on column type.

    Args:
        column: Column definition dict

    Returns:
        Column definition with defaults applied
    """
    import copy
    col = copy.deepcopy(column)
    col_type = col.get('type')

    if col_type and col_type in COLUMN_TYPES:
        spec = COLUMN_TYPES[col_type]
        # Apply defaults for optional fields
        for field, default_value in spec.get('optional_fields', {}).items():
            if field not in col and default_value is not None:
                col[field] = default_value

    return col
