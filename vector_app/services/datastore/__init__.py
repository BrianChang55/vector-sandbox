"""
DataStore Module

Centralized module for all database schema handling, validation, and operations.
"""

from .schema import (
    SYSTEM_COLUMNS,
    RESERVED_FIELD_NAMES,
    COLUMN_TYPES,
    COLUMN_OPTIONS,
    get_system_columns,
    is_reserved_field,
    get_column_type_spec,
    apply_column_defaults,
)

from .validator import SchemaValidator

from .parser import TableDefinitionParser

from .context_builder import DataStoreContextBuilder

from .fetcher import (
    build_data_store_context,
    build_data_store_context_for_version,
    get_table_summary,
)

from .table_creator import create_tables_from_definitions

__all__ = [
    # Schema constants and utilities
    'SYSTEM_COLUMNS',
    'RESERVED_FIELD_NAMES',
    'COLUMN_TYPES',
    'COLUMN_OPTIONS',
    'get_system_columns',
    'is_reserved_field',
    'get_column_type_spec',
    'apply_column_defaults',
    # Validation
    'SchemaValidator',
    # Parsing
    'TableDefinitionParser',
    # Context building
    'DataStoreContextBuilder',
    # Fetching/formatting
    'build_data_store_context',
    'build_data_store_context_for_version',
    'get_table_summary',
    # Table creation
    'create_tables_from_definitions',
]
