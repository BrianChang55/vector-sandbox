"""
Data Store Context Generator for LLM Consumption.

DEPRECATED: This module is a backwards-compatibility wrapper.
All new code should import directly from vector_app.services.datastore instead.
"""
from .datastore import (
    build_data_store_context,
    build_data_store_context_for_version,
    get_table_summary,
)

__all__ = [
    'build_data_store_context',
    'build_data_store_context_for_version',
    'get_table_summary',
]
