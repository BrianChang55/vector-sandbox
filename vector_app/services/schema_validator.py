"""
Schema Validator for App Data Store.

DEPRECATED: This module is a backwards-compatibility wrapper.
All new code should import directly from vector_app.services.datastore instead.
"""
from .datastore import SchemaValidator

__all__ = ['SchemaValidator']
