"""
Intent Handlers Package

Specialized handlers for different user intents:
- GenerateHandler: Full app generation from scratch
- EditHandler: Surgical code modifications
- FeatureHandler: Adding new features
- SchemaHandler: Data model changes
- FixHandler: Bug fixes
"""

from .base_handler import BaseHandler
from .generate_handler import GenerateHandler
from .edit_handler import EditHandler
from .feature_handler import FeatureHandler
from .schema_handler import SchemaHandler
from .verification_mixin import VerificationMixin

__all__ = [
    'BaseHandler',
    'GenerateHandler',
    'EditHandler',
    'FeatureHandler',
    'SchemaHandler',
    'VerificationMixin',
]

