"""
Backend Adapter System

This module provides the adapter architecture for connecting to different backend systems.
All backend communication goes through adapters to ensure security and abstraction.
"""

from .base import BackendAdapter, AdapterContext, UserContext
from .supabase import SupabaseAdapter

__all__ = ['BackendAdapter', 'AdapterContext', 'UserContext', 'SupabaseAdapter']

