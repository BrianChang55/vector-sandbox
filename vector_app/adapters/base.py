"""
Base adapter protocol and context classes.
"""

from typing import Protocol, List, Dict, Any, Optional
from dataclasses import dataclass


@dataclass
class AdapterContext:
    """
    Context for adapter operations in metadata mode.
    May use service role keys for introspection.

    For Supabase: backend_url, service_role_key, anon_key
    For PostgreSQL/MySQL: host, port, database, username, password
    """

    # Supabase fields
    backend_url: Optional[str] = None
    service_role_key: Optional[str] = None
    anon_key: Optional[str] = None

    # Direct database connection fields
    host: Optional[str] = None
    port: Optional[int] = None
    database: Optional[str] = None
    username: Optional[str] = None
    password: Optional[str] = None
    ssl_mode: Optional[str] = None


@dataclass
class UserContext:
    """
    Context for adapter operations in user mode.

    For Supabase: must use user JWT (RLS enforced)
    For PostgreSQL/MySQL: uses the same connection as AdapterContext
    """

    # Supabase fields
    backend_url: Optional[str] = None
    user_jwt: Optional[str] = None
    anon_key: Optional[str] = None

    # Direct database connection fields
    host: Optional[str] = None
    port: Optional[int] = None
    database: Optional[str] = None
    username: Optional[str] = None
    password: Optional[str] = None
    ssl_mode: Optional[str] = None


@dataclass
class Resource:
    """Adapter-agnostic resource representation."""

    id: str  # e.g., "public.users"
    name: str
    kind: str  # "table", "view", "collection"
    namespace: Optional[str] = None


@dataclass
class ResourceField:
    """Field definition in a resource schema."""

    name: str
    type: str
    nullable: bool
    primary_key: bool = False


@dataclass
class Relationship:
    """Relationship between resources."""

    from_field: str
    to_resource_id: str
    to_field: str
    type: str  # "one_to_one", "one_to_many", "many_to_many"


@dataclass
class ResourceSchema:
    """Complete schema for a resource."""

    fields: List[ResourceField]
    relationships: List[Relationship]
    capabilities: List[str]  # ["read", "row_secured", "update", "rpc", "soft_delete"]


class BackendAdapter(Protocol):
    """
    Protocol defining the interface for backend adapters.

    All backend communication must go through adapters to ensure:
    - Security (RLS enforcement via user JWT)
    - Abstraction (adapter-agnostic core logic)
    - Safety (no direct SQL/raw queries)
    """

    @property
    def type(self) -> str:
        """Adapter type identifier, e.g., 'supabase'."""
        ...

    def list_resources(self, ctx: AdapterContext) -> List[Resource]:
        """
        List all available resources (tables, views, etc.).
        Uses metadata mode (service role key allowed).
        """
        ...

    def get_resource_schema(self, resource_id: str, ctx: AdapterContext) -> ResourceSchema:
        """
        Get schema for a specific resource.
        Uses metadata mode (service role key allowed).
        """
        ...

    def query(self, resource_id: str, query_spec: Dict[str, Any], user_ctx: UserContext) -> Dict[str, Any]:
        """
        Execute a read query on a resource.
        Uses user mode (user JWT only, RLS enforced).

        Args:
            resource_id: Resource identifier
            query_spec: Query specification dict with select, filters, orderBy, limit, offset
            user_ctx: User context with JWT

        Returns:
            Query result dict with data array
        """
        ...

    def execute_action(
        self, action_def: Dict[str, Any], args: Dict[str, Any], user_ctx: UserContext
    ) -> Dict[str, Any]:
        """
        Execute an allowlisted action.
        Uses user mode (user JWT only, RLS enforced).

        Args:
            action_def: Action definition from registry
            args: Action arguments (validated against input_schema)
            user_ctx: User context with JWT

        Returns:
            Action result dict
        """
        ...

    def get_capabilities(self, ctx: AdapterContext) -> Dict[str, Any]:
        """
        Get adapter capabilities and metadata.
        Uses metadata mode.
        """
        ...
