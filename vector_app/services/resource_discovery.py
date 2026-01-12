"""
Resource discovery service
"""

import logging
from typing import List, Dict, Any
from ..models import BackendConnection, ResourceRegistryEntry
from ..adapters.supabase import SupabaseAdapter
from ..adapters.postgresql import PostgreSQLAdapter
from ..adapters.mysql import MySQLAdapter
from ..adapters.base import AdapterContext

logger = logging.getLogger(__name__)


class ResourceDiscoveryService:
    """Service for discovering resources from backends."""

    def __init__(self):
        self.adapters = {
            "supabase": SupabaseAdapter(),
            "postgresql": PostgreSQLAdapter(),
            "mysql": MySQLAdapter(),
        }

    def _get_adapter_context(self, backend_connection: BackendConnection) -> AdapterContext:
        """Create adapter context from backend configuration."""
        config = backend_connection.get_config()

        if backend_connection.adapter_type == BackendConnection.ADAPTER_SUPABASE:
            return AdapterContext(
                backend_url=config.get("supabase_url", ""),
                service_role_key=config.get("service_role_key"),
                anon_key=config.get("anon_key"),
            )
        elif backend_connection.adapter_type in [
            BackendConnection.ADAPTER_POSTGRESQL,
            BackendConnection.ADAPTER_MYSQL,
        ]:
            return AdapterContext(
                host=config.get("host", ""),
                port=config.get("port"),
                database=config.get("database", ""),
                username=config.get("username", ""),
                password=config.get("password", ""),
                ssl_mode=config.get("ssl_mode", "disable"),
            )
        return AdapterContext()

    def discover_resources(self, backend_connection: BackendConnection) -> List[ResourceRegistryEntry]:
        """
        Discover resources from a backend connection and create/update registry entries.

        Args:
            backend_connection: BackendConnection instance

        Returns:
            List of created/updated ResourceRegistryEntry instances
        """
        adapter = self.adapters.get(backend_connection.adapter_type)
        if not adapter:
            raise ValueError(f"Unknown adapter type: {backend_connection.adapter_type}")

        # Get adapter context
        ctx = self._get_adapter_context(backend_connection)

        logger.info(
            f"Discovering resources for {backend_connection.display_name} ({backend_connection.adapter_type})"
        )

        # List resources from the backend
        resources = adapter.list_resources(ctx)

        created_entries = []
        for resource in resources:
            # Get schema for each resource
            try:
                schema = adapter.get_resource_schema(resource.id, ctx)
                schema_json = {
                    "fields": [
                        {
                            "name": f.name,
                            "type": f.type,
                            "nullable": f.nullable,
                            "primaryKey": f.primary_key,
                        }
                        for f in schema.fields
                    ],
                    "relationships": [
                        {
                            "fromField": r.from_field,
                            "toResourceId": r.to_resource_id,
                            "toField": r.to_field,
                            "type": r.type,
                        }
                        for r in schema.relationships
                    ],
                    "capabilities": schema.capabilities,
                }

                # Extract field names for exposed_fields_json (all fields by default)
                exposed_fields = [f.name for f in schema.fields]

            except Exception as e:
                logger.warning(f"Could not get schema for {resource.id}: {e}")
                schema_json = {"fields": [], "relationships": [], "capabilities": ["read"]}
                exposed_fields = []

            # Create or update registry entry
            entry, created = ResourceRegistryEntry.objects.update_or_create(
                backend_connection=backend_connection,
                resource_id=resource.id,
                defaults={
                    "organization": backend_connection.organization,
                    "resource_name": resource.name,
                    "schema_json": schema_json,
                    "exposed_fields_json": exposed_fields,
                    # Don't override enabled flag if already set
                },
            )

            created_entries.append(entry)
            logger.info(f"{'Created' if created else 'Updated'} registry entry: {resource.id}")

        logger.info(f"Discovery complete: {len(created_entries)} resources found")
        return created_entries

    def discover_resource_schema(
        self, backend_connection: BackendConnection, resource_id: str
    ) -> Dict[str, Any]:
        """
        Discover schema for a specific resource.

        Args:
            backend_connection: BackendConnection instance
            resource_id: Resource identifier

        Returns:
            ResourceSchema as dictionary
        """
        adapter = self.adapters.get(backend_connection.adapter_type)
        if not adapter:
            raise ValueError(f"Unknown adapter type: {backend_connection.adapter_type}")

        # Get adapter context
        ctx = self._get_adapter_context(backend_connection)

        # Get schema
        schema = adapter.get_resource_schema(resource_id, ctx)

        # Convert to dict
        return {
            "fields": [
                {
                    "name": f.name,
                    "type": f.type,
                    "nullable": f.nullable,
                    "primaryKey": f.primary_key,
                }
                for f in schema.fields
            ],
            "relationships": [
                {
                    "fromField": r.from_field,
                    "toResourceId": r.to_resource_id,
                    "toField": r.to_field,
                    "type": r.type,
                }
                for r in schema.relationships
            ],
            "capabilities": schema.capabilities,
        }
