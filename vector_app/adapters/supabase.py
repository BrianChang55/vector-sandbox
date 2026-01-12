"""
Supabase Adapter Implementation

Provides metadata mode (schema introspection) and user mode (data queries with user JWT).
Uses PostgREST REST API directly for maximum compatibility.
"""

import logging
from typing import List, Dict, Any, Optional
import requests
import json

from .base import (
    BackendAdapter,
    AdapterContext,
    UserContext,
    Resource,
    ResourceField,
    Relationship,
    ResourceSchema,
)

logger = logging.getLogger(__name__)


class SupabaseAdapter:
    """
    Supabase adapter implementation.

    Metadata mode: Uses service role key for schema introspection
    User mode: Uses user JWT for data queries (RLS enforced)
    """

    @property
    def type(self) -> str:
        return "supabase"

    def list_resources(self, ctx: AdapterContext) -> List[Resource]:
        """
        List all tables and views in the Supabase database.
        Uses metadata mode (service role key).

        Note: For production, use Supabase Management API or query information_schema.
        This is a simplified V1 implementation.
        """
        try:
            # For V1, we'll return an empty list and let the discovery process
            # populate resources from the database connection
            # In production, use Supabase Management API:
            # from supabase import create_client, Client
            # supabase: Client = create_client(url, service_role_key)
            # tables = supabase.table('information_schema.tables').select('*').execute()

            logger.info("list_resources: Using placeholder - discovery should populate resources")
            return []

        except Exception as e:
            logger.error(f"Error in list_resources: {e}")
            raise

    def get_resource_schema(self, resource_id: str, ctx: AdapterContext) -> ResourceSchema:
        """
        Get schema for a specific resource (table/view).
        Uses metadata mode.

        Note: For production, use Supabase Management API or query information_schema.
        This is a simplified V1 implementation.
        """
        try:
            schema_name, table_name = (
                resource_id.split(".", 1) if "." in resource_id else ("public", resource_id)
            )

            # For V1, return a basic schema structure
            # In production, query information_schema.columns to get actual schema
            # Example query:
            # SELECT column_name, data_type, is_nullable, column_default
            # FROM information_schema.columns
            # WHERE table_schema = schema_name AND table_name = table_name

            logger.info(f"get_resource_schema: Using placeholder for {resource_id}")

            fields = []
            relationships = []
            capabilities = ["read"]  # Default capabilities

            return ResourceSchema(fields=fields, relationships=relationships, capabilities=capabilities)

        except Exception as e:
            logger.error(f"Error in get_resource_schema: {e}")
            raise

    def query(self, resource_id: str, query_spec: Dict[str, Any], user_ctx: UserContext) -> Dict[str, Any]:
        """
        Execute a read query on a resource.
        Uses user mode (user JWT only, RLS enforced).

        Args:
            resource_id: Resource identifier (e.g., "public.users")
            query_spec: Query specification with select, filters, orderBy, limit, offset
            user_ctx: User context with JWT

        Returns:
            Query result dict with data array
        """
        try:
            schema_name, table_name = (
                resource_id.split(".", 1) if "." in resource_id else ("public", resource_id)
            )

            # Build PostgREST URL
            base_url = user_ctx.backend_url.rstrip("/")
            table_url = f"{base_url}/rest/v1/{table_name}"

            # Headers with user JWT
            headers = {
                "apikey": user_ctx.anon_key or "",
                "Authorization": f"Bearer {user_ctx.user_jwt}",
                "Content-Type": "application/json",
                "Prefer": "return=representation",
            }

            # Build query parameters
            params = {}

            # Select fields
            select_fields = query_spec.get("select", ["*"])
            if select_fields != ["*"]:
                params["select"] = ",".join(select_fields)

            # Apply filters (PostgREST query syntax)
            filters = query_spec.get("filters", [])
            for filter_item in filters:
                field = filter_item.get("field")
                op = filter_item.get("op", "eq")
                value = filter_item.get("value")

                # PostgREST uses query parameters like: field=eq.value
                param_key = f"{field}"
                if op == "eq":
                    params[param_key] = f"eq.{value}"
                elif op == "neq":
                    params[param_key] = f"neq.{value}"
                elif op == "gt":
                    params[param_key] = f"gt.{value}"
                elif op == "gte":
                    params[param_key] = f"gte.{value}"
                elif op == "lt":
                    params[param_key] = f"lt.{value}"
                elif op == "lte":
                    params[param_key] = f"lte.{value}"
                elif op == "like":
                    params[param_key] = f"like.*{value}*"
                elif op == "ilike":
                    params[param_key] = f"ilike.*{value}*"
                elif op == "in":
                    # For 'in', use comma-separated values
                    value_str = ",".join(str(v) for v in value) if isinstance(value, list) else str(value)
                    params[param_key] = f"in.({value_str})"
                elif op == "is":
                    params[param_key] = f"is.{value}"
                else:
                    logger.warning(f"Unsupported filter operator: {op}, using eq")
                    params[param_key] = f"eq.{value}"

            # Apply ordering
            order_by = query_spec.get("orderBy", [])
            if order_by:
                order_parts = []
                for order_item in order_by:
                    field = order_item.get("field")
                    direction = order_item.get("dir", "asc")
                    order_parts.append(f"{field}.{direction}")
                params["order"] = ",".join(order_parts)

            # Apply pagination
            limit = query_spec.get("limit", 50)
            offset = query_spec.get("offset", 0)
            params["limit"] = str(limit)
            params["offset"] = str(offset)

            # Execute GET request
            response = requests.get(table_url, headers=headers, params=params, timeout=30)
            response.raise_for_status()

            data = response.json() if response.content else []

            return {
                "data": data if isinstance(data, list) else [data],
                "count": len(data) if isinstance(data, list) else 1,
            }

        except requests.exceptions.HTTPError as e:
            logger.error(
                f"HTTP error in query for {resource_id}: {e.response.status_code} - {e.response.text}"
            )
            raise
        except Exception as e:
            logger.error(f"Error in query for {resource_id}: {e}")
            raise

    def execute_action(
        self, action_def: Dict[str, Any], args: Dict[str, Any], user_ctx: UserContext
    ) -> Dict[str, Any]:
        """
        Execute an allowlisted action.
        Uses user mode (user JWT only, RLS enforced).

        Supported action types:
        - update_one: Update a single row
        - soft_delete: Soft delete a row (update deleted_at)
        - rpc: Call a PostgreSQL function

        Args:
            action_def: Action definition from registry
            args: Action arguments (validated against input_schema)
            user_ctx: User context with JWT

        Returns:
            Action result dict
        """
        try:
            action_type = action_def.get("action_type")
            resource_id = action_def.get("resource_id")
            schema_name, table_name = (
                resource_id.split(".", 1) if "." in resource_id else ("public", resource_id)
            )

            base_url = user_ctx.backend_url.rstrip("/")
            headers = {
                "apikey": user_ctx.anon_key or "",
                "Authorization": f"Bearer {user_ctx.user_jwt}",
                "Content-Type": "application/json",
                "Prefer": "return=representation",
            }

            if action_type == "update_one":
                # Update a single row
                primary_key_field = action_def.get("metadata", {}).get("primary_key_field", "id")
                primary_key_value = args.get(primary_key_field)

                if not primary_key_value:
                    raise ValueError(f"Primary key field {primary_key_field} is required")

                # Get updateable fields from metadata
                updateable_fields = action_def.get("metadata", {}).get("updateable_fields", [])
                update_data = {
                    k: v for k, v in args.items() if k in updateable_fields and k != primary_key_field
                }

                # PATCH request to update row
                table_url = f"{base_url}/rest/v1/{table_name}"
                params = {primary_key_field: f"eq.{primary_key_value}"}

                response = requests.patch(
                    table_url, headers=headers, params=params, json=update_data, timeout=30
                )
                response.raise_for_status()

                data = response.json() if response.content else []
                result_data = (
                    data[0]
                    if isinstance(data, list) and data
                    else (data if not isinstance(data, list) else None)
                )

                return {
                    "success": True,
                    "data": result_data,
                }

            elif action_type == "soft_delete":
                # Soft delete (update deleted_at field)
                primary_key_field = action_def.get("metadata", {}).get("primary_key_field", "id")
                primary_key_value = args.get(primary_key_field)

                if not primary_key_value:
                    raise ValueError(f"Primary key field {primary_key_field} is required")

                from datetime import datetime

                update_data = {"deleted_at": datetime.utcnow().isoformat()}

                table_url = f"{base_url}/rest/v1/{table_name}"
                params = {primary_key_field: f"eq.{primary_key_value}"}

                response = requests.patch(
                    table_url, headers=headers, params=params, json=update_data, timeout=30
                )
                response.raise_for_status()

                data = response.json() if response.content else []
                result_data = (
                    data[0]
                    if isinstance(data, list) and data
                    else (data if not isinstance(data, list) else None)
                )

                return {
                    "success": True,
                    "data": result_data,
                }

            elif action_type == "rpc":
                # Call a PostgreSQL function
                function_name = action_def.get("metadata", {}).get("function_name")
                if not function_name:
                    raise ValueError("function_name is required for RPC actions")

                rpc_url = f"{base_url}/rest/v1/rpc/{function_name}"

                response = requests.post(rpc_url, headers=headers, json=args, timeout=30)
                response.raise_for_status()

                data = response.json() if response.content else None

                return {
                    "success": True,
                    "data": data,
                }
            else:
                raise ValueError(f"Unsupported action type: {action_type}")

        except requests.exceptions.HTTPError as e:
            logger.error(
                f"HTTP error executing action {action_def.get('action_id')}: {e.response.status_code} - {e.response.text}"
            )
            raise
        except Exception as e:
            logger.error(f"Error executing action {action_def.get('action_id')}: {e}")
            raise

    def get_capabilities(self, ctx: AdapterContext) -> Dict[str, Any]:
        """
        Get adapter capabilities and metadata.
        Uses metadata mode.
        """
        return {
            "adapter_type": "supabase",
            "capabilities": [
                "read",
                "update_one",
                "soft_delete",
                "rpc",
            ],
            "supported_query_operators": [
                "eq",
                "neq",
                "gt",
                "gte",
                "lt",
                "lte",
                "like",
                "ilike",
                "in",
                "is",
            ],
        }
