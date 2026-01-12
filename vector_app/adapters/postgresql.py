"""
PostgreSQL Adapter Implementation

Provides direct PostgreSQL connection for schema introspection and data queries.
"""

import logging
from typing import List, Dict, Any, Optional
import psycopg2
from psycopg2.extras import RealDictCursor

from .base import (
    AdapterContext,
    UserContext,
    Resource,
    ResourceField,
    Relationship,
    ResourceSchema,
)

logger = logging.getLogger(__name__)


class PostgreSQLAdapter:
    """
    PostgreSQL adapter implementation.

    Uses direct database connection for both metadata and queries.
    """

    @property
    def type(self) -> str:
        return "postgresql"

    def _get_connection(self, ctx: AdapterContext):
        """Create a database connection from context."""
        return psycopg2.connect(
            host=ctx.host,
            port=ctx.port or 5432,
            database=ctx.database,
            user=ctx.username,
            password=ctx.password,
            connect_timeout=10,
        )

    def test_connection(self, ctx: AdapterContext) -> Dict[str, Any]:
        """Test the database connection."""
        try:
            conn = self._get_connection(ctx)
            cursor = conn.cursor()
            cursor.execute("SELECT version();")
            version = cursor.fetchone()[0]
            cursor.close()
            conn.close()
            return {
                "success": True,
                "message": "Connection successful",
                "version": version,
            }
        except Exception as e:
            logger.error(f"PostgreSQL connection test failed: {e}")
            return {
                "success": False,
                "message": str(e),
            }

    def list_resources(self, ctx: AdapterContext) -> List[Resource]:
        """
        List all tables and views in the PostgreSQL database.
        """
        resources = []
        try:
            conn = self._get_connection(ctx)
            cursor = conn.cursor(cursor_factory=RealDictCursor)

            # Get tables and views from information_schema
            cursor.execute(
                """
                SELECT 
                    table_schema,
                    table_name,
                    table_type
                FROM information_schema.tables
                WHERE table_schema NOT IN ('pg_catalog', 'information_schema')
                AND table_type IN ('BASE TABLE', 'VIEW')
                ORDER BY table_schema, table_name
            """
            )

            for row in cursor.fetchall():
                resource_id = f"{row['table_schema']}.{row['table_name']}"
                kind = "view" if row["table_type"] == "VIEW" else "table"

                resources.append(
                    Resource(
                        id=resource_id,
                        name=row["table_name"],
                        kind=kind,
                        namespace=row["table_schema"],
                    )
                )

            cursor.close()
            conn.close()

            logger.info(f"PostgreSQL list_resources: Found {len(resources)} resources")
            return resources

        except Exception as e:
            logger.error(f"Error in PostgreSQL list_resources: {e}")
            raise

    def get_resource_schema(self, resource_id: str, ctx: AdapterContext) -> ResourceSchema:
        """
        Get schema for a specific resource (table/view).
        """
        try:
            schema_name, table_name = (
                resource_id.split(".", 1) if "." in resource_id else ("public", resource_id)
            )

            conn = self._get_connection(ctx)
            cursor = conn.cursor(cursor_factory=RealDictCursor)

            # Get column information
            cursor.execute(
                """
                SELECT 
                    column_name,
                    data_type,
                    is_nullable,
                    column_default,
                    ordinal_position
                FROM information_schema.columns
                WHERE table_schema = %s AND table_name = %s
                ORDER BY ordinal_position
            """,
                (schema_name, table_name),
            )

            fields = []
            for row in cursor.fetchall():
                is_primary = False
                # Check if this column is part of primary key
                cursor.execute(
                    """
                    SELECT kcu.column_name
                    FROM information_schema.table_constraints tc
                    JOIN information_schema.key_column_usage kcu 
                        ON tc.constraint_name = kcu.constraint_name
                        AND tc.table_schema = kcu.table_schema
                    WHERE tc.constraint_type = 'PRIMARY KEY'
                    AND tc.table_schema = %s
                    AND tc.table_name = %s
                    AND kcu.column_name = %s
                """,
                    (schema_name, table_name, row["column_name"]),
                )

                if cursor.fetchone():
                    is_primary = True

                fields.append(
                    ResourceField(
                        name=row["column_name"],
                        type=self._map_pg_type(row["data_type"]),
                        nullable=row["is_nullable"] == "YES",
                        primary_key=is_primary,
                    )
                )

            # Get foreign key relationships
            cursor.execute(
                """
                SELECT
                    kcu.column_name as from_column,
                    ccu.table_schema as to_schema,
                    ccu.table_name as to_table,
                    ccu.column_name as to_column
                FROM information_schema.table_constraints tc
                JOIN information_schema.key_column_usage kcu
                    ON tc.constraint_name = kcu.constraint_name
                    AND tc.table_schema = kcu.table_schema
                JOIN information_schema.constraint_column_usage ccu
                    ON ccu.constraint_name = tc.constraint_name
                WHERE tc.constraint_type = 'FOREIGN KEY'
                AND tc.table_schema = %s
                AND tc.table_name = %s
            """,
                (schema_name, table_name),
            )

            relationships = []
            for row in cursor.fetchall():
                relationships.append(
                    Relationship(
                        from_field=row["from_column"],
                        to_resource_id=f"{row['to_schema']}.{row['to_table']}",
                        to_field=row["to_column"],
                        type="many_to_one",
                    )
                )

            cursor.close()
            conn.close()

            return ResourceSchema(
                fields=fields,
                relationships=relationships,
                capabilities=["read", "update", "delete"],
            )

        except Exception as e:
            logger.error(f"Error in PostgreSQL get_resource_schema for {resource_id}: {e}")
            raise

    def query(self, resource_id: str, query_spec: Dict[str, Any], user_ctx: UserContext) -> Dict[str, Any]:
        """
        Execute a read query on a resource.
        """
        try:
            schema_name, table_name = (
                resource_id.split(".", 1) if "." in resource_id else ("public", resource_id)
            )

            conn = self._get_connection(user_ctx)
            cursor = conn.cursor(cursor_factory=RealDictCursor)

            # Build SELECT query
            select_fields = query_spec.get("select", ["*"])
            if select_fields == ["*"]:
                select_clause = "*"
            else:
                select_clause = ", ".join([f'"{f}"' for f in select_fields])

            sql = f'SELECT {select_clause} FROM "{schema_name}"."{table_name}"'
            params = []

            # Apply filters
            filters = query_spec.get("filters", [])
            if filters:
                where_clauses = []
                for filter_item in filters:
                    field = filter_item.get("field")
                    op = filter_item.get("op", "eq")
                    value = filter_item.get("value")

                    op_map = {
                        "eq": "=",
                        "neq": "!=",
                        "gt": ">",
                        "gte": ">=",
                        "lt": "<",
                        "lte": "<=",
                        "like": "LIKE",
                        "ilike": "ILIKE",
                    }

                    if op == "in":
                        placeholders = ", ".join(["%s"] * len(value))
                        where_clauses.append(f'"{field}" IN ({placeholders})')
                        params.extend(value)
                    elif op == "is":
                        where_clauses.append(f'"{field}" IS %s')
                        params.append(value)
                    else:
                        sql_op = op_map.get(op, "=")
                        if op in ("like", "ilike"):
                            where_clauses.append(f'"{field}" {sql_op} %s')
                            params.append(f"%{value}%")
                        else:
                            where_clauses.append(f'"{field}" {sql_op} %s')
                            params.append(value)

                sql += " WHERE " + " AND ".join(where_clauses)

            # Apply ordering
            order_by = query_spec.get("orderBy", [])
            if order_by:
                order_clauses = [f'"{o["field"]}" {o.get("dir", "asc").upper()}' for o in order_by]
                sql += " ORDER BY " + ", ".join(order_clauses)

            # Apply pagination
            limit = query_spec.get("limit", 50)
            offset = query_spec.get("offset", 0)
            sql += f" LIMIT {limit} OFFSET {offset}"

            cursor.execute(sql, params)
            data = cursor.fetchall()

            cursor.close()
            conn.close()

            return {
                "data": [dict(row) for row in data],
                "count": len(data),
            }

        except Exception as e:
            logger.error(f"Error in PostgreSQL query for {resource_id}: {e}")
            raise

    def execute_action(
        self, action_def: Dict[str, Any], args: Dict[str, Any], user_ctx: UserContext
    ) -> Dict[str, Any]:
        """
        Execute an allowlisted action.
        """
        try:
            action_type = action_def.get("action_type")
            resource_id = action_def.get("resource_id")
            schema_name, table_name = (
                resource_id.split(".", 1) if "." in resource_id else ("public", resource_id)
            )

            conn = self._get_connection(user_ctx)
            cursor = conn.cursor(cursor_factory=RealDictCursor)

            if action_type == "update_one":
                primary_key_field = action_def.get("metadata", {}).get("primary_key_field", "id")
                primary_key_value = args.get(primary_key_field)
                updateable_fields = action_def.get("metadata", {}).get("updateable_fields", [])

                update_data = {
                    k: v for k, v in args.items() if k in updateable_fields and k != primary_key_field
                }

                set_clauses = [f'"{k}" = %s' for k in update_data.keys()]
                params = list(update_data.values()) + [primary_key_value]

                sql = f'UPDATE "{schema_name}"."{table_name}" SET {", ".join(set_clauses)} WHERE "{primary_key_field}" = %s RETURNING *'
                cursor.execute(sql, params)
                result = cursor.fetchone()
                conn.commit()

                cursor.close()
                conn.close()

                return {"success": True, "data": dict(result) if result else None}

            elif action_type == "soft_delete":
                primary_key_field = action_def.get("metadata", {}).get("primary_key_field", "id")
                primary_key_value = args.get(primary_key_field)

                sql = f'UPDATE "{schema_name}"."{table_name}" SET deleted_at = NOW() WHERE "{primary_key_field}" = %s RETURNING *'
                cursor.execute(sql, [primary_key_value])
                result = cursor.fetchone()
                conn.commit()

                cursor.close()
                conn.close()

                return {"success": True, "data": dict(result) if result else None}
            else:
                raise ValueError(f"Unsupported action type: {action_type}")

        except Exception as e:
            logger.error(f"Error executing PostgreSQL action {action_def.get('action_id')}: {e}")
            raise

    def get_capabilities(self, ctx: AdapterContext) -> Dict[str, Any]:
        """Get adapter capabilities and metadata."""
        return {
            "adapter_type": "postgresql",
            "capabilities": ["read", "update_one", "soft_delete"],
            "supported_query_operators": ["eq", "neq", "gt", "gte", "lt", "lte", "like", "ilike", "in", "is"],
        }

    def _map_pg_type(self, pg_type: str) -> str:
        """Map PostgreSQL types to generic types."""
        type_map = {
            "integer": "integer",
            "bigint": "integer",
            "smallint": "integer",
            "numeric": "number",
            "decimal": "number",
            "real": "number",
            "double precision": "number",
            "character varying": "string",
            "varchar": "string",
            "text": "string",
            "char": "string",
            "boolean": "boolean",
            "timestamp without time zone": "datetime",
            "timestamp with time zone": "datetime",
            "date": "date",
            "time": "time",
            "uuid": "uuid",
            "json": "json",
            "jsonb": "json",
            "ARRAY": "array",
        }
        return type_map.get(pg_type.lower(), "string")
