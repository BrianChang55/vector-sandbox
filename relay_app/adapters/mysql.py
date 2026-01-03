"""
MySQL Adapter Implementation

Provides direct MySQL connection for schema introspection and data queries.
"""
import logging
from typing import List, Dict, Any, Optional
import pymysql
from pymysql.cursors import DictCursor

from .base import (
    AdapterContext,
    UserContext,
    Resource,
    ResourceField,
    Relationship,
    ResourceSchema,
)

logger = logging.getLogger(__name__)


class MySQLAdapter:
    """
    MySQL adapter implementation.
    
    Uses direct database connection for both metadata and queries.
    """
    
    @property
    def type(self) -> str:
        return "mysql"
    
    def _get_connection(self, ctx: AdapterContext):
        """Create a database connection from context."""
        return pymysql.connect(
            host=ctx.host,
            port=ctx.port or 3306,
            database=ctx.database,
            user=ctx.username,
            password=ctx.password,
            connect_timeout=10,
            cursorclass=DictCursor,
        )
    
    def test_connection(self, ctx: AdapterContext) -> Dict[str, Any]:
        """Test the database connection."""
        try:
            conn = self._get_connection(ctx)
            cursor = conn.cursor()
            cursor.execute("SELECT VERSION() as version;")
            result = cursor.fetchone()
            version = result['version'] if result else 'Unknown'
            cursor.close()
            conn.close()
            return {
                "success": True,
                "message": "Connection successful",
                "version": version,
            }
        except Exception as e:
            logger.error(f"MySQL connection test failed: {e}")
            return {
                "success": False,
                "message": str(e),
            }
    
    def list_resources(self, ctx: AdapterContext) -> List[Resource]:
        """
        List all tables and views in the MySQL database.
        """
        resources = []
        try:
            conn = self._get_connection(ctx)
            cursor = conn.cursor()
            
            # Get tables and views from information_schema
            cursor.execute("""
                SELECT 
                    TABLE_SCHEMA as table_schema,
                    TABLE_NAME as table_name,
                    TABLE_TYPE as table_type
                FROM information_schema.TABLES
                WHERE TABLE_SCHEMA = %s
                AND TABLE_TYPE IN ('BASE TABLE', 'VIEW')
                ORDER BY TABLE_NAME
            """, (ctx.database,))
            
            for row in cursor.fetchall():
                resource_id = f"{row['table_schema']}.{row['table_name']}"
                kind = 'view' if row['table_type'] == 'VIEW' else 'table'
                
                resources.append(Resource(
                    id=resource_id,
                    name=row['table_name'],
                    kind=kind,
                    namespace=row['table_schema'],
                ))
            
            cursor.close()
            conn.close()
            
            logger.info(f"MySQL list_resources: Found {len(resources)} resources")
            return resources
            
        except Exception as e:
            logger.error(f"Error in MySQL list_resources: {e}")
            raise
    
    def get_resource_schema(self, resource_id: str, ctx: AdapterContext) -> ResourceSchema:
        """
        Get schema for a specific resource (table/view).
        """
        try:
            schema_name, table_name = resource_id.split('.', 1) if '.' in resource_id else (ctx.database, resource_id)
            
            conn = self._get_connection(ctx)
            cursor = conn.cursor()
            
            # Get column information
            cursor.execute("""
                SELECT 
                    COLUMN_NAME as column_name,
                    DATA_TYPE as data_type,
                    IS_NULLABLE as is_nullable,
                    COLUMN_DEFAULT as column_default,
                    COLUMN_KEY as column_key,
                    ORDINAL_POSITION as ordinal_position
                FROM information_schema.COLUMNS
                WHERE TABLE_SCHEMA = %s AND TABLE_NAME = %s
                ORDER BY ORDINAL_POSITION
            """, (schema_name, table_name))
            
            fields = []
            for row in cursor.fetchall():
                fields.append(ResourceField(
                    name=row['column_name'],
                    type=self._map_mysql_type(row['data_type']),
                    nullable=row['is_nullable'] == 'YES',
                    primary_key=row['column_key'] == 'PRI',
                ))
            
            # Get foreign key relationships
            cursor.execute("""
                SELECT
                    COLUMN_NAME as from_column,
                    REFERENCED_TABLE_SCHEMA as to_schema,
                    REFERENCED_TABLE_NAME as to_table,
                    REFERENCED_COLUMN_NAME as to_column
                FROM information_schema.KEY_COLUMN_USAGE
                WHERE TABLE_SCHEMA = %s
                AND TABLE_NAME = %s
                AND REFERENCED_TABLE_NAME IS NOT NULL
            """, (schema_name, table_name))
            
            relationships = []
            for row in cursor.fetchall():
                relationships.append(Relationship(
                    from_field=row['from_column'],
                    to_resource_id=f"{row['to_schema']}.{row['to_table']}",
                    to_field=row['to_column'],
                    type='many_to_one',
                ))
            
            cursor.close()
            conn.close()
            
            return ResourceSchema(
                fields=fields,
                relationships=relationships,
                capabilities=["read", "update", "delete"],
            )
            
        except Exception as e:
            logger.error(f"Error in MySQL get_resource_schema for {resource_id}: {e}")
            raise
    
    def query(
        self,
        resource_id: str,
        query_spec: Dict[str, Any],
        user_ctx: UserContext
    ) -> Dict[str, Any]:
        """
        Execute a read query on a resource.
        """
        try:
            schema_name, table_name = resource_id.split('.', 1) if '.' in resource_id else (user_ctx.database, resource_id)
            
            conn = self._get_connection(user_ctx)
            cursor = conn.cursor()
            
            # Build SELECT query
            select_fields = query_spec.get('select', ['*'])
            if select_fields == ['*']:
                select_clause = '*'
            else:
                select_clause = ', '.join([f'`{f}`' for f in select_fields])
            
            sql = f'SELECT {select_clause} FROM `{schema_name}`.`{table_name}`'
            params = []
            
            # Apply filters
            filters = query_spec.get('filters', [])
            if filters:
                where_clauses = []
                for filter_item in filters:
                    field = filter_item.get('field')
                    op = filter_item.get('op', 'eq')
                    value = filter_item.get('value')
                    
                    op_map = {
                        'eq': '=', 'neq': '!=', 'gt': '>', 'gte': '>=',
                        'lt': '<', 'lte': '<=', 'like': 'LIKE',
                    }
                    
                    if op == 'in':
                        placeholders = ', '.join(['%s'] * len(value))
                        where_clauses.append(f'`{field}` IN ({placeholders})')
                        params.extend(value)
                    elif op == 'is':
                        where_clauses.append(f'`{field}` IS %s')
                        params.append(value)
                    else:
                        sql_op = op_map.get(op, '=')
                        if op == 'like':
                            where_clauses.append(f'`{field}` {sql_op} %s')
                            params.append(f'%{value}%')
                        else:
                            where_clauses.append(f'`{field}` {sql_op} %s')
                            params.append(value)
                
                sql += ' WHERE ' + ' AND '.join(where_clauses)
            
            # Apply ordering
            order_by = query_spec.get('orderBy', [])
            if order_by:
                order_clauses = [f'`{o["field"]}` {o.get("dir", "asc").upper()}' for o in order_by]
                sql += ' ORDER BY ' + ', '.join(order_clauses)
            
            # Apply pagination
            limit = query_spec.get('limit', 50)
            offset = query_spec.get('offset', 0)
            sql += f' LIMIT {limit} OFFSET {offset}'
            
            cursor.execute(sql, params)
            data = cursor.fetchall()
            
            cursor.close()
            conn.close()
            
            return {
                "data": list(data),
                "count": len(data),
            }
            
        except Exception as e:
            logger.error(f"Error in MySQL query for {resource_id}: {e}")
            raise
    
    def execute_action(
        self,
        action_def: Dict[str, Any],
        args: Dict[str, Any],
        user_ctx: UserContext
    ) -> Dict[str, Any]:
        """
        Execute an allowlisted action.
        """
        try:
            action_type = action_def.get('action_type')
            resource_id = action_def.get('resource_id')
            schema_name, table_name = resource_id.split('.', 1) if '.' in resource_id else (user_ctx.database, resource_id)
            
            conn = self._get_connection(user_ctx)
            cursor = conn.cursor()
            
            if action_type == 'update_one':
                primary_key_field = action_def.get('metadata', {}).get('primary_key_field', 'id')
                primary_key_value = args.get(primary_key_field)
                updateable_fields = action_def.get('metadata', {}).get('updateable_fields', [])
                
                update_data = {k: v for k, v in args.items() if k in updateable_fields and k != primary_key_field}
                
                set_clauses = [f'`{k}` = %s' for k in update_data.keys()]
                params = list(update_data.values()) + [primary_key_value]
                
                sql = f'UPDATE `{schema_name}`.`{table_name}` SET {", ".join(set_clauses)} WHERE `{primary_key_field}` = %s'
                cursor.execute(sql, params)
                conn.commit()
                
                # Fetch updated row
                cursor.execute(f'SELECT * FROM `{schema_name}`.`{table_name}` WHERE `{primary_key_field}` = %s', [primary_key_value])
                result = cursor.fetchone()
                
                cursor.close()
                conn.close()
                
                return {"success": True, "data": result}
                
            elif action_type == 'soft_delete':
                primary_key_field = action_def.get('metadata', {}).get('primary_key_field', 'id')
                primary_key_value = args.get(primary_key_field)
                
                sql = f'UPDATE `{schema_name}`.`{table_name}` SET deleted_at = NOW() WHERE `{primary_key_field}` = %s'
                cursor.execute(sql, [primary_key_value])
                conn.commit()
                
                # Fetch updated row
                cursor.execute(f'SELECT * FROM `{schema_name}`.`{table_name}` WHERE `{primary_key_field}` = %s', [primary_key_value])
                result = cursor.fetchone()
                
                cursor.close()
                conn.close()
                
                return {"success": True, "data": result}
            else:
                raise ValueError(f"Unsupported action type: {action_type}")
                
        except Exception as e:
            logger.error(f"Error executing MySQL action {action_def.get('action_id')}: {e}")
            raise
    
    def get_capabilities(self, ctx: AdapterContext) -> Dict[str, Any]:
        """Get adapter capabilities and metadata."""
        return {
            "adapter_type": "mysql",
            "capabilities": ["read", "update_one", "soft_delete"],
            "supported_query_operators": ["eq", "neq", "gt", "gte", "lt", "lte", "like", "in", "is"],
        }
    
    def _map_mysql_type(self, mysql_type: str) -> str:
        """Map MySQL types to generic types."""
        type_map = {
            'int': 'integer', 'bigint': 'integer', 'smallint': 'integer', 'tinyint': 'integer', 'mediumint': 'integer',
            'decimal': 'number', 'float': 'number', 'double': 'number',
            'varchar': 'string', 'text': 'string', 'char': 'string', 'longtext': 'string', 'mediumtext': 'string',
            'tinyint(1)': 'boolean', 'boolean': 'boolean',
            'datetime': 'datetime', 'timestamp': 'datetime',
            'date': 'date', 'time': 'time',
            'json': 'json', 'blob': 'binary', 'longblob': 'binary',
        }
        return type_map.get(mysql_type.lower(), 'string')

