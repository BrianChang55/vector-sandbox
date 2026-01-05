"""
App Data Service for managing Internal App data tables and rows.

This service provides CRUD operations for app data tables and rows,
with schema validation and query capabilities.
"""
import uuid
from typing import Any, Optional
from dataclasses import dataclass
from django.db import transaction
from django.db.models import Q, F, Max
from django.utils.text import slugify

from ..models import InternalApp, AppDataTable, AppDataRow
from .schema_validator import SchemaValidator


@dataclass
class QueryResult:
    """Result of a query operation."""
    rows: list[dict]
    total_count: int
    limit: int
    offset: int
    has_more: bool


class AppDataService:
    """
    Service for managing app data tables and rows.
    
    Provides CRUD operations with schema validation, querying,
    and automatic field handling.
    """
    
    # =========================================================================
    # Table Operations
    # =========================================================================
    
    @staticmethod
    def create_table(
        app: InternalApp,
        name: str,
        schema: dict,
        description: str = ''
    ) -> tuple[Optional[AppDataTable], list[str]]:
        """
        Create a new data table with schema validation.
        
        Args:
            app: The InternalApp to create the table in
            name: Display name for the table
            schema: Table schema with columns and optional indexes
            description: Optional table description
            
        Returns:
            Tuple of (table, errors). Table is None if validation fails.
        """
        # Validate schema
        is_valid, errors = SchemaValidator.validate_schema(schema)
        if not is_valid:
            return None, errors
        
        # Generate slug from name
        base_slug = slugify(name)
        slug = base_slug
        
        # Ensure unique slug within app
        counter = 1
        while AppDataTable.objects.filter(internal_app=app, slug=slug).exists():
            slug = f"{base_slug}-{counter}"
            counter += 1
        
        # Create table
        table = AppDataTable.objects.create(
            internal_app=app,
            name=name,
            slug=slug,
            description=description,
            schema_json=schema,
            row_count=0
        )
        
        return table, []
    
    @staticmethod
    def update_table_schema(
        table: AppDataTable,
        schema: dict,
        name: Optional[str] = None,
        description: Optional[str] = None
    ) -> tuple[Optional[AppDataTable], list[str]]:
        """
        Update a table's schema.
        
        Note: Schema updates are allowed freely. Existing rows will have
        missing fields treated as null, and extra fields will be preserved.
        
        Args:
            table: The table to update
            schema: New schema definition
            name: Optional new name
            description: Optional new description
            
        Returns:
            Tuple of (table, errors). Table is None if validation fails.
        """
        # Validate new schema
        is_valid, errors = SchemaValidator.validate_schema(schema)
        if not is_valid:
            return None, errors
        
        # Update table
        table.schema_json = schema
        
        if name is not None:
            table.name = name
            # Update slug if name changed
            base_slug = slugify(name)
            slug = base_slug
            counter = 1
            while AppDataTable.objects.filter(
                internal_app=table.internal_app,
                slug=slug
            ).exclude(id=table.id).exists():
                slug = f"{base_slug}-{counter}"
                counter += 1
            table.slug = slug
        
        if description is not None:
            table.description = description
        
        table.save()
        
        return table, []
    
    @staticmethod
    def delete_table(table: AppDataTable) -> None:
        """
        Delete a table and all its rows.
        
        Args:
            table: The table to delete
        """
        table.delete()
    
    @staticmethod
    def get_table(app: InternalApp, slug: str) -> Optional[AppDataTable]:
        """
        Get a table by slug.
        
        Args:
            app: The InternalApp
            slug: Table slug
            
        Returns:
            The table or None if not found
        """
        return AppDataTable.objects.filter(
            internal_app=app,
            slug=slug
        ).first()
    
    @staticmethod
    def list_tables(app: InternalApp) -> list[AppDataTable]:
        """
        List all tables for an app.
        
        Args:
            app: The InternalApp
            
        Returns:
            List of tables
        """
        return list(AppDataTable.objects.filter(internal_app=app).order_by('name'))
    
    # =========================================================================
    # Row Operations
    # =========================================================================
    
    @staticmethod
    def insert_row(
        table: AppDataTable,
        data: dict
    ) -> tuple[Optional[AppDataRow], list[str]]:
        """
        Insert a single row with schema validation and auto-fields.
        
        Args:
            table: The table to insert into
            data: Row data
            
        Returns:
            Tuple of (row, errors). Row is None if validation fails.
        """
        schema = table.schema_json
        
        # Apply defaults and auto-generated fields
        data_with_defaults = SchemaValidator.apply_defaults(schema, data)
        
        # Validate data
        is_valid, errors = SchemaValidator.validate_row(schema, data_with_defaults)
        if not is_valid:
            return None, errors
        
        # Check unique constraints
        unique_errors = AppDataService._check_unique_constraints(table, data_with_defaults)
        if unique_errors:
            return None, unique_errors
        
        # Create row with transaction for row count update
        with transaction.atomic():
            row = AppDataRow.objects.create(
                table=table,
                data=data_with_defaults
            )
            table.increment_row_count()
        
        return row, []
    
    @staticmethod
    def insert_rows_bulk(
        table: AppDataTable,
        rows_data: list[dict]
    ) -> tuple[list[AppDataRow], list[str]]:
        """
        Insert multiple rows at once.
        
        Args:
            table: The table to insert into
            rows_data: List of row data dicts
            
        Returns:
            Tuple of (created_rows, errors). Rows list may be empty if all fail.
        """
        schema = table.schema_json
        created_rows = []
        all_errors = []
        
        # Get starting row index
        max_index = AppDataRow.objects.filter(table=table).aggregate(
            max_index=Max('row_index')
        )['max_index'] or 0
        
        with transaction.atomic():
            for i, data in enumerate(rows_data):
                # Apply defaults
                data_with_defaults = SchemaValidator.apply_defaults(schema, data)
                
                # Validate
                is_valid, errors = SchemaValidator.validate_row(schema, data_with_defaults)
                if not is_valid:
                    all_errors.extend([f'Row {i}: {e}' for e in errors])
                    continue
                
                # Check unique constraints
                unique_errors = AppDataService._check_unique_constraints(
                    table, data_with_defaults, exclude_rows=created_rows
                )
                if unique_errors:
                    all_errors.extend([f'Row {i}: {e}' for e in unique_errors])
                    continue
                
                # Create row
                max_index += 1
                row = AppDataRow(
                    table=table,
                    data=data_with_defaults,
                    row_index=max_index
                )
                created_rows.append(row)
            
            if created_rows:
                # Bulk create
                AppDataRow.objects.bulk_create(created_rows)
                # Update row count
                table.row_count = F('row_count') + len(created_rows)
                table.save(update_fields=['row_count', 'updated_at'])
                table.refresh_from_db(fields=['row_count'])
        
        return created_rows, all_errors
    
    @staticmethod
    def update_row(
        row: AppDataRow,
        data: dict
    ) -> tuple[Optional[AppDataRow], list[str]]:
        """
        Update a row with partial data.
        
        Args:
            row: The row to update
            data: Partial row data
            
        Returns:
            Tuple of (row, errors). Row is None if validation fails.
        """
        table = row.table
        schema = table.schema_json
        
        # Merge with existing data
        merged_data = {**row.data, **data}
        
        # Apply auto_now fields
        merged_data = SchemaValidator.apply_auto_now(schema, merged_data)
        
        # Validate merged data
        is_valid, errors = SchemaValidator.validate_row(schema, merged_data)
        if not is_valid:
            return None, errors
        
        # Check unique constraints
        unique_errors = AppDataService._check_unique_constraints(
            table, merged_data, exclude_row_id=row.id
        )
        if unique_errors:
            return None, unique_errors
        
        row.data = merged_data
        row.save()
        
        return row, []
    
    @staticmethod
    def delete_row(row: AppDataRow) -> None:
        """
        Delete a single row.
        
        Args:
            row: The row to delete
        """
        table = row.table
        with transaction.atomic():
            row.delete()
            table.decrement_row_count()
    
    @staticmethod
    def delete_rows_bulk(table: AppDataTable, row_ids: list[uuid.UUID]) -> int:
        """
        Delete multiple rows by ID.
        
        Args:
            table: The table
            row_ids: List of row UUIDs to delete
            
        Returns:
            Number of rows deleted
        """
        with transaction.atomic():
            deleted_count, _ = AppDataRow.objects.filter(
                table=table,
                id__in=row_ids
            ).delete()
            if deleted_count > 0:
                table.decrement_row_count(deleted_count)
        return deleted_count
    
    @staticmethod
    def get_row(table: AppDataTable, row_id: uuid.UUID) -> Optional[AppDataRow]:
        """
        Get a single row by ID.
        
        Args:
            table: The table
            row_id: Row UUID
            
        Returns:
            The row or None if not found
        """
        return AppDataRow.objects.filter(table=table, id=row_id).first()
    
    # =========================================================================
    # Query Operations
    # =========================================================================
    
    @staticmethod
    def query_rows(
        table: AppDataTable,
        query_spec: Optional[dict] = None
    ) -> QueryResult:
        """
        Query rows with filtering, sorting, and pagination.
        
        Query spec format:
        {
            "select": ["field1", "field2"],  # Optional: fields to return
            "filters": [
                {"field": "status", "op": "eq", "value": "active"}
            ],
            "order_by": [
                {"field": "created_at", "dir": "desc"}
            ],
            "limit": 50,
            "offset": 0
        }
        
        Supported operators: eq, neq, gt, gte, lt, lte, in, not_in,
                            contains, icontains, is_null
        
        Args:
            table: The table to query
            query_spec: Query specification dict
            
        Returns:
            QueryResult with matching rows
        """
        query_spec = query_spec or {}
        
        # Start with all rows for this table
        queryset = AppDataRow.objects.filter(table=table)
        
        # Apply filters
        filters = query_spec.get('filters', [])
        for filter_def in filters:
            queryset = AppDataService._apply_filter(queryset, filter_def)
        
        # Get total count before pagination
        total_count = queryset.count()
        
        # Apply ordering
        order_by = query_spec.get('order_by', [])
        if order_by:
            queryset = AppDataService._apply_ordering(queryset, order_by)
        else:
            # Default ordering by row_index
            queryset = queryset.order_by('row_index')
        
        # Apply pagination
        limit = min(query_spec.get('limit', 50), 1000)  # Max 1000 rows
        offset = query_spec.get('offset', 0)
        
        queryset = queryset[offset:offset + limit]
        
        # Execute query
        rows = list(queryset)
        
        # Extract data, optionally filtering fields
        select_fields = query_spec.get('select')
        result_rows = []
        for row in rows:
            row_data = {
                'id': str(row.id),
                'row_index': row.row_index,
                'created_at': row.created_at.isoformat() if row.created_at else None,
                'updated_at': row.updated_at.isoformat() if row.updated_at else None,
            }
            
            if select_fields:
                row_data['data'] = {
                    k: v for k, v in row.data.items()
                    if k in select_fields
                }
            else:
                row_data['data'] = row.data
            
            result_rows.append(row_data)
        
        return QueryResult(
            rows=result_rows,
            total_count=total_count,
            limit=limit,
            offset=offset,
            has_more=(offset + limit) < total_count
        )
    
    @staticmethod
    def _apply_filter(queryset, filter_def: dict):
        """Apply a single filter to a queryset."""
        field = filter_def.get('field')
        op = filter_def.get('op', 'eq')
        value = filter_def.get('value')
        
        if not field:
            return queryset
        
        # Build JSON field lookup
        json_path = f'data__{field}'
        
        if op == 'eq':
            return queryset.filter(**{json_path: value})
        elif op == 'neq':
            return queryset.exclude(**{json_path: value})
        elif op == 'gt':
            return queryset.filter(**{f'{json_path}__gt': value})
        elif op == 'gte':
            return queryset.filter(**{f'{json_path}__gte': value})
        elif op == 'lt':
            return queryset.filter(**{f'{json_path}__lt': value})
        elif op == 'lte':
            return queryset.filter(**{f'{json_path}__lte': value})
        elif op == 'in':
            return queryset.filter(**{f'{json_path}__in': value})
        elif op == 'not_in':
            return queryset.exclude(**{f'{json_path}__in': value})
        elif op == 'contains':
            return queryset.filter(**{f'{json_path}__contains': value})
        elif op == 'icontains':
            return queryset.filter(**{f'{json_path}__icontains': value})
        elif op == 'is_null':
            if value:
                return queryset.filter(**{f'{json_path}__isnull': True})
            else:
                return queryset.exclude(**{f'{json_path}__isnull': True})
        
        return queryset
    
    @staticmethod
    def _apply_ordering(queryset, order_by: list):
        """Apply ordering to a queryset."""
        order_fields = []
        
        for order_def in order_by:
            field = order_def.get('field')
            direction = order_def.get('dir', 'asc')
            
            if not field:
                continue
            
            # Handle special fields
            if field in ('row_index', 'created_at', 'updated_at'):
                order_field = field
            else:
                # JSON field ordering
                order_field = f'data__{field}'
            
            if direction == 'desc':
                order_field = f'-{order_field}'
            
            order_fields.append(order_field)
        
        if order_fields:
            return queryset.order_by(*order_fields)
        
        return queryset
    
    @staticmethod
    def _check_unique_constraints(
        table: AppDataTable,
        data: dict,
        exclude_row_id: Optional[uuid.UUID] = None,
        exclude_rows: Optional[list] = None
    ) -> list[str]:
        """Check unique constraints from schema."""
        errors = []
        schema = table.schema_json
        columns = schema.get('columns', [])
        
        # Check column-level unique constraints
        for col in columns:
            if col.get('unique') and not col.get('primary_key'):
                col_name = col['name']
                value = data.get(col_name)
                
                if value is None:
                    continue
                
                # Check against database
                exists_query = AppDataRow.objects.filter(
                    table=table,
                    **{f'data__{col_name}': value}
                )
                
                if exclude_row_id:
                    exists_query = exists_query.exclude(id=exclude_row_id)
                
                if exists_query.exists():
                    errors.append(f'Value for "{col_name}" must be unique')
                
                # Check against pending rows (for bulk insert)
                if exclude_rows:
                    for pending_row in exclude_rows:
                        if pending_row.data.get(col_name) == value:
                            errors.append(f'Value for "{col_name}" must be unique')
                            break
        
        # Check index-level unique constraints
        indexes = schema.get('indexes', [])
        for idx in indexes:
            if idx.get('unique'):
                idx_columns = idx.get('columns', [])
                idx_values = {col: data.get(col) for col in idx_columns}
                
                # Skip if any value is None
                if any(v is None for v in idx_values.values()):
                    continue
                
                # Check against database
                filter_kwargs = {
                    f'data__{col}': val for col, val in idx_values.items()
                }
                exists_query = AppDataRow.objects.filter(table=table, **filter_kwargs)
                
                if exclude_row_id:
                    exists_query = exists_query.exclude(id=exclude_row_id)
                
                if exists_query.exists():
                    errors.append(f'Combination of ({", ".join(idx_columns)}) must be unique')
                
                # Check against pending rows
                if exclude_rows:
                    for pending_row in exclude_rows:
                        pending_values = {col: pending_row.data.get(col) for col in idx_columns}
                        if pending_values == idx_values:
                            errors.append(f'Combination of ({", ".join(idx_columns)}) must be unique')
                            break
        
        return errors
    
    # =========================================================================
    # Versioned Table Operations
    # =========================================================================
    
    @staticmethod
    def create_table_versioned(
        app: 'InternalApp',
        version: 'AppVersion',
        name: str,
        schema: dict,
        description: str = ''
    ) -> tuple[Optional['AppDataTable'], list[str]]:
        """
        Create a new data table with schema validation and version snapshot.
        
        Args:
            app: The InternalApp to create the table in
            version: The AppVersion to associate the snapshot with
            name: Display name for the table
            schema: Table schema with columns and optional indexes
            description: Optional table description
            
        Returns:
            Tuple of (table, errors). Table is None if validation fails.
        """
        from ..models import AppDataTableSnapshot
        
        # Create the table using the standard method
        table, errors = AppDataService.create_table(app, name, schema, description)
        
        if table is None:
            return None, errors
        
        # Create version snapshot
        AppDataTableSnapshot.create_snapshot(
            app_version=version,
            table=table,
            operation='create'
        )
        
        return table, []
    
    @staticmethod
    def update_table_schema_versioned(
        table: 'AppDataTable',
        version: 'AppVersion',
        schema: dict,
        name: Optional[str] = None,
        description: Optional[str] = None
    ) -> tuple[Optional['AppDataTable'], list[str], dict]:
        """
        Update a table's schema with version snapshot.
        
        Args:
            table: The table to update
            version: The AppVersion to associate the snapshot with
            schema: New schema definition
            name: Optional new name
            description: Optional new description
            
        Returns:
            Tuple of (table, errors, changes). Table is None if validation fails.
            Changes dict contains 'added', 'removed', 'modified' column lists.
        """
        from ..models import AppDataTableSnapshot
        
        # Store previous schema for comparison
        previous_schema = table.schema_json.copy()
        
        # Update using standard method
        table, errors = AppDataService.update_table_schema(
            table, schema, name, description
        )
        
        if table is None:
            return None, errors, {}
        
        # Create version snapshot
        snapshot = AppDataTableSnapshot.create_snapshot(
            app_version=version,
            table=table,
            operation='update',
            previous_schema=previous_schema
        )
        
        # Compute changes
        changes = snapshot.get_column_changes()
        
        return table, [], changes
    
    @staticmethod
    def delete_table_versioned(
        table: 'AppDataTable',
        version: 'AppVersion',
        force: bool = False
    ) -> tuple[bool, Optional[str]]:
        """
        Delete a table with version snapshot.
        
        Args:
            table: The table to delete
            version: The AppVersion to associate the snapshot with
            force: If True, delete even if table has data
            
        Returns:
            Tuple of (success, error_message)
        """
        from ..models import AppDataTableSnapshot
        
        # Check if table has data
        if not force and table.row_count > 0:
            return False, f'Table "{table.name}" has {table.row_count} rows. Use force=True to delete anyway.'
        
        # Create delete snapshot before deletion
        AppDataTableSnapshot.create_snapshot(
            app_version=version,
            table=table,
            operation='delete'
        )
        
        # Delete the table
        table.delete()
        
        return True, None
    
    @staticmethod
    def get_schema_at_version(
        table: 'AppDataTable',
        version: 'AppVersion'
    ) -> Optional[dict]:
        """
        Get a table's schema at a specific app version.
        
        Args:
            table: The table to get schema for
            version: The AppVersion to get schema from
            
        Returns:
            Schema dict or None if no snapshot exists for that version
        """
        from ..models import AppDataTableSnapshot
        
        snapshot = AppDataTableSnapshot.objects.filter(
            table=table,
            app_version=version
        ).first()
        
        if snapshot:
            return snapshot.schema_json
        
        # Fall back to current schema if no snapshot
        return table.schema_json
    
    @staticmethod
    def get_tables_at_version(
        app: 'InternalApp',
        version: 'AppVersion'
    ) -> list[dict]:
        """
        Get all tables and their schemas at a specific app version.
        
        Args:
            app: The InternalApp
            version: The AppVersion to get tables from
            
        Returns:
            List of table info dicts with schema at that version
        """
        from ..models import AppDataTableSnapshot
        
        # Get all snapshots for this version
        snapshots = AppDataTableSnapshot.objects.filter(
            app_version=version,
            table__internal_app=app
        ).exclude(operation='delete').select_related('table')
        
        tables = []
        for snapshot in snapshots:
            tables.append({
                'id': str(snapshot.table.id),
                'slug': snapshot.table_slug,
                'name': snapshot.table_name,
                'description': snapshot.table_description,
                'schema': snapshot.schema_json,
                'row_count': snapshot.table.row_count,
            })
        
        return tables
    
    @staticmethod
    def rollback_tables_to_version(
        app: 'InternalApp',
        target_version: 'AppVersion'
    ) -> tuple[bool, list[str]]:
        """
        Rollback table schemas to match a specific version.
        
        Note: This only affects schemas, not data. Rows are preserved.
        
        Args:
            app: The InternalApp
            target_version: The AppVersion to rollback to
            
        Returns:
            Tuple of (success, messages) describing what was rolled back
        """
        from ..models import AppDataTableSnapshot
        
        messages = []
        
        # Get snapshots at target version
        snapshots = AppDataTableSnapshot.objects.filter(
            app_version=target_version,
            table__internal_app=app
        ).select_related('table')
        
        with transaction.atomic():
            for snapshot in snapshots:
                table = snapshot.table
                
                if snapshot.operation == 'delete':
                    # Table was deleted in this version - we can't easily restore it
                    messages.append(f'Cannot restore deleted table "{snapshot.table_name}"')
                    continue
                
                # Restore schema from snapshot
                table.schema_json = snapshot.schema_json
                table.name = snapshot.table_name
                table.description = snapshot.table_description
                table.save()
                
                messages.append(f'Restored "{table.name}" schema to version {target_version.version_number}')
        
        return True, messages

