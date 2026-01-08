"""
Runtime Data Proxy API.

Single endpoint for generated apps to interact with the App Data Store.
Handles authentication via app context and routes to appropriate operations.

Authorization:
- All requests must be authenticated
- User must be a member of the app's organization
"""
from __future__ import annotations

import logging
from typing import Tuple, Optional
from rest_framework import status
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework.permissions import AllowAny

from ..models import InternalApp, AppDataTable, AppVersion, AppDataTableSnapshot, UserOrganization
from ..services.app_data_service import AppDataService

logger = logging.getLogger(__name__)


def check_app_access(request, app: InternalApp) -> Tuple[bool, Optional[str]]:
    """
    Check if the request has access to the app.
    
    All requests must be authenticated and user must be a member of the app's organization.
    
    Returns:
        (has_access, error_message) - if has_access is False, error_message explains why
    """
    user = request.user
    
    # Require authentication
    if not user or not user.is_authenticated:
        return False, "Authentication required"
    
    # Verify user belongs to the app's organization
    is_member = UserOrganization.objects.filter(
        user=user,
        organization=app.organization
    ).exists()
    
    if not is_member:
        return False, "You don't have access to this app"
    
    return True, None


class RuntimeDataProxyView(APIView):
    """
    Unified runtime proxy for App Data Store operations.
    
    This endpoint is called by generated apps at runtime to perform
    CRUD operations on the app's data tables.
    
    Authorization:
    - All requests must be authenticated
    - User must be a member of the app's organization
    
    Request format:
    {
        "appId": "uuid",
        "versionId": "uuid",  // Optional - for versioned schema access
        "operation": "query" | "insert" | "update" | "delete" | "bulkInsert" | "bulkDelete" | "listTables" | "getSchema",
        "tableSlug": "table-slug",  // Required for table operations
        "params": { ... }  // Operation-specific parameters
    }
    """
    
    # Allow the request through - we do custom authorization below
    permission_classes = [AllowAny]
    
    def post(self, request):
        """Handle all data store operations."""
        data = request.data
        
        # Extract required fields
        app_id = data.get('appId')
        operation = data.get('operation')
        table_slug = data.get('tableSlug')
        params = data.get('params', {})
        version_id = data.get('versionId')
        
        # Validate required fields
        if not app_id:
            return Response(
                {'error': 'appId is required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        if not operation:
            return Response(
                {'error': 'operation is required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get the app
        try:
            app = InternalApp.objects.select_related('organization').get(id=app_id)
        except InternalApp.DoesNotExist:
            return Response(
                {'error': 'App not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        # Authorization check
        has_access, error_message = check_app_access(request, app)
        if not has_access:
            return Response(
                {'error': error_message},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Handle meta operations that don't require a table
        if operation == 'listTables':
            return self._handle_list_tables(app)
        
        # All other operations require tableSlug
        if not table_slug and operation not in ('listTables',):
            return Response(
                {'error': 'tableSlug is required for this operation'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get the table
        try:
            table = AppDataTable.objects.get(internal_app=app, slug=table_slug)
        except AppDataTable.DoesNotExist:
            return Response(
                {'error': f'Table "{table_slug}" not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        # Get schema (potentially from version snapshot)
        schema = self._get_schema(table, version_id)
        
        # Route to appropriate handler
        handlers = {
            'query': self._handle_query,
            'insert': self._handle_insert,
            'update': self._handle_update,
            'delete': self._handle_delete,
            'bulkInsert': self._handle_bulk_insert,
            'bulkDelete': self._handle_bulk_delete,
            'getSchema': self._handle_get_schema,
        }
        
        handler = handlers.get(operation)
        if not handler:
            return Response(
                {'error': f'Unknown operation: {operation}'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            return handler(app, table, schema, params)
        except Exception as e:
            logger.exception(f"Runtime data operation failed: {operation}")
            return Response(
                {'error': str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )
    
    def _get_schema(self, table: AppDataTable, version_id: str = None) -> dict:
        """Get table schema, optionally from a version snapshot."""
        if version_id:
            try:
                snapshot = AppDataTableSnapshot.objects.get(
                    table=table,
                    app_version_id=version_id
                )
                return snapshot.schema_json
            except AppDataTableSnapshot.DoesNotExist:
                pass
        
        return table.schema_json
    
    def _handle_list_tables(self, app: InternalApp) -> Response:
        """List all tables for an app."""
        tables = AppDataTable.objects.filter(internal_app=app).order_by('name')
        
        result = []
        for table in tables:
            result.append({
                'slug': table.slug,
                'name': table.name,
                'description': table.description,
                'row_count': table.row_count,
                'column_count': len(table.schema_json.get('columns', [])),
            })
        
        return Response({'tables': result})
    
    def _handle_get_schema(
        self, 
        app: InternalApp, 
        table: AppDataTable, 
        schema: dict,
        params: dict
    ) -> Response:
        """Get table schema."""
        return Response({
            'slug': table.slug,
            'name': table.name,
            'description': table.description,
            'schema': schema,
            'row_count': table.row_count,
        })
    
    def _handle_query(
        self, 
        app: InternalApp, 
        table: AppDataTable, 
        schema: dict,
        params: dict
    ) -> Response:
        """Query rows with filters, sorting, and pagination."""
        filters = params.get('filters', [])
        order_by = params.get('orderBy', [])
        limit = params.get('limit', 50)
        offset = params.get('offset', 0)
        select = params.get('select')
        
        # Build query_spec dict for the service
        query_spec = {
            'filters': filters,
            'order_by': order_by,
            'limit': min(limit, 1000),  # Cap at 1000
            'offset': offset,
        }
        if select:
            query_spec['select'] = select
        
        result = AppDataService.query_rows(
            table=table,
            query_spec=query_spec,
        )
        
        return Response({
            'rows': result.rows,
            'total_count': result.total_count,
            'limit': result.limit,
            'offset': result.offset,
            'has_more': result.has_more,
        })
    
    def _handle_insert(
        self, 
        app: InternalApp, 
        table: AppDataTable, 
        schema: dict,
        params: dict
    ) -> Response:
        """Insert a new row."""
        data = params.get('data', {})
        
        if not data:
            return Response(
                {'error': 'data is required for insert operation'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        row, errors = AppDataService.insert_row(table, data)
        
        if errors:
            return Response(
                {'error': 'Validation failed', 'errors': errors},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        return Response({
            'id': str(row.id),
            'data': row.data,
            'row_index': row.row_index,
            'created_at': row.created_at.isoformat() if row.created_at else None,
        }, status=status.HTTP_201_CREATED)
    
    def _handle_update(
        self, 
        app: InternalApp, 
        table: AppDataTable, 
        schema: dict,
        params: dict
    ) -> Response:
        """Update an existing row."""
        row_id = params.get('rowId')
        data = params.get('data', {})
        
        if not row_id:
            return Response(
                {'error': 'rowId is required for update operation'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        if not data:
            return Response(
                {'error': 'data is required for update operation'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Fetch the row first
        from ..models import AppDataRow
        try:
            row_obj = AppDataRow.objects.get(table=table, id=row_id)
        except AppDataRow.DoesNotExist:
            return Response(
                {'error': 'Row not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        row, errors = AppDataService.update_row(row_obj, data)
        
        if errors:
            return Response(
                {'error': 'Update failed', 'errors': errors},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        if not row:
            return Response(
                {'error': 'Row not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        return Response({
            'id': str(row.id),
            'data': row.data,
            'row_index': row.row_index,
            'updated_at': row.updated_at.isoformat() if row.updated_at else None,
        })
    
    def _handle_delete(
        self, 
        app: InternalApp, 
        table: AppDataTable, 
        schema: dict,
        params: dict
    ) -> Response:
        """Delete a row."""
        row_id = params.get('rowId')
        
        if not row_id:
            return Response(
                {'error': 'rowId is required for delete operation'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Fetch the row first
        from ..models import AppDataRow
        try:
            row_obj = AppDataRow.objects.get(table=table, id=row_id)
        except AppDataRow.DoesNotExist:
            return Response(
                {'error': 'Row not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        AppDataService.delete_row(row_obj)
        
        return Response({'success': True})
    
    def _handle_bulk_insert(
        self, 
        app: InternalApp, 
        table: AppDataTable, 
        schema: dict,
        params: dict
    ) -> Response:
        """Bulk insert multiple rows."""
        rows_data = params.get('rows', [])
        
        if not rows_data:
            return Response(
                {'error': 'rows array is required for bulkInsert operation'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        if len(rows_data) > 1000:
            return Response(
                {'error': 'Maximum 1000 rows per bulk insert'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        created_rows, errors = AppDataService.insert_rows_bulk(table, rows_data)
        
        if errors:
            return Response({
                'error': 'Some rows failed to insert',
                'errors': errors,
                'rows': [
                    {
                        'id': str(r.id),
                        'data': r.data,
                        'row_index': r.row_index,
                    }
                    for r in created_rows
                ],
            }, status=status.HTTP_400_BAD_REQUEST)
        
        return Response({
            'created_count': len(created_rows),
            'rows': [
                {
                    'id': str(r.id),
                    'data': r.data,
                    'row_index': r.row_index,
                }
                for r in created_rows
            ],
        }, status=status.HTTP_201_CREATED)
    
    def _handle_bulk_delete(
        self, 
        app: InternalApp, 
        table: AppDataTable, 
        schema: dict,
        params: dict
    ) -> Response:
        """Bulk delete multiple rows."""
        row_ids = params.get('rowIds', [])
        
        if not row_ids:
            return Response(
                {'error': 'rowIds array is required for bulkDelete operation'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        if len(row_ids) > 1000:
            return Response(
                {'error': 'Maximum 1000 rows per bulk delete'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        deleted_count = AppDataService.delete_rows_bulk(table, row_ids)
        
        return Response({
            'deleted_count': deleted_count,
        })

