"""
Views for App Data Store (tables and rows).

Provides REST API endpoints for managing data tables and rows within Internal Apps.
"""
from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from rest_framework.views import APIView
from django.shortcuts import get_object_or_404

from ..models import InternalApp, AppDataTable, AppDataRow, UserOrganization
from ..serializers.app_data import (
    AppDataTableSerializer,
    AppDataTableCreateSerializer,
    AppDataTableUpdateSerializer,
    AppDataRowSerializer,
    AppDataRowDataSerializer,
    AppDataRowBulkSerializer,
    AppDataRowBulkDeleteSerializer,
    QuerySpecSerializer,
    QueryResultSerializer,
)
from ..services.app_data_service import AppDataService


class AppDataTableViewSet(viewsets.ViewSet):
    """
    ViewSet for managing data tables within an Internal App.
    
    Endpoints:
    - GET /apps/{app_id}/data/tables/ - List tables
    - POST /apps/{app_id}/data/tables/ - Create table
    - GET /apps/{app_id}/data/tables/{slug}/ - Get table
    - PATCH /apps/{app_id}/data/tables/{slug}/ - Update table
    - DELETE /apps/{app_id}/data/tables/{slug}/ - Delete table
    """
    permission_classes = [IsAuthenticated]
    
    def _get_app(self, request, app_pk):
        """Get app and verify user access."""
        app = get_object_or_404(InternalApp, pk=app_pk)
        
        # Verify user belongs to the app's organization
        if not UserOrganization.objects.filter(
            user=request.user,
            organization=app.organization
        ).exists():
            return None
        
        return app
    
    def list(self, request, internal_app_pk=None):
        """List all tables for an app."""
        app = self._get_app(request, internal_app_pk)
        if not app:
            return Response(
                {'error': 'App not found or access denied'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        tables = AppDataService.list_tables(app)
        serializer = AppDataTableSerializer(tables, many=True)
        return Response(serializer.data)
    
    def create(self, request, internal_app_pk=None):
        """Create a new data table."""
        app = self._get_app(request, internal_app_pk)
        if not app:
            return Response(
                {'error': 'App not found or access denied'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        serializer = AppDataTableCreateSerializer(data=request.data)
        if not serializer.is_valid():
            return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
        
        table, errors = AppDataService.create_table(
            app=app,
            name=serializer.validated_data['name'],
            schema=serializer.validated_data['schema_json'],
            description=serializer.validated_data.get('description', '')
        )
        
        if errors:
            return Response({'errors': errors}, status=status.HTTP_400_BAD_REQUEST)
        
        return Response(
            AppDataTableSerializer(table).data,
            status=status.HTTP_201_CREATED
        )
    
    def retrieve(self, request, internal_app_pk=None, pk=None):
        """Get a table by slug."""
        app = self._get_app(request, internal_app_pk)
        if not app:
            return Response(
                {'error': 'App not found or access denied'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        table = AppDataService.get_table(app, pk)
        if not table:
            return Response(
                {'error': 'Table not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        return Response(AppDataTableSerializer(table).data)
    
    def partial_update(self, request, internal_app_pk=None, pk=None):
        """Update a table."""
        app = self._get_app(request, internal_app_pk)
        if not app:
            return Response(
                {'error': 'App not found or access denied'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        table = AppDataService.get_table(app, pk)
        if not table:
            return Response(
                {'error': 'Table not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        serializer = AppDataTableUpdateSerializer(data=request.data)
        if not serializer.is_valid():
            return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
        
        updated_table, errors = AppDataService.update_table_schema(
            table=table,
            schema=serializer.validated_data.get('schema_json', table.schema_json),
            name=serializer.validated_data.get('name'),
            description=serializer.validated_data.get('description')
        )
        
        if errors:
            return Response({'errors': errors}, status=status.HTTP_400_BAD_REQUEST)
        
        return Response(AppDataTableSerializer(updated_table).data)
    
    def destroy(self, request, internal_app_pk=None, pk=None):
        """Delete a table."""
        app = self._get_app(request, internal_app_pk)
        if not app:
            return Response(
                {'error': 'App not found or access denied'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        table = AppDataService.get_table(app, pk)
        if not table:
            return Response(
                {'error': 'Table not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        AppDataService.delete_table(table)
        return Response(status=status.HTTP_204_NO_CONTENT)


class AppDataRowViewSet(viewsets.ViewSet):
    """
    ViewSet for managing rows within a data table.
    
    Endpoints:
    - GET /apps/{app_id}/data/tables/{slug}/rows/ - Query rows
    - POST /apps/{app_id}/data/tables/{slug}/rows/ - Insert row
    - GET /apps/{app_id}/data/tables/{slug}/rows/{id}/ - Get row
    - PATCH /apps/{app_id}/data/tables/{slug}/rows/{id}/ - Update row
    - DELETE /apps/{app_id}/data/tables/{slug}/rows/{id}/ - Delete row
    - POST /apps/{app_id}/data/tables/{slug}/rows/bulk/ - Bulk insert
    - DELETE /apps/{app_id}/data/tables/{slug}/rows/bulk/ - Bulk delete
    """
    permission_classes = [IsAuthenticated]
    
    def _get_table(self, request, app_pk, table_slug):
        """Get table and verify user access."""
        app = get_object_or_404(InternalApp, pk=app_pk)
        
        # Verify user belongs to the app's organization
        if not UserOrganization.objects.filter(
            user=request.user,
            organization=app.organization
        ).exists():
            return None, None
        
        table = AppDataService.get_table(app, table_slug)
        return app, table
    
    def list(self, request, internal_app_pk=None, table_slug=None):
        """Query rows with optional filtering, sorting, and pagination."""
        app, table = self._get_table(request, internal_app_pk, table_slug)
        if not app:
            return Response(
                {'error': 'App not found or access denied'},
                status=status.HTTP_404_NOT_FOUND
            )
        if not table:
            return Response(
                {'error': 'Table not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        # Parse query spec from query params or body
        query_spec = {}
        
        # Support query params
        if 'limit' in request.query_params:
            try:
                query_spec['limit'] = int(request.query_params['limit'])
            except ValueError:
                pass
        
        if 'offset' in request.query_params:
            try:
                query_spec['offset'] = int(request.query_params['offset'])
            except ValueError:
                pass
        
        # Validate query spec
        serializer = QuerySpecSerializer(data=query_spec)
        if not serializer.is_valid():
            return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
        
        result = AppDataService.query_rows(table, serializer.validated_data)
        
        return Response(QueryResultSerializer(result).data)
    
    def create(self, request, internal_app_pk=None, table_slug=None):
        """Insert a single row."""
        app, table = self._get_table(request, internal_app_pk, table_slug)
        if not app:
            return Response(
                {'error': 'App not found or access denied'},
                status=status.HTTP_404_NOT_FOUND
            )
        if not table:
            return Response(
                {'error': 'Table not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        serializer = AppDataRowDataSerializer(data=request.data)
        if not serializer.is_valid():
            return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
        
        row, errors = AppDataService.insert_row(table, serializer.validated_data['data'])
        
        if errors:
            return Response({'errors': errors}, status=status.HTTP_400_BAD_REQUEST)
        
        return Response(
            AppDataRowSerializer(row).data,
            status=status.HTTP_201_CREATED
        )
    
    def retrieve(self, request, internal_app_pk=None, table_slug=None, pk=None):
        """Get a single row by ID."""
        app, table = self._get_table(request, internal_app_pk, table_slug)
        if not app:
            return Response(
                {'error': 'App not found or access denied'},
                status=status.HTTP_404_NOT_FOUND
            )
        if not table:
            return Response(
                {'error': 'Table not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        row = AppDataService.get_row(table, pk)
        if not row:
            return Response(
                {'error': 'Row not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        return Response(AppDataRowSerializer(row).data)
    
    def partial_update(self, request, internal_app_pk=None, table_slug=None, pk=None):
        """Update a row with partial data."""
        app, table = self._get_table(request, internal_app_pk, table_slug)
        if not app:
            return Response(
                {'error': 'App not found or access denied'},
                status=status.HTTP_404_NOT_FOUND
            )
        if not table:
            return Response(
                {'error': 'Table not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        row = AppDataService.get_row(table, pk)
        if not row:
            return Response(
                {'error': 'Row not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        serializer = AppDataRowDataSerializer(data=request.data)
        if not serializer.is_valid():
            return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
        
        updated_row, errors = AppDataService.update_row(row, serializer.validated_data['data'])
        
        if errors:
            return Response({'errors': errors}, status=status.HTTP_400_BAD_REQUEST)
        
        return Response(AppDataRowSerializer(updated_row).data)
    
    def destroy(self, request, internal_app_pk=None, table_slug=None, pk=None):
        """Delete a row."""
        app, table = self._get_table(request, internal_app_pk, table_slug)
        if not app:
            return Response(
                {'error': 'App not found or access denied'},
                status=status.HTTP_404_NOT_FOUND
            )
        if not table:
            return Response(
                {'error': 'Table not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        row = AppDataService.get_row(table, pk)
        if not row:
            return Response(
                {'error': 'Row not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        AppDataService.delete_row(row)
        return Response(status=status.HTTP_204_NO_CONTENT)


class AppDataRowBulkView(APIView):
    """
    View for bulk row operations.
    
    POST - Bulk insert rows
    DELETE - Bulk delete rows
    """
    permission_classes = [IsAuthenticated]
    
    def _get_table(self, request, app_pk, table_slug):
        """Get table and verify user access."""
        app = get_object_or_404(InternalApp, pk=app_pk)
        
        # Verify user belongs to the app's organization
        if not UserOrganization.objects.filter(
            user=request.user,
            organization=app.organization
        ).exists():
            return None, None
        
        table = AppDataService.get_table(app, table_slug)
        return app, table
    
    def post(self, request, internal_app_pk=None, table_slug=None):
        """Bulk insert rows."""
        app, table = self._get_table(request, internal_app_pk, table_slug)
        if not app:
            return Response(
                {'error': 'App not found or access denied'},
                status=status.HTTP_404_NOT_FOUND
            )
        if not table:
            return Response(
                {'error': 'Table not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        serializer = AppDataRowBulkSerializer(data=request.data)
        if not serializer.is_valid():
            return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
        
        created_rows, errors = AppDataService.insert_rows_bulk(
            table, serializer.validated_data['rows']
        )
        
        response_data = {
            'created_count': len(created_rows),
            'rows': AppDataRowSerializer(created_rows, many=True).data,
        }
        
        if errors:
            response_data['errors'] = errors
            return Response(response_data, status=status.HTTP_207_MULTI_STATUS)
        
        return Response(response_data, status=status.HTTP_201_CREATED)
    
    def delete(self, request, internal_app_pk=None, table_slug=None):
        """Bulk delete rows."""
        app, table = self._get_table(request, internal_app_pk, table_slug)
        if not app:
            return Response(
                {'error': 'App not found or access denied'},
                status=status.HTTP_404_NOT_FOUND
            )
        if not table:
            return Response(
                {'error': 'Table not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        serializer = AppDataRowBulkDeleteSerializer(data=request.data)
        if not serializer.is_valid():
            return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
        
        deleted_count = AppDataService.delete_rows_bulk(
            table, serializer.validated_data['row_ids']
        )
        
        return Response({'deleted_count': deleted_count})


class AppDataQueryView(APIView):
    """
    View for querying table data with complex filters.
    
    POST /apps/{app_id}/data/tables/{slug}/query/
    
    Allows passing complex query specifications in the request body.
    """
    permission_classes = [IsAuthenticated]
    
    def _get_table(self, request, app_pk, table_slug):
        """Get table and verify user access."""
        app = get_object_or_404(InternalApp, pk=app_pk)
        
        # Verify user belongs to the app's organization
        if not UserOrganization.objects.filter(
            user=request.user,
            organization=app.organization
        ).exists():
            return None, None
        
        table = AppDataService.get_table(app, table_slug)
        return app, table
    
    def post(self, request, internal_app_pk=None, table_slug=None):
        """Execute a query with complex filters."""
        app, table = self._get_table(request, internal_app_pk, table_slug)
        if not app:
            return Response(
                {'error': 'App not found or access denied'},
                status=status.HTTP_404_NOT_FOUND
            )
        if not table:
            return Response(
                {'error': 'Table not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        serializer = QuerySpecSerializer(data=request.data)
        if not serializer.is_valid():
            return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
        
        result = AppDataService.query_rows(table, serializer.validated_data)
        
        return Response(QueryResultSerializer(result).data)

