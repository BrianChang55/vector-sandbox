#!/usr/bin/env python
"""
Comprehensive End-to-End Test Suite for App Data Store Integration.

This script tests the complete data store functionality including:
1. Backend data models (AppDataTable, AppDataRow)
2. AppDataService CRUD operations
3. Runtime Data Proxy API
4. Schema versioning with AppDataTableSnapshot
5. Data store context generation for LLMs
6. Agentic service table definition parsing

Run with: python manage.py shell < scripts/test_data_store_e2e.py
Or:       cd internal-apps-backend && python -c "import django; django.setup(); exec(open('scripts/test_data_store_e2e.py').read())"

Prerequisites:
- Django environment configured
- Database migrations applied
"""

import os
import sys
import json
import traceback
from datetime import datetime
from typing import Dict, Any, List, Tuple

# Setup Django if not already configured
if not os.environ.get('DJANGO_SETTINGS_MODULE'):
    os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'internal_apps.settings')
    import django
    django.setup()

from django.db import transaction
from django.contrib.auth import get_user_model
from django.test import Client, RequestFactory
from rest_framework.test import APIClient

from relay_app.models import (
    InternalApp, AppVersion, AppDataTable, AppDataRow, 
    AppDataTableSnapshot, Organization, UserOrganization
)
from relay_app.services.app_data_service import AppDataService
from relay_app.services.data_store_context import build_data_store_context


# =============================================================================
# Test Utilities
# =============================================================================

class TestContext:
    """Holds test fixtures and state."""
    def __init__(self):
        self.user = None
        self.organization = None
        self.app = None
        self.version = None
        self.tables: Dict[str, AppDataTable] = {}
        self.rows: Dict[str, List[AppDataRow]] = {}

    def cleanup(self):
        """Clean up all test data."""
        try:
            if self.app:
                self.app.delete()
            if self.organization:
                self.organization.delete()
            if self.user:
                self.user.delete()
        except Exception as e:
            print(f"  Warning: Cleanup error: {e}")


class TestResult:
    """Tracks test results."""
    def __init__(self):
        self.passed = 0
        self.failed = 0
        self.errors: List[Tuple[str, str]] = []

    def success(self, test_name: str):
        self.passed += 1
        print(f"  ✓ {test_name}")

    def failure(self, test_name: str, error: str):
        self.failed += 1
        self.errors.append((test_name, error))
        print(f"  ✗ {test_name}: {error}")

    def summary(self):
        total = self.passed + self.failed
        print(f"\n{'='*60}")
        print(f"Test Results: {self.passed}/{total} passed")
        if self.failed > 0:
            print("\nFailed tests:")
            for name, error in self.errors:
                print(f"  - {name}: {error}")
        print('='*60)
        return self.failed == 0


def setup_test_context() -> TestContext:
    """Create test fixtures."""
    import uuid
    ctx = TestContext()
    User = get_user_model()
    
    unique_id = str(uuid.uuid4())[:8]
    
    # Create test user
    ctx.user = User.objects.create_user(
        username=f'test_user_{unique_id}',
        email=f'test_{unique_id}@example.com',
        password='testpass123'
    )
    
    # Create organization with unique slug
    ctx.organization = Organization.objects.create(
        name=f'Test Org {unique_id}',
        slug=f'test-org-{unique_id}'
    )
    
    # Link user to organization
    UserOrganization.objects.create(
        user=ctx.user,
        organization=ctx.organization,
        role='admin'
    )
    
    # Create internal app
    ctx.app = InternalApp.objects.create(
        organization=ctx.organization,
        name=f'Test App {datetime.now().timestamp()}',
        description='Test app for E2E data store tests'
    )
    
    # Create app version
    ctx.version = AppVersion.objects.create(
        internal_app=ctx.app,
        version_number=1,
        source='ai_edit',
        created_by=ctx.user,
        spec_json={'files': [], 'resources': []}  # Required field
    )
    
    return ctx


# =============================================================================
# Test Cases
# =============================================================================

def test_create_table(ctx: TestContext, results: TestResult):
    """Test table creation via AppDataService."""
    schema = {
        'columns': [
            {'name': 'id', 'type': 'uuid', 'primary_key': True},
            {'name': 'name', 'type': 'string', 'nullable': False},
            {'name': 'email', 'type': 'string', 'unique': True},
            {'name': 'age', 'type': 'integer', 'nullable': True},
            {'name': 'status', 'type': 'enum', 'enum_values': ['active', 'inactive', 'pending']},
        ]
    }
    
    table, errors = AppDataService.create_table(
        app=ctx.app,
        name='Customers',
        schema=schema,
        description='Customer records'
    )
    
    if errors:
        results.failure("create_table", f"Errors: {errors}")
        return
    
    if not table:
        results.failure("create_table", "Table not created")
        return
    
    if table.slug != 'customers':
        results.failure("create_table", f"Expected slug 'customers', got '{table.slug}'")
        return
    
    if len(table.schema_json.get('columns', [])) != 5:
        results.failure("create_table", f"Expected 5 columns, got {len(table.schema_json.get('columns', []))}")
        return
    
    ctx.tables['customers'] = table
    results.success("create_table")


def test_create_table_with_version(ctx: TestContext, results: TestResult):
    """Test versioned table creation with snapshot."""
    schema = {
        'columns': [
            {'name': 'id', 'type': 'uuid', 'primary_key': True},
            {'name': 'title', 'type': 'string', 'nullable': False},
            {'name': 'completed', 'type': 'boolean', 'default': False},
        ]
    }
    
    table, errors = AppDataService.create_table_versioned(
        app=ctx.app,
        version=ctx.version,
        name='Tasks',
        schema=schema,
        description='Task items'
    )
    
    if errors:
        results.failure("create_table_versioned", f"Errors: {errors}")
        return
    
    # Check snapshot was created
    snapshot = AppDataTableSnapshot.objects.filter(
        table=table,
        app_version=ctx.version
    ).first()
    
    if not snapshot:
        results.failure("create_table_versioned", "Snapshot not created")
        return
    
    if snapshot.schema_json != schema:
        results.failure("create_table_versioned", "Snapshot schema doesn't match")
        return
    
    ctx.tables['tasks'] = table
    results.success("create_table_versioned")


def test_insert_row(ctx: TestContext, results: TestResult):
    """Test row insertion."""
    table = ctx.tables.get('customers')
    if not table:
        results.failure("insert_row", "Customers table not found")
        return
    
    row_data = {
        'name': 'John Doe',
        'email': 'john@example.com',
        'age': 30,
        'status': 'active'
    }
    
    row, errors = AppDataService.insert_row(table, row_data)
    
    if errors:
        results.failure("insert_row", f"Errors: {errors}")
        return
    
    if not row:
        results.failure("insert_row", "Row not created")
        return
    
    if row.data.get('name') != 'John Doe':
        results.failure("insert_row", f"Expected name 'John Doe', got '{row.data.get('name')}'")
        return
    
    ctx.rows.setdefault('customers', []).append(row)
    results.success("insert_row")


def test_bulk_insert(ctx: TestContext, results: TestResult):
    """Test bulk row insertion."""
    table = ctx.tables.get('customers')
    if not table:
        results.failure("bulk_insert", "Customers table not found")
        return
    
    rows_data = [
        {'name': 'Jane Smith', 'email': 'jane@example.com', 'age': 25, 'status': 'active'},
        {'name': 'Bob Wilson', 'email': 'bob@example.com', 'age': 35, 'status': 'pending'},
        {'name': 'Alice Brown', 'email': 'alice@example.com', 'age': 28, 'status': 'inactive'},
    ]
    
    rows, errors = AppDataService.insert_rows_bulk(table, rows_data)
    
    if errors:
        results.failure("bulk_insert", f"Errors: {errors}")
        return
    
    if len(rows) != 3:
        results.failure("bulk_insert", f"Expected 3 rows, got {len(rows)}")
        return
    
    ctx.rows.setdefault('customers', []).extend(rows)
    results.success("bulk_insert")


def test_query_rows(ctx: TestContext, results: TestResult):
    """Test row querying with filters."""
    table = ctx.tables.get('customers')
    if not table:
        results.failure("query_rows", "Customers table not found")
        return
    
    # Query active customers
    query_spec = {
        'filters': [{'field': 'status', 'op': 'eq', 'value': 'active'}],
        'order_by': [{'field': 'name', 'dir': 'asc'}],
        'limit': 10,
        'offset': 0,
    }
    
    result = AppDataService.query_rows(table, query_spec)
    
    if not hasattr(result, 'rows'):
        results.failure("query_rows", "Result missing 'rows' attribute")
        return
    
    # Should have 2 active customers (John and Jane)
    if len(result.rows) != 2:
        results.failure("query_rows", f"Expected 2 active customers, got {len(result.rows)}")
        return
    
    # Check ordering (Alice, Jane, John... but only active so Jane, John)
    names = [r['data']['name'] for r in result.rows]
    if names != ['Jane Smith', 'John Doe']:
        results.failure("query_rows", f"Expected ['Jane Smith', 'John Doe'], got {names}")
        return
    
    results.success("query_rows")


def test_update_row(ctx: TestContext, results: TestResult):
    """Test row update."""
    rows = ctx.rows.get('customers', [])
    if not rows:
        results.failure("update_row", "No rows to update")
        return
    
    row = rows[0]
    updated_row, errors = AppDataService.update_row(row, {'age': 31, 'status': 'pending'})
    
    if errors:
        results.failure("update_row", f"Errors: {errors}")
        return
    
    if updated_row.data.get('age') != 31:
        results.failure("update_row", f"Expected age 31, got {updated_row.data.get('age')}")
        return
    
    if updated_row.data.get('status') != 'pending':
        results.failure("update_row", f"Expected status 'pending', got {updated_row.data.get('status')}")
        return
    
    # Name should remain unchanged
    if updated_row.data.get('name') != 'John Doe':
        results.failure("update_row", f"Name should remain 'John Doe', got {updated_row.data.get('name')}")
        return
    
    results.success("update_row")


def test_update_table_schema_versioned(ctx: TestContext, results: TestResult):
    """Test versioned schema update."""
    table = ctx.tables.get('customers')
    if not table:
        results.failure("update_table_schema_versioned", "Customers table not found")
        return
    
    # Create new version
    new_version = AppVersion.objects.create(
        internal_app=ctx.app,
        version_number=2,
        source='ai_edit',
        created_by=ctx.user,
        spec_json={'files': [], 'resources': []}  # Required field
    )
    
    # Add new column
    new_schema = {
        'columns': [
            {'name': 'id', 'type': 'uuid', 'primary_key': True},
            {'name': 'name', 'type': 'string', 'nullable': False},
            {'name': 'email', 'type': 'string', 'unique': True},
            {'name': 'age', 'type': 'integer', 'nullable': True},
            {'name': 'status', 'type': 'enum', 'enum_values': ['active', 'inactive', 'pending']},
            {'name': 'phone', 'type': 'string', 'nullable': True},  # New column
        ]
    }
    
    updated_table, errors, changes = AppDataService.update_table_schema_versioned(
        table=table,
        version=new_version,
        schema=new_schema
    )
    
    if errors:
        results.failure("update_table_schema_versioned", f"Errors: {errors}")
        return
    
    if not updated_table:
        results.failure("update_table_schema_versioned", "Table not updated")
        return
    
    # Check snapshot was created
    snapshot = AppDataTableSnapshot.objects.filter(
        table=table,
        app_version=new_version
    ).first()
    
    if not snapshot:
        results.failure("update_table_schema_versioned", "Snapshot not created")
        return
    
    # Snapshot.schema_json contains NEW schema (current state)
    # Snapshot.previous_schema_json contains OLD schema (for update operations)
    if len(snapshot.schema_json.get('columns', [])) != 6:
        results.failure("update_table_schema_versioned", f"Snapshot schema should have 6 columns, got {len(snapshot.schema_json.get('columns', []))}")
        return
    
    # previous_schema_json should have OLD schema (5 columns)
    if not snapshot.previous_schema_json or len(snapshot.previous_schema_json.get('columns', [])) != 5:
        prev_count = len(snapshot.previous_schema_json.get('columns', [])) if snapshot.previous_schema_json else 0
        results.failure("update_table_schema_versioned", f"Previous schema should have 5 columns, got {prev_count}")
        return
    
    # Current table should have NEW schema (6 columns)
    if len(updated_table.schema_json.get('columns', [])) != 6:
        results.failure("update_table_schema_versioned", "Table should have new schema")
        return
    
    results.success("update_table_schema_versioned")


def test_delete_row(ctx: TestContext, results: TestResult):
    """Test row deletion."""
    rows = ctx.rows.get('customers', [])
    if len(rows) < 2:
        results.failure("delete_row", "Not enough rows to test deletion")
        return
    
    # Delete the last row
    row_to_delete = rows[-1]
    row_id = str(row_to_delete.id)
    
    AppDataService.delete_row(row_to_delete)
    
    # Verify deletion
    exists = AppDataRow.objects.filter(id=row_id).exists()
    if exists:
        results.failure("delete_row", "Row was not deleted")
        return
    
    ctx.rows['customers'] = rows[:-1]
    results.success("delete_row")


def test_bulk_delete(ctx: TestContext, results: TestResult):
    """Test bulk row deletion."""
    table = ctx.tables.get('customers')
    rows = ctx.rows.get('customers', [])
    
    if len(rows) < 2:
        results.failure("bulk_delete", "Not enough rows for bulk delete test")
        return
    
    row_ids = [str(r.id) for r in rows[-2:]]
    deleted_count = AppDataService.delete_rows_bulk(table, row_ids)
    
    if deleted_count != 2:
        results.failure("bulk_delete", f"Expected 2 deleted, got {deleted_count}")
        return
    
    ctx.rows['customers'] = rows[:-2]
    results.success("bulk_delete")


def test_data_store_context(ctx: TestContext, results: TestResult):
    """Test LLM context generation."""
    context = build_data_store_context(ctx.app)
    
    if not context:
        results.failure("data_store_context", "Context is empty")
        return
    
    # Should contain table names
    if 'Customers' not in context and 'customers' not in context:
        results.failure("data_store_context", "Context missing customers table")
        return
    
    # Should contain column info
    if 'name' not in context or 'email' not in context:
        results.failure("data_store_context", "Context missing column names")
        return
    
    results.success("data_store_context")


def test_delete_table(ctx: TestContext, results: TestResult):
    """Test table deletion."""
    table = ctx.tables.get('tasks')
    if not table:
        results.failure("delete_table", "Tasks table not found")
        return
    
    table_id = str(table.id)
    AppDataService.delete_table(table)
    
    # Verify deletion
    exists = AppDataTable.objects.filter(id=table_id).exists()
    if exists:
        results.failure("delete_table", "Table was not deleted")
        return
    
    del ctx.tables['tasks']
    results.success("delete_table")


# =============================================================================
# Runtime API Tests
# =============================================================================

def test_runtime_api_list_tables(ctx: TestContext, results: TestResult):
    """Test Runtime API: listTables operation."""
    client = APIClient()
    
    response = client.post(
        '/api/v1/runtime/data/',
        {
            'appId': str(ctx.app.id),
            'operation': 'listTables',
        },
        format='json'
    )
    
    if response.status_code != 200:
        results.failure("runtime_api_list_tables", f"Status {response.status_code}: {response.data}")
        return
    
    tables = response.data.get('tables', [])
    if not isinstance(tables, list):
        results.failure("runtime_api_list_tables", "Response 'tables' is not a list")
        return
    
    # Should have at least the customers table
    table_slugs = [t.get('slug') for t in tables]
    if 'customers' not in table_slugs:
        results.failure("runtime_api_list_tables", f"Expected 'customers' in {table_slugs}")
        return
    
    results.success("runtime_api_list_tables")


def test_runtime_api_get_schema(ctx: TestContext, results: TestResult):
    """Test Runtime API: getSchema operation."""
    client = APIClient()
    
    response = client.post(
        '/api/v1/runtime/data/',
        {
            'appId': str(ctx.app.id),
            'operation': 'getSchema',
            'tableSlug': 'customers',
        },
        format='json'
    )
    
    if response.status_code != 200:
        results.failure("runtime_api_get_schema", f"Status {response.status_code}: {response.data}")
        return
    
    schema = response.data.get('schema', {})
    columns = schema.get('columns', [])
    
    if len(columns) < 5:
        results.failure("runtime_api_get_schema", f"Expected at least 5 columns, got {len(columns)}")
        return
    
    results.success("runtime_api_get_schema")


def test_runtime_api_insert_query_update_delete(ctx: TestContext, results: TestResult):
    """Test Runtime API: full CRUD cycle."""
    client = APIClient()
    
    # 1. Insert
    insert_response = client.post(
        '/api/v1/runtime/data/',
        {
            'appId': str(ctx.app.id),
            'operation': 'insert',
            'tableSlug': 'customers',
            'params': {
                'data': {
                    'name': 'API Test User',
                    'email': 'apitest@example.com',
                    'age': 40,
                    'status': 'active',
                }
            }
        },
        format='json'
    )
    
    if insert_response.status_code != 201:
        results.failure("runtime_api_crud", f"Insert failed: {insert_response.status_code} - {insert_response.data}")
        return
    
    row_id = insert_response.data.get('id')
    if not row_id:
        results.failure("runtime_api_crud", "Insert response missing row id")
        return
    
    # 2. Query
    query_response = client.post(
        '/api/v1/runtime/data/',
        {
            'appId': str(ctx.app.id),
            'operation': 'query',
            'tableSlug': 'customers',
            'params': {
                'filters': [{'field': 'email', 'op': 'eq', 'value': 'apitest@example.com'}],
                'limit': 10,
            }
        },
        format='json'
    )
    
    if query_response.status_code != 200:
        results.failure("runtime_api_crud", f"Query failed: {query_response.status_code} - {query_response.data}")
        return
    
    rows = query_response.data.get('rows', [])
    if len(rows) != 1:
        results.failure("runtime_api_crud", f"Expected 1 row from query, got {len(rows)}")
        return
    
    # 3. Update
    update_response = client.post(
        '/api/v1/runtime/data/',
        {
            'appId': str(ctx.app.id),
            'operation': 'update',
            'tableSlug': 'customers',
            'params': {
                'rowId': row_id,
                'data': {'age': 41, 'status': 'inactive'}
            }
        },
        format='json'
    )
    
    if update_response.status_code != 200:
        results.failure("runtime_api_crud", f"Update failed: {update_response.status_code} - {update_response.data}")
        return
    
    if update_response.data.get('data', {}).get('age') != 41:
        results.failure("runtime_api_crud", "Update did not apply correctly")
        return
    
    # 4. Delete
    delete_response = client.post(
        '/api/v1/runtime/data/',
        {
            'appId': str(ctx.app.id),
            'operation': 'delete',
            'tableSlug': 'customers',
            'params': {
                'rowId': row_id
            }
        },
        format='json'
    )
    
    if delete_response.status_code != 200:
        results.failure("runtime_api_crud", f"Delete failed: {delete_response.status_code} - {delete_response.data}")
        return
    
    if not delete_response.data.get('success'):
        results.failure("runtime_api_crud", "Delete response missing success flag")
        return
    
    results.success("runtime_api_crud")


def test_runtime_api_bulk_operations(ctx: TestContext, results: TestResult):
    """Test Runtime API: bulk insert and delete."""
    client = APIClient()
    
    # Bulk insert
    bulk_insert_response = client.post(
        '/api/v1/runtime/data/',
        {
            'appId': str(ctx.app.id),
            'operation': 'bulkInsert',
            'tableSlug': 'customers',
            'params': {
                'rows': [
                    {'name': 'Bulk User 1', 'email': 'bulk1@example.com', 'age': 20, 'status': 'active'},
                    {'name': 'Bulk User 2', 'email': 'bulk2@example.com', 'age': 21, 'status': 'active'},
                    {'name': 'Bulk User 3', 'email': 'bulk3@example.com', 'age': 22, 'status': 'active'},
                ]
            }
        },
        format='json'
    )
    
    if bulk_insert_response.status_code != 201:
        results.failure("runtime_api_bulk", f"Bulk insert failed: {bulk_insert_response.status_code} - {bulk_insert_response.data}")
        return
    
    if bulk_insert_response.data.get('created_count') != 3:
        results.failure("runtime_api_bulk", f"Expected 3 created, got {bulk_insert_response.data.get('created_count')}")
        return
    
    row_ids = [r['id'] for r in bulk_insert_response.data.get('rows', [])]
    
    # Bulk delete
    bulk_delete_response = client.post(
        '/api/v1/runtime/data/',
        {
            'appId': str(ctx.app.id),
            'operation': 'bulkDelete',
            'tableSlug': 'customers',
            'params': {
                'rowIds': row_ids
            }
        },
        format='json'
    )
    
    if bulk_delete_response.status_code != 200:
        results.failure("runtime_api_bulk", f"Bulk delete failed: {bulk_delete_response.status_code} - {bulk_delete_response.data}")
        return
    
    if bulk_delete_response.data.get('deleted_count') != 3:
        results.failure("runtime_api_bulk", f"Expected 3 deleted, got {bulk_delete_response.data.get('deleted_count')}")
        return
    
    results.success("runtime_api_bulk")


def test_runtime_api_error_handling(ctx: TestContext, results: TestResult):
    """Test Runtime API: error handling."""
    client = APIClient()
    
    # Missing appId
    response1 = client.post('/api/v1/runtime/data/', {'operation': 'listTables'}, format='json')
    if response1.status_code != 400 or 'appId' not in response1.data.get('error', ''):
        results.failure("runtime_api_errors", f"Missing appId should return 400: {response1.status_code}")
        return
    
    # Invalid appId
    response2 = client.post('/api/v1/runtime/data/', {
        'appId': '00000000-0000-0000-0000-000000000000',
        'operation': 'listTables'
    }, format='json')
    if response2.status_code != 404:
        results.failure("runtime_api_errors", f"Invalid appId should return 404: {response2.status_code}")
        return
    
    # Invalid table
    response3 = client.post('/api/v1/runtime/data/', {
        'appId': str(ctx.app.id),
        'operation': 'query',
        'tableSlug': 'nonexistent_table'
    }, format='json')
    if response3.status_code != 404:
        results.failure("runtime_api_errors", f"Invalid table should return 404: {response3.status_code}")
        return
    
    # Invalid operation
    response4 = client.post('/api/v1/runtime/data/', {
        'appId': str(ctx.app.id),
        'operation': 'invalidOp',
        'tableSlug': 'customers'
    }, format='json')
    if response4.status_code != 400:
        results.failure("runtime_api_errors", f"Invalid operation should return 400: {response4.status_code}")
        return
    
    results.success("runtime_api_errors")


# =============================================================================
# Main Test Runner
# =============================================================================

def run_all_tests():
    """Run all E2E tests."""
    print("\n" + "="*60)
    print("App Data Store - End-to-End Test Suite")
    print("="*60 + "\n")
    
    results = TestResult()
    ctx = None
    
    try:
        print("Setting up test fixtures...")
        ctx = setup_test_context()
        print(f"  Created app: {ctx.app.name} (ID: {ctx.app.id})\n")
        
        # Service Layer Tests
        print("Service Layer Tests:")
        test_create_table(ctx, results)
        test_create_table_with_version(ctx, results)
        test_insert_row(ctx, results)
        test_bulk_insert(ctx, results)
        test_query_rows(ctx, results)
        test_update_row(ctx, results)
        test_update_table_schema_versioned(ctx, results)
        test_delete_row(ctx, results)
        test_bulk_delete(ctx, results)
        test_data_store_context(ctx, results)
        test_delete_table(ctx, results)
        
        print("\nRuntime API Tests:")
        test_runtime_api_list_tables(ctx, results)
        test_runtime_api_get_schema(ctx, results)
        test_runtime_api_insert_query_update_delete(ctx, results)
        test_runtime_api_bulk_operations(ctx, results)
        test_runtime_api_error_handling(ctx, results)
        
    except Exception as e:
        print(f"\n!!! Test suite error: {e}")
        traceback.print_exc()
        results.failure("TEST_SUITE", str(e))
    
    finally:
        if ctx:
            print("\nCleaning up test data...")
            ctx.cleanup()
            print("  Done.")
    
    return results.summary()


if __name__ == '__main__':
    success = run_all_tests()
    sys.exit(0 if success else 1)

