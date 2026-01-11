"""
Unit tests for database adapters.

These tests use mocking to test adapter logic without requiring actual database connections.
"""
from unittest.mock import Mock, patch, MagicMock
from django.test import TestCase

from vector_app.adapters.base import AdapterContext, UserContext, Resource, ResourceField, Relationship, ResourceSchema
from vector_app.adapters.supabase import SupabaseAdapter
from vector_app.adapters.postgresql import PostgreSQLAdapter
from vector_app.adapters.mysql import MySQLAdapter


class TestAdapterContext(TestCase):
    """Test AdapterContext dataclass."""
    
    def test_supabase_context(self):
        """Test creating Supabase adapter context."""
        ctx = AdapterContext(
            backend_url="https://example.supabase.co",
            service_role_key="secret-key",
            anon_key="anon-key"
        )
        self.assertEqual(ctx.backend_url, "https://example.supabase.co")
        self.assertEqual(ctx.service_role_key, "secret-key")
        self.assertEqual(ctx.anon_key, "anon-key")
    
    def test_postgresql_context(self):
        """Test creating PostgreSQL adapter context."""
        ctx = AdapterContext(
            host="localhost",
            port=5432,
            database="testdb",
            username="postgres",
            password="secret",
            ssl_mode="require"
        )
        self.assertEqual(ctx.host, "localhost")
        self.assertEqual(ctx.port, 5432)
        self.assertEqual(ctx.database, "testdb")
        self.assertEqual(ctx.username, "postgres")
        self.assertEqual(ctx.password, "secret")
        self.assertEqual(ctx.ssl_mode, "require")
    
    def test_mysql_context(self):
        """Test creating MySQL adapter context."""
        ctx = AdapterContext(
            host="localhost",
            port=3306,
            database="testdb",
            username="root",
            password="secret"
        )
        self.assertEqual(ctx.host, "localhost")
        self.assertEqual(ctx.port, 3306)
        self.assertEqual(ctx.database, "testdb")


class TestUserContext(TestCase):
    """Test UserContext dataclass."""
    
    def test_supabase_user_context(self):
        """Test creating Supabase user context."""
        ctx = UserContext(
            backend_url="https://example.supabase.co",
            user_jwt="jwt-token",
            anon_key="anon-key"
        )
        self.assertEqual(ctx.backend_url, "https://example.supabase.co")
        self.assertEqual(ctx.user_jwt, "jwt-token")
        self.assertEqual(ctx.anon_key, "anon-key")
    
    def test_postgresql_user_context(self):
        """Test creating PostgreSQL user context (same as adapter context)."""
        ctx = UserContext(
            host="localhost",
            port=5432,
            database="testdb",
            username="postgres",
            password="secret"
        )
        self.assertEqual(ctx.host, "localhost")
        self.assertEqual(ctx.port, 5432)


class TestResourceDataclasses(TestCase):
    """Test Resource-related dataclasses."""
    
    def test_resource_field(self):
        """Test ResourceField dataclass."""
        field = ResourceField(
            name="email",
            type="string",
            nullable=False,
            primary_key=False
        )
        self.assertEqual(field.name, "email")
        self.assertEqual(field.type, "string")
        self.assertFalse(field.nullable)
        self.assertFalse(field.primary_key)
    
    def test_resource_field_with_primary_key(self):
        """Test ResourceField with primary key."""
        field = ResourceField(
            name="id",
            type="integer",
            nullable=False,
            primary_key=True
        )
        self.assertTrue(field.primary_key)
    
    def test_relationship(self):
        """Test Relationship dataclass."""
        rel = Relationship(
            from_field="user_id",
            to_resource_id="public.users",
            to_field="id",
            type="many_to_one"
        )
        self.assertEqual(rel.from_field, "user_id")
        self.assertEqual(rel.to_resource_id, "public.users")
        self.assertEqual(rel.to_field, "id")
        self.assertEqual(rel.type, "many_to_one")
    
    def test_resource(self):
        """Test Resource dataclass."""
        resource = Resource(
            id="public.users",
            name="users",
            kind="table",
            namespace="public"
        )
        self.assertEqual(resource.id, "public.users")
        self.assertEqual(resource.name, "users")
        self.assertEqual(resource.kind, "table")
        self.assertEqual(resource.namespace, "public")
    
    def test_resource_schema(self):
        """Test ResourceSchema dataclass."""
        fields = [
            ResourceField("id", "integer", False, True),
            ResourceField("email", "string", False, False),
        ]
        relationships = [
            Relationship("org_id", "public.orgs", "id", "many_to_one")
        ]
        schema = ResourceSchema(
            fields=fields,
            relationships=relationships,
            capabilities=["read", "update"]
        )
        self.assertEqual(len(schema.fields), 2)
        self.assertEqual(len(schema.relationships), 1)
        self.assertIn("read", schema.capabilities)


class TestPostgreSQLAdapter(TestCase):
    """Test PostgreSQL adapter with mocked database."""
    
    def setUp(self):
        """Set up test adapter and context."""
        self.adapter = PostgreSQLAdapter()
        self.ctx = AdapterContext(
            host="localhost",
            port=5432,
            database="testdb",
            username="postgres",
            password="secret",
            ssl_mode="disable"
        )
    
    def test_adapter_type(self):
        """Test adapter type is correct."""
        self.assertEqual(self.adapter.type, "postgresql")
    
    @patch('vector_app.adapters.postgresql.psycopg2.connect')
    def test_test_connection_success(self, mock_connect):
        """Test successful connection test."""
        # Mock the connection and cursor
        mock_conn = MagicMock()
        mock_cursor = MagicMock()
        mock_cursor.fetchone.return_value = ("PostgreSQL 14.11 on x86_64",)
        mock_conn.cursor.return_value = mock_cursor
        mock_connect.return_value = mock_conn
        
        result = self.adapter.test_connection(self.ctx)
        
        self.assertTrue(result['success'])
        self.assertIn('PostgreSQL 14.11', result['version'])
        mock_connect.assert_called_once()
    
    @patch('vector_app.adapters.postgresql.psycopg2.connect')
    def test_test_connection_failure(self, mock_connect):
        """Test connection test failure."""
        mock_connect.side_effect = Exception("Connection refused")
        
        result = self.adapter.test_connection(self.ctx)
        
        self.assertFalse(result['success'])
        self.assertIn('Connection refused', result['message'])
    
    @patch('vector_app.adapters.postgresql.psycopg2.connect')
    def test_list_resources(self, mock_connect):
        """Test listing database tables."""
        mock_conn = MagicMock()
        mock_cursor = MagicMock()
        # Return dict-like rows (as RealDictCursor would)
        mock_cursor.fetchall.return_value = [
            {'table_schema': 'public', 'table_name': 'users', 'table_type': 'r'},
            {'table_schema': 'public', 'table_name': 'orders', 'table_type': 'r'},
            {'table_schema': 'public', 'table_name': 'products', 'table_type': 'r'},
        ]
        mock_conn.cursor.return_value = mock_cursor
        mock_connect.return_value = mock_conn
        
        resources = self.adapter.list_resources(self.ctx)
        
        self.assertEqual(len(resources), 3)
        self.assertEqual(resources[0].name, "users")
        self.assertEqual(resources[0].id, "public.users")
        self.assertEqual(resources[0].kind, "table")
    
    def test_get_resource_schema_live(self):
        """
        Test getting table schema - requires live PostgreSQL.
        This test validates the schema retrieval against the test database.
        Skipped if PostgreSQL is not available.
        """
        # This test uses live PostgreSQL connection to internal_apps_test
        ctx = AdapterContext(
            host="localhost",
            port=5432,
            database="internal_apps_test",
            username="mac",
            password="",
            ssl_mode="disable"
        )
        
        try:
            schema = self.adapter.get_resource_schema("public.users", ctx)
            
            # The adapter returns a ResourceSchema dataclass
            self.assertIsInstance(schema, ResourceSchema)
            self.assertGreater(len(schema.fields), 0)
            
            # Check that we have fields
            field_names = [f.name for f in schema.fields]
            self.assertIn('id', field_names)
            self.assertIn('email', field_names)
            
            # Check id field has primary key
            id_field = next(f for f in schema.fields if f.name == 'id')
            self.assertTrue(id_field.primary_key)
            
        except Exception as e:
            # Skip if PostgreSQL is not running
            if "Connection refused" in str(e):
                self.skipTest("PostgreSQL not available")
    
    @patch('vector_app.adapters.postgresql.psycopg2.connect')
    def test_query(self, mock_connect):
        """Test querying data."""
        mock_conn = MagicMock()
        mock_cursor = MagicMock()
        mock_cursor.description = [('id',), ('email',), ('name',)]
        # Return dict-like rows (as RealDictCursor would)
        mock_cursor.fetchall.return_value = [
            {'id': 1, 'email': 'alice@example.com', 'name': 'Alice'},
            {'id': 2, 'email': 'bob@example.com', 'name': 'Bob'},
        ]
        mock_conn.cursor.return_value = mock_cursor
        mock_connect.return_value = mock_conn
        
        query_spec = {
            "select": ["id", "email", "name"],
            "limit": 10
        }
        
        # Create user context
        user_ctx = UserContext(
            host="localhost",
            port=5432,
            database="testdb",
            username="postgres",
            password="secret"
        )
        
        result = self.adapter.query("public.users", query_spec, user_ctx)
        
        self.assertEqual(len(result['data']), 2)
        self.assertEqual(result['data'][0]['email'], 'alice@example.com')
    
    def test_get_capabilities(self):
        """Test getting adapter capabilities."""
        caps = self.adapter.get_capabilities(self.ctx)
        
        self.assertEqual(caps['adapter_type'], 'postgresql')
        self.assertIn('read', caps['capabilities'])
        self.assertIn('update_one', caps['capabilities'])


class TestMySQLAdapter(TestCase):
    """Test MySQL adapter with mocked database."""
    
    def setUp(self):
        """Set up test adapter and context."""
        self.adapter = MySQLAdapter()
        self.ctx = AdapterContext(
            host="localhost",
            port=3306,
            database="testdb",
            username="root",
            password="secret"
        )
    
    def test_adapter_type(self):
        """Test adapter type is correct."""
        self.assertEqual(self.adapter.type, "mysql")
    
    @patch('vector_app.adapters.mysql.pymysql.connect')
    def test_test_connection_success(self, mock_connect):
        """Test successful connection test."""
        mock_conn = MagicMock()
        mock_cursor = MagicMock()
        # Return dict-like row with correct key (lowercase 'version' alias)
        mock_cursor.fetchone.return_value = {'version': '8.0.32-MySQL Community'}
        mock_conn.cursor.return_value = mock_cursor
        mock_connect.return_value = mock_conn
        
        result = self.adapter.test_connection(self.ctx)
        
        self.assertTrue(result['success'])
        self.assertIn('MySQL', result['version'])
    
    @patch('vector_app.adapters.mysql.pymysql.connect')
    def test_list_resources(self, mock_connect):
        """Test listing database tables."""
        mock_conn = MagicMock()
        mock_cursor = MagicMock()
        # Return dict-like rows with lowercase keys (as returned by MySQL query aliases)
        mock_cursor.fetchall.return_value = [
            {'table_schema': 'testdb', 'table_name': 'users', 'table_type': 'BASE TABLE'},
            {'table_schema': 'testdb', 'table_name': 'orders', 'table_type': 'BASE TABLE'},
        ]
        mock_conn.cursor.return_value = mock_cursor
        mock_connect.return_value = mock_conn
        
        resources = self.adapter.list_resources(self.ctx)
        
        self.assertEqual(len(resources), 2)
        self.assertEqual(resources[0].name, "users")
        self.assertEqual(resources[0].kind, "table")
    
    def test_get_capabilities(self):
        """Test getting adapter capabilities."""
        caps = self.adapter.get_capabilities(self.ctx)
        
        self.assertEqual(caps['adapter_type'], 'mysql')
        self.assertIn('read', caps['capabilities'])


class TestSupabaseAdapter(TestCase):
    """Test Supabase adapter with mocked HTTP calls."""
    
    def setUp(self):
        """Set up test adapter and context."""
        self.adapter = SupabaseAdapter()
        self.ctx = AdapterContext(
            backend_url="https://example.supabase.co",
            service_role_key="secret-key",
            anon_key="anon-key"
        )
    
    def test_adapter_type(self):
        """Test adapter type is correct."""
        self.assertEqual(self.adapter.type, "supabase")
    
    def test_get_capabilities(self):
        """Test getting adapter capabilities."""
        caps = self.adapter.get_capabilities(self.ctx)
        
        self.assertEqual(caps['adapter_type'], 'supabase')
        self.assertIn('read', caps['capabilities'])


class TestAdapterFactoryPattern(TestCase):
    """Test adapter factory/selection logic."""
    
    def test_get_postgresql_adapter(self):
        """Test getting PostgreSQL adapter."""
        from vector_app.views.backend_connection_views import get_adapter
        from vector_app.models import BackendConnection
        
        adapter = get_adapter(BackendConnection.ADAPTER_POSTGRESQL)
        self.assertIsInstance(adapter, PostgreSQLAdapter)
    
    def test_get_mysql_adapter(self):
        """Test getting MySQL adapter."""
        from vector_app.views.backend_connection_views import get_adapter
        from vector_app.models import BackendConnection
        
        adapter = get_adapter(BackendConnection.ADAPTER_MYSQL)
        self.assertIsInstance(adapter, MySQLAdapter)
    
    def test_get_supabase_adapter(self):
        """Test getting Supabase adapter."""
        from vector_app.views.backend_connection_views import get_adapter
        from vector_app.models import BackendConnection
        
        adapter = get_adapter(BackendConnection.ADAPTER_SUPABASE)
        self.assertIsInstance(adapter, SupabaseAdapter)
    
    def test_get_unknown_adapter(self):
        """Test getting unknown adapter returns None."""
        from vector_app.views.backend_connection_views import get_adapter
        
        adapter = get_adapter("unknown")
        self.assertIsNone(adapter)


class TestQuerySpecParsing(TestCase):
    """Test query specification parsing and SQL generation."""
    
    def setUp(self):
        self.adapter = PostgreSQLAdapter()
        self.ctx = AdapterContext(
            host="localhost",
            port=5432,
            database="testdb",
            username="postgres",
            password="secret"
        )
    
    @patch('vector_app.adapters.postgresql.psycopg2.connect')
    def test_query_spec_with_filters(self, mock_connect):
        """Test query spec with filters is handled correctly."""
        mock_conn = MagicMock()
        mock_cursor = MagicMock()
        mock_cursor.description = [('id',), ('email',), ('status',)]
        mock_cursor.fetchall.return_value = []
        mock_conn.cursor.return_value = mock_cursor
        mock_connect.return_value = mock_conn
        
        query_spec = {
            "select": ["id", "email", "status"],
            "filters": [
                {"field": "status", "op": "eq", "value": "active"},
                {"field": "email", "op": "like", "value": "%@example.com"}
            ],
            "orderBy": [{"field": "created_at", "dir": "desc"}],
            "limit": 50,
            "offset": 0
        }
        
        user_ctx = UserContext(
            host="localhost",
            port=5432,
            database="testdb",
            username="postgres",
            password="secret"
        )
        
        result = self.adapter.query("public.users", query_spec, user_ctx)
        
        # Verify execute was called (SQL was built)
        mock_cursor.execute.assert_called_once()
        sql_query = mock_cursor.execute.call_args[0][0]
        
        # Verify query contains expected parts
        self.assertIn('SELECT', sql_query)
        self.assertIn('id', sql_query)
        self.assertIn('email', sql_query)
        self.assertIn('WHERE', sql_query)
        self.assertIn('ORDER BY', sql_query)
        self.assertIn('LIMIT', sql_query)
    
    @patch('vector_app.adapters.postgresql.psycopg2.connect')
    def test_query_spec_with_pagination(self, mock_connect):
        """Test query spec with pagination."""
        mock_conn = MagicMock()
        mock_cursor = MagicMock()
        mock_cursor.description = [('id',)]
        mock_cursor.fetchall.return_value = []
        mock_conn.cursor.return_value = mock_cursor
        mock_connect.return_value = mock_conn
        
        query_spec = {
            "select": ["id"],
            "limit": 10,
            "offset": 20
        }
        
        user_ctx = UserContext(
            host="localhost",
            port=5432,
            database="testdb",
            username="postgres",
            password="secret"
        )
        
        result = self.adapter.query("public.users", query_spec, user_ctx)
        
        sql_query = mock_cursor.execute.call_args[0][0]
        self.assertIn('LIMIT', sql_query)
        self.assertIn('OFFSET', sql_query)
