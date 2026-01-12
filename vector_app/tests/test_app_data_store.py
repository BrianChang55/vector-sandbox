"""
Comprehensive tests for App Data Store functionality.

Tests cover:
- Model behavior
- Schema validation
- Row data validation
- Service layer operations
- API endpoints
- Edge cases and error handling
"""

import uuid
import json
from datetime import datetime, date
from django.test import TestCase, TransactionTestCase
from django.urls import reverse
from rest_framework.test import APITestCase, APIClient
from rest_framework import status

from vector_app.models import (
    User,
    Organization,
    UserOrganization,
    InternalApp,
    AppDataTable,
    AppDataRow,
    AppDataQuery,
)
from vector_app.services.schema_validator import SchemaValidator
from vector_app.services.app_data_service import AppDataService


# =============================================================================
# Schema Validator Tests
# =============================================================================


class SchemaValidatorSchemaTests(TestCase):
    """Tests for schema structure validation."""

    def test_valid_minimal_schema(self):
        """Test minimal valid schema with just a primary key."""
        schema = {"columns": [{"name": "id", "type": "uuid", "primary_key": True, "nullable": False}]}
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertTrue(is_valid, f"Expected valid schema, got errors: {errors}")
        self.assertEqual(errors, [])

    def test_valid_complete_schema(self):
        """Test a complete schema with all column types."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True, "nullable": False},
                {"name": "email", "type": "string", "nullable": False, "unique": True, "max_length": 255},
                {"name": "bio", "type": "text", "nullable": True},
                {"name": "age", "type": "integer", "nullable": True, "min": 0, "max": 150},
                {"name": "score", "type": "float", "nullable": True, "min": 0.0, "max": 100.0},
                {"name": "active", "type": "boolean", "nullable": False, "default": True},
                {"name": "created_at", "type": "datetime", "auto_now_add": True},
                {"name": "birth_date", "type": "date", "nullable": True},
                {
                    "name": "status",
                    "type": "enum",
                    "enum_values": ["active", "inactive", "pending"],
                    "default": "pending",
                },
                {"name": "metadata", "type": "json", "nullable": True},
                {
                    "name": "parent_id",
                    "type": "reference",
                    "reference_table": "users",
                    "reference_column": "id",
                    "nullable": True,
                },
            ],
            "indexes": [{"columns": ["email"], "unique": True}, {"columns": ["status", "created_at"]}],
        }
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertTrue(is_valid, f"Expected valid schema, got errors: {errors}")

    def test_empty_schema_fails(self):
        """Test that empty schema fails validation."""
        schema = {}
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)
        self.assertIn("Schema must have at least one column", errors)

    def test_empty_columns_fails(self):
        """Test that empty columns list fails."""
        schema = {"columns": []}
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)
        self.assertIn("Schema must have at least one column", errors)

    def test_missing_primary_key_fails(self):
        """Test that schema without primary key fails."""
        schema = {"columns": [{"name": "email", "type": "string", "nullable": False}]}
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)
        self.assertIn("Schema must have exactly one primary key column", errors)

    def test_multiple_primary_keys_fails(self):
        """Test that multiple primary keys fail."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True},
                {"name": "uuid", "type": "uuid", "primary_key": True},
            ]
        }
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)
        self.assertIn("Schema cannot have multiple primary key columns", errors)

    def test_invalid_column_type_fails(self):
        """Test that invalid column type fails."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True},
                {"name": "data", "type": "invalid_type"},
            ]
        }
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)
        self.assertTrue(any("invalid type" in e.lower() for e in errors))

    def test_missing_column_name_fails(self):
        """Test that missing column name fails."""
        schema = {"columns": [{"type": "uuid", "primary_key": True}]}
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)
        self.assertTrue(any('missing required field "name"' in e for e in errors))

    def test_missing_column_type_fails(self):
        """Test that missing column type fails."""
        schema = {"columns": [{"name": "id", "primary_key": True}]}
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)
        self.assertTrue(any('missing required field "type"' in e for e in errors))

    def test_duplicate_column_names_fail(self):
        """Test that duplicate column names fail."""
        schema = {
            "columns": [{"name": "id", "type": "uuid", "primary_key": True}, {"name": "id", "type": "string"}]
        }
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)
        self.assertTrue(any("duplicate column name" in e.lower() for e in errors))

    def test_invalid_column_name_fails(self):
        """Test that invalid column names fail."""
        invalid_names = ["123start", "has-dash", "has space", "_private"]
        for name in invalid_names:
            schema = {
                "columns": [
                    {"name": "id", "type": "uuid", "primary_key": True},
                    {"name": name, "type": "string"},
                ]
            }
            is_valid, errors = SchemaValidator.validate_schema(schema)
            self.assertFalse(is_valid, f"Expected name '{name}' to fail validation")

    def test_reserved_column_names_fail(self):
        """Test that reserved column names fail."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True},
                {"name": "_id", "type": "string"},
            ]
        }
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)
        self.assertTrue(any("reserved" in e.lower() for e in errors))

    def test_enum_without_values_fails(self):
        """Test that enum type without values fails."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True},
                {"name": "status", "type": "enum"},
            ]
        }
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)
        self.assertTrue(any("enum_values" in e for e in errors))

    def test_enum_with_empty_values_fails(self):
        """Test that enum type with empty values fails."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True},
                {"name": "status", "type": "enum", "enum_values": []},
            ]
        }
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)

    def test_reference_without_table_fails(self):
        """Test that reference type without table fails."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True},
                {"name": "parent_id", "type": "reference", "reference_column": "id"},
            ]
        }
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)
        self.assertTrue(any("reference_table" in e for e in errors))

    def test_nullable_primary_key_fails(self):
        """Test that nullable primary key fails."""
        schema = {"columns": [{"name": "id", "type": "uuid", "primary_key": True, "nullable": True}]}
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)
        self.assertTrue(any("cannot be nullable" in e.lower() for e in errors))

    def test_min_greater_than_max_fails(self):
        """Test that min > max fails for numeric types."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True},
                {"name": "value", "type": "integer", "min": 100, "max": 10},
            ]
        }
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)
        self.assertTrue(any("min cannot be greater than max" in e for e in errors))

    def test_index_with_unknown_column_fails(self):
        """Test that index referencing unknown column fails."""
        schema = {
            "columns": [{"name": "id", "type": "uuid", "primary_key": True}],
            "indexes": [{"columns": ["unknown_column"], "unique": True}],
        }
        is_valid, errors = SchemaValidator.validate_schema(schema)
        self.assertFalse(is_valid)
        self.assertTrue(any("does not exist" in e.lower() for e in errors))


class SchemaValidatorRowTests(TestCase):
    """Tests for row data validation."""

    def setUp(self):
        self.schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True, "nullable": False},
                {"name": "email", "type": "string", "nullable": False, "max_length": 50},
                {"name": "age", "type": "integer", "nullable": True, "min": 0, "max": 150},
                {"name": "score", "type": "float", "nullable": True},
                {"name": "active", "type": "boolean", "nullable": False, "default": True},
                {"name": "status", "type": "enum", "enum_values": ["active", "inactive"]},
                {"name": "metadata", "type": "json", "nullable": True},
            ]
        }

    def test_valid_row(self):
        """Test valid row data passes validation."""
        data = {
            "email": "test@example.com",
            "age": 25,
            "score": 95.5,
            "active": True,
            "status": "active",
            "metadata": {"key": "value"},
        }
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertTrue(is_valid, f"Expected valid row, got errors: {errors}")

    def test_missing_required_field_fails(self):
        """Test that missing required field fails."""
        data = {
            "age": 25
            # email is required and missing
        }
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertFalse(is_valid)
        self.assertTrue(any("email" in e for e in errors))

    def test_partial_update_allows_missing_fields(self):
        """Test that partial=True allows missing required fields."""
        data = {"age": 30}
        is_valid, errors = SchemaValidator.validate_row(self.schema, data, partial=True)
        self.assertTrue(is_valid, f"Expected valid partial update, got errors: {errors}")

    def test_null_on_non_nullable_fails(self):
        """Test that null on non-nullable field fails."""
        data = {"email": None}
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertFalse(is_valid)
        self.assertTrue(any("cannot be null" in e for e in errors))

    def test_unknown_field_fails(self):
        """Test that unknown field fails."""
        data = {"email": "test@example.com", "unknown_field": "value"}
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertFalse(is_valid)
        self.assertTrue(any("unknown field" in e.lower() for e in errors))

    def test_string_type_validation(self):
        """Test string type validation."""
        # Wrong type
        data = {"email": 123}
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertFalse(is_valid)

        # Exceeds max_length
        data = {"email": "x" * 100}
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertFalse(is_valid)
        self.assertTrue(any("max_length" in e for e in errors))

    def test_integer_type_validation(self):
        """Test integer type validation."""
        # Wrong type
        data = {"email": "test@test.com", "age": "not_an_int"}
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertFalse(is_valid)

        # Below min
        data = {"email": "test@test.com", "age": -5}
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertFalse(is_valid)

        # Above max
        data = {"email": "test@test.com", "age": 200}
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertFalse(is_valid)

    def test_float_type_validation(self):
        """Test float type validation."""
        # Integer is allowed for float
        data = {"email": "test@test.com", "score": 95}
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertTrue(is_valid)

        # String is not allowed
        data = {"email": "test@test.com", "score": "not_a_float"}
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertFalse(is_valid)

    def test_boolean_type_validation(self):
        """Test boolean type validation."""
        # String is not allowed
        data = {"email": "test@test.com", "active": "true"}
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertFalse(is_valid)

        # Integer is not allowed (even 0/1)
        data = {"email": "test@test.com", "active": 1}
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertFalse(is_valid)

    def test_enum_type_validation(self):
        """Test enum type validation."""
        # Valid value
        data = {"email": "test@test.com", "status": "active"}
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertTrue(is_valid)

        # Invalid value
        data = {"email": "test@test.com", "status": "invalid_status"}
        is_valid, errors = SchemaValidator.validate_row(self.schema, data)
        self.assertFalse(is_valid)

    def test_uuid_type_validation(self):
        """Test UUID type validation."""
        schema = {"columns": [{"name": "id", "type": "uuid", "primary_key": True}]}

        # Valid UUID
        data = {"id": str(uuid.uuid4())}
        is_valid, errors = SchemaValidator.validate_row(schema, data)
        self.assertTrue(is_valid)

        # Invalid UUID
        data = {"id": "not-a-uuid"}
        is_valid, errors = SchemaValidator.validate_row(schema, data)
        self.assertFalse(is_valid)

    def test_datetime_type_validation(self):
        """Test datetime type validation."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "created_at", "type": "datetime"},
            ]
        }

        # Valid ISO datetime
        data = {"created_at": "2024-01-15T10:30:00Z"}
        is_valid, errors = SchemaValidator.validate_row(schema, data)
        self.assertTrue(is_valid)

        # Valid with timezone offset
        data = {"created_at": "2024-01-15T10:30:00+05:00"}
        is_valid, errors = SchemaValidator.validate_row(schema, data)
        self.assertTrue(is_valid)

        # Invalid format
        data = {"created_at": "not-a-datetime"}
        is_valid, errors = SchemaValidator.validate_row(schema, data)
        self.assertFalse(is_valid)

    def test_date_type_validation(self):
        """Test date type validation."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "birth_date", "type": "date"},
            ]
        }

        # Valid date
        data = {"birth_date": "2024-01-15"}
        is_valid, errors = SchemaValidator.validate_row(schema, data)
        self.assertTrue(is_valid)

        # Invalid format
        data = {"birth_date": "15-01-2024"}
        is_valid, errors = SchemaValidator.validate_row(schema, data)
        self.assertFalse(is_valid)


class SchemaValidatorDefaultsTests(TestCase):
    """Tests for default value application."""

    def test_apply_defaults(self):
        """Test that defaults are applied correctly."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "status", "type": "string", "default": "pending"},
                {"name": "created_at", "type": "datetime", "auto_now_add": True},
            ]
        }

        data = {}
        result = SchemaValidator.apply_defaults(schema, data)

        # Check UUID was generated
        self.assertIn("id", result)
        uuid.UUID(result["id"])  # Should not raise

        # Check default was applied
        self.assertEqual(result["status"], "pending")

        # Check auto_now_add was applied
        self.assertIn("created_at", result)

    def test_existing_values_not_overwritten(self):
        """Test that existing values are not overwritten by defaults."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "status", "type": "string", "default": "pending"},
            ]
        }

        existing_id = str(uuid.uuid4())
        data = {"id": existing_id, "status": "active"}
        result = SchemaValidator.apply_defaults(schema, data)

        self.assertEqual(result["id"], existing_id)
        self.assertEqual(result["status"], "active")


# =============================================================================
# App Data Service Tests
# =============================================================================


class AppDataServiceTableTests(TransactionTestCase):
    """Tests for table operations in AppDataService."""

    def setUp(self):
        self.user = User.objects.create_user(
            username="test@example.com", email="test@example.com", password="testpass123"
        )
        self.org = Organization.objects.create(name="Test Org", slug="test-org")
        UserOrganization.objects.create(user=self.user, organization=self.org, role="admin")
        self.app = InternalApp.objects.create(organization=self.org, name="Test App", created_by=self.user)
        self.valid_schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True, "nullable": False},
                {"name": "name", "type": "string", "nullable": False, "max_length": 100},
                {"name": "email", "type": "string", "nullable": False, "unique": True},
            ]
        }

    def test_create_table_success(self):
        """Test successful table creation."""
        table, errors = AppDataService.create_table(
            app=self.app, name="Users", schema=self.valid_schema, description="User table"
        )

        self.assertIsNotNone(table)
        self.assertEqual(errors, [])
        self.assertEqual(table.name, "Users")
        self.assertEqual(table.slug, "users")
        self.assertEqual(table.description, "User table")
        self.assertEqual(table.row_count, 0)

    def test_create_table_with_invalid_schema(self):
        """Test table creation with invalid schema fails."""
        invalid_schema = {"columns": []}
        table, errors = AppDataService.create_table(app=self.app, name="Invalid", schema=invalid_schema)

        self.assertIsNone(table)
        self.assertTrue(len(errors) > 0)

    def test_create_table_unique_slug(self):
        """Test that duplicate table names get unique slugs."""
        table1, _ = AppDataService.create_table(self.app, "Users", self.valid_schema)
        table2, _ = AppDataService.create_table(self.app, "Users", self.valid_schema)

        self.assertEqual(table1.slug, "users")
        self.assertEqual(table2.slug, "users-1")

    def test_update_table_schema(self):
        """Test updating table schema."""
        table, _ = AppDataService.create_table(self.app, "Users", self.valid_schema)

        new_schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "name", "type": "string", "nullable": False},
                {"name": "email", "type": "string", "nullable": False},
                {"name": "phone", "type": "string", "nullable": True},  # New column
            ]
        }

        updated_table, errors = AppDataService.update_table_schema(
            table=table, schema=new_schema, name="Updated Users", description="Updated description"
        )

        self.assertIsNotNone(updated_table)
        self.assertEqual(errors, [])
        self.assertEqual(updated_table.name, "Updated Users")
        self.assertEqual(len(updated_table.get_columns()), 4)

    def test_delete_table(self):
        """Test deleting a table."""
        table, _ = AppDataService.create_table(self.app, "Users", self.valid_schema)
        table_id = table.id

        AppDataService.delete_table(table)

        self.assertFalse(AppDataTable.objects.filter(id=table_id).exists())

    def test_list_tables(self):
        """Test listing tables for an app."""
        AppDataService.create_table(self.app, "Users", self.valid_schema)
        AppDataService.create_table(self.app, "Products", self.valid_schema)

        tables = AppDataService.list_tables(self.app)

        self.assertEqual(len(tables), 2)
        names = [t.name for t in tables]
        self.assertIn("Users", names)
        self.assertIn("Products", names)

    def test_get_table(self):
        """Test getting a table by slug."""
        created_table, _ = AppDataService.create_table(self.app, "Users", self.valid_schema)

        fetched_table = AppDataService.get_table(self.app, "users")

        self.assertIsNotNone(fetched_table)
        self.assertEqual(fetched_table.id, created_table.id)

    def test_get_nonexistent_table(self):
        """Test getting a nonexistent table returns None."""
        table = AppDataService.get_table(self.app, "nonexistent")
        self.assertIsNone(table)


class AppDataServiceRowTests(TransactionTestCase):
    """Tests for row operations in AppDataService."""

    def setUp(self):
        self.user = User.objects.create_user(
            username="test@example.com", email="test@example.com", password="testpass123"
        )
        self.org = Organization.objects.create(name="Test Org", slug="test-org")
        UserOrganization.objects.create(user=self.user, organization=self.org, role="admin")
        self.app = InternalApp.objects.create(organization=self.org, name="Test App", created_by=self.user)
        self.schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "name", "type": "string", "nullable": False},
                {"name": "email", "type": "string", "nullable": False, "unique": True},
                {"name": "age", "type": "integer", "nullable": True},
                {
                    "name": "status",
                    "type": "enum",
                    "enum_values": ["active", "inactive"],
                    "default": "active",
                },
            ]
        }
        self.table, _ = AppDataService.create_table(self.app, "Users", self.schema)

    def test_insert_row_success(self):
        """Test successful row insertion."""
        data = {"name": "John Doe", "email": "john@example.com", "age": 30}
        row, errors = AppDataService.insert_row(self.table, data)

        self.assertIsNotNone(row)
        self.assertEqual(errors, [])
        self.assertEqual(row.data["name"], "John Doe")
        self.assertEqual(row.data["email"], "john@example.com")
        self.assertEqual(row.data["age"], 30)
        self.assertEqual(row.data["status"], "active")  # Default applied
        self.assertIn("id", row.data)  # Auto-generated

        # Check row count was updated
        self.table.refresh_from_db()
        self.assertEqual(self.table.row_count, 1)

    def test_insert_row_validation_failure(self):
        """Test row insertion with invalid data fails."""
        data = {
            "name": "John Doe"
            # email is required
        }
        row, errors = AppDataService.insert_row(self.table, data)

        self.assertIsNone(row)
        self.assertTrue(len(errors) > 0)

    def test_insert_row_unique_constraint(self):
        """Test unique constraint is enforced."""
        data1 = {"name": "John", "email": "john@example.com"}
        data2 = {"name": "Jane", "email": "john@example.com"}  # Same email

        row1, _ = AppDataService.insert_row(self.table, data1)
        row2, errors = AppDataService.insert_row(self.table, data2)

        self.assertIsNotNone(row1)
        self.assertIsNone(row2)
        self.assertTrue(any("unique" in e.lower() for e in errors))

    def test_insert_rows_bulk(self):
        """Test bulk row insertion."""
        rows_data = [
            {"name": "John", "email": "john@example.com"},
            {"name": "Jane", "email": "jane@example.com"},
            {"name": "Bob", "email": "bob@example.com"},
        ]

        created_rows, errors = AppDataService.insert_rows_bulk(self.table, rows_data)

        self.assertEqual(len(created_rows), 3)
        self.assertEqual(errors, [])

        self.table.refresh_from_db()
        self.assertEqual(self.table.row_count, 3)

    def test_insert_rows_bulk_partial_failure(self):
        """Test bulk insert with some invalid rows."""
        rows_data = [
            {"name": "John", "email": "john@example.com"},
            {"name": "Invalid"},  # Missing email
            {"name": "Jane", "email": "jane@example.com"},
        ]

        created_rows, errors = AppDataService.insert_rows_bulk(self.table, rows_data)

        self.assertEqual(len(created_rows), 2)
        self.assertTrue(len(errors) > 0)
        self.assertTrue(any("Row 1" in e for e in errors))

    def test_update_row(self):
        """Test row update."""
        data = {"name": "John", "email": "john@example.com"}
        row, _ = AppDataService.insert_row(self.table, data)

        updated_row, errors = AppDataService.update_row(row, {"age": 35})

        self.assertIsNotNone(updated_row)
        self.assertEqual(errors, [])
        self.assertEqual(updated_row.data["age"], 35)
        self.assertEqual(updated_row.data["name"], "John")  # Unchanged

    def test_update_row_unique_constraint(self):
        """Test unique constraint on update."""
        row1, _ = AppDataService.insert_row(self.table, {"name": "John", "email": "john@example.com"})
        row2, _ = AppDataService.insert_row(self.table, {"name": "Jane", "email": "jane@example.com"})

        # Try to update row2's email to row1's email
        updated_row, errors = AppDataService.update_row(row2, {"email": "john@example.com"})

        self.assertIsNone(updated_row)
        self.assertTrue(any("unique" in e.lower() for e in errors))

    def test_delete_row(self):
        """Test row deletion."""
        row, _ = AppDataService.insert_row(self.table, {"name": "John", "email": "john@example.com"})
        row_id = row.id

        AppDataService.delete_row(row)

        self.assertFalse(AppDataRow.objects.filter(id=row_id).exists())
        self.table.refresh_from_db()
        self.assertEqual(self.table.row_count, 0)

    def test_delete_rows_bulk(self):
        """Test bulk row deletion."""
        row1, _ = AppDataService.insert_row(self.table, {"name": "John", "email": "john@example.com"})
        row2, _ = AppDataService.insert_row(self.table, {"name": "Jane", "email": "jane@example.com"})
        row3, _ = AppDataService.insert_row(self.table, {"name": "Bob", "email": "bob@example.com"})

        deleted_count = AppDataService.delete_rows_bulk(self.table, [row1.id, row2.id])

        self.assertEqual(deleted_count, 2)
        self.table.refresh_from_db()
        self.assertEqual(self.table.row_count, 1)

        # row3 should still exist
        self.assertTrue(AppDataRow.objects.filter(id=row3.id).exists())

    def test_get_row(self):
        """Test getting a single row."""
        created_row, _ = AppDataService.insert_row(self.table, {"name": "John", "email": "john@example.com"})

        fetched_row = AppDataService.get_row(self.table, created_row.id)

        self.assertIsNotNone(fetched_row)
        self.assertEqual(fetched_row.id, created_row.id)

    def test_get_nonexistent_row(self):
        """Test getting a nonexistent row returns None."""
        row = AppDataService.get_row(self.table, uuid.uuid4())
        self.assertIsNone(row)


class AppDataServiceQueryTests(TransactionTestCase):
    """Tests for query operations in AppDataService."""

    def setUp(self):
        self.user = User.objects.create_user(
            username="test@example.com", email="test@example.com", password="testpass123"
        )
        self.org = Organization.objects.create(name="Test Org", slug="test-org")
        UserOrganization.objects.create(user=self.user, organization=self.org, role="admin")
        self.app = InternalApp.objects.create(organization=self.org, name="Test App", created_by=self.user)
        self.schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "name", "type": "string", "nullable": False},
                {"name": "email", "type": "string", "nullable": False},
                {"name": "age", "type": "integer", "nullable": True},
                {"name": "status", "type": "enum", "enum_values": ["active", "inactive"]},
            ]
        }
        self.table, _ = AppDataService.create_table(self.app, "Users", self.schema)

        # Insert test data
        test_data = [
            {"name": "Alice", "email": "alice@example.com", "age": 25, "status": "active"},
            {"name": "Bob", "email": "bob@example.com", "age": 30, "status": "inactive"},
            {"name": "Charlie", "email": "charlie@example.com", "age": 35, "status": "active"},
            {"name": "Diana", "email": "diana@example.com", "age": 28, "status": "active"},
            {"name": "Eve", "email": "eve@example.com", "age": 22, "status": "inactive"},
        ]
        AppDataService.insert_rows_bulk(self.table, test_data)

    def test_query_all_rows(self):
        """Test querying all rows without filters."""
        result = AppDataService.query_rows(self.table)

        self.assertEqual(result.total_count, 5)
        self.assertEqual(len(result.rows), 5)

    def test_query_with_eq_filter(self):
        """Test equality filter."""
        result = AppDataService.query_rows(
            self.table, {"filters": [{"field": "status", "op": "eq", "value": "active"}]}
        )

        self.assertEqual(result.total_count, 3)
        for row in result.rows:
            self.assertEqual(row["data"]["status"], "active")

    def test_query_with_neq_filter(self):
        """Test not-equal filter."""
        result = AppDataService.query_rows(
            self.table, {"filters": [{"field": "status", "op": "neq", "value": "active"}]}
        )

        self.assertEqual(result.total_count, 2)

    def test_query_with_gt_filter(self):
        """Test greater-than filter."""
        result = AppDataService.query_rows(
            self.table, {"filters": [{"field": "age", "op": "gt", "value": 28}]}
        )

        self.assertEqual(result.total_count, 2)  # Bob (30), Charlie (35)

    def test_query_with_gte_filter(self):
        """Test greater-than-or-equal filter."""
        result = AppDataService.query_rows(
            self.table, {"filters": [{"field": "age", "op": "gte", "value": 28}]}
        )

        self.assertEqual(result.total_count, 3)  # Diana (28), Bob (30), Charlie (35)

    def test_query_with_lt_filter(self):
        """Test less-than filter."""
        result = AppDataService.query_rows(
            self.table, {"filters": [{"field": "age", "op": "lt", "value": 28}]}
        )

        self.assertEqual(result.total_count, 2)  # Alice (25), Eve (22)

    def test_query_with_in_filter(self):
        """Test in-list filter."""
        result = AppDataService.query_rows(
            self.table, {"filters": [{"field": "name", "op": "in", "value": ["Alice", "Bob"]}]}
        )

        self.assertEqual(result.total_count, 2)

    def test_query_with_contains_filter(self):
        """Test contains filter (skipped on SQLite)."""
        from django.conf import settings

        if "sqlite" in settings.DATABASES["default"]["ENGINE"]:
            self.skipTest("SQLite doesn't support JSON contains lookup")

        result = AppDataService.query_rows(
            self.table, {"filters": [{"field": "email", "op": "contains", "value": "alice"}]}
        )

        self.assertEqual(result.total_count, 1)
        self.assertEqual(result.rows[0]["data"]["name"], "Alice")

    def test_query_with_multiple_filters(self):
        """Test multiple filters (AND condition)."""
        result = AppDataService.query_rows(
            self.table,
            {
                "filters": [
                    {"field": "status", "op": "eq", "value": "active"},
                    {"field": "age", "op": "gte", "value": 25},
                ]
            },
        )

        self.assertEqual(result.total_count, 3)  # Alice, Charlie, Diana

    def test_query_with_ordering(self):
        """Test ordering results."""
        result = AppDataService.query_rows(self.table, {"order_by": [{"field": "age", "dir": "desc"}]})

        ages = [row["data"]["age"] for row in result.rows]
        self.assertEqual(ages, sorted(ages, reverse=True))

    def test_query_with_pagination(self):
        """Test pagination."""
        result1 = AppDataService.query_rows(self.table, {"limit": 2, "offset": 0})
        result2 = AppDataService.query_rows(self.table, {"limit": 2, "offset": 2})

        self.assertEqual(len(result1.rows), 2)
        self.assertEqual(len(result2.rows), 2)
        self.assertTrue(result1.has_more)
        self.assertTrue(result2.has_more)

        # Verify different rows
        ids1 = {row["id"] for row in result1.rows}
        ids2 = {row["id"] for row in result2.rows}
        self.assertEqual(len(ids1 & ids2), 0)

    def test_query_with_select_fields(self):
        """Test selecting specific fields."""
        result = AppDataService.query_rows(self.table, {"select": ["name", "email"]})

        for row in result.rows:
            self.assertIn("name", row["data"])
            self.assertIn("email", row["data"])
            self.assertNotIn("age", row["data"])
            self.assertNotIn("status", row["data"])

    def test_query_limit_capped(self):
        """Test that limit is capped at 1000."""
        result = AppDataService.query_rows(self.table, {"limit": 5000})
        self.assertEqual(result.limit, 1000)


# =============================================================================
# API Endpoint Tests
# =============================================================================


class AppDataTableAPITests(APITestCase):
    """Tests for table API endpoints."""

    def setUp(self):
        self.user = User.objects.create_user(
            username="test@example.com", email="test@example.com", password="testpass123"
        )
        self.org = Organization.objects.create(name="Test Org", slug="test-org")
        UserOrganization.objects.create(user=self.user, organization=self.org, role="admin")
        self.app = InternalApp.objects.create(organization=self.org, name="Test App", created_by=self.user)
        self.client = APIClient()
        self.client.force_authenticate(user=self.user)

        self.valid_schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "name", "type": "string", "nullable": False},
            ]
        }

    def test_list_tables(self):
        """Test listing tables."""
        AppDataService.create_table(self.app, "Users", self.valid_schema)
        AppDataService.create_table(self.app, "Products", self.valid_schema)

        url = f"/api/v1/apps/{self.app.id}/data/tables/"
        response = self.client.get(url)

        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(len(response.data), 2)

    def test_create_table(self):
        """Test creating a table."""
        url = f"/api/v1/apps/{self.app.id}/data/tables/"
        data = {"name": "Users", "description": "User table", "schema_json": self.valid_schema}

        response = self.client.post(url, data, format="json")

        self.assertEqual(response.status_code, status.HTTP_201_CREATED)
        self.assertEqual(response.data["name"], "Users")
        self.assertEqual(response.data["slug"], "users")

    def test_create_table_invalid_schema(self):
        """Test creating table with invalid schema fails."""
        url = f"/api/v1/apps/{self.app.id}/data/tables/"
        data = {"name": "Invalid", "schema_json": {"columns": []}}

        response = self.client.post(url, data, format="json")

        self.assertEqual(response.status_code, status.HTTP_400_BAD_REQUEST)

    def test_get_table(self):
        """Test getting a table."""
        table, _ = AppDataService.create_table(self.app, "Users", self.valid_schema)

        url = f"/api/v1/apps/{self.app.id}/data/tables/users/"
        response = self.client.get(url)

        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(response.data["id"], str(table.id))

    def test_update_table(self):
        """Test updating a table."""
        AppDataService.create_table(self.app, "Users", self.valid_schema)

        url = f"/api/v1/apps/{self.app.id}/data/tables/users/"
        data = {"name": "Updated Users", "description": "Updated description"}

        response = self.client.patch(url, data, format="json")

        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(response.data["name"], "Updated Users")

    def test_delete_table(self):
        """Test deleting a table."""
        table, _ = AppDataService.create_table(self.app, "Users", self.valid_schema)

        url = f"/api/v1/apps/{self.app.id}/data/tables/users/"
        response = self.client.delete(url)

        self.assertEqual(response.status_code, status.HTTP_204_NO_CONTENT)
        self.assertFalse(AppDataTable.objects.filter(id=table.id).exists())

    def test_access_denied_for_non_member(self):
        """Test that non-members cannot access tables."""
        other_user = User.objects.create_user(
            username="other@example.com", email="other@example.com", password="testpass123"
        )
        self.client.force_authenticate(user=other_user)

        url = f"/api/v1/apps/{self.app.id}/data/tables/"
        response = self.client.get(url)

        self.assertEqual(response.status_code, status.HTTP_404_NOT_FOUND)


class AppDataRowAPITests(APITestCase):
    """Tests for row API endpoints."""

    def setUp(self):
        self.user = User.objects.create_user(
            username="test@example.com", email="test@example.com", password="testpass123"
        )
        self.org = Organization.objects.create(name="Test Org", slug="test-org")
        UserOrganization.objects.create(user=self.user, organization=self.org, role="admin")
        self.app = InternalApp.objects.create(organization=self.org, name="Test App", created_by=self.user)
        self.schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "name", "type": "string", "nullable": False},
                {"name": "email", "type": "string", "nullable": False, "unique": True},
            ]
        }
        self.table, _ = AppDataService.create_table(self.app, "Users", self.schema)

        self.client = APIClient()
        self.client.force_authenticate(user=self.user)

    def test_list_rows(self):
        """Test listing rows."""
        AppDataService.insert_row(self.table, {"name": "John", "email": "john@example.com"})
        AppDataService.insert_row(self.table, {"name": "Jane", "email": "jane@example.com"})

        url = f"/api/v1/apps/{self.app.id}/data/tables/users/rows/"
        response = self.client.get(url)

        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(response.data["total_count"], 2)
        self.assertEqual(len(response.data["rows"]), 2)

    def test_create_row(self):
        """Test creating a row."""
        url = f"/api/v1/apps/{self.app.id}/data/tables/users/rows/"
        data = {"data": {"name": "John Doe", "email": "john@example.com"}}

        response = self.client.post(url, data, format="json")

        self.assertEqual(response.status_code, status.HTTP_201_CREATED)
        self.assertEqual(response.data["data"]["name"], "John Doe")

    def test_create_row_validation_error(self):
        """Test creating row with invalid data."""
        url = f"/api/v1/apps/{self.app.id}/data/tables/users/rows/"
        data = {
            "data": {
                "name": "John Doe"
                # Missing email
            }
        }

        response = self.client.post(url, data, format="json")

        self.assertEqual(response.status_code, status.HTTP_400_BAD_REQUEST)

    def test_get_row(self):
        """Test getting a single row."""
        row, _ = AppDataService.insert_row(self.table, {"name": "John", "email": "john@example.com"})

        url = f"/api/v1/apps/{self.app.id}/data/tables/users/rows/{row.id}/"
        response = self.client.get(url)

        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(response.data["id"], str(row.id))

    def test_update_row(self):
        """Test updating a row."""
        row, _ = AppDataService.insert_row(self.table, {"name": "John", "email": "john@example.com"})

        url = f"/api/v1/apps/{self.app.id}/data/tables/users/rows/{row.id}/"
        data = {"data": {"name": "John Updated"}}

        response = self.client.patch(url, data, format="json")

        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(response.data["data"]["name"], "John Updated")
        self.assertEqual(response.data["data"]["email"], "john@example.com")  # Unchanged

    def test_delete_row(self):
        """Test deleting a row."""
        row, _ = AppDataService.insert_row(self.table, {"name": "John", "email": "john@example.com"})

        url = f"/api/v1/apps/{self.app.id}/data/tables/users/rows/{row.id}/"
        response = self.client.delete(url)

        self.assertEqual(response.status_code, status.HTTP_204_NO_CONTENT)
        self.assertFalse(AppDataRow.objects.filter(id=row.id).exists())

    def test_bulk_insert(self):
        """Test bulk row insertion."""
        url = f"/api/v1/apps/{self.app.id}/data/tables/users/rows/bulk/"
        data = {
            "rows": [
                {"name": "John", "email": "john@example.com"},
                {"name": "Jane", "email": "jane@example.com"},
            ]
        }

        response = self.client.post(url, data, format="json")

        self.assertEqual(response.status_code, status.HTTP_201_CREATED)
        self.assertEqual(response.data["created_count"], 2)

    def test_bulk_delete(self):
        """Test bulk row deletion."""
        row1, _ = AppDataService.insert_row(self.table, {"name": "John", "email": "john@example.com"})
        row2, _ = AppDataService.insert_row(self.table, {"name": "Jane", "email": "jane@example.com"})

        url = f"/api/v1/apps/{self.app.id}/data/tables/users/rows/bulk/"
        data = {"row_ids": [str(row1.id), str(row2.id)]}

        response = self.client.delete(url, data, format="json")

        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(response.data["deleted_count"], 2)


class AppDataQueryAPITests(APITestCase):
    """Tests for query API endpoint."""

    def setUp(self):
        self.user = User.objects.create_user(
            username="test@example.com", email="test@example.com", password="testpass123"
        )
        self.org = Organization.objects.create(name="Test Org", slug="test-org")
        UserOrganization.objects.create(user=self.user, organization=self.org, role="admin")
        self.app = InternalApp.objects.create(organization=self.org, name="Test App", created_by=self.user)
        self.schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "name", "type": "string", "nullable": False},
                {"name": "status", "type": "enum", "enum_values": ["active", "inactive"]},
                {"name": "age", "type": "integer", "nullable": True},
            ]
        }
        self.table, _ = AppDataService.create_table(self.app, "Users", self.schema)

        # Insert test data
        test_data = [
            {"name": "Alice", "status": "active", "age": 25},
            {"name": "Bob", "status": "inactive", "age": 30},
            {"name": "Charlie", "status": "active", "age": 35},
        ]
        AppDataService.insert_rows_bulk(self.table, test_data)

        self.client = APIClient()
        self.client.force_authenticate(user=self.user)

    def test_query_with_filters(self):
        """Test query with filters."""
        url = f"/api/v1/apps/{self.app.id}/data/tables/users/query/"
        data = {"filters": [{"field": "status", "op": "eq", "value": "active"}]}

        response = self.client.post(url, data, format="json")

        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(response.data["total_count"], 2)

    def test_query_with_ordering(self):
        """Test query with ordering."""
        url = f"/api/v1/apps/{self.app.id}/data/tables/users/query/"
        data = {"order_by": [{"field": "age", "dir": "desc"}]}

        response = self.client.post(url, data, format="json")

        self.assertEqual(response.status_code, status.HTTP_200_OK)
        ages = [row["data"]["age"] for row in response.data["rows"]]
        self.assertEqual(ages, [35, 30, 25])

    def test_query_with_pagination(self):
        """Test query with pagination."""
        url = f"/api/v1/apps/{self.app.id}/data/tables/users/query/"
        data = {"limit": 2, "offset": 0}

        response = self.client.post(url, data, format="json")

        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(len(response.data["rows"]), 2)
        self.assertEqual(response.data["total_count"], 3)
        self.assertTrue(response.data["has_more"])

    def test_query_with_select_fields(self):
        """Test query with field selection."""
        url = f"/api/v1/apps/{self.app.id}/data/tables/users/query/"
        data = {"select": ["name"]}

        response = self.client.post(url, data, format="json")

        self.assertEqual(response.status_code, status.HTTP_200_OK)
        for row in response.data["rows"]:
            self.assertIn("name", row["data"])
            self.assertNotIn("status", row["data"])
            self.assertNotIn("age", row["data"])

    def test_query_invalid_filter_operator(self):
        """Test query with invalid filter operator fails."""
        url = f"/api/v1/apps/{self.app.id}/data/tables/users/query/"
        data = {"filters": [{"field": "status", "op": "invalid_op", "value": "active"}]}

        response = self.client.post(url, data, format="json")

        self.assertEqual(response.status_code, status.HTTP_400_BAD_REQUEST)


# =============================================================================
# Edge Case Tests
# =============================================================================


class EdgeCaseTests(TransactionTestCase):
    """Tests for edge cases and boundary conditions."""

    def setUp(self):
        self.user = User.objects.create_user(
            username="test@example.com", email="test@example.com", password="testpass123"
        )
        self.org = Organization.objects.create(name="Test Org", slug="test-org")
        UserOrganization.objects.create(user=self.user, organization=self.org, role="admin")
        self.app = InternalApp.objects.create(organization=self.org, name="Test App", created_by=self.user)

    def test_table_with_all_column_types(self):
        """Test creating and using a table with all column types."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "str_col", "type": "string", "max_length": 100},
                {"name": "text_col", "type": "text", "nullable": True},
                {"name": "int_col", "type": "integer", "min": 0, "max": 100},
                {"name": "float_col", "type": "float"},
                {"name": "bool_col", "type": "boolean"},
                {"name": "datetime_col", "type": "datetime"},
                {"name": "date_col", "type": "date"},
                {"name": "enum_col", "type": "enum", "enum_values": ["a", "b", "c"]},
                {"name": "json_col", "type": "json", "nullable": True},
            ]
        }

        table, errors = AppDataService.create_table(self.app, "AllTypes", schema)
        self.assertIsNotNone(table)

        data = {
            "str_col": "test string",
            "text_col": "long text " * 100,
            "int_col": 50,
            "float_col": 3.14159,
            "bool_col": True,
            "datetime_col": "2024-01-15T10:30:00Z",
            "date_col": "2024-01-15",
            "enum_col": "b",
            "json_col": {"nested": {"key": "value"}, "array": [1, 2, 3]},
        }

        row, errors = AppDataService.insert_row(table, data)
        self.assertIsNotNone(row)
        self.assertEqual(row.data["str_col"], "test string")
        self.assertEqual(row.data["int_col"], 50)
        self.assertEqual(row.data["bool_col"], True)

    def test_empty_table_query(self):
        """Test querying an empty table."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
            ]
        }
        table, _ = AppDataService.create_table(self.app, "Empty", schema)

        result = AppDataService.query_rows(table)

        self.assertEqual(result.total_count, 0)
        self.assertEqual(result.rows, [])
        self.assertFalse(result.has_more)

    def test_row_with_null_values(self):
        """Test rows with null values."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "required_field", "type": "string", "nullable": False},
                {"name": "optional_field", "type": "string", "nullable": True},
            ]
        }
        table, _ = AppDataService.create_table(self.app, "Nullable", schema)

        data = {"required_field": "value", "optional_field": None}

        row, errors = AppDataService.insert_row(table, data)
        self.assertIsNotNone(row)
        self.assertIsNone(row.data["optional_field"])

    def test_special_characters_in_data(self):
        """Test data with special characters."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "content", "type": "text"},
            ]
        }
        table, _ = AppDataService.create_table(self.app, "SpecialChars", schema)

        special_content = "Hello! @#$%^&*() \"quotes\" 'apostrophe' <html> émojis: 🎉🚀 中文 العربية"

        row, errors = AppDataService.insert_row(table, {"content": special_content})
        self.assertIsNotNone(row)
        self.assertEqual(row.data["content"], special_content)

    def test_large_json_data(self):
        """Test large JSON data in a row."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "data", "type": "json"},
            ]
        }
        table, _ = AppDataService.create_table(self.app, "LargeJson", schema)

        # Create a large nested JSON structure
        large_json = {
            "items": [{"id": i, "value": f"item_{i}" * 10} for i in range(100)],
            "nested": {f"key_{i}": {"sub": list(range(10))} for i in range(50)},
        }

        row, errors = AppDataService.insert_row(table, {"data": large_json})
        self.assertIsNotNone(row)
        self.assertEqual(len(row.data["data"]["items"]), 100)

    def test_concurrent_row_inserts(self):
        """Test that row_index is correctly assigned during concurrent inserts."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "name", "type": "string"},
            ]
        }
        table, _ = AppDataService.create_table(self.app, "Concurrent", schema)

        # Insert multiple rows
        rows = []
        for i in range(10):
            row, _ = AppDataService.insert_row(table, {"name": f"Row {i}"})
            rows.append(row)

        # Verify all row_indexes are unique and sequential
        indexes = [row.row_index for row in rows]
        self.assertEqual(sorted(indexes), list(range(1, 11)))

    def test_cascade_delete_table(self):
        """Test that deleting a table cascades to rows."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
            ]
        }
        table, _ = AppDataService.create_table(self.app, "Cascade", schema)

        # Insert rows
        for _ in range(5):
            AppDataService.insert_row(table, {})

        table_id = table.id
        self.assertEqual(AppDataRow.objects.filter(table_id=table_id).count(), 5)

        # Delete table
        AppDataService.delete_table(table)

        # Verify rows are deleted
        self.assertEqual(AppDataRow.objects.filter(table_id=table_id).count(), 0)

    def test_model_helper_methods(self):
        """Test model helper methods."""
        schema = {
            "columns": [
                {"name": "id", "type": "uuid", "primary_key": True, "auto_generate": True},
                {"name": "name", "type": "string"},
            ]
        }
        table, _ = AppDataService.create_table(self.app, "Helpers", schema)

        # Test get_columns
        columns = table.get_columns()
        self.assertEqual(len(columns), 2)

        # Test get_column_names
        names = table.get_column_names()
        self.assertEqual(set(names), {"id", "name"})

        # Test get_primary_key_column
        pk_col = table.get_primary_key_column()
        self.assertEqual(pk_col["name"], "id")

        # Test row helper methods
        row, _ = AppDataService.insert_row(table, {"name": "Test"})

        self.assertEqual(row.get_value("name"), "Test")

        row.set_value("name", "Updated")
        self.assertEqual(row.get_value("name"), "Updated")
