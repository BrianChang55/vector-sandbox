
"""
Tests for TypeScript type generator.
These tests verify that:
1. Generated TypeScript syntax is valid
2. Types compile with tsc (TypeScript compiler)
3. Types are correctly structured
4. Types can be imported and used in other files
"""
import os
import json
import tempfile
import subprocess
from pathlib import Path
from unittest import TestCase, skipIf
from unittest.mock import Mock

from vector_app.services.typescript_types_generator import (
    generate_typescript_types,
    _generate_table_type,
    _map_column_type_to_typescript,
    _to_pascal_case,
)


def has_typescript_compiler() -> bool:
    """Check if TypeScript compiler is available."""
    try:
        subprocess.run(['tsc', '--version'], capture_output=True, check=True)
        return True
    except (subprocess.CalledProcessError, FileNotFoundError):
        return False


class TestPascalCaseConversion(TestCase):
    """Test slug to PascalCase conversion."""

    def test_kebab_case(self):
        """Test conversion from kebab-case."""
        self.assertEqual(_to_pascal_case('task-items'), 'TaskItems')
        self.assertEqual(_to_pascal_case('user-profiles'), 'UserProfiles')
        self.assertEqual(_to_pascal_case('simple'), 'Simple')

    def test_snake_case(self):
        """Test conversion from snake_case."""
        self.assertEqual(_to_pascal_case('task_items'), 'TaskItems')
        self.assertEqual(_to_pascal_case('user_profiles'), 'UserProfiles')

    def test_mixed_case(self):
        """Test conversion from mixed separators."""
        self.assertEqual(_to_pascal_case('task-items_data'), 'TaskItemsData')


class TestColumnTypeMapping(TestCase):
    """Test column type to TypeScript type mapping."""

    def test_basic_types(self):
        """Test basic type mappings."""
        test_cases = [
            ({'type': 'string', 'nullable': False}, 'string'),
            ({'type': 'integer', 'nullable': False}, 'number'),
            ({'type': 'float', 'nullable': False}, 'number'),
            ({'type': 'boolean', 'nullable': False}, 'boolean'),
            ({'type': 'uuid', 'nullable': False}, 'string'),
            ({'type': 'text', 'nullable': False}, 'string'),
            ({'type': 'datetime', 'nullable': False}, 'string'),
            ({'type': 'date', 'nullable': False}, 'string'),
            ({'type': 'json', 'nullable': False}, 'any'),
            ({'type': 'reference', 'nullable': False}, 'string'),
        ]

        for col, expected in test_cases:
            with self.subTest(type=col['type']):
                result = _map_column_type_to_typescript(col)
                self.assertEqual(result, expected)

    def test_nullable_types(self):
        """Test nullable type handling."""
        col = {'type': 'string', 'nullable': True}
        result = _map_column_type_to_typescript(col)
        self.assertEqual(result, 'string | null')

    def test_enum_types(self):
        """Test enum type generation."""
        col = {
            'type': 'enum',
            'enum_values': ['draft', 'active', 'archived'],
            'nullable': False
        }
        result = _map_column_type_to_typescript(col)
        self.assertEqual(result, "'draft' | 'active' | 'archived'")

    def test_nullable_enum(self):
        """Test nullable enum type."""
        col = {
            'type': 'enum',
            'enum_values': ['pending', 'done'],
            'nullable': True
        }
        result = _map_column_type_to_typescript(col)
        self.assertEqual(result, "'pending' | 'done' | null")


class TestTableTypeGeneration(TestCase):
    """Test table type generation."""

    def test_basic_table(self):
        """Test basic table type generation."""
        table = Mock()
        table.name = 'Tasks'
        table.slug = 'tasks'
        table.description = 'Task items'
        table.row_count = 0
        table.schema_json = {
            'columns': [
                {
                    'name': 'id',
                    'type': 'uuid',
                    'primary_key': True,
                    'auto_generate': True,
                    'nullable': False
                },
                {
                    'name': 'title',
                    'type': 'string',
                    'nullable': False
                },
                {
                    'name': 'completed',
                    'type': 'boolean',
                    'nullable': True,
                    'default': False
                },
            ]
        }

        result = _generate_table_type(table)

        # Verify output contains expected elements
        self.assertIn('export interface Tasks', result)
        self.assertIn('export interface TasksInsert', result)
        self.assertIn('export interface TasksUpdate', result)
        self.assertIn('id?: string;', result)  # Primary key optional in main type
        self.assertIn('title: string;', result)  # Required field
        self.assertIn('completed?: boolean | null;', result)  # Optional with default

        # Verify id NOT in insert type (auto-generated)
        insert_section = result.split('export interface TasksInsert')[1].split('}')[0]
        self.assertNotIn('id', insert_section)

        # Verify id NOT in update type (primary key)
        update_section = result.split('export interface TasksUpdate')[1].split('}')[0]
        self.assertNotIn('id', update_section)

    def test_table_with_enum(self):
        """Test table with enum column."""
        table = Mock()
        table.name = 'Items'
        table.slug = 'items'
        table.description = ''
        table.row_count = 5
        table.schema_json = {
            'columns': [
                {
                    'name': 'id',
                    'type': 'uuid',
                    'primary_key': True,
                    'auto_generate': True,
                    'nullable': False
                },
                {
                    'name': 'status',
                    'type': 'enum',
                    'enum_values': ['draft', 'published'],
                    'nullable': False
                },
            ]
        }

        result = _generate_table_type(table)
        self.assertIn("'draft' | 'published'", result)

    def test_empty_table(self):
        """Test table with no columns."""
        table = Mock()
        table.name = 'Empty'
        table.slug = 'empty'
        table.description = ''
        table.row_count = 0
        table.schema_json = {'columns': []}

        result = _generate_table_type(table)
        self.assertIsNone(result)


class TestFullTypeGeneration(TestCase):
    """Test full type file generation."""

    def test_empty_tables(self):
        """Test generation with no tables."""
        result = generate_typescript_types([])
        self.assertIn('export type Database = Record<string, never>', result)
        self.assertIn('export type TableSlug = never', result)

    def test_single_table(self):
        """Test generation with single table."""
        table = Mock()
        table.name = 'Users'
        table.slug = 'users'
        table.description = 'User accounts'
        table.row_count = 10
        table.schema_json = {
            'columns': [
                {
                    'name': 'id',
                    'type': 'uuid',
                    'primary_key': True,
                    'auto_generate': True,
                    'nullable': False
                },
                {
                    'name': 'email',
                    'type': 'string',
                    'nullable': False
                },
            ]
        }

        result = generate_typescript_types([table])

        # Verify header
        self.assertIn('AUTO-GENERATED - DO NOT EDIT MANUALLY', result)

        # Verify table types
        self.assertIn('export interface Users', result)
        self.assertIn('export interface UsersInsert', result)
        self.assertIn('export interface UsersUpdate', result)

        # Verify Database type
        self.assertIn('export interface Database', result)
        self.assertIn("'users':", result)
        self.assertIn('row: Users;', result)
        self.assertIn('insert: UsersInsert;', result)
        self.assertIn('update: UsersUpdate;', result)

        # Verify TableSlug type
        self.assertIn("export type TableSlug = 'users';", result)

    def test_multiple_tables(self):
        """Test generation with multiple tables."""
        table1 = Mock()
        table1.name = 'Users'
        table1.slug = 'users'
        table1.description = ''
        table1.row_count = 0
        table1.schema_json = {
            'columns': [
                {'name': 'id', 'type': 'uuid', 'primary_key': True, 'auto_generate': True, 'nullable': False},
                {'name': 'name', 'type': 'string', 'nullable': False},
            ]
        }

        table2 = Mock()
        table2.name = 'Tasks'
        table2.slug = 'tasks'
        table2.description = ''
        table2.row_count = 0
        table2.schema_json = {
            'columns': [
                {'name': 'id', 'type': 'uuid', 'primary_key': True, 'auto_generate': True, 'nullable': False},
                {'name': 'title', 'type': 'string', 'nullable': False},
            ]
        }

        result = generate_typescript_types([table1, table2])

        # Verify both tables
        self.assertIn('export interface Users', result)
        self.assertIn('export interface Tasks', result)

        # Verify Database includes both
        self.assertIn("'users':", result)
        self.assertIn("'tasks':", result)

        # Verify TableSlug union
        self.assertIn("export type TableSlug = 'users' | 'tasks';", result)


@skipIf(not has_typescript_compiler(), "TypeScript compiler not available")
class TestTypeScriptCompilation(TestCase):
    """Test that generated types compile with tsc."""

    def setUp(self):
        """Create temporary directory for test files."""
        self.temp_dir = tempfile.mkdtemp()

    def tearDown(self):
        """Clean up temporary directory."""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_generated_types_compile(self):
        """Test that generated types compile successfully."""
        # Create mock table
        table = Mock()
        table.name = 'Tasks'
        table.slug = 'tasks'
        table.description = 'Task management'
        table.row_count = 0
        table.schema_json = {
            'columns': [
                {
                    'name': 'id',
                    'type': 'uuid',
                    'primary_key': True,
                    'auto_generate': True,
                    'nullable': False
                },
                {
                    'name': 'title',
                    'type': 'string',
                    'nullable': False
                },
                {
                    'name': 'status',
                    'type': 'enum',
                    'enum_values': ['pending', 'done'],
                    'nullable': False,
                    'default': 'pending'
                },
                {
                    'name': 'priority',
                    'type': 'integer',
                    'nullable': True
                },
                {
                    'name': 'completed',
                    'type': 'boolean',
                    'nullable': True,
                    'default': False
                },
                {
                    'name': 'created_at',
                    'type': 'datetime',
                    'auto_now_add': True,
                    'nullable': False
                },
            ]
        }

        # Generate types
        types_content = generate_typescript_types([table])

        # Write to temp file
        types_file = Path(self.temp_dir) / 'database.ts'
        types_file.write_text(types_content)

        # Try to compile
        result = subprocess.run(
            ['tsc', '--noEmit', '--strict', str(types_file)],
            capture_output=True,
            text=True
        )

        # Assert no compilation errors
        self.assertEqual(result.returncode, 0, f"TypeScript compilation failed:\n{result.stderr}")

    def test_types_can_be_imported(self):
        """Test that generated types can be imported in other TypeScript files."""
        # Create mock table
        table = Mock()
        table.name = 'Users'
        table.slug = 'users'
        table.description = ''
        table.row_count = 0
        table.schema_json = {
            'columns': [
                {'name': 'id', 'type': 'uuid', 'primary_key': True, 'auto_generate': True, 'nullable': False},
                {'name': 'email', 'type': 'string', 'nullable': False},
                {'name': 'name', 'type': 'string', 'nullable': True},
            ]
        }

        # Generate types
        types_content = generate_typescript_types([table])

        # Write database.ts
        types_file = Path(self.temp_dir) / 'database.ts'
        types_file.write_text(types_content)

        # Create a consumer file that imports the types
        consumer_content = """
import type { Users, UsersInsert, UsersUpdate, Database, TableSlug } from './database';
// Test using the row type
const user: Users = {
  id: '123e4567-e89b-12d3-a456-426614174000',
  email: 'test@example.com',
  name: 'John Doe',
  created_at: '2024-01-01T00:00:00Z',
  updated_at: '2024-01-01T00:00:00Z'
};
// Test using the insert type
const newUser: UsersInsert = {
  email: 'new@example.com',
  name: 'Jane Doe'
};
// Test using the update type
const updates: UsersUpdate = {
  name: 'Updated Name'
};
// Test using Database type
type UserRow = Database['users']['row'];
type UserInsert = Database['users']['insert'];
// Test using TableSlug
const tableName: TableSlug = 'users';
// This should cause a type error if uncommented:
// const invalidTable: TableSlug = 'invalid';
"""

        consumer_file = Path(self.temp_dir) / 'consumer.ts'
        consumer_file.write_text(consumer_content)

        # Try to compile consumer file
        result = subprocess.run(
            ['tsc', '--noEmit', '--strict', str(consumer_file)],
            capture_output=True,
            text=True,
            cwd=self.temp_dir
        )

        # Assert no compilation errors
        self.assertEqual(result.returncode, 0, f"Consumer compilation failed:\n{result.stderr}")

    def test_type_safety_catches_errors(self):
        """Test that TypeScript catches type errors."""
        # Create mock table
        table = Mock()
        table.name = 'Tasks'
        table.slug = 'tasks'
        table.description = ''
        table.row_count = 0
        table.schema_json = {
            'columns': [
                {'name': 'id', 'type': 'uuid', 'primary_key': True, 'auto_generate': True, 'nullable': False},
                {'name': 'title', 'type': 'string', 'nullable': False},
            ]
        }

        # Generate types
        types_content = generate_typescript_types([table])
        types_file = Path(self.temp_dir) / 'database.ts'
        types_file.write_text(types_content)

        # Create a file with type errors
        error_content = """
import type { TasksInsert } from './database';
// This should fail - 'text' field doesn't exist
const newTask: TasksInsert = {
  text: 'This should fail'  // Wrong field name!
};
"""

        error_file = Path(self.temp_dir) / 'error.ts'
        error_file.write_text(error_content)

        # Try to compile - should fail
        result = subprocess.run(
            ['tsc', '--noEmit', '--strict', str(error_file)],
            capture_output=True,
            text=True,
            cwd=self.temp_dir
        )

        # Assert compilation fails with type error
        self.assertNotEqual(result.returncode, 0, "Expected compilation to fail due to type error")
        self.assertIn('text', result.stderr, "Expected error about 'text' field")

    def test_enum_type_safety(self):
        """Test that enum types are properly enforced."""
        table = Mock()
        table.name = 'Tasks'
        table.slug = 'tasks'
        table.description = ''
        table.row_count = 0
        table.schema_json = {
            'columns': [
                {'name': 'id', 'type': 'uuid', 'primary_key': True, 'auto_generate': True, 'nullable': False},
                {
                    'name': 'status',
                    'type': 'enum',
                    'enum_values': ['pending', 'done'],
                    'nullable': False,
                    'default': 'pending'
                },
            ]
        }

        types_content = generate_typescript_types([table])
        types_file = Path(self.temp_dir) / 'database.ts'
        types_file.write_text(types_content)

        # Valid enum value - should compile
        valid_content = """
import type { TasksInsert } from './database';
const newTask: TasksInsert = {
  status: 'pending'  // Valid
};
"""
        valid_file = Path(self.temp_dir) / 'valid.ts'
        valid_file.write_text(valid_content)

        result = subprocess.run(
            ['tsc', '--noEmit', '--strict', str(valid_file)],
            capture_output=True,
            text=True,
            cwd=self.temp_dir
        )
        self.assertEqual(result.returncode, 0, f"Valid enum should compile:\n{result.stderr}")

        # Invalid enum value - should fail
        invalid_content = """
import type { TasksInsert } from './database';
const newTask: TasksInsert = {
  status: 'invalid'  // Not in enum!
};
"""
        invalid_file = Path(self.temp_dir) / 'invalid.ts'
        invalid_file.write_text(invalid_content)

        result = subprocess.run(
            ['tsc', '--noEmit', '--strict', str(invalid_file)],
            capture_output=True,
            text=True,
            cwd=self.temp_dir
        )
        self.assertNotEqual(result.returncode, 0, "Invalid enum value should fail compilation")


class TestComplexSchemas(TestCase):
    """Test generation with complex schema scenarios."""

    def test_table_with_all_types(self):
        """Test table using all supported column types."""
        table = Mock()
        table.name = 'Complex'
        table.slug = 'complex'
        table.description = ''
        table.row_count = 0
        table.schema_json = {
            'columns': [
                {'name': 'id', 'type': 'uuid', 'nullable': False},
                {'name': 'text_field', 'type': 'string', 'nullable': False},
                {'name': 'long_text', 'type': 'text', 'nullable': True},
                {'name': 'count', 'type': 'integer', 'nullable': True},
                {'name': 'price', 'type': 'float', 'nullable': True},
                {'name': 'active', 'type': 'boolean', 'nullable': True},
                {'name': 'created', 'type': 'datetime', 'nullable': False},
                {'name': 'birth_date', 'type': 'date', 'nullable': True},
                {'name': 'metadata', 'type': 'json', 'nullable': True},
                {'name': 'status', 'type': 'enum', 'enum_values': ['a', 'b'], 'nullable': True},
                {'name': 'ref', 'type': 'reference', 'nullable': True, 'reference_table': 'other'},
            ]
        }

        result = _generate_table_type(table)

        # Verify all types are present
        self.assertIn('text_field: string;', result)
        self.assertIn('long_text?: string | null;', result)
        self.assertIn('count?: number | null;', result)
        self.assertIn('price?: number | null;', result)
        self.assertIn('active?: boolean | null;', result)
        self.assertIn('created: string;', result)
        self.assertIn('birth_date?: string | null;', result)
        self.assertIn('metadata?: any | null;', result)
        self.assertIn("'a' | 'b'", result)
        self.assertIn('ref?: string | null;', result)