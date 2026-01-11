#!/usr/bin/env python
"""
Standalone test script for TypeScript generator.
Runs without Django dependencies.
"""
import sys
from unittest.mock import Mock

# Add parent directory to path
sys.path.insert(0, '.')

# Import directly by loading the module file
import importlib.util
spec = importlib.util.spec_from_file_location(
    "typescript_generator",
    "vector_app/services/datastore/typescript_generator.py"
)
ts_gen = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ts_gen)

# Extract functions
generate_typescript_types = ts_gen.generate_typescript_types
_to_pascal_case = ts_gen._to_pascal_case
_map_column_type_to_typescript = ts_gen._map_column_type_to_typescript
_generate_table_type = ts_gen._generate_table_type


def test_pascal_case():
    """Test slug to PascalCase conversion."""
    print("Testing PascalCase conversion...")
    assert _to_pascal_case('task-items') == 'TaskItems'
    assert _to_pascal_case('user_profiles') == 'UserProfiles'
    assert _to_pascal_case('simple') == 'Simple'
    print("✓ PascalCase conversion works")


def test_type_mapping():
    """Test column type to TypeScript mapping."""
    print("\nTesting type mapping...")

    # Basic types
    assert _map_column_type_to_typescript({'type': 'string', 'nullable': False}) == 'string'
    assert _map_column_type_to_typescript({'type': 'integer', 'nullable': False}) == 'number'
    assert _map_column_type_to_typescript({'type': 'boolean', 'nullable': False}) == 'boolean'

    # Nullable types
    assert _map_column_type_to_typescript({'type': 'string', 'nullable': True}) == 'string | null'

    # Enum types
    result = _map_column_type_to_typescript({
        'type': 'enum',
        'enum_values': ['draft', 'published'],
        'nullable': False
    })
    assert "'draft' | 'published'" == result

    print("✓ Type mapping works")


def test_table_generation():
    """Test complete table type generation."""
    print("\nTesting table generation...")

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
                'name': 'completed',
                'type': 'boolean',
                'nullable': True,
                'default': False
            },
        ]
    }

    result = _generate_table_type(table)

    # Verify essential elements
    assert 'export interface Tasks' in result
    assert 'export interface TasksInsert' in result
    assert 'export interface TasksUpdate' in result
    assert 'title: string;' in result
    assert "'pending' | 'done'" in result

    # Verify id NOT in insert type (auto-generated)
    insert_section = result.split('export interface TasksInsert')[1].split('}')[0]
    assert 'id' not in insert_section

    print("✓ Table generation works")
    print("\nGenerated TypeScript:\n")
    print(result[:500] + "...\n")


def test_full_generation():
    """Test full file generation."""
    print("\nTesting full file generation...")

    # Create mock tables
    table1 = Mock()
    table1.name = 'Users'
    table1.slug = 'users'
    table1.description = 'User accounts'
    table1.row_count = 0
    table1.schema_json = {
        'columns': [
            {'name': 'id', 'type': 'uuid', 'primary_key': True, 'auto_generate': True, 'nullable': False},
            {'name': 'email', 'type': 'string', 'nullable': False},
            {'name': 'name', 'type': 'string', 'nullable': True},
        ]
    }

    table2 = Mock()
    table2.name = 'Tasks'
    table2.slug = 'tasks'
    table2.description = 'Task management'
    table2.row_count = 0
    table2.schema_json = {
        'columns': [
            {'name': 'id', 'type': 'uuid', 'primary_key': True, 'auto_generate': True, 'nullable': False},
            {'name': 'title', 'type': 'string', 'nullable': False},
            {'name': 'user_id', 'type': 'reference', 'nullable': False, 'reference_table': 'users'},
        ]
    }

    result = generate_typescript_types([table1, table2])

    # Verify structure
    assert 'AUTO-GENERATED - DO NOT EDIT MANUALLY' in result
    assert 'export interface Users' in result
    assert 'export interface Tasks' in result
    assert 'export interface Database' in result
    assert "'users':" in result
    assert "'tasks':" in result
    assert "export type TableSlug = 'users' | 'tasks';" in result

    print("✓ Full file generation works")
    print(f"\nGenerated {len(result)} characters of TypeScript")


def test_empty_tables():
    """Test generation with no tables."""
    print("\nTesting empty tables...")

    result = generate_typescript_types([])

    assert 'export type Database = Record<string, never>' in result
    assert 'export type TableSlug = never' in result

    print("✓ Empty table generation works")


def main():
    """Run all tests."""
    print("=" * 60)
    print("TypeScript Generator - Standalone Tests")
    print("=" * 60)

    try:
        test_pascal_case()
        test_type_mapping()
        test_table_generation()
        test_full_generation()
        test_empty_tables()

        print("\n" + "=" * 60)
        print("✅ ALL TESTS PASSED")
        print("=" * 60)
        return 0

    except AssertionError as e:
        print("\n" + "=" * 60)
        print(f"❌ TEST FAILED: {e}")
        print("=" * 60)
        return 1
    except Exception as e:
        print("\n" + "=" * 60)
        print(f"❌ ERROR: {e}")
        import traceback
        traceback.print_exc()
        print("=" * 60)
        return 1


if __name__ == '__main__':
    sys.exit(main())
