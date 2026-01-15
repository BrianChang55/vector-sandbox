"""
Test script for schema extraction service JSON parsing.

Run with: python test_schema_extraction.py
"""
import sys
import os

# Add project to path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Mock Django settings before importing
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'internal_apps_backend.settings')

import json
import re
from typing import Any, Dict, List


def parse_table_definitions(content: str) -> List[Dict[str, Any]]:
    """
    Standalone version of parse_table_definitions for testing.
    This mirrors the logic in schema_extraction_service.py - returns dictionaries directly.
    """
    try:
        # Parse JSON directly - we instruct the LLM to return ONLY JSON
        data = json.loads(content.strip())
        tables = data.get('tables', [])

        if not tables:
            print("⚠️  No tables found in schema extraction response")
            return []

        # Validate and return table definitions as dictionaries
        valid_tables = []
        for table in tables:
            slug = table.get('slug', '')
            name = table.get('name', '')
            columns = table.get('columns', [])

            if not slug or not columns:
                print(f"⚠️  Skipping invalid table definition: {table}")
                continue

            # Validate columns have required fields
            valid_columns = []
            for col in columns:
                if col.get('name') and col.get('type'):
                    valid_columns.append(col)
                else:
                    print(f"⚠️  Skipping invalid column in table '{slug}': {col}")

            if valid_columns:
                valid_tables.append({
                    'slug': slug,
                    'name': name,
                    'description': table.get('description', ''),
                    'columns': valid_columns
                })

        print(f"✅ Parsed {len(valid_tables)} table definitions from JSON")
        return valid_tables

    except json.JSONDecodeError as e:
        print(f"❌ Failed to parse JSON: {e}")
        print(f"Content preview: {content[:500]}...")
        return []
    except Exception as e:
        print(f"❌ Error parsing table definitions: {e}")
        import traceback
        traceback.print_exc()
        return []


def test_simple_json():
    """Test Case 1: Simple JSON with one table"""
    print("\n" + "="*80)
    print("TEST 1: Simple JSON - Single Table")
    print("="*80)

    test_json = '''{
  "tables": [
    {
      "slug": "tasks",
      "name": "Tasks",
      "description": "User tasks",
      "columns": [
        {"name": "title", "type": "string", "nullable": false},
        {"name": "status", "type": "enum", "enum_values": ["todo", "done"], "default": "todo"}
      ]
    }
  ]
}'''

    definitions = parse_table_definitions(test_json)
    assert len(definitions) == 1, f"Expected 1 table, got {len(definitions)}"

    table = definitions[0]
    assert table['slug'] == 'tasks', f"Expected slug 'tasks', got {table['slug']}"
    assert table['name'] == 'Tasks', f"Expected name 'Tasks', got {table['name']}"
    assert len(table['columns']) == 2, f"Expected 2 columns, got {len(table['columns'])}"

    # Check columns
    title_col = next(c for c in table['columns'] if c['name'] == 'title')
    assert title_col['type'] == 'string', "Title should be string type"
    assert title_col['nullable'] == False, "Title should not be nullable"

    status_col = next(c for c in table['columns'] if c['name'] == 'status')
    assert status_col['type'] == 'enum', "Status should be enum type"
    assert status_col['enum_values'] == ['todo', 'done'], "Status enum values incorrect"

    print("✅ PASSED")
    print(json.dumps(table, indent=2))


def test_relationships_and_foreign_keys():
    """Test Case 2: Multiple tables with foreign keys and join tables"""
    print("\n" + "="*80)
    print("TEST 2: Complex Schema - Foreign Keys & Join Tables")
    print("="*80)

    test_json = '''{
  "tables": [
    {
      "slug": "projects",
      "name": "Projects",
      "description": "Project records",
      "columns": [
        {"name": "name", "type": "string", "nullable": false},
        {"name": "status", "type": "enum", "enum_values": ["planning", "active", "completed"], "default": "planning"}
      ]
    },
    {
      "slug": "tasks",
      "name": "Tasks",
      "description": "Tasks within projects",
      "columns": [
        {"name": "title", "type": "string", "nullable": false},
        {"name": "project_id", "type": "reference", "reference_table": "projects", "nullable": false},
        {"name": "priority", "type": "enum", "enum_values": ["low", "medium", "high"], "default": "medium"}
      ]
    },
    {
      "slug": "team-members",
      "name": "Team Members",
      "description": "Team member profiles",
      "columns": [
        {"name": "name", "type": "string", "nullable": false},
        {"name": "email", "type": "string", "nullable": false, "unique": true}
      ]
    },
    {
      "slug": "project-members",
      "name": "Project Members",
      "description": "Join table for project assignments",
      "columns": [
        {"name": "project_id", "type": "reference", "reference_table": "projects", "nullable": false},
        {"name": "member_id", "type": "reference", "reference_table": "team-members", "nullable": false},
        {"name": "role", "type": "string"}
      ]
    }
  ]
}'''

    definitions = parse_table_definitions(test_json)
    assert len(definitions) == 4, f"Expected 4 tables, got {len(definitions)}"

    # Verify each table
    table_slugs = [d['slug'] for d in definitions]
    assert "projects" in table_slugs, "Missing projects table"
    assert "tasks" in table_slugs, "Missing tasks table"
    assert "team-members" in table_slugs, "Missing team-members table"
    assert "project-members" in table_slugs, "Missing project-members join table"

    # Verify foreign keys
    tasks_table = next(d for d in definitions if d['slug'] == 'tasks')
    project_id_col = next(c for c in tasks_table['columns'] if c['name'] == 'project_id')
    assert project_id_col['type'] == 'reference', "project_id should be reference type"
    assert project_id_col['reference_table'] == 'projects', "Missing foreign key to projects"

    join_table = next(d for d in definitions if d['slug'] == 'project-members')
    proj_col = next(c for c in join_table['columns'] if c['name'] == 'project_id')
    member_col = next(c for c in join_table['columns'] if c['name'] == 'member_id')
    assert proj_col['reference_table'] == 'projects', "Missing FK to projects in join table"
    assert member_col['reference_table'] == 'team-members', "Missing FK to team-members in join table"

    print("✅ PASSED")
    for i, defn in enumerate(definitions, 1):
        print(f"\n--- Table {i} ---")
        print(json.dumps(defn, indent=2))


def test_all_field_types():
    """Test Case 4: All supported field types"""
    print("\n" + "="*80)
    print("TEST 4: All Field Types")
    print("="*80)

    test_json = '''{
  "tables": [
    {
      "slug": "comprehensive",
      "name": "Comprehensive Test",
      "description": "Tests all field types",
      "columns": [
        {"name": "text_field", "type": "string", "max_length": 100},
        {"name": "long_text", "type": "text"},
        {"name": "count", "type": "integer", "default": 0},
        {"name": "price", "type": "float"},
        {"name": "is_active", "type": "boolean", "default": true},
        {"name": "created", "type": "datetime"},
        {"name": "due_date", "type": "date"},
        {"name": "status", "type": "enum", "enum_values": ["draft", "published"]},
        {"name": "metadata", "type": "json"},
        {"name": "parent_id", "type": "reference", "reference_table": "comprehensive"}
      ]
    }
  ]
}'''

    definitions = parse_table_definitions(test_json)
    assert len(definitions) == 1, f"Expected 1 table, got {len(definitions)}"

    table = definitions[0]
    columns = {col['name']: col for col in table['columns']}

    assert columns['text_field']['type'] == 'string', "Missing string type"
    assert columns['long_text']['type'] == 'text', "Missing text type"
    assert columns['count']['type'] == 'integer', "Missing integer type"
    assert columns['price']['type'] == 'float', "Missing float type"
    assert columns['is_active']['type'] == 'boolean', "Missing boolean type"
    assert columns['created']['type'] == 'datetime', "Missing datetime type"
    assert columns['due_date']['type'] == 'date', "Missing date type"
    assert columns['status']['type'] == 'enum', "Missing enum type"
    assert columns['metadata']['type'] == 'json', "Missing json type"
    assert columns['parent_id']['type'] == 'reference', "Missing reference type"
    assert columns['text_field'].get('max_length') == 100, "Missing max_length"

    print("✅ PASSED")
    print(json.dumps(table, indent=2))


def test_empty_tables():
    """Test Case 5: Empty tables array"""
    print("\n" + "="*80)
    print("TEST 5: Empty Tables Array")
    print("="*80)

    test_json = '{"tables": []}'

    definitions = parse_table_definitions(test_json)
    assert len(definitions) == 0, f"Expected 0 tables, got {len(definitions)}"
    print("✅ PASSED - Correctly handled empty tables array")


def test_invalid_json():
    """Test Case 6: Invalid JSON"""
    print("\n" + "="*80)
    print("TEST 6: Invalid JSON Handling")
    print("="*80)

    test_invalid = "This is not JSON {invalid"

    definitions = parse_table_definitions(test_invalid)
    assert len(definitions) == 0, "Should return empty list for invalid JSON"
    print("✅ PASSED - Correctly handled invalid JSON")


def run_all_tests():
    """Run all test cases"""
    print("\n" + "="*80)
    print("SCHEMA EXTRACTION SERVICE - JSON PARSING TESTS")
    print("="*80)

    tests = [
        test_simple_json,
        test_relationships_and_foreign_keys,
        test_all_field_types,
        test_empty_tables,
        test_invalid_json,
    ]

    passed = 0
    failed = 0

    for test in tests:
        try:
            test()
            passed += 1
        except AssertionError as e:
            print(f"❌ FAILED: {e}")
            failed += 1
        except Exception as e:
            print(f"❌ ERROR: {e}")
            import traceback
            traceback.print_exc()
            failed += 1

    print("\n" + "="*80)
    print("TEST SUMMARY")
    print("="*80)
    print(f"✅ Passed: {passed}/{len(tests)}")
    print(f"❌ Failed: {failed}/{len(tests)}")
    print("="*80)

    return failed == 0


if __name__ == "__main__":
    success = run_all_tests()
    sys.exit(0 if success else 1)
