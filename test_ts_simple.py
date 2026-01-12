#!/usr/bin/env python
"""
Standalone test for TypeScript generator (no Django dependencies).
"""
import sys
import importlib.util
from unittest.mock import Mock

# Load the module directly
spec = importlib.util.spec_from_file_location(
    "typescript_types_generator",
    "vector_app/services/typescript_types_generator.py"
)
ts_gen = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ts_gen)

generate_typescript_types = ts_gen.generate_typescript_types

# Create a realistic table
table = Mock()
table.name = 'Tasks'
table.slug = 'tasks'
table.description = 'Task management for projects'
table.row_count = 42
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
            'name': 'description',
            'type': 'text',
            'nullable': True
        },
        {
            'name': 'status',
            'type': 'enum',
            'enum_values': ['pending', 'in_progress', 'done', 'blocked'],
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
        {
            'name': 'updated_at',
            'type': 'datetime',
            'auto_now': True,
            'nullable': False
        },
    ]
}

print("=" * 80)
print("GENERATING TYPESCRIPT TYPES")
print("=" * 80)

result = generate_typescript_types([table])

# Write to file for inspection
with open('generated_database.ts', 'w') as f:
    f.write(result)

print(result)

print("\n" + "=" * 80)
print("VERIFICATION CHECKLIST")
print("=" * 80)

checks = [
    ('✓' if 'export interface Tasks' in result else '✗', 'Tasks interface exported'),
    ('✓' if 'export interface TasksInsert' in result else '✗', 'TasksInsert interface exported'),
    ('✓' if 'export interface TasksUpdate' in result else '✗', 'TasksUpdate interface exported'),
    ('✓' if 'export interface Database' in result else '✗', 'Database interface exported'),
    ('✓' if "export type TableSlug = 'tasks';" in result else '✗', 'TableSlug type exported'),
    ('✓' if 'title: string;' in result else '✗', 'Required field (title) is non-optional'),
    ('✓' if 'id?: string;' in result else '✗', 'Auto-generated field (id) is optional in main type'),
    ('✓' if "'pending' | 'in_progress' | 'done' | 'blocked'" in result else '✗', 'Enum union type generated'),
    ('✓' if 'completed?: boolean | null;' in result else '✗', 'Nullable field with default is optional'),
]

# Check that id is NOT in insert type
insert_section = result.split('export interface TasksInsert')[1].split('}')[0] if 'export interface TasksInsert' in result else ''
checks.append(('✓' if 'id' not in insert_section else '✗', 'Auto-generated id excluded from insert type'))

# Check that id is NOT in update type
update_section = result.split('export interface TasksUpdate')[1].split('}')[0] if 'export interface TasksUpdate' in result else ''
checks.append(('✓' if 'id' not in update_section else '✗', 'Primary key id excluded from update type'))

# Check that created_at is NOT in insert type (auto_now_add)
checks.append(('✓' if 'created_at' not in insert_section else '✗', 'Auto-set created_at excluded from insert type'))

# Check that updated_at is NOT in insert type (auto_now)
checks.append(('✓' if 'updated_at' not in insert_section else '✗', 'Auto-set updated_at excluded from insert type'))

# Check that updated_at is NOT in update type (auto_now)
checks.append(('✓' if 'updated_at' not in update_section else '✗', 'Auto-set updated_at excluded from update type'))

for check, description in checks:
    print(f"{check} {description}")

all_passed = all(check == '✓' for check, _ in checks)
print("\n" + "=" * 80)
if all_passed:
    print("✅ ALL CHECKS PASSED!")
    print(f"📄 Generated TypeScript saved to: generated_database.ts")
else:
    print("❌ SOME CHECKS FAILED!")
print("=" * 80)

sys.exit(0 if all_passed else 1)
