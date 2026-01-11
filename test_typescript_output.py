#!/usr/bin/env python
"""
Generate a sample TypeScript types file to verify output format.
"""
import sys
from unittest.mock import Mock
import importlib.util

# Load the generator module directly
spec = importlib.util.spec_from_file_location(
    "typescript_generator",
    "vector_app/services/datastore/typescript_generator.py"
)
ts_gen = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ts_gen)

generate_typescript_types = ts_gen.generate_typescript_types

# Create realistic mock tables
print("Creating sample database schema...")

tasks_table = Mock()
tasks_table.name = 'Tasks'
tasks_table.slug = 'tasks'
tasks_table.description = 'Task management for project tracking'
tasks_table.row_count = 42
tasks_table.schema_json = {
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
            'name': 'due_date',
            'type': 'date',
            'nullable': True
        },
        {
            'name': 'assignee_id',
            'type': 'reference',
            'nullable': True,
            'reference_table': 'users'
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

users_table = Mock()
users_table.name = 'Users'
users_table.slug = 'users'
users_table.description = 'User accounts'
users_table.row_count = 15
users_table.schema_json = {
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
            'nullable': False,
            'unique': True
        },
        {
            'name': 'name',
            'type': 'string',
            'nullable': False
        },
        {
            'name': 'role',
            'type': 'enum',
            'enum_values': ['admin', 'user', 'guest'],
            'nullable': False,
            'default': 'user'
        },
        {
            'name': 'active',
            'type': 'boolean',
            'nullable': False,
            'default': True
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

# Generate TypeScript
print("\nGenerating TypeScript types...\n")
result = generate_typescript_types([users_table, tasks_table])

# Write to file
output_file = 'sample_database.ts'
with open(output_file, 'w') as f:
    f.write(result)

print(f"✅ Generated {len(result)} characters of TypeScript")
print(f"✅ Saved to {output_file}\n")

# Show the output
print("=" * 80)
print("GENERATED TYPESCRIPT CODE")
print("=" * 80)
print(result)
print("=" * 80)

print("\n✅ Sample TypeScript types file generated successfully!")
print(f"📄 Check {output_file} for the full output")
