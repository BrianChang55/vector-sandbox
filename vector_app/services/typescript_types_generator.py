"""
TypeScript Type Generator for Database Schemas

Generates TypeScript type definitions from table schemas to provide
static type safety for generated code.
"""
from typing import Dict, Any, List, Optional


def generate_typescript_types(tables: List[Any]) -> str:
    """
    Generate a complete TypeScript types file from table schemas.

    Args:
        tables: List of AppDataTable objects

    Returns:
        Complete TypeScript file content with type definitions
    """
    if not tables:
        return _generate_empty_types()

    sections = []

    # Header
    sections.append(_generate_header())

    # Individual table types
    table_types = []
    for table in tables:
        table_type = _generate_table_type(table)
        if table_type:
            sections.append(table_type)
            table_types.append({
                'name': _to_pascal_case(table.slug),
                'slug': table.slug
            })

    # TableSlug enum (must come before Database which references it)
    sections.append(_generate_table_slug_type(table_types))

    # Database type mapping
    sections.append(_generate_database_type(table_types))

    return "\n\n".join(sections)


def _generate_header() -> str:
    """Generate file header with documentation."""
    return """/**
 * Database Type Definitions
 *
 * AUTO-GENERATED - DO NOT EDIT MANUALLY
 *
 * This file contains TypeScript type definitions for all database tables.
 * Import these types to get compile-time type safety for dataStore operations.
 *
 * @example
 * ```typescript
 * import { Task, Database } from './types/database';
 * import { dataStore } from './lib/dataStore';
 *
 * // Type-safe insert
 * const newTask: Database['tasks']['insert'] = {
 *   title: 'New task',
 *   status: 'pending'
 * };
 * await dataStore.insert('tasks', newTask);
 *
 * // Type-safe query
 * const result = await dataStore.query('tasks', {});
 * const tasks: Task[] = result.rows;
 * ```
 */"""


def _generate_empty_types() -> str:
    """Generate types file when no tables exist."""
    return """/**
 * Database Type Definitions
 *
 * No tables have been defined yet.
 */

export type Database = Record<string, never>;
export const enum TableSlug {}
"""


def _generate_table_type(table: Any) -> Optional[str]:
    """
    Generate TypeScript interface for a single table.

    Args:
        table: AppDataTable object

    Returns:
        TypeScript interface definition
    """
    schema = table.schema_json or {}
    columns = schema.get('columns', [])

    if not columns:
        return None

    type_name = _to_pascal_case(table.slug)

    lines = [
        f"/**",
        f" * {table.name}",
    ]

    if table.description:
        lines.append(f" * {table.description}")

    lines.extend([
        f" * ",
        f" * Table: `{table.slug}` ({table.row_count} rows)",
        f" */",
        f"export interface {type_name} {{",
    ])

    # Generate field definitions
    for col in columns:
        col_name = col.get('name', '')
        ts_type = _map_column_type_to_typescript(col)
        is_optional = col.get('nullable', True) or col.get('auto_generate') or col.get('auto_now_add') or col.get('auto_now') or 'default' in col

        # Add JSDoc comment for field
        field_doc = []
        if col.get('primary_key'):
            field_doc.append('Primary key')
        if col.get('auto_generate'):
            field_doc.append('Auto-generated')
        if col.get('auto_now_add'):
            field_doc.append('Auto-set on create')
        if col.get('auto_now'):
            field_doc.append('Auto-set on update')
        if col.get('unique') and not col.get('primary_key'):
            field_doc.append('Unique')

        if field_doc:
            lines.append(f"  /** {', '.join(field_doc)} */")

        optional_marker = '?' if is_optional else ''
        lines.append(f"  {col_name}{optional_marker}: {ts_type};")

    lines.append("}")

    # Generate insert type (excludes auto-generated fields)
    insert_fields = []
    for col in columns:
        col_name = col.get('name', '')
        ts_type = _map_column_type_to_typescript(col)

        # Skip auto-generated fields in insert type
        if col.get('auto_generate') or col.get('auto_now_add') or col.get('auto_now'):
            continue

        # Field is optional if nullable or has default
        is_optional = col.get('nullable', True) or 'default' in col
        optional_marker = '?' if is_optional else ''
        insert_fields.append(f"  {col_name}{optional_marker}: {ts_type};")

    if insert_fields:
        lines.extend([
            "",
            f"/**",
            f" * Insert type for {table.name}",
            f" * Excludes auto-generated fields",
            f" */",
            f"export interface {type_name}Insert {{",
            *insert_fields,
            "}",
        ])

    # Generate update type (all fields optional, excludes auto-generated)
    update_fields = []
    for col in columns:
        col_name = col.get('name', '')
        ts_type = _map_column_type_to_typescript(col)

        # Skip auto-generated fields in update type
        if col.get('auto_generate') or col.get('primary_key') or col.get('auto_now'):
            continue

        update_fields.append(f"  {col_name}?: {ts_type};")

    if update_fields:
        lines.extend([
            "",
            f"/**",
            f" * Update type for {table.name}",
            f" * All fields optional, excludes primary key and auto-fields",
            f" */",
            f"export interface {type_name}Update {{",
            *update_fields,
            "}",
        ])

    return "\n".join(lines)


def _generate_database_type(table_types: List[Dict[str, str]]) -> str:
    """Generate the Database type mapping using TableSlug enum keys."""
    if not table_types:
        return "export type Database = Record<string, never>;"

    lines = [
        "/**",
        " * Database schema mapping",
        " * Maps table slugs to their types",
        " */",
        "export interface Database {",
    ]

    for table_info in table_types:
        type_name = table_info['name']
        lines.extend([
            f"  [TableSlug.{type_name}]: {{",
            f"    row: {type_name};",
            f"    insert: {type_name}Insert;",
            f"    update: {type_name}Update;",
            f"  }};",
        ])

    lines.append("}")

    return "\n".join(lines)


def _generate_table_slug_type(table_types: List[Dict[str, str]]) -> str:
    """Generate TableSlug const enum for type-safe slug references."""
    if not table_types:
        return "export const enum TableSlug {}"

    lines = [
        "/**",
        " * Valid table slugs",
        " * Use this enum for type-safe table slug references",
        " */",
        "export const enum TableSlug {",
    ]

    for t in table_types:
        enum_key = t['name']  # Already PascalCase
        lines.append(f"  {enum_key} = '{t['slug']}',")

    lines.append("}")
    return "\n".join(lines)


def _map_column_type_to_typescript(col: Dict[str, Any]) -> str:
    """
    Map a column type to its TypeScript equivalent.

    Args:
        col: Column definition dictionary

    Returns:
        TypeScript type string
    """
    col_type = col.get('type', 'string')
    nullable = col.get('nullable', True)

    # Base type mapping
    type_map = {
        'uuid': 'string',
        'string': 'string',
        'text': 'string',
        'integer': 'number',
        'float': 'number',
        'boolean': 'boolean',
        'datetime': 'string',  # ISO 8601 string
        'date': 'string',  # YYYY-MM-DD string
        'json': 'any',  # Could be more specific if we had JSON schema
        'reference': 'string',  # UUID reference
    }

    # Handle enum types specially
    if col_type == 'enum':
        enum_values = col.get('enum_values', [])
        if enum_values:
            # Generate union type: 'value1' | 'value2' | 'value3'
            quoted_values = [f"'{v}'" for v in enum_values]
            base_type = ' | '.join(quoted_values)
        else:
            base_type = 'string'
    else:
        base_type = type_map.get(col_type, 'any')

    # Add null union if nullable
    if nullable:
        return f"{base_type} | null"

    return base_type


def _to_pascal_case(slug: str) -> str:
    """
    Convert slug to PascalCase for type names.

    Args:
        slug: kebab-case or snake_case string

    Returns:
        PascalCase string

    Examples:
        'task-items' -> 'TaskItems'
        'user_profiles' -> 'UserProfiles'
    """
    # Replace hyphens and underscores with spaces, then title case
    words = slug.replace('-', ' ').replace('_', ' ').split()
    return ''.join(word.capitalize() for word in words)
