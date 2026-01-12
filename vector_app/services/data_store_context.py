"""
Data Store Context Generator for LLM Consumption.

Provides functions to generate clean, structured string representations
of an app's data store for use in LLM prompts.
"""

from typing import Any, Dict, List, Optional
from ..models import InternalApp, AppDataTable, AppDataRow


def build_data_store_context(app: InternalApp, include_samples: bool = True, max_samples: int = 3) -> str:
    """
    Generate an LLM-friendly representation of an app's data store.

    Args:
        app: The InternalApp to describe
        include_samples: Whether to include sample data rows
        max_samples: Maximum number of sample rows per table

    Returns:
        A formatted string describing tables, schemas, and available operations.
    """
    tables = AppDataTable.objects.filter(internal_app=app).order_by("name")

    if not tables.exists():
        return _build_empty_context()

    sections = ["## App Data Store\n"]
    sections.append(_build_tables_section(tables, include_samples, max_samples))
    sections.append(_build_operations_section())
    sections.append(_build_table_creation_section())

    return "\n".join(sections)


def build_data_store_context_for_version(
    app: InternalApp, version_id: str, include_samples: bool = True, max_samples: int = 3
) -> str:
    """
    Generate LLM-friendly representation using versioned schema snapshots.

    Args:
        app: The InternalApp to describe
        version_id: The AppVersion ID to get schema snapshots from
        include_samples: Whether to include sample data rows
        max_samples: Maximum number of sample rows per table

    Returns:
        A formatted string describing tables at the specific version.
    """
    from ..models import AppVersion, AppDataTableSnapshot

    try:
        version = AppVersion.objects.get(id=version_id, internal_app=app)
    except AppVersion.DoesNotExist:
        return build_data_store_context(app, include_samples, max_samples)

    snapshots = (
        AppDataTableSnapshot.objects.filter(app_version=version)
        .exclude(operation="delete")
        .select_related("table")
    )

    if not snapshots.exists():
        # Fall back to current tables
        return build_data_store_context(app, include_samples, max_samples)

    sections = ["## App Data Store\n"]
    sections.append(_build_tables_section_from_snapshots(snapshots, include_samples, max_samples))
    sections.append(_build_operations_section())
    sections.append(_build_table_creation_section())

    return "\n".join(sections)


def _build_empty_context() -> str:
    """Build context when no tables exist yet."""
    return (
        """## App Data Store

No tables have been created yet for this app.

You can create data tables to store and manage data for your application.

"""
        + _build_table_creation_section()
        + "\n\n"
        + _build_operations_section()
    )


def _build_tables_section(tables: List[AppDataTable], include_samples: bool, max_samples: int) -> str:
    """Build the tables section describing all tables and their schemas."""
    lines = ["### Tables\n"]

    for table in tables:
        lines.append(_format_table(table, include_samples, max_samples))

    return "\n".join(lines)


def _build_tables_section_from_snapshots(snapshots: List, include_samples: bool, max_samples: int) -> str:
    """Build the tables section from versioned snapshots."""
    lines = ["### Tables\n"]

    for snapshot in snapshots:
        lines.append(_format_table_from_snapshot(snapshot, include_samples, max_samples))

    return "\n".join(lines)


def _format_table(table: AppDataTable, include_samples: bool, max_samples: int) -> str:
    """Format a single table's description."""
    lines = [f"**{table.name}** (slug: `{table.slug}`, {table.row_count} rows)"]

    if table.description:
        lines.append(f"  {table.description}")

    lines.append("\nColumns:")

    schema = table.schema_json or {}
    columns = schema.get("columns", [])

    for col in columns:
        lines.append(_format_column(col))

    if include_samples and table.row_count > 0:
        lines.append(_format_sample_data(table, columns, max_samples))

    lines.append("")  # Empty line between tables
    return "\n".join(lines)


def _format_table_from_snapshot(snapshot, include_samples: bool, max_samples: int) -> str:
    """Format a table description from a versioned snapshot."""
    table = snapshot.table
    schema = snapshot.schema_json or {}
    columns = schema.get("columns", [])

    lines = [f"**{table.name}** (slug: `{table.slug}`, {table.row_count} rows)"]

    if table.description:
        lines.append(f"  {table.description}")

    lines.append("\nColumns:")

    for col in columns:
        lines.append(_format_column(col))

    if include_samples and table.row_count > 0:
        lines.append(_format_sample_data(table, columns, max_samples))

    lines.append("")
    return "\n".join(lines)


def _format_column(col: Dict[str, Any]) -> str:
    """Format a single column definition."""
    name = col.get("name", "unknown")
    col_type = col.get("type", "string")

    constraints = []

    if col.get("primary_key"):
        constraints.append("primary key")
    if col.get("auto_generate"):
        constraints.append("auto-generated")
    if col.get("unique") and not col.get("primary_key"):
        constraints.append("unique")
    if not col.get("nullable", True) and not col.get("primary_key"):
        constraints.append("required")
    if col.get("auto_now_add"):
        constraints.append("auto on create")
    if col.get("auto_now"):
        constraints.append("auto on update")

    # Type-specific details
    if col_type == "enum" and col.get("enum_values"):
        type_desc = f"enum [{', '.join(col['enum_values'])}]"
    elif col_type == "string" and col.get("max_length"):
        type_desc = f"string(max {col['max_length']})"
    elif col_type == "reference" and col.get("reference_table"):
        type_desc = f"reference -> {col['reference_table']}"
    else:
        type_desc = col_type

    if constraints:
        return f"- `{name}`: {type_desc} ({', '.join(constraints)})"
    else:
        return f"- `{name}`: {type_desc}"


def _format_sample_data(table: AppDataTable, columns: List[Dict], max_samples: int) -> str:
    """Format sample data rows as a markdown table."""
    rows = AppDataRow.objects.filter(table=table).order_by("row_index")[:max_samples]

    if not rows:
        return ""

    # Get display columns (skip long text/json fields)
    display_cols = []
    for col in columns[:6]:  # Max 6 columns for readability
        col_type = col.get("type", "string")
        if col_type not in ("text", "json"):
            display_cols.append(col)

    if not display_cols:
        display_cols = columns[:4]

    col_names = [c.get("name", "") for c in display_cols]

    lines = ["\nSample data:"]

    # Header
    header = "| " + " | ".join(col_names) + " |"
    separator = "| " + " | ".join(["---"] * len(col_names)) + " |"
    lines.append(header)
    lines.append(separator)

    # Rows
    for row in rows:
        values = []
        for col_name in col_names:
            value = row.data.get(col_name)
            formatted = _format_cell_value(value)
            values.append(formatted)
        lines.append("| " + " | ".join(values) + " |")

    if table.row_count > max_samples:
        lines.append(f"... and {table.row_count - max_samples} more rows")

    return "\n".join(lines)


def _format_cell_value(value: Any, max_length: int = 30) -> str:
    """Format a cell value for display."""
    if value is None:
        return "—"

    if isinstance(value, bool):
        return "true" if value else "false"

    if isinstance(value, (dict, list)):
        return "[json]"

    str_value = str(value)

    # Truncate UUIDs
    if len(str_value) == 36 and str_value.count("-") == 4:
        return str_value[:8] + "..."

    # Truncate long strings
    if len(str_value) > max_length:
        return str_value[: max_length - 3] + "..."

    return str_value


def _build_operations_section() -> str:
    """Build the available operations section."""
    return """### Available Operations

Use the `dataStore` API in your generated code to interact with tables:

```typescript
import { dataStore } from './lib/dataStore';

// Query rows with optional filtering, sorting, pagination
const result = await dataStore.query('table-slug', {
  filters: [{ field: 'status', op: 'eq', value: 'active' }],
  orderBy: [{ field: 'created_at', dir: 'desc' }],
  limit: 50,
  offset: 0
});
// Returns: { rows: [...], total_count: number, has_more: boolean }

// Insert a new row
const newRow = await dataStore.insert('table-slug', {
  name: 'New Item',
  status: 'active'
});
// Returns: { id: 'uuid', data: {...}, row_index: number }

// Update an existing row
const updated = await dataStore.update('table-slug', 'row-uuid', {
  status: 'inactive'
});

// Delete a row
await dataStore.delete('table-slug', 'row-uuid');

// Bulk insert multiple rows
const bulkResult = await dataStore.bulkInsert('table-slug', [
  { name: 'Item 1', status: 'active' },
  { name: 'Item 2', status: 'pending' }
]);
// Returns: { created_count: number, rows: [...] }

// Bulk delete multiple rows
await dataStore.bulkDelete('table-slug', ['uuid-1', 'uuid-2']);
```

**Filter Operators:**
- `eq`, `neq` - Equal, not equal
- `gt`, `gte`, `lt`, `lte` - Comparison operators
- `in`, `not_in` - Value in list
- `contains`, `icontains` - String contains (case-sensitive/insensitive)
- `is_null` - Check for null values
"""


def _build_table_creation_section() -> str:
    """Build the table creation instructions section."""
    return """### Creating New Tables

To create a new data table, include a TABLE_DEFINITION block in your response:

🚨 **DO NOT define 'id', 'created_at', 'updated_at' - these are auto-generated!**

```table:table-slug
name: Display Name
description: Optional description of the table
columns:
  - name: title, type: string, nullable: false
  - name: description, type: text
  - name: status, type: enum, enum_values: [draft, active, archived]
  - name: count, type: integer, default: 0
  - name: reference_code, type: string  # If you need a custom identifier, use a different name!
  - name: price, type: float
  - name: is_featured, type: boolean, default: false
  - name: metadata, type: json
```

**System auto-adds:** `id` (UUID), `created_at`, `updated_at` to every table.

**Supported Column Types:**
- `uuid` - UUID identifier (use for primary keys)
- `string` - Short text (max 255 chars by default)
- `text` - Long text (unlimited)
- `integer` - Whole numbers
- `float` - Decimal numbers
- `boolean` - True/false
- `datetime` - Date and time with timezone
- `date` - Date only
- `enum` - Fixed set of values (requires `enum_values`)
- `json` - Arbitrary JSON data
- `reference` - Foreign key (requires `reference_table`)

**Column Options:**
- `nullable: true/false` - Allow null values (default: true)
- `unique: true` - Enforce unique values
- `default: value` - Default value for new rows
- `primary_key: true` - Mark as primary key
- `auto_generate: true` - Auto-generate UUID values
- `auto_now_add: true` - Set to current datetime on create
- `auto_now: true` - Update to current datetime on every save
- `max_length: number` - Maximum string length
- `enum_values: [a, b, c]` - Allowed values for enum type
"""


def get_table_summary(app: InternalApp) -> str:
    """
    Get a brief summary of tables (for use in planning prompts).

    Args:
        app: The InternalApp to summarize

    Returns:
        A brief string listing table names and row counts.
    """
    tables = AppDataTable.objects.filter(internal_app=app).order_by("name")

    if not tables.exists():
        return "No data tables created yet."

    summaries = []
    for table in tables:
        col_count = len(table.schema_json.get("columns", []))
        summaries.append(f"- {table.name} ({table.slug}): {table.row_count} rows, {col_count} columns")

    return "Existing tables:\n" + "\n".join(summaries)
