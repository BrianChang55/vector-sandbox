"""
DataStore Fetcher

Handles fetching and formatting data store schemas and data for LLM consumption.
"""
from typing import Any, Dict, List, Optional


def build_data_store_context(app, include_samples: bool = True, max_samples: int = 3) -> str:
    """
    Generate an LLM-friendly representation of an app's data store.

    Args:
        app: The InternalApp to describe
        include_samples: Whether to include sample data rows
        max_samples: Maximum number of sample rows per table

    Returns:
        A formatted string describing tables, schemas, and available operations.
    """
    from ...models import AppDataTable
    from .context_builder import DataStoreContextBuilder

    tables = AppDataTable.objects.filter(internal_app=app).order_by('name')

    if not tables.exists():
        return _build_empty_context()

    sections = ["## App Data Store\n"]
    sections.append(_build_tables_section(tables, include_samples, max_samples))
    sections.append(DataStoreContextBuilder.build_datastore_operations())
    sections.append(DataStoreContextBuilder.build_table_creation_instructions())

    return "\n".join(sections)


def build_data_store_context_for_version(
    app,
    version_id: str,
    include_samples: bool = True,
    max_samples: int = 3
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
    from ...models import AppVersion, AppDataTableSnapshot
    from .context_builder import DataStoreContextBuilder

    try:
        version = AppVersion.objects.get(id=version_id, internal_app=app)
    except AppVersion.DoesNotExist:
        return build_data_store_context(app, include_samples, max_samples)

    snapshots = AppDataTableSnapshot.objects.filter(
        app_version=version
    ).exclude(operation='delete').select_related('table')

    if not snapshots.exists():
        # Fall back to current tables
        return build_data_store_context(app, include_samples, max_samples)

    sections = ["## App Data Store\n"]
    sections.append(_build_tables_section_from_snapshots(snapshots, include_samples, max_samples))
    sections.append(DataStoreContextBuilder.build_datastore_operations())
    sections.append(DataStoreContextBuilder.build_table_creation_instructions())

    return "\n".join(sections)


def get_table_summary(app) -> str:
    """
    Get a brief summary of tables (for use in planning prompts).

    Args:
        app: The InternalApp to summarize

    Returns:
        A brief string listing table names and row counts.
    """
    from ...models import AppDataTable

    tables = AppDataTable.objects.filter(internal_app=app).order_by('name')

    if not tables.exists():
        return "No data tables created yet."

    summaries = []
    for table in tables:
        col_count = len(table.schema_json.get('columns', []))
        summaries.append(f"- {table.name} ({table.slug}): {table.row_count} rows, {col_count} columns")

    return "Existing tables:\n" + "\n".join(summaries)


def _build_empty_context() -> str:
    """Build context when no tables exist yet."""
    from .context_builder import DataStoreContextBuilder

    return """## App Data Store

No tables have been created yet for this app.

You can create data tables to store and manage data for your application.

""" + DataStoreContextBuilder.build_table_creation_instructions() + "\n\n" + DataStoreContextBuilder.build_datastore_operations()


def _build_tables_section(
    tables,
    include_samples: bool,
    max_samples: int
) -> str:
    """Build the tables section describing all tables and their schemas."""
    lines = ["### Tables\n"]

    for table in tables:
        lines.append(_format_table(table, include_samples, max_samples))

    return "\n".join(lines)


def _build_tables_section_from_snapshots(
    snapshots,
    include_samples: bool,
    max_samples: int
) -> str:
    """Build the tables section from versioned snapshots."""
    lines = ["### Tables\n"]

    for snapshot in snapshots:
        lines.append(_format_table_from_snapshot(snapshot, include_samples, max_samples))

    return "\n".join(lines)


def _format_table(table, include_samples: bool, max_samples: int) -> str:
    """Format a single table's description."""
    lines = [f"**{table.name}** (slug: `{table.slug}`, {table.row_count} rows)"]

    if table.description:
        lines.append(f"  {table.description}")

    lines.append("\nColumns:")

    schema = table.schema_json or {}
    columns = schema.get('columns', [])

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
    columns = schema.get('columns', [])

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
    name = col.get('name', 'unknown')
    col_type = col.get('type', 'string')

    constraints = []

    if col.get('primary_key'):
        constraints.append('primary key')
    if col.get('auto_generate'):
        constraints.append('auto-generated')
    if col.get('unique') and not col.get('primary_key'):
        constraints.append('unique')
    if not col.get('nullable', True) and not col.get('primary_key'):
        constraints.append('required')
    if col.get('auto_now_add'):
        constraints.append('auto on create')
    if col.get('auto_now'):
        constraints.append('auto on update')

    # Type-specific details
    if col_type == 'enum' and col.get('enum_values'):
        type_desc = f"enum [{', '.join(col['enum_values'])}]"
    elif col_type == 'string' and col.get('max_length'):
        type_desc = f"string(max {col['max_length']})"
    elif col_type == 'reference' and col.get('reference_table'):
        type_desc = f"reference -> {col['reference_table']}"
    else:
        type_desc = col_type

    if constraints:
        return f"- `{name}`: {type_desc} ({', '.join(constraints)})"
    else:
        return f"- `{name}`: {type_desc}"


def _format_sample_data(table, columns: List[Dict], max_samples: int) -> str:
    """Format sample data rows as a markdown table."""
    from ...models import AppDataRow

    rows = AppDataRow.objects.filter(table=table).order_by('row_index')[:max_samples]

    if not rows:
        return ""

    # Get display columns (skip long text/json fields)
    display_cols = []
    for col in columns[:6]:  # Max 6 columns for readability
        col_type = col.get('type', 'string')
        if col_type not in ('text', 'json'):
            display_cols.append(col)

    if not display_cols:
        display_cols = columns[:4]

    col_names = [c.get('name', '') for c in display_cols]

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
    if len(str_value) == 36 and str_value.count('-') == 4:
        return str_value[:8] + "..."

    # Truncate long strings
    if len(str_value) > max_length:
        return str_value[:max_length - 3] + "..."

    return str_value
