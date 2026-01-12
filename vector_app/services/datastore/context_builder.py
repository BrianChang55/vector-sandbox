"""
Context Builder

Builds LLM-friendly documentation and context for the data store.
"""
from typing import Any, Dict, List
from .schema import COLUMN_TYPES, COLUMN_OPTIONS, RESERVED_FIELD_NAMES


class DataStoreContextBuilder:
    """Builds context strings for LLM prompts about the data store."""

    @staticmethod
    def build_table_creation_instructions() -> str:
        """
        Build instructions for creating new tables.

        Returns:
            Formatted markdown instructions for table creation
        """
        # Build column types list
        column_types_list = []
        for col_type, spec in COLUMN_TYPES.items():
            desc = spec['description']
            required = spec.get('required_fields', [])
            optional = spec.get('optional_fields', {})

            if required:
                req_str = ', '.join(required)
                if optional:
                    opt_list = [f"{k} (defaults to {v})" if v is not None else k
                               for k, v in optional.items()]
                    opt_str = ', '.join(opt_list)
                    detail = f"(requires {req_str}; optional: {opt_str})"
                else:
                    detail = f"(requires {req_str})"
            elif optional:
                opt_list = [f"{k} (defaults to {v})" if v is not None else k
                           for k, v in optional.items()]
                opt_str = ', '.join(opt_list)
                detail = f"(optional: {opt_str})"
            else:
                detail = ""

            column_types_list.append(f"- `{col_type}` - {desc} {detail}".strip())

        # Build column options list
        column_options_list = []
        for opt_name, opt_spec in COLUMN_OPTIONS.items():
            desc = opt_spec['description']
            default = opt_spec.get('default')
            if default is not None:
                column_options_list.append(f"- `{opt_name}`: {desc} (default: {default})")
            else:
                column_options_list.append(f"- `{opt_name}`: {desc}")

        reserved_fields_str = "', '".join(sorted(RESERVED_FIELD_NAMES))

        return f"""### Creating New Tables

To create a new data table, include a TABLE_DEFINITION block in your response:

🚨 **DO NOT define '{reserved_fields_str}' - these are auto-generated!**

```table:table-slug
name: Display Name
description: Optional description of the table
columns:
  - name: title, type: string, nullable: false
  - name: description, type: text
  - name: status, type: enum, enum_values: [draft, active, archived]
  - name: count, type: integer, default: 0
  - name: project_id, type: reference, reference_table: projects  # reference_column defaults to 'id'
  - name: price, type: float
  - name: is_featured, type: boolean, default: false
  - name: metadata, type: json
```

**System auto-adds:** {', '.join([f'`{f}`' for f in sorted(RESERVED_FIELD_NAMES)])} to every table.

**Supported Column Types:**
{chr(10).join(column_types_list)}

**Column Options:**
{chr(10).join(column_options_list)}
"""

    @staticmethod
    def build_datastore_operations() -> str:
        """
        Build documentation for dataStore API operations.

        Returns:
            Formatted markdown documentation for the dataStore API
        """
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

    @staticmethod
    def build_full_context(existing_tables_summary: str = "") -> str:
        """
        Build complete data store context for LLM.

        Args:
            existing_tables_summary: Optional summary of existing tables

        Returns:
            Complete formatted context string
        """
        sections = ["## App Data Store\n"]

        if existing_tables_summary:
            sections.append(existing_tables_summary + "\n")
        else:
            sections.append("No tables have been created yet for this app.\n")

        sections.append(DataStoreContextBuilder.build_datastore_operations())
        sections.append(DataStoreContextBuilder.build_table_creation_instructions())

        return "\n".join(sections)
