"""
Table Definition Parser

Parses table definitions from LLM-generated content.
"""
import re
import logging
from typing import Dict, List, Any, Optional

from .schema import RESERVED_FIELD_NAMES, apply_column_defaults

logger = logging.getLogger(__name__)


class TableDefinitionParser:
    """
    Parses table definitions from LLM output.

    Supports the format:
    ```table:table-slug
    name: Table Name
    description: Optional description
    columns:
      - name: col1, type: string, nullable: false
      - name: col2, type: integer, default: 0
    ```
    """

    # Pattern to match table definition blocks
    TABLE_BLOCK_PATTERN = r'```table:([a-z0-9-]+)\n(.*?)```'

    # Pattern to extract list values like enum_values: [a, b, c]
    LIST_PATTERN = r'(\w+):\s*\[([^\]]+)\]'

    @classmethod
    def parse_table_definitions(cls, content: str) -> List[Dict[str, Any]]:
        """
        Parse all TABLE_DEFINITION blocks from LLM output.

        Args:
            content: The full LLM response content

        Returns:
            List of parsed table definitions
        """
        tables = []
        matches = re.findall(cls.TABLE_BLOCK_PATTERN, content, re.DOTALL | re.IGNORECASE)

        for slug, table_content in matches:
            try:
                table_def = cls.parse_single_table(slug.strip(), table_content.strip())
                if table_def:
                    tables.append(table_def)
            except Exception as e:
                logger.warning(f"Failed to parse table definition for {slug}: {e}")

        return tables

    @classmethod
    def parse_single_table(cls, slug: str, content: str) -> Optional[Dict[str, Any]]:
        """
        Parse a single table definition.

        Args:
            slug: The table slug (from ```table:slug)
            content: The content inside the table block

        Returns:
            Parsed table definition dict, or None if invalid
        """
        lines = content.strip().split('\n')

        name = slug.replace('-', ' ').title()
        description = ''
        columns = []
        in_columns = False

        for line in lines:
            line = line.strip()

            if line.startswith('name:'):
                name = line[5:].strip()
            elif line.startswith('description:'):
                description = line[12:].strip()
            elif line.startswith('columns:'):
                in_columns = True
            elif in_columns and line.startswith('- '):
                col = cls.parse_column(line[2:].strip())
                if col:
                    # Filter out reserved fields - they're auto-generated
                    col_name = col.get('name', '').lower()
                    if col_name not in RESERVED_FIELD_NAMES:
                        # Apply defaults for optional fields
                        col_with_defaults = apply_column_defaults(col)
                        columns.append(col_with_defaults)
                    else:
                        logger.info(f"Filtered out reserved field '{col_name}' from table '{slug}'")

        # Reject tables that only have reserved fields (no user-defined columns)
        if not columns:
            logger.warning(f"Table '{slug}' has no user-defined columns after filtering reserved fields")
            return None

        return {
            'slug': slug,
            'name': name,
            'description': description,
            'columns': columns,
        }

    @classmethod
    def parse_column(cls, line: str) -> Optional[Dict[str, Any]]:
        """
        Parse a column definition line.

        Format: name: column_name, type: string, nullable: false, ...

        Args:
            line: The column definition line (without the '- ' prefix)

        Returns:
            Parsed column dict, or None if invalid
        """
        col = {}

        # Extract list values first (e.g., enum_values: [a, b, c])
        list_matches = re.findall(cls.LIST_PATTERN, line)
        for key, value_str in list_matches:
            values = [v.strip().strip('"').strip("'") for v in value_str.split(',')]
            col[key] = values
            # Remove the list from the line so it doesn't interfere with comma splitting
            line = re.sub(rf'{key}:\s*\[[^\]]+\]', '', line)

        # Parse remaining key-value pairs (comma-separated)
        parts = [p.strip() for p in line.split(',') if p.strip()]

        for part in parts:
            # Remove inline comments
            if '#' in part:
                part = part.split('#')[0].strip()

            if ':' in part:
                key, value = part.split(':', 1)
                key = key.strip()
                value = value.strip()

                # Skip if already parsed (from list extraction)
                if key in col:
                    continue

                # Type coercion
                if value.lower() == 'true':
                    value = True
                elif value.lower() == 'false':
                    value = False
                elif value.isdigit():
                    value = int(value)
                elif cls._is_float(value):
                    value = float(value)
                # Otherwise keep as string

                col[key] = value

        # Validate required fields
        if 'name' not in col or 'type' not in col:
            return None

        return col

    @staticmethod
    def _is_float(value: str) -> bool:
        """Check if a string can be converted to float."""
        try:
            float(value)
            return '.' in value  # Distinguish from integers
        except ValueError:
            return False
