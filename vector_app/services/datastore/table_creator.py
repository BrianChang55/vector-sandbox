"""
Table Creator

Creates database tables from parsed TABLE_DEFINITION blocks.
"""
import logging
from typing import Any, List, TYPE_CHECKING

from .parser import TableDefinitionParser
from .schema import get_system_columns

if TYPE_CHECKING:
    from vector_app.models import InternalApp, AppDataTable

logger = logging.getLogger(__name__)


def create_tables_from_definitions(
    app: 'InternalApp',
    table_definitions: List[str]
) -> List['AppDataTable']:
    """
    Create database tables from TABLE_DEFINITION blocks.

    Args:
        app: The InternalApp to create tables in
        table_definitions: List of table definition strings (e.g., ```table:slug...```)

    Returns:
        List of created AppDataTable objects
    """
    from vector_app.services.app_data_service import AppDataService

    parser = TableDefinitionParser()
    created_tables = []

    # Parse all table definitions
    parsed_tables = parser.parse_table_definitions("\n\n".join(table_definitions))

    for table_def in parsed_tables:
        slug = table_def['slug']
        name = table_def['name']
        description = table_def.get('description', '')
        columns = table_def.get('columns', [])

        # Build schema with system columns
        schema = {
            'columns': get_system_columns() + columns,
            'indexes': []  # Can add indexes later if needed
        }

        # Create the table
        table, errors = AppDataService.create_table(
            app=app,
            name=name,
            schema=schema,
            description=description
        )

        if table:
            logger.info(f"✅ Created table '{slug}' with {len(columns)} user-defined columns")
            created_tables.append(table)
        else:
            logger.error(f"❌ Failed to create table '{slug}': {'; '.join(errors)}")

    return created_tables
