"""
Schema Validator

Consolidated validation logic for table schemas and row data.
"""
import uuid
import re
from typing import Any, Dict, List, Tuple
from datetime import datetime, date
from decimal import InvalidOperation

from .schema import (
    COLUMN_TYPES,
    RESERVED_FIELD_NAMES,
    apply_column_defaults,
)


class SchemaValidator:
    """
    Validates table schemas and row data.
    """

    # Valid column name pattern
    COLUMN_NAME_PATTERN = re.compile(r'^[a-zA-Z][a-zA-Z0-9_]*$')

    # Legacy reserved names (underscore-prefixed)
    LEGACY_RESERVED_NAMES = ['_id', '_index', '_created_at', '_updated_at']

    @classmethod
    def validate_schema(cls, schema: Dict[str, Any]) -> Tuple[bool, List[str]]:
        """
        Validate a table schema structure.

        Args:
            schema: Schema dict with 'columns' and optional 'indexes'

        Returns:
            Tuple of (is_valid, error_messages)
        """
        errors = []

        if not isinstance(schema, dict):
            return False, ['Schema must be a dictionary']

        # Validate columns
        columns = schema.get('columns', [])
        if not columns:
            errors.append('Schema must have at least one column')

        if not isinstance(columns, list):
            errors.append('columns must be a list')
            return False, errors

        column_names = set()
        primary_key_count = 0

        for i, col in enumerate(columns):
            # Apply defaults before validation
            col_with_defaults = apply_column_defaults(col)
            col_errors = cls._validate_column(col_with_defaults, i, column_names)
            errors.extend(col_errors)

            if col.get('primary_key'):
                primary_key_count += 1

            if col.get('name'):
                column_names.add(col['name'])

        if primary_key_count == 0:
            errors.append('Schema must have exactly one primary key column')
        elif primary_key_count > 1:
            errors.append('Schema cannot have multiple primary key columns')

        # Validate indexes (optional)
        indexes = schema.get('indexes', [])
        if indexes:
            if not isinstance(indexes, list):
                errors.append('indexes must be a list')
            else:
                for i, idx in enumerate(indexes):
                    idx_errors = cls._validate_index(idx, i, column_names)
                    errors.extend(idx_errors)

        return len(errors) == 0, errors

    @classmethod
    def _validate_column(cls, col: Dict[str, Any], index: int, existing_names: set) -> List[str]:
        """Validate a single column definition."""
        errors = []
        prefix = f'Column {index}'

        if not isinstance(col, dict):
            return [f'{prefix}: must be a dictionary']

        # Validate name
        name = col.get('name')
        if not name:
            errors.append(f'{prefix}: missing required field "name"')
        elif not isinstance(name, str):
            errors.append(f'{prefix}: name must be a string')
        else:
            prefix = f'Column "{name}"'

            if not cls.COLUMN_NAME_PATTERN.match(name):
                errors.append(f'{prefix}: name must start with a letter and contain only letters, numbers, and underscores')

            # Check reserved names - but allow system columns if they match expected definition
            if name in RESERVED_FIELD_NAMES:
                # Allow if this matches the system column definition exactly
                from .schema import SYSTEM_COLUMNS
                system_col = next((sc for sc in SYSTEM_COLUMNS if sc['name'] == name), None)
                if system_col:
                    # Check if it matches the system definition
                    is_system_col = all(
                        col.get(key) == value
                        for key, value in system_col.items()
                        if key != 'name'  # Name already matched
                    )
                    if not is_system_col:
                        errors.append(f'{prefix}: name "{name}" is reserved (system columns cannot be customized)')
                else:
                    errors.append(f'{prefix}: name "{name}" is reserved')
            elif name in cls.LEGACY_RESERVED_NAMES:
                errors.append(f'{prefix}: name "{name}" is reserved')

            if name in existing_names:
                errors.append(f'{prefix}: duplicate column name')

        # Validate type
        col_type = col.get('type')
        if not col_type:
            errors.append(f'{prefix}: missing required field "type"')
        elif col_type not in COLUMN_TYPES:
            valid_types = ', '.join(COLUMN_TYPES.keys())
            errors.append(f'{prefix}: invalid type "{col_type}". Valid types: {valid_types}')
        else:
            # Type-specific validation
            type_spec = COLUMN_TYPES[col_type]

            # Auto-apply defaults for optional fields
            for field, default_value in type_spec.get('optional_fields', {}).items():
                if field not in col and default_value is not None:
                    col[field] = default_value

            # Check required fields for this type
            for required_field in type_spec.get('required_fields', []):
                if not col.get(required_field):
                    errors.append(f'{prefix}: {col_type} type requires "{required_field}"')

            # Validate specific types
            if col_type == 'enum':
                enum_values = col.get('enum_values')
                if enum_values:  # Already checked by required_fields
                    if not isinstance(enum_values, list) or len(enum_values) == 0:
                        errors.append(f'{prefix}: enum_values must be a non-empty list')
                    elif not all(isinstance(v, str) for v in enum_values):
                        errors.append(f'{prefix}: enum_values must all be strings')

            elif col_type == 'string':
                max_length = col.get('max_length')
                if max_length is not None:
                    if not isinstance(max_length, int) or max_length <= 0:
                        errors.append(f'{prefix}: max_length must be a positive integer')

            elif col_type in ('integer', 'float'):
                min_val = col.get('min')
                max_val = col.get('max')
                if min_val is not None and max_val is not None:
                    if min_val > max_val:
                        errors.append(f'{prefix}: min cannot be greater than max')

        # Validate nullable
        if 'nullable' in col and not isinstance(col['nullable'], bool):
            errors.append(f'{prefix}: nullable must be a boolean')

        # Validate primary_key
        if 'primary_key' in col and not isinstance(col['primary_key'], bool):
            errors.append(f'{prefix}: primary_key must be a boolean')

        # Primary key cannot be nullable
        if col.get('primary_key') and col.get('nullable'):
            errors.append(f'{prefix}: primary key cannot be nullable')

        # Validate unique
        if 'unique' in col and not isinstance(col['unique'], bool):
            errors.append(f'{prefix}: unique must be a boolean')

        return errors

    @classmethod
    def _validate_index(cls, idx: Dict[str, Any], index: int, column_names: set) -> List[str]:
        """Validate a single index definition."""
        errors = []
        prefix = f'Index {index}'

        if not isinstance(idx, dict):
            return [f'{prefix}: must be a dictionary']

        columns = idx.get('columns')
        if not columns:
            errors.append(f'{prefix}: missing required field "columns"')
        elif not isinstance(columns, list) or len(columns) == 0:
            errors.append(f'{prefix}: columns must be a non-empty list')
        else:
            for col_name in columns:
                if col_name not in column_names:
                    errors.append(f'{prefix}: column "{col_name}" does not exist in schema')

        if 'unique' in idx and not isinstance(idx['unique'], bool):
            errors.append(f'{prefix}: unique must be a boolean')

        return errors

    @classmethod
    def validate_row(cls, schema: Dict[str, Any], data: Dict[str, Any], partial: bool = False) -> Tuple[bool, List[str]]:
        """
        Validate row data against a table schema.

        Args:
            schema: The table schema
            data: The row data to validate
            partial: If True, allow missing required fields (for updates)

        Returns:
            Tuple of (is_valid, error_messages)
        """
        errors = []
        columns = schema.get('columns', [])
        column_map = {col['name']: col for col in columns}

        # Check for unknown fields
        for field_name in data.keys():
            if field_name not in column_map:
                errors.append(f'Unknown field: "{field_name}"')

        # Validate each column
        for col in columns:
            col_name = col['name']
            col_type = col['type']
            nullable = col.get('nullable', True)
            value = data.get(col_name)

            # Check for auto-generated fields (skip validation, they'll be generated)
            if col.get('auto_generate') or col.get('auto_now_add') or col.get('auto_now'):
                continue

            # Check for required fields
            if col_name not in data:
                if not partial:
                    # Check if field has a default or is nullable
                    if not nullable and 'default' not in col:
                        errors.append(f'Missing required field: "{col_name}"')
                continue

            # Check for null values
            if value is None:
                if not nullable:
                    errors.append(f'Field "{col_name}" cannot be null')
                continue

            # Type-specific validation
            type_errors = cls._validate_value_type(col_name, value, col)
            errors.extend(type_errors)

        return len(errors) == 0, errors

    @classmethod
    def _validate_value_type(cls, col_name: str, value: Any, col: Dict[str, Any]) -> List[str]:
        """Validate a single value against its column type."""
        errors = []
        col_type = col['type']

        if col_type == 'uuid':
            if not isinstance(value, str):
                errors.append(f'Field "{col_name}": must be a string (UUID format)')
            else:
                try:
                    uuid.UUID(value)
                except ValueError:
                    errors.append(f'Field "{col_name}": invalid UUID format')

        elif col_type == 'string':
            if not isinstance(value, str):
                errors.append(f'Field "{col_name}": must be a string')
            else:
                max_length = col.get('max_length')
                if max_length and len(value) > max_length:
                    errors.append(f'Field "{col_name}": exceeds max_length of {max_length}')

        elif col_type == 'text':
            if not isinstance(value, str):
                errors.append(f'Field "{col_name}": must be a string')

        elif col_type == 'integer':
            if not isinstance(value, int) or isinstance(value, bool):
                errors.append(f'Field "{col_name}": must be an integer')
            else:
                min_val = col.get('min')
                max_val = col.get('max')
                if min_val is not None and value < min_val:
                    errors.append(f'Field "{col_name}": must be >= {min_val}')
                if max_val is not None and value > max_val:
                    errors.append(f'Field "{col_name}": must be <= {max_val}')

        elif col_type == 'float':
            if not isinstance(value, (int, float)) or isinstance(value, bool):
                errors.append(f'Field "{col_name}": must be a number')
            else:
                min_val = col.get('min')
                max_val = col.get('max')
                if min_val is not None and value < min_val:
                    errors.append(f'Field "{col_name}": must be >= {min_val}')
                if max_val is not None and value > max_val:
                    errors.append(f'Field "{col_name}": must be <= {max_val}')

        elif col_type == 'boolean':
            if not isinstance(value, bool):
                errors.append(f'Field "{col_name}": must be a boolean')

        elif col_type == 'datetime':
            if isinstance(value, str):
                try:
                    datetime.fromisoformat(value.replace('Z', '+00:00'))
                except ValueError:
                    errors.append(f'Field "{col_name}": invalid datetime format (use ISO 8601)')
            elif not isinstance(value, datetime):
                errors.append(f'Field "{col_name}": must be a datetime string (ISO 8601)')

        elif col_type == 'date':
            if isinstance(value, str):
                try:
                    date.fromisoformat(value)
                except ValueError:
                    errors.append(f'Field "{col_name}": invalid date format (use YYYY-MM-DD)')
            elif not isinstance(value, date):
                errors.append(f'Field "{col_name}": must be a date string (YYYY-MM-DD)')

        elif col_type == 'enum':
            enum_values = col.get('enum_values', [])
            if value not in enum_values:
                errors.append(f'Field "{col_name}": must be one of {enum_values}')

        elif col_type == 'json':
            # JSON can be any serializable type
            if not isinstance(value, (dict, list, str, int, float, bool, type(None))):
                errors.append(f'Field "{col_name}": must be a valid JSON value')

        elif col_type == 'reference':
            # Reference validation - just check it's a valid UUID
            if not isinstance(value, str):
                errors.append(f'Field "{col_name}": must be a string (UUID reference)')
            else:
                try:
                    uuid.UUID(value)
                except ValueError:
                    errors.append(f'Field "{col_name}": invalid UUID reference')

        return errors

    @classmethod
    def coerce_value(cls, value: Any, col: Dict[str, Any]) -> Any:
        """
        Coerce a value to the expected type.

        Args:
            value: The value to coerce
            col: The column definition

        Returns:
            The coerced value, or the original if coercion fails
        """
        if value is None:
            return None

        col_type = col['type']

        try:
            if col_type == 'uuid':
                if isinstance(value, str):
                    # Validate and normalize UUID
                    return str(uuid.UUID(value))

            elif col_type in ('string', 'text'):
                return str(value)

            elif col_type == 'integer':
                if isinstance(value, str):
                    return int(value)
                elif isinstance(value, float):
                    return int(value)

            elif col_type == 'float':
                if isinstance(value, str):
                    return float(value)
                elif isinstance(value, int):
                    return float(value)

            elif col_type == 'boolean':
                if isinstance(value, str):
                    if value.lower() in ('true', '1', 'yes'):
                        return True
                    elif value.lower() in ('false', '0', 'no'):
                        return False

            elif col_type == 'datetime':
                if isinstance(value, str):
                    return datetime.fromisoformat(value.replace('Z', '+00:00')).isoformat()

            elif col_type == 'date':
                if isinstance(value, str):
                    return date.fromisoformat(value).isoformat()
        except (ValueError, TypeError, InvalidOperation):
            pass

        return value

    @classmethod
    def apply_defaults(cls, schema: Dict[str, Any], data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Apply default values for missing fields.

        Args:
            schema: The table schema
            data: The row data

        Returns:
            Data with defaults applied
        """
        result = dict(data)
        columns = schema.get('columns', [])

        for col in columns:
            col_name = col['name']

            if col_name in result:
                continue

            # Apply auto-generated values
            if col.get('auto_generate') and col['type'] == 'uuid':
                result[col_name] = str(uuid.uuid4())

            elif col.get('auto_now_add') and col['type'] == 'datetime':
                result[col_name] = datetime.utcnow().isoformat() + 'Z'

            elif col.get('auto_now_add') and col['type'] == 'date':
                result[col_name] = date.today().isoformat()

            # Apply explicit defaults
            elif 'default' in col:
                result[col_name] = col['default']

        return result

    @classmethod
    def apply_auto_now(cls, schema: Dict[str, Any], data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Apply auto_now values for update operations.

        Args:
            schema: The table schema
            data: The row data

        Returns:
            Data with auto_now fields updated
        """
        result = dict(data)
        columns = schema.get('columns', [])

        for col in columns:
            col_name = col['name']

            if col.get('auto_now'):
                if col['type'] == 'datetime':
                    result[col_name] = datetime.utcnow().isoformat() + 'Z'
                elif col['type'] == 'date':
                    result[col_name] = date.today().isoformat()

        return result
