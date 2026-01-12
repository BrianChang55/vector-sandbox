"""
Schema Migration Service for Enterprise Version Management.

Provides compatibility checking and migration hints when reverting
between versions with different schemas. This ensures users understand
the impact of schema changes on their data.
"""

import logging
from typing import Optional

logger = logging.getLogger(__name__)


class SchemaMigrationService:
    """
    Service for checking schema compatibility and generating migration hints.

    Provides:
    - Compatibility checking between schema versions
    - Data loss risk assessment
    - Migration hints and suggestions
    - Type conversion safety checks
    """

    # Type compatibility matrix - maps (from_type, to_type) to compatibility
    TYPE_COMPATIBILITY = {
        # String conversions (generally safe)
        ("string", "text"): "safe",
        ("text", "string"): "warning",  # May truncate
        # Integer conversions
        ("integer", "number"): "safe",
        ("number", "integer"): "warning",  # May lose precision
        # Boolean conversions
        ("boolean", "string"): "safe",
        ("string", "boolean"): "warning",  # May fail
        ("boolean", "integer"): "safe",
        ("integer", "boolean"): "safe",
        # Date conversions
        ("datetime", "string"): "safe",
        ("string", "datetime"): "warning",  # May fail to parse
        ("date", "datetime"): "safe",
        ("datetime", "date"): "warning",  # Loses time
        # JSON conversions
        ("object", "string"): "safe",  # Serialize to JSON
        ("array", "string"): "safe",  # Serialize to JSON
        ("string", "object"): "warning",  # May fail to parse
        ("string", "array"): "warning",  # May fail to parse
    }

    @classmethod
    def check_compatibility(cls, from_schema: dict, to_schema: dict) -> dict:
        """
        Check if reverting from from_schema to to_schema is safe.

        Args:
            from_schema: Current schema (the one being reverted from)
            to_schema: Target schema (the one being reverted to)

        Returns:
            Dict with:
            - is_compatible: bool
            - risk_level: 'safe', 'warning', 'danger'
            - column_changes: Details of each column change
            - warnings: List of warning messages
            - errors: List of error messages
        """
        from_columns = {c["name"]: c for c in from_schema.get("columns", [])}
        to_columns = {c["name"]: c for c in to_schema.get("columns", [])}

        warnings = []
        errors = []
        column_changes = []

        # Check columns that will be removed (exist in from but not in to)
        removed_columns = set(from_columns.keys()) - set(to_columns.keys())
        for col_name in removed_columns:
            col = from_columns[col_name]
            column_changes.append(
                {
                    "name": col_name,
                    "change": "removed",
                    "from_type": col.get("type"),
                    "to_type": None,
                    "risk": "warning",
                }
            )
            warnings.append(
                f"Column '{col_name}' will be removed from schema. "
                f"Data in this column will be preserved but not accessible via the schema."
            )

        # Check columns that will be added (exist in to but not in from)
        added_columns = set(to_columns.keys()) - set(from_columns.keys())
        for col_name in added_columns:
            col = to_columns[col_name]
            is_required = not col.get("nullable", True) and col.get("default") is None

            change_info = {
                "name": col_name,
                "change": "added",
                "from_type": None,
                "to_type": col.get("type"),
                "risk": "danger" if is_required else "safe",
            }
            column_changes.append(change_info)

            if is_required:
                errors.append(
                    f"Column '{col_name}' is required but has no default. "
                    f"Existing rows may have null values that violate this constraint."
                )
            else:
                warnings.append(f"Column '{col_name}' will be added to schema with default/nullable values.")

        # Check columns that exist in both but may have changed
        common_columns = set(from_columns.keys()) & set(to_columns.keys())
        for col_name in common_columns:
            from_col = from_columns[col_name]
            to_col = to_columns[col_name]

            changes = []
            risk = "safe"

            # Type change
            from_type = from_col.get("type", "string")
            to_type = to_col.get("type", "string")
            if from_type != to_type:
                type_risk = cls._check_type_compatibility(from_type, to_type)
                if type_risk == "danger":
                    risk = "danger"
                    errors.append(
                        f"Column '{col_name}' type change from '{from_type}' to '{to_type}' "
                        f"may cause data loss or errors."
                    )
                elif type_risk == "warning":
                    risk = "warning" if risk != "danger" else risk
                    warnings.append(
                        f"Column '{col_name}' type change from '{from_type}' to '{to_type}' "
                        f"may require data conversion."
                    )
                changes.append({"field": "type", "from": from_type, "to": to_type})

            # Nullable change
            from_nullable = from_col.get("nullable", True)
            to_nullable = to_col.get("nullable", True)
            if from_nullable and not to_nullable:
                # Making a column required - check if it has a default
                if to_col.get("default") is None:
                    risk = "danger"
                    errors.append(
                        f"Column '{col_name}' is being made required without a default. "
                        f"Existing null values will violate this constraint."
                    )
                else:
                    risk = "warning" if risk != "danger" else risk
                    warnings.append(
                        f"Column '{col_name}' is being made required. "
                        f"Null values will use the default: {to_col.get('default')}"
                    )
                changes.append({"field": "nullable", "from": from_nullable, "to": to_nullable})

            # Unique constraint change
            from_unique = from_col.get("unique", False)
            to_unique = to_col.get("unique", False)
            if not from_unique and to_unique:
                risk = "warning" if risk != "danger" else risk
                warnings.append(
                    f"Column '{col_name}' is being made unique. "
                    f"Existing duplicate values may cause constraint violations."
                )
                changes.append({"field": "unique", "from": from_unique, "to": to_unique})

            if changes:
                column_changes.append(
                    {
                        "name": col_name,
                        "change": "modified",
                        "from_type": from_type,
                        "to_type": to_type,
                        "risk": risk,
                        "changes": changes,
                    }
                )

        # Determine overall risk level
        if errors:
            risk_level = "danger"
        elif warnings:
            risk_level = "warning"
        else:
            risk_level = "safe"

        return {
            "is_compatible": len(errors) == 0,
            "risk_level": risk_level,
            "column_changes": column_changes,
            "warnings": warnings,
            "errors": errors,
            "summary": {
                "columns_removed": len(removed_columns),
                "columns_added": len(added_columns),
                "columns_modified": len([c for c in column_changes if c["change"] == "modified"]),
            },
        }

    @classmethod
    def _check_type_compatibility(cls, from_type: str, to_type: str) -> str:
        """
        Check if a type conversion is safe.

        Returns:
            'safe', 'warning', or 'danger'
        """
        if from_type == to_type:
            return "safe"

        # Check known compatibility mappings
        key = (from_type.lower(), to_type.lower())
        if key in cls.TYPE_COMPATIBILITY:
            return cls.TYPE_COMPATIBILITY[key]

        # Default to warning for unknown conversions
        return "warning"

    @classmethod
    def generate_migration_hints(cls, from_schema: dict, to_schema: dict) -> list:
        """
        Generate suggestions for handling schema incompatibilities.

        Args:
            from_schema: Current schema
            to_schema: Target schema

        Returns:
            List of migration hint dicts
        """
        compatibility = cls.check_compatibility(from_schema, to_schema)
        hints = []

        for change in compatibility["column_changes"]:
            col_name = change["name"]

            if change["change"] == "removed":
                hints.append(
                    {
                        "column": col_name,
                        "type": "column_removed",
                        "severity": "info",
                        "suggestion": (
                            f"Column '{col_name}' will be removed from the schema but data is preserved. "
                            f"You can still access this data through direct database queries if needed."
                        ),
                        "action": "none_required",
                    }
                )

            elif change["change"] == "added":
                if change["risk"] == "danger":
                    hints.append(
                        {
                            "column": col_name,
                            "type": "required_column_added",
                            "severity": "warning",
                            "suggestion": (
                                f"Column '{col_name}' is required without a default. "
                                f"Consider adding a default value to the schema before reverting, "
                                f"or update existing rows to have a value for this column."
                            ),
                            "action": "set_default",
                            "recommended_action": {
                                "type": "add_default",
                                "column": col_name,
                            },
                        }
                    )
                else:
                    hints.append(
                        {
                            "column": col_name,
                            "type": "column_added",
                            "severity": "info",
                            "suggestion": (f"Column '{col_name}' will be added with default/null values."),
                            "action": "none_required",
                        }
                    )

            elif change["change"] == "modified":
                from_type = change.get("from_type")
                to_type = change.get("to_type")

                if from_type != to_type:
                    hints.append(
                        {
                            "column": col_name,
                            "type": "type_change",
                            "severity": "warning" if change["risk"] == "warning" else "error",
                            "suggestion": cls._get_type_conversion_hint(from_type, to_type, col_name),
                            "action": "review_data",
                        }
                    )

                # Check for nullability changes
                for col_change in change.get("changes", []):
                    if col_change.get("field") == "nullable":
                        if not col_change["to"]:  # Making required
                            hints.append(
                                {
                                    "column": col_name,
                                    "type": "made_required",
                                    "severity": "warning",
                                    "suggestion": (
                                        f"Column '{col_name}' is being made required. "
                                        f"Ensure no existing rows have null values, or add a default."
                                    ),
                                    "action": "check_nulls",
                                }
                            )

                    if col_change.get("field") == "unique":
                        if col_change["to"]:  # Making unique
                            hints.append(
                                {
                                    "column": col_name,
                                    "type": "made_unique",
                                    "severity": "warning",
                                    "suggestion": (
                                        f"Column '{col_name}' is being made unique. "
                                        f"Check for duplicate values that would violate this constraint."
                                    ),
                                    "action": "check_duplicates",
                                }
                            )

        return hints

    @classmethod
    def _get_type_conversion_hint(cls, from_type: str, to_type: str, col_name: str) -> str:
        """Get a helpful hint for a specific type conversion."""
        hints = {
            ("string", "integer"): (
                f"Column '{col_name}' is changing from string to integer. "
                f"Ensure all values are numeric strings or they will fail to convert."
            ),
            ("string", "boolean"): (
                f"Column '{col_name}' is changing from string to boolean. "
                f"Values like 'true', 'false', '1', '0' will convert; others may fail."
            ),
            ("number", "integer"): (
                f"Column '{col_name}' is changing from number to integer. "
                f"Decimal values will be truncated."
            ),
            ("datetime", "date"): (
                f"Column '{col_name}' is changing from datetime to date. " f"Time information will be lost."
            ),
            ("text", "string"): (
                f"Column '{col_name}' is changing from text to string. " f"Long text values may be truncated."
            ),
        }

        key = (from_type.lower(), to_type.lower())
        if key in hints:
            return hints[key]

        return (
            f"Column '{col_name}' type is changing from '{from_type}' to '{to_type}'. "
            f"Review data to ensure conversion is valid."
        )

    @classmethod
    def check_table_compatibility(cls, from_tables: list, to_tables: list) -> dict:
        """
        Check compatibility between two sets of table definitions.

        Args:
            from_tables: List of table dicts from current version
            to_tables: List of table dicts from target version

        Returns:
            Dict with overall compatibility and per-table details
        """
        from_by_slug = {t["slug"]: t for t in from_tables}
        to_by_slug = {t["slug"]: t for t in to_tables}

        result = {
            "is_compatible": True,
            "risk_level": "safe",
            "tables": [],
            "summary": {
                "tables_removed": 0,
                "tables_added": 0,
                "tables_modified": 0,
                "total_warnings": 0,
                "total_errors": 0,
            },
        }

        all_warnings = []
        all_errors = []

        # Tables being removed
        for slug in set(from_by_slug.keys()) - set(to_by_slug.keys()):
            result["tables"].append(
                {
                    "slug": slug,
                    "name": from_by_slug[slug].get("name", slug),
                    "change": "removed",
                    "risk": "info",
                    "compatibility": None,
                }
            )
            result["summary"]["tables_removed"] += 1

        # Tables being added
        for slug in set(to_by_slug.keys()) - set(from_by_slug.keys()):
            result["tables"].append(
                {
                    "slug": slug,
                    "name": to_by_slug[slug].get("name", slug),
                    "change": "added",
                    "risk": "safe",
                    "compatibility": None,
                }
            )
            result["summary"]["tables_added"] += 1

        # Tables in both - check schema compatibility
        for slug in set(from_by_slug.keys()) & set(to_by_slug.keys()):
            from_table = from_by_slug[slug]
            to_table = to_by_slug[slug]

            from_schema = from_table.get("schema", {})
            to_schema = to_table.get("schema", {})

            if from_schema != to_schema:
                compatibility = cls.check_compatibility(from_schema, to_schema)

                result["tables"].append(
                    {
                        "slug": slug,
                        "name": to_table.get("name", slug),
                        "change": "modified",
                        "risk": compatibility["risk_level"],
                        "compatibility": compatibility,
                    }
                )
                result["summary"]["tables_modified"] += 1

                all_warnings.extend(compatibility["warnings"])
                all_errors.extend(compatibility["errors"])

                if not compatibility["is_compatible"]:
                    result["is_compatible"] = False

        # Determine overall risk level
        result["summary"]["total_warnings"] = len(all_warnings)
        result["summary"]["total_errors"] = len(all_errors)

        if all_errors:
            result["risk_level"] = "danger"
        elif all_warnings:
            result["risk_level"] = "warning"

        return result
