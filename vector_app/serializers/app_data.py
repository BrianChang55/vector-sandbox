"""
Serializers for App Data Store (tables and rows).
"""

from rest_framework import serializers
from ..models import AppDataTable, AppDataRow, InternalApp


class AppDataTableSerializer(serializers.ModelSerializer):
    """
    Serializer for AppDataTable model.

    Used for listing and retrieving table metadata.
    """

    column_count = serializers.SerializerMethodField()

    class Meta:
        model = AppDataTable
        fields = [
            "id",
            "internal_app",
            "name",
            "slug",
            "description",
            "schema_json",
            "row_count",
            "column_count",
            "created_at",
            "updated_at",
        ]
        read_only_fields = ["id", "slug", "row_count", "created_at", "updated_at"]

    def get_column_count(self, obj) -> int:
        """Get the number of columns in the schema."""
        return len(obj.get_columns())


class AppDataTableCreateSerializer(serializers.Serializer):
    """
    Serializer for creating a new data table.
    """

    name = serializers.CharField(max_length=255)
    description = serializers.CharField(required=False, allow_blank=True, default="")
    schema_json = serializers.JSONField()

    def validate_schema_json(self, value):
        """Validate schema structure."""
        from ..services.schema_validator import SchemaValidator

        is_valid, errors = SchemaValidator.validate_schema(value)
        if not is_valid:
            raise serializers.ValidationError(errors)

        return value


class AppDataTableUpdateSerializer(serializers.Serializer):
    """
    Serializer for updating a data table.
    """

    name = serializers.CharField(max_length=255, required=False)
    description = serializers.CharField(required=False, allow_blank=True)
    schema_json = serializers.JSONField(required=False)

    def validate_schema_json(self, value):
        """Validate schema structure if provided."""
        if value is not None:
            from ..services.schema_validator import SchemaValidator

            is_valid, errors = SchemaValidator.validate_schema(value)
            if not is_valid:
                raise serializers.ValidationError(errors)

        return value


class AppDataRowSerializer(serializers.ModelSerializer):
    """
    Serializer for AppDataRow model.

    Used for listing and retrieving rows.
    """

    class Meta:
        model = AppDataRow
        fields = [
            "id",
            "table",
            "data",
            "row_index",
            "created_at",
            "updated_at",
        ]
        read_only_fields = ["id", "table", "row_index", "created_at", "updated_at"]


class AppDataRowDataSerializer(serializers.Serializer):
    """
    Serializer for row data input.

    The data field is a JSON object with column values.
    """

    data = serializers.JSONField()

    def validate_data(self, value):
        """Validate that data is a dictionary."""
        if not isinstance(value, dict):
            raise serializers.ValidationError("data must be a JSON object")
        return value


class AppDataRowBulkSerializer(serializers.Serializer):
    """
    Serializer for bulk row operations.
    """

    rows = serializers.ListField(child=serializers.JSONField(), min_length=1, max_length=1000)

    def validate_rows(self, value):
        """Validate that each row is a dictionary."""
        for i, row in enumerate(value):
            if not isinstance(row, dict):
                raise serializers.ValidationError(f"Row {i}: must be a JSON object")
        return value


class AppDataRowBulkDeleteSerializer(serializers.Serializer):
    """
    Serializer for bulk row deletion.
    """

    row_ids = serializers.ListField(child=serializers.UUIDField(), min_length=1, max_length=1000)


class QuerySpecSerializer(serializers.Serializer):
    """
    Serializer for query specification.
    """

    select = serializers.ListField(child=serializers.CharField(), required=False, allow_empty=True)
    filters = serializers.ListField(child=serializers.DictField(), required=False, allow_empty=True)
    order_by = serializers.ListField(child=serializers.DictField(), required=False, allow_empty=True)
    limit = serializers.IntegerField(required=False, min_value=1, max_value=1000, default=50)
    offset = serializers.IntegerField(required=False, min_value=0, default=0)

    def validate_filters(self, value):
        """Validate filter definitions."""
        valid_ops = [
            "eq",
            "neq",
            "gt",
            "gte",
            "lt",
            "lte",
            "in",
            "not_in",
            "contains",
            "icontains",
            "is_null",
        ]

        for i, f in enumerate(value or []):
            if "field" not in f:
                raise serializers.ValidationError(f'Filter {i}: missing "field"')

            op = f.get("op", "eq")
            if op not in valid_ops:
                raise serializers.ValidationError(
                    f'Filter {i}: invalid operator "{op}". Valid: {", ".join(valid_ops)}'
                )

            if "value" not in f and op != "is_null":
                raise serializers.ValidationError(f'Filter {i}: missing "value"')

        return value

    def validate_order_by(self, value):
        """Validate order_by definitions."""
        for i, o in enumerate(value or []):
            if "field" not in o:
                raise serializers.ValidationError(f'Order {i}: missing "field"')

            direction = o.get("dir", "asc")
            if direction not in ("asc", "desc"):
                raise serializers.ValidationError(
                    f'Order {i}: invalid direction "{direction}". Use "asc" or "desc"'
                )

        return value


class QueryResultSerializer(serializers.Serializer):
    """
    Serializer for query results.
    """

    rows = serializers.ListField(child=serializers.DictField())
    total_count = serializers.IntegerField()
    limit = serializers.IntegerField()
    offset = serializers.IntegerField()
    has_more = serializers.BooleanField()
