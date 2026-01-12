"""
Resource Registry serializers
"""

from rest_framework import serializers
from ..models import ResourceRegistryEntry


class ResourceRegistryEntrySerializer(serializers.ModelSerializer):
    """Serializer for ResourceRegistryEntry model."""

    class Meta:
        model = ResourceRegistryEntry
        fields = [
            "id",
            "organization",
            "backend_connection",
            "resource_id",
            "resource_name",
            "schema_json",
            "enabled",
            "exposed_fields_json",
            "ui_constraints_json",
            "allowed_actions_json",
            "created_at",
            "updated_at",
        ]
        read_only_fields = ["id", "created_at", "updated_at"]

    def validate_exposed_fields_json(self, value):
        """Validate exposed_fields_json is a list of strings."""
        if not isinstance(value, list):
            raise serializers.ValidationError("exposed_fields_json must be a list.")
        if not all(isinstance(item, str) for item in value):
            raise serializers.ValidationError("All items in exposed_fields_json must be strings.")
        return value

    def validate_allowed_actions_json(self, value):
        """Validate allowed_actions_json is a list of ActionDef objects."""
        if not isinstance(value, list):
            raise serializers.ValidationError("allowed_actions_json must be a list.")
        # TODO: Add more detailed validation for ActionDef structure
        return value
