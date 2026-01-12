"""
Backend Connection serializers
"""

from rest_framework import serializers
from ..models import BackendConnection


class BackendConnectionSerializer(serializers.ModelSerializer):
    """Serializer for BackendConnection model."""

    adapter_type_display = serializers.CharField(source="get_adapter_type_display", read_only=True)

    class Meta:
        model = BackendConnection
        fields = [
            "id",
            "organization",
            "adapter_type",
            "adapter_type_display",
            "display_name",
            "created_at",
            "updated_at",
        ]
        read_only_fields = ["id", "created_at", "updated_at"]


class BackendConnectionCreateSerializer(serializers.Serializer):
    """Serializer for creating backend connections."""

    adapter_type = serializers.ChoiceField(choices=BackendConnection.ADAPTER_CHOICES)
    display_name = serializers.CharField(max_length=255)

    # Supabase configuration
    supabase_url = serializers.URLField(required=False)
    anon_key = serializers.CharField(required=False, allow_blank=True)
    service_role_key = serializers.CharField(required=False, allow_blank=True)

    # Direct database configuration (PostgreSQL, MySQL)
    host = serializers.CharField(required=False, max_length=255)
    port = serializers.IntegerField(required=False, min_value=1, max_value=65535)
    database = serializers.CharField(required=False, max_length=255)
    username = serializers.CharField(required=False, max_length=255)
    password = serializers.CharField(required=False, allow_blank=True)
    ssl_mode = serializers.ChoiceField(
        choices=[("disable", "Disable"), ("require", "Require"), ("verify-full", "Verify Full")],
        required=False,
        default="disable",
    )

    def validate(self, attrs):
        """Validate configuration based on adapter type."""
        adapter_type = attrs.get("adapter_type")

        if adapter_type == BackendConnection.ADAPTER_SUPABASE:
            if not attrs.get("supabase_url"):
                raise serializers.ValidationError({"supabase_url": "Required for Supabase adapter."})
            if not attrs.get("service_role_key"):
                raise serializers.ValidationError({"service_role_key": "Required for Supabase adapter."})

        elif adapter_type in [BackendConnection.ADAPTER_POSTGRESQL, BackendConnection.ADAPTER_MYSQL]:
            if not attrs.get("host"):
                raise serializers.ValidationError({"host": "Required for database connection."})
            if not attrs.get("database"):
                raise serializers.ValidationError({"database": "Required for database connection."})
            if not attrs.get("username"):
                raise serializers.ValidationError({"username": "Required for database connection."})

        return attrs

    def create(self, validated_data):
        """Create backend connection with encrypted config."""
        organization = self.context["organization"]
        adapter_type = validated_data.pop("adapter_type")
        display_name = validated_data.pop("display_name")

        # Extract config based on adapter type
        config = {}
        if adapter_type == BackendConnection.ADAPTER_SUPABASE:
            config = {
                "supabase_url": validated_data.pop("supabase_url", ""),
                "anon_key": validated_data.pop("anon_key", ""),
                "service_role_key": validated_data.pop("service_role_key", ""),
            }
        elif adapter_type in [BackendConnection.ADAPTER_POSTGRESQL, BackendConnection.ADAPTER_MYSQL]:
            default_port = 5432 if adapter_type == BackendConnection.ADAPTER_POSTGRESQL else 3306
            config = {
                "host": validated_data.pop("host", ""),
                "port": validated_data.pop("port", default_port),
                "database": validated_data.pop("database", ""),
                "username": validated_data.pop("username", ""),
                "password": validated_data.pop("password", ""),
                "ssl_mode": validated_data.pop("ssl_mode", "disable"),
            }

        # Create connection
        connection = BackendConnection.objects.create(
            organization=organization,
            adapter_type=adapter_type,
            display_name=display_name,
        )

        # Set encrypted config
        connection.set_config(config)
        connection.save()

        return connection


class BackendConnectionTestSerializer(serializers.Serializer):
    """Serializer for testing backend connections."""

    success = serializers.BooleanField()
    message = serializers.CharField()
    capabilities = serializers.DictField(required=False)
