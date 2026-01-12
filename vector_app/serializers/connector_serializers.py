"""
Serializers for Connector & Integration models.

User-facing serializers that abstract away internal Merge details.
Organization-level integration model.

Note: Merge API credentials are read from environment variables,
not from user input. The frontend no longer needs to provide them.
"""

from rest_framework import serializers
from ..models import (
    MergeIntegrationProvider,
    ConnectorCache,
    OrganizationConnectorLink,
)
from ..services.merge_service import is_merge_configured


class IntegrationProviderSerializer(serializers.ModelSerializer):
    """
    Serializer for reading integration providers.
    Hides internal Merge details from API response.
    """

    organization_name = serializers.CharField(source="organization.name", read_only=True)
    connector_count = serializers.SerializerMethodField()
    connected_count = serializers.SerializerMethodField()
    is_registered = serializers.SerializerMethodField()
    is_configured = serializers.SerializerMethodField()

    class Meta:
        model = MergeIntegrationProvider
        fields = [
            "id",
            "organization",
            "organization_name",
            "display_name",
            "is_active",
            "is_registered",
            "is_configured",
            "connector_count",
            "connected_count",
            "created_at",
            "updated_at",
        ]
        read_only_fields = ["id", "organization", "created_at", "updated_at"]

    def get_connector_count(self, obj):
        return obj.connectors.filter(is_enabled=True).count()

    def get_connected_count(self, obj):
        return OrganizationConnectorLink.objects.filter(
            provider=obj,
            is_connected=True,
        ).count()

    def get_is_registered(self, obj):
        return bool(obj.merge_registered_user_id)

    def get_is_configured(self, obj):
        """Check if Merge API credentials are configured in environment."""
        return is_merge_configured()


class IntegrationProviderCreateSerializer(serializers.ModelSerializer):
    """
    Serializer for creating integration providers.

    Note: Merge credentials are no longer required from users - they are
    read from environment variables. Users only need to specify the display name.
    """

    class Meta:
        model = MergeIntegrationProvider
        fields = [
            "id",
            "display_name",
            "is_active",
        ]
        read_only_fields = ["id"]

    def validate(self, attrs):
        """Validate that Merge is configured in environment."""
        if not is_merge_configured():
            raise serializers.ValidationError(
                "Integrations are not available. Merge API credentials are not configured."
            )
        return attrs

    def create(self, validated_data):
        organization = self.context.get("organization")
        if not organization:
            raise serializers.ValidationError("Organization context is required")

        provider = MergeIntegrationProvider.objects.create(organization=organization, **validated_data)

        return provider

    def update(self, instance, validated_data):
        for attr, value in validated_data.items():
            setattr(instance, attr, value)

        instance.save()
        return instance


class ConnectorToolSerializer(serializers.Serializer):
    """Serializer for connector tools."""

    id = serializers.CharField()
    name = serializers.CharField()
    description = serializers.CharField(allow_blank=True)
    parameters = serializers.JSONField(default=dict)


class ConnectorSerializer(serializers.ModelSerializer):
    """
    Serializer for listing connectors.
    """

    tool_count = serializers.SerializerMethodField()

    class Meta:
        model = ConnectorCache
        fields = [
            "id",
            "connector_id",
            "connector_name",
            "category",
            "icon_url",
            "description",
            "is_enabled",
            "tool_count",
            "last_synced_at",
        ]

    def get_tool_count(self, obj):
        return len(obj.tools_json or [])


class ConnectorDetailSerializer(serializers.ModelSerializer):
    """
    Detailed serializer for connectors including tools.
    """

    tools = serializers.SerializerMethodField()
    tool_count = serializers.SerializerMethodField()

    class Meta:
        model = ConnectorCache
        fields = [
            "id",
            "connector_id",
            "connector_name",
            "category",
            "icon_url",
            "description",
            "is_enabled",
            "tools",
            "tool_count",
            "last_synced_at",
        ]

    def get_tools(self, obj):
        return obj.tools_json or []

    def get_tool_count(self, obj):
        return len(obj.tools_json or [])


class OrgConnectorStatusSerializer(serializers.Serializer):
    """Serializer for organization connector connection status."""

    connector_id = serializers.CharField()
    connector_name = serializers.CharField()
    is_connected = serializers.BooleanField()
    connected_at = serializers.DateTimeField(allow_null=True)
    connected_by = serializers.CharField(allow_null=True)


class OrganizationConnectorLinkSerializer(serializers.ModelSerializer):
    """Serializer for organization connector links."""

    connector_id = serializers.CharField(source="connector.connector_id", read_only=True)
    connector_name = serializers.CharField(source="connector.connector_name", read_only=True)
    connected_by_email = serializers.CharField(source="connected_by.email", read_only=True, allow_null=True)

    class Meta:
        model = OrganizationConnectorLink
        fields = [
            "id",
            "connector_id",
            "connector_name",
            "is_connected",
            "connected_at",
            "connected_by_email",
        ]
        read_only_fields = ["id", "connected_at", "connected_by_email"]
