"""
Internal App serializers
"""

from rest_framework import serializers
from ..models import InternalApp, AppFavorite


class InternalAppSerializer(serializers.ModelSerializer):
    """Serializer for InternalApp model."""

    status_display = serializers.CharField(source="get_status_display", read_only=True)
    created_by_email = serializers.EmailField(source="created_by.email", read_only=True)
    backend_connection_name = serializers.CharField(source="backend_connection.display_name", read_only=True)
    published_url = serializers.SerializerMethodField()
    published_version_id = serializers.UUIDField(
        source="published_version.id", read_only=True, allow_null=True
    )
    published_version_number = serializers.IntegerField(
        source="published_version.version_number", read_only=True, allow_null=True
    )
    published_at = serializers.DateTimeField(
        source="published_version.created_at", read_only=True, allow_null=True
    )
    organization_slug = serializers.CharField(source="organization.slug", read_only=True)

    class Meta:
        model = InternalApp
        fields = [
            "id",
            "organization",
            "organization_slug",
            "name",
            "slug",
            "description",
            "status",
            "status_display",
            "backend_connection",
            "backend_connection_name",
            "published_version_id",
            "published_version_number",
            "published_at",
            "published_url",
            "allow_actions_in_preview",
            "created_by",
            "created_by_email",
            "created_at",
            "updated_at",
        ]
        read_only_fields = [
            "id",
            "slug",
            "created_by",
            "created_at",
            "updated_at",
            "published_version_id",
            "published_version_number",
            "published_at",
        ]

    def get_published_url(self, obj):
        """Generate the published URL for this app."""
        if obj.published_version and obj.slug and obj.organization:
            return f"/{obj.organization.slug}/{obj.slug}"
        return None


class InternalAppCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating internal apps."""

    backend_connection = serializers.PrimaryKeyRelatedField(
        queryset=InternalApp._meta.get_field("backend_connection").related_model.objects.all(),
        required=False,
        allow_null=True,
    )
    slug = serializers.SlugField(required=False, allow_blank=True, allow_null=True)

    class Meta:
        model = InternalApp
        fields = [
            "name",
            "slug",
            "description",
            "backend_connection",
            "allow_actions_in_preview",
        ]

    def create(self, validated_data):
        """Create app and set creator. Slug is auto-generated in model.save() if not provided."""
        validated_data["created_by"] = self.context["request"].user
        validated_data["organization"] = self.context["organization"]
        return super().create(validated_data)


class AppFavoriteSerializer(serializers.ModelSerializer):
    """Serializer for app favorites."""

    app_id = serializers.UUIDField(source="app.id", read_only=True)

    class Meta:
        model = AppFavorite
        fields = ["id", "app_id", "created_at"]
        read_only_fields = ["id", "created_at"]
