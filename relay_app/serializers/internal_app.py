"""
Internal App serializers
"""
from rest_framework import serializers
from ..models import InternalApp


class InternalAppSerializer(serializers.ModelSerializer):
    """Serializer for InternalApp model."""
    status_display = serializers.CharField(source='get_status_display', read_only=True)
    created_by_email = serializers.EmailField(source='created_by.email', read_only=True)
    backend_connection_name = serializers.CharField(source='backend_connection.display_name', read_only=True)
    
    class Meta:
        model = InternalApp
        fields = [
            'id',
            'organization',
            'name',
            'description',
            'status',
            'status_display',
            'backend_connection',
            'backend_connection_name',
            'allow_actions_in_preview',
            'created_by',
            'created_by_email',
            'created_at',
            'updated_at',
        ]
        read_only_fields = ['id', 'created_by', 'created_at', 'updated_at']


class InternalAppCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating internal apps."""
    backend_connection = serializers.PrimaryKeyRelatedField(
        queryset=InternalApp._meta.get_field('backend_connection').related_model.objects.all(),
        required=False,
        allow_null=True,
    )
    
    class Meta:
        model = InternalApp
        fields = [
            'name',
            'description',
            'backend_connection',
            'allow_actions_in_preview',
        ]
    
    def create(self, validated_data):
        """Create app and set creator."""
        validated_data['created_by'] = self.context['request'].user
        validated_data['organization'] = self.context['organization']
        return super().create(validated_data)

