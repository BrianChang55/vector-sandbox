"""
Task serializers for task list management.
"""
from rest_framework import serializers
from tasks.models import Task


class TaskSerializer(serializers.ModelSerializer):
    """Serializer for reading Task objects."""
    created_by_email = serializers.EmailField(source='created_by.email', read_only=True)

    class Meta:
        model = Task
        fields = [
            'id',
            'organization',
            'title',
            'description',
            'completed',
            'completed_at',
            'created_by',
            'created_by_email',
            'created_at',
            'updated_at',
        ]
        read_only_fields = ['id', 'organization', 'created_by', 'created_at', 'updated_at', 'completed_at']


class TaskCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating Task objects."""

    class Meta:
        model = Task
        fields = ['title', 'description']

    def create(self, validated_data):
        validated_data['created_by'] = self.context['request'].user
        validated_data['organization'] = self.context['organization']
        return super().create(validated_data)


class TaskUpdateSerializer(serializers.ModelSerializer):
    """Serializer for updating Task objects."""

    class Meta:
        model = Task
        fields = ['title', 'description', 'completed']
