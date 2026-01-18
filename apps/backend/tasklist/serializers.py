"""
Tasklist serializers.
"""
from rest_framework import serializers
from django.utils import timezone

from .models import Task


class TaskSerializer(serializers.ModelSerializer):
    """Serializer for Task model - read operations."""

    created_by_email = serializers.EmailField(source="created_by.email", read_only=True)

    class Meta:
        model = Task
        fields = [
            "id",
            "title",
            "description",
            "is_completed",
            "completed_at",
            "created_by_email",
            "created_at",
            "updated_at",
        ]
        read_only_fields = ["id", "completed_at", "created_by_email", "created_at", "updated_at"]


class TaskCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating a task."""

    class Meta:
        model = Task
        fields = ["title", "description"]

    def create(self, validated_data):
        validated_data["created_by"] = self.context["request"].user
        validated_data["organization"] = self.context["organization"]
        return super().create(validated_data)


class TaskUpdateSerializer(serializers.ModelSerializer):
    """Serializer for updating a task."""

    class Meta:
        model = Task
        fields = ["title", "description", "is_completed"]

    def update(self, instance, validated_data):
        # Track completion timestamp
        if "is_completed" in validated_data:
            if validated_data["is_completed"] and not instance.is_completed:
                validated_data["completed_at"] = timezone.now()
            elif not validated_data["is_completed"]:
                validated_data["completed_at"] = None
        return super().update(instance, validated_data)
