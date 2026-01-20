"""
CRM serializers for CustomerGroup model.
"""
from rest_framework import serializers
from .models import CustomerGroup, HealthCategory


class CustomerGroupSerializer(serializers.ModelSerializer):
    """Serializer for CustomerGroup model."""
    health_display = serializers.CharField(source='get_health_display', read_only=True)
    created_by_email = serializers.EmailField(source='created_by.email', read_only=True)

    class Meta:
        model = CustomerGroup
        fields = [
            'id',
            'organization',
            'name',
            'description',
            'health',
            'health_display',
            'potential_value',
            'created_by',
            'created_by_email',
            'created_at',
            'updated_at',
        ]
        read_only_fields = ['id', 'organization', 'created_by', 'created_at', 'updated_at']

    def validate_potential_value(self, value):
        """Ensure potential value is non-negative."""
        if value < 0:
            raise serializers.ValidationError("Potential value must be non-negative.")
        return value

    def validate_health(self, value):
        """Ensure health is a valid category."""
        valid_choices = [choice[0] for choice in HealthCategory.choices]
        if value not in valid_choices:
            raise serializers.ValidationError(
                f"Invalid health category. Must be one of: {', '.join(valid_choices)}"
            )
        return value


class CustomerGroupCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating customer groups."""

    class Meta:
        model = CustomerGroup
        fields = [
            'name',
            'description',
            'health',
            'potential_value',
        ]

    def validate_potential_value(self, value):
        """Ensure potential value is non-negative."""
        if value is not None and value < 0:
            raise serializers.ValidationError("Potential value must be non-negative.")
        return value

    def create(self, validated_data):
        """Create customer group and set creator."""
        validated_data['created_by'] = self.context['request'].user
        validated_data['organization'] = self.context['organization']
        return super().create(validated_data)


class CustomerGroupStatsSerializer(serializers.Serializer):
    """Serializer for dashboard statistics."""
    total_groups = serializers.IntegerField()
    total_value = serializers.DecimalField(max_digits=15, decimal_places=2)
    by_health = serializers.DictField(child=serializers.IntegerField())
    value_by_health = serializers.DictField(
        child=serializers.DecimalField(max_digits=15, decimal_places=2)
    )
    top_groups = CustomerGroupSerializer(many=True)
