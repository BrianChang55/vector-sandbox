"""
Organization serializers
"""
from rest_framework import serializers
from ..models import Organization, UserOrganization


class OrganizationSerializer(serializers.ModelSerializer):
    """Serializer for Organization model."""
    
    class Meta:
        model = Organization
        fields = [
            'id',
            'name',
            'slug',
            'created_at',
            'updated_at',
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']


class OrganizationCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating organizations."""
    
    class Meta:
        model = Organization
        fields = ['name', 'slug']
    
    def validate_slug(self, value):
        """Validate slug is unique."""
        if Organization.objects.filter(slug=value).exists():
            raise serializers.ValidationError("Organization with this slug already exists.")
        return value
    
    def create(self, validated_data):
        """Create organization and add creator as admin."""
        organization = Organization.objects.create(**validated_data)
        
        # Add creator as admin
        user = self.context['request'].user
        UserOrganization.objects.create(
            user=user,
            organization=organization,
            role=UserOrganization.ROLE_ADMIN
        )
        
        return organization


class UserOrganizationSerializer(serializers.ModelSerializer):
    """Serializer for UserOrganization membership."""
    user_email = serializers.EmailField(source='user.email', read_only=True)
    organization_name = serializers.CharField(source='organization.name', read_only=True)
    
    class Meta:
        model = UserOrganization
        fields = [
            'id',
            'user',
            'user_email',
            'organization',
            'organization_name',
            'role',
            'created_at',
            'updated_at',
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']

