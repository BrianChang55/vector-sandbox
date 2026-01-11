"""
Organization serializers
"""
from rest_framework import serializers
from ..models import Organization, UserOrganization


class OrganizationSerializer(serializers.ModelSerializer):
    """Serializer for Organization model."""
    logo_url = serializers.SerializerMethodField()
    
    class Meta:
        model = Organization
        fields = [
            'id',
            'name',
            'slug',
            'logo_url',
            'created_at',
            'updated_at',
        ]
        read_only_fields = ['id', 'slug', 'created_at', 'updated_at']
    
    def get_logo_url(self, obj):
        """Return full URL for the logo if it exists."""
        if obj.logo:
            request = self.context.get('request')
            if request:
                return request.build_absolute_uri(obj.logo.url)
            return obj.logo.url
        return None


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


class OrganizationUpdateSerializer(serializers.ModelSerializer):
    """Serializer for updating organization."""
    
    class Meta:
        model = Organization
        fields = ['name', 'slug']
    
    def validate_slug(self, value):
        """Validate slug is unique (excluding current org)."""
        instance = self.instance
        if Organization.objects.filter(slug=value).exclude(id=instance.id).exists():
            raise serializers.ValidationError("Organization with this slug already exists.")
        return value


class OrganizationLogoUploadSerializer(serializers.Serializer):
    """Serializer for logo upload."""
    logo = serializers.ImageField(required=True)
    
    def validate_logo(self, value):
        """Validate the uploaded image."""
        # Check file size (max 5MB)
        max_size = 5 * 1024 * 1024  # 5MB
        if value.size > max_size:
            raise serializers.ValidationError("Logo file size must be less than 5MB.")
        
        # Check file type
        allowed_types = ['image/jpeg', 'image/png', 'image/gif', 'image/webp']
        if value.content_type not in allowed_types:
            raise serializers.ValidationError("Logo must be a JPEG, PNG, GIF, or WebP image.")
        
        return value


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
