"""
Member and Invitation serializers for organization management.
"""
from rest_framework import serializers
from django.utils import timezone

from internal_apps.utils.enum import choices
from ..models import Organization, OrganizationInvite, User, UserOrganization, UserOrganizationRole


class OrgMemberSerializer(serializers.ModelSerializer):
    """Serializer for organization member (UserOrganization with user details)."""
    user_id = serializers.UUIDField(source='user.id', read_only=True)
    email = serializers.EmailField(source='user.email', read_only=True)
    first_name = serializers.CharField(source='user.first_name', read_only=True)
    last_name = serializers.CharField(source='user.last_name', read_only=True)
    profile_image_url = serializers.SerializerMethodField()
    joined_at = serializers.DateTimeField(source='created_at', read_only=True)
    role_display = serializers.CharField(source='get_role_display', read_only=True)
    
    class Meta:
        model = UserOrganization
        fields = [
            'id',
            'user_id',
            'email',
            'first_name',
            'last_name',
            'profile_image_url',
            'role',
            'role_display',
            'joined_at',
        ]
        read_only_fields = ['id', 'user_id', 'email', 'first_name', 'last_name', 'profile_image_url', 'joined_at']
    
    def get_profile_image_url(self, obj):
        """
        Get the user's profile image URL.
        
        Uses the model's get_profile_image_url method which handles
        both uploaded images (cloud/local) and OAuth provider URLs.
        """
        return obj.user.get_profile_image_url()


class OrgMemberUpdateSerializer(serializers.Serializer):
    """Serializer for updating a member's role."""
    role = serializers.ChoiceField(choices=choices(UserOrganizationRole))
    
    def validate_role(self, value):
        """Validate role change is allowed."""
        member = self.context.get('member')
        requesting_user = self.context.get('request').user
        
        # Cannot change your own role
        if member and member.user == requesting_user:
            raise serializers.ValidationError("You cannot change your own role.")
        
        return value


class OrgInviteSerializer(serializers.ModelSerializer):
    """Serializer for organization invitation."""
    invited_by_email = serializers.EmailField(source='invited_by.email', read_only=True, allow_null=True)
    invited_by_name = serializers.SerializerMethodField()
    organization_name = serializers.CharField(source='organization.name', read_only=True)
    role_display = serializers.SerializerMethodField()
    is_expired = serializers.BooleanField(read_only=True)
    is_valid = serializers.BooleanField(read_only=True)
    
    class Meta:
        model = OrganizationInvite
        fields = [
            'id',
            'email',
            'role',
            'role_display',
            'invited_by_email',
            'invited_by_name',
            'organization_name',
            'created_at',
            'expires_at',
            'is_expired',
            'is_valid',
            'is_accepted',
        ]
        read_only_fields = ['id', 'created_at', 'expires_at', 'is_expired', 'is_valid', 'is_accepted']
    
    def get_invited_by_name(self, obj):
        if obj.invited_by:
            first = obj.invited_by.first_name or ''
            last = obj.invited_by.last_name or ''
            return f"{first} {last}".strip() or obj.invited_by.email
        return None
    
    def get_role_display(self, obj):
        return dict(choices(UserOrganizationRole)).get(obj.role, obj.role)


class OrgInviteCreateSerializer(serializers.Serializer):
    """Serializer for creating an organization invitation."""
    email = serializers.EmailField()
    role = serializers.ChoiceField(
        choices=choices(UserOrganizationRole),
        default=UserOrganizationRole.EDITOR
    )
    
    def validate_email(self, value):
        """Validate email is not already a member."""
        organization = self.context.get('organization')
        
        if not organization:
            raise serializers.ValidationError("Organization not found.")
        
        # Check if user is already a member
        user = User.objects.filter(email=value).first()
        if user:
            existing = UserOrganization.objects.filter(
                user=user,
                organization=organization
            ).exists()
            if existing:
                raise serializers.ValidationError(
                    "This user is already a member of the organization."
                )
        
        return value.lower()
    
    def create(self, validated_data):
        """Create the invitation."""
        organization = self.context.get('organization')
        invited_by = self.context.get('request').user
        
        invite, raw_token = OrganizationInvite.create_invite(
            organization=organization,
            email=validated_data['email'],
            role=validated_data['role'],
            invited_by=invited_by,
        )
        
        # Attach raw token to instance for email sending
        invite._raw_token = raw_token
        
        return invite


class InviteAcceptSerializer(serializers.Serializer):
    """Serializer for accepting an organization invitation."""
    token = serializers.CharField(required=True)
    
    def validate_token(self, value):
        """Validate the invitation token."""
        invite = OrganizationInvite.verify_token(value)
        
        if not invite:
            raise serializers.ValidationError(
                "Invalid or expired invitation. Please request a new one."
            )
        
        # Store the invite for use in view
        self.invite = invite
        return value


class InviteVerifySerializer(serializers.Serializer):
    """Serializer for verifying an invitation token (without accepting)."""
    token = serializers.CharField(required=True)
    
    def validate_token(self, value):
        """Validate the invitation token."""
        invite = OrganizationInvite.verify_token(value)
        
        if not invite:
            raise serializers.ValidationError(
                "Invalid or expired invitation."
            )
        
        self.invite = invite
        return value


class InviteDetailsSerializer(serializers.Serializer):
    """Serializer for invitation details (public, for accept page)."""
    organization_name = serializers.CharField()
    organization_logo_url = serializers.URLField(allow_null=True)
    role = serializers.CharField()
    role_display = serializers.CharField()
    invited_by_name = serializers.CharField(allow_null=True)
    email = serializers.EmailField()
    expires_at = serializers.DateTimeField()


class MembersListResponseSerializer(serializers.Serializer):
    """Response serializer for members list endpoint."""
    members = OrgMemberSerializer(many=True)
    pending_invites = OrgInviteSerializer(many=True)
    current_user_role = serializers.CharField()
    can_manage_members = serializers.BooleanField()

