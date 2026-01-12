"""
Authentication serializers for user registration and login.
"""

from rest_framework import serializers
from django.contrib.auth.password_validation import validate_password
from django.contrib.auth import authenticate
from ..models import User, MagicLinkToken


class UserSerializer(serializers.ModelSerializer):
    """
    Serializer for User model.
    """

    class Meta:
        model = User
        fields = [
            "id",
            "email",
            "first_name",
            "last_name",
            "date_joined",
            "created_at",
            "updated_at",
        ]
        read_only_fields = ["id", "date_joined", "created_at", "updated_at"]


class SignUpSerializer(serializers.ModelSerializer):
    """
    Serializer for user registration (sign up).
    """

    password = serializers.CharField(
        write_only=True, required=True, validators=[validate_password], style={"input_type": "password"}
    )
    password_confirm = serializers.CharField(write_only=True, required=True, style={"input_type": "password"})

    class Meta:
        model = User
        fields = ["email", "first_name", "last_name", "password", "password_confirm"]
        extra_kwargs = {
            "first_name": {"required": True},
            "last_name": {"required": True},
        }

    def validate(self, attrs):
        if attrs["password"] != attrs["password_confirm"]:
            raise serializers.ValidationError({"password_confirm": "Password fields didn't match."})
        return attrs

    def create(self, validated_data):
        validated_data.pop("password_confirm")
        password = validated_data.pop("password")

        user = User.objects.create(**validated_data)
        user.set_password(password)
        user.username = validated_data["email"]  # Use email as username
        user.save()
        return user


class LoginSerializer(serializers.Serializer):
    """
    Serializer for user login.
    """

    email = serializers.EmailField(required=True)
    password = serializers.CharField(required=True, write_only=True, style={"input_type": "password"})

    def validate(self, attrs):
        email = attrs.get("email")
        password = attrs.get("password")

        if email and password:
            user = authenticate(request=self.context.get("request"), username=email, password=password)

            if not user:
                raise serializers.ValidationError("Invalid email or password.")

            if not user.is_active:
                raise serializers.ValidationError("User account is disabled.")

            attrs["user"] = user
            return attrs
        else:
            raise serializers.ValidationError('Must include "email" and "password".')


# ============================================================================
# Magic Link Authentication Serializers
# ============================================================================


class MagicLinkRequestSerializer(serializers.Serializer):
    """
    Serializer for requesting a magic link.
    Works for both signin (existing users) and signup (new users).
    """

    email = serializers.EmailField(required=True)
    # Optional fields for signup flow
    first_name = serializers.CharField(required=False, allow_blank=True, max_length=150)
    last_name = serializers.CharField(required=False, allow_blank=True, max_length=150)

    def validate_email(self, value):
        """Normalize email to lowercase."""
        return value.lower().strip()


class MagicLinkVerifySerializer(serializers.Serializer):
    """
    Serializer for verifying a magic link token.
    """

    token = serializers.CharField(required=True)

    def validate_token(self, value):
        """Verify the magic link token."""
        magic_link = MagicLinkToken.verify_token(value)
        if not magic_link:
            raise serializers.ValidationError("Invalid or expired magic link. Please request a new one.")

        # Store the magic link for use in the view
        self.magic_link = magic_link
        return value
