#!/usr/bin/env python
"""
Generate JWT tokens for a dev user.
Usage: python scripts/dev_login.py
Outputs: JSON with access and refresh tokens

SECURITY: This script only runs when DEBUG=True in Django settings.
"""
import os
import sys
import json
import django

# Setup Django
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'internal_apps.settings')
django.setup()

from django.conf import settings
from django.contrib.auth import get_user_model
from rest_framework_simplejwt.tokens import RefreshToken
from accounts.models import Organization, UserOrganization
from accounts.types import UserOrganizationRole

# SECURITY: Only allow in development mode
if not settings.DEBUG:
    print("ERROR: dev_login.py can only be used when DEBUG=True", file=sys.stderr)
    sys.exit(1)

User = get_user_model()

DEV_EMAIL = "dev@localhost"
DEV_FIRST_NAME = "Dev"
DEV_LAST_NAME = "User"


def get_or_create_dev_user():
    """Get or create the dev user with a default organization."""
    user, created = User.objects.get_or_create(
        email=DEV_EMAIL,
        defaults={
            'username': DEV_EMAIL,
            'first_name': DEV_FIRST_NAME,
            'last_name': DEV_LAST_NAME,
            'is_active': True,
        }
    )
    
    if created:
        # Create default organization for dev user
        org = Organization.objects.create(
            name="Dev Organization",
            slug="dev-org",
        )
        UserOrganization.objects.create(
            user=user,
            organization=org,
            role=UserOrganizationRole.ADMIN,
        )
    
    return user


def generate_tokens(user):
    """Generate JWT tokens for a user."""
    refresh = RefreshToken.for_user(user)
    return {
        'access': str(refresh.access_token),
        'refresh': str(refresh),
    }


if __name__ == '__main__':
    user = get_or_create_dev_user()
    tokens = generate_tokens(user)
    print(json.dumps(tokens))
