"""
Custom permission classes for role-based access control.

These permission classes work with the UserOrganization model's role system:
- Admin: Full access - manage org, members, integrations, publish
- Editor: Can create/edit apps, versions, data
- Viewer: Read-only access to published apps

Usage:
    class MyViewSet(viewsets.ModelViewSet):
        permission_classes = [IsAuthenticated, IsOrgEditorOrAbove]

    # Or for function-based views:
    @api_view(['POST'])
    @permission_classes([IsAuthenticated, IsOrgAdmin])
    def my_view(request, org_id):
        ...
"""

import logging
from rest_framework.permissions import BasePermission
from django.shortcuts import get_object_or_404

from .models import UserOrganization, Organization, InternalApp

logger = logging.getLogger(__name__)


def get_organization_from_request(request, view):
    """
    Extract organization from the request/view context.

    Looks for organization in multiple places:
    1. URL kwargs: org_id, organization_pk, pk (for org endpoints)
    2. Request data: organization_id, org_id
    3. Related object: app -> organization
    """
    # Check URL kwargs
    org_id = (
        view.kwargs.get("org_id") or view.kwargs.get("organization_pk") or view.kwargs.get("organization_id")
    )

    if org_id:
        try:
            return Organization.objects.get(id=org_id)
        except Organization.DoesNotExist:
            return None

    # Check if this is an organization endpoint (pk is the org id)
    if hasattr(view, "basename") and view.basename == "organization":
        pk = view.kwargs.get("pk")
        if pk:
            try:
                return Organization.objects.get(id=pk)
            except Organization.DoesNotExist:
                return None

    # Check for app-based endpoints
    app_id = (
        view.kwargs.get("internal_app_pk")
        or view.kwargs.get("app_id")
        or view.kwargs.get("pk")  # For direct app access like /apps/:id/
    )

    if app_id:
        try:
            app = InternalApp.objects.select_related("organization").get(id=app_id)
            return app.organization
        except (InternalApp.DoesNotExist, ValueError):
            pass

    # Check request data
    if hasattr(request, "data"):
        data_org_id = request.data.get("organization_id") or request.data.get("org_id")
        if data_org_id:
            try:
                return Organization.objects.get(id=data_org_id)
            except Organization.DoesNotExist:
                return None

    return None


def get_user_membership(user, organization):
    """Get the user's membership in an organization."""
    if not user or not user.is_authenticated or not organization:
        return None

    try:
        return UserOrganization.objects.get(user=user, organization=organization)
    except UserOrganization.DoesNotExist:
        return None


class IsOrgMember(BasePermission):
    """
    Permission class that checks if the user is a member of the organization.

    This is the base permission - any role (admin, editor, viewer) passes.
    """

    message = "You must be a member of this organization."

    def has_permission(self, request, view):
        if not request.user or not request.user.is_authenticated:
            return False

        organization = get_organization_from_request(request, view)
        if not organization:
            # If we can't determine the organization, let the view handle it
            # This allows list views that filter by user's orgs to work
            return True

        membership = get_user_membership(request.user, organization)
        if membership:
            # Store membership on request for later use
            request.org_membership = membership
            return True

        return False


class IsOrgEditorOrAbove(BasePermission):
    """
    Permission class that checks if the user is an editor or admin.

    Editors and admins can:
    - Create and edit apps
    - Create and edit versions
    - Manage app data (tables/rows)
    - Generate code with AI
    """

    message = "You must be an editor or admin of this organization to perform this action."

    def has_permission(self, request, view):
        if not request.user or not request.user.is_authenticated:
            return False

        # Allow safe methods (GET, HEAD, OPTIONS) for all members
        # Only restrict mutating methods
        if request.method in ("GET", "HEAD", "OPTIONS"):
            organization = get_organization_from_request(request, view)
            if not organization:
                return True
            membership = get_user_membership(request.user, organization)
            if membership:
                request.org_membership = membership
                return True
            return False

        organization = get_organization_from_request(request, view)
        if not organization:
            return True

        membership = get_user_membership(request.user, organization)
        if membership and membership.is_editor_or_above():
            request.org_membership = membership
            return True

        return False


class IsOrgAdmin(BasePermission):
    """
    Permission class that checks if the user is an admin.

    Admins can:
    - Manage organization settings
    - Manage members and invitations
    - Manage integrations
    - Publish apps
    - Delete apps
    - Manage backend connections
    """

    message = "You must be an admin of this organization to perform this action."

    def has_permission(self, request, view):
        if not request.user or not request.user.is_authenticated:
            return False

        # Allow safe methods (GET, HEAD, OPTIONS) for all members
        # Only restrict mutating methods
        if request.method in ("GET", "HEAD", "OPTIONS"):
            organization = get_organization_from_request(request, view)
            if not organization:
                return True
            membership = get_user_membership(request.user, organization)
            if membership:
                request.org_membership = membership
                return True
            return False

        organization = get_organization_from_request(request, view)
        if not organization:
            return True

        membership = get_user_membership(request.user, organization)
        if membership and membership.is_admin():
            request.org_membership = membership
            return True

        return False


# =============================================================================
# Helper functions for views that need fine-grained control
# =============================================================================


def require_org_membership(request, organization):
    """
    Check if user is a member of the organization.

    Returns (membership, error_response) tuple.
    If membership is None, error_response contains the Response to return.
    """
    from rest_framework.response import Response
    from rest_framework import status

    if not request.user or not request.user.is_authenticated:
        return None, Response({"error": "Authentication required"}, status=status.HTTP_401_UNAUTHORIZED)

    membership = get_user_membership(request.user, organization)
    if not membership:
        return None, Response(
            {"error": "You are not a member of this organization"}, status=status.HTTP_403_FORBIDDEN
        )

    return membership, None


def require_editor_or_above(request, organization):
    """
    Check if user is an editor or admin of the organization.

    Returns (membership, error_response) tuple.
    """
    from rest_framework.response import Response
    from rest_framework import status

    membership, error = require_org_membership(request, organization)
    if error:
        return None, error

    if not membership.is_editor_or_above():
        return None, Response(
            {"error": "You must be an editor or admin to perform this action"},
            status=status.HTTP_403_FORBIDDEN,
        )

    return membership, None


def require_admin(request, organization):
    """
    Check if user is an admin of the organization.

    Returns (membership, error_response) tuple.
    """
    from rest_framework.response import Response
    from rest_framework import status

    membership, error = require_org_membership(request, organization)
    if error:
        return None, error

    if not membership.is_admin():
        return None, Response(
            {"error": "Only admins can perform this action"}, status=status.HTTP_403_FORBIDDEN
        )

    return membership, None
