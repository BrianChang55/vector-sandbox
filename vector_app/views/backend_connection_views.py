"""
Backend Connection API views
"""

import logging
from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.shortcuts import get_object_or_404

from ..models import BackendConnection, Organization, ProjectUserBackendAuth
from ..serializers import (
    BackendConnectionSerializer,
    BackendConnectionCreateSerializer,
    BackendConnectionTestSerializer,
)
from ..adapters.supabase import SupabaseAdapter
from ..adapters.postgresql import PostgreSQLAdapter
from ..adapters.mysql import MySQLAdapter
from ..adapters.base import AdapterContext
from ..utils.encryption import encrypt_string
from ..permissions import IsOrgAdmin, require_admin

logger = logging.getLogger(__name__)


def get_adapter(adapter_type: str):
    """Get the appropriate adapter based on type."""
    adapters = {
        BackendConnection.ADAPTER_SUPABASE: SupabaseAdapter,
        BackendConnection.ADAPTER_POSTGRESQL: PostgreSQLAdapter,
        BackendConnection.ADAPTER_MYSQL: MySQLAdapter,
    }
    adapter_class = adapters.get(adapter_type)
    return adapter_class() if adapter_class else None


def get_adapter_context(backend: BackendConnection) -> AdapterContext:
    """Create adapter context from backend configuration."""
    config = backend.get_config()

    if backend.adapter_type == BackendConnection.ADAPTER_SUPABASE:
        return AdapterContext(
            backend_url=config.get("supabase_url", ""),
            service_role_key=config.get("service_role_key"),
            anon_key=config.get("anon_key"),
        )
    elif backend.adapter_type in [BackendConnection.ADAPTER_POSTGRESQL, BackendConnection.ADAPTER_MYSQL]:
        return AdapterContext(
            host=config.get("host", ""),
            port=config.get("port"),
            database=config.get("database", ""),
            username=config.get("username", ""),
            password=config.get("password", ""),
            ssl_mode=config.get("ssl_mode", "disable"),
        )
    return AdapterContext()


class BackendConnectionViewSet(viewsets.ModelViewSet):
    """
    ViewSet for BackendConnection model.
    GET /api/v1/orgs/:org_id/backends - List backends (any member)
    POST /api/v1/orgs/:org_id/backends - Create backend (admin only)
    GET /api/v1/backends/:id - Get backend (any member)
    PATCH /api/v1/backends/:id - Update backend (admin only)
    DELETE /api/v1/backends/:id - Delete backend (admin only)
    POST /api/v1/backends/:id/test - Test connection (admin only)
    POST /api/v1/backends/:id/user-auth - Store user JWT (any member)
    """

    permission_classes = [IsAuthenticated, IsOrgAdmin]
    serializer_class = BackendConnectionSerializer

    def get_queryset(self):
        """Filter backends to organization or return all user-accessible backends."""
        org_id = self.kwargs.get("organization_pk")
        if org_id:
            return BackendConnection.objects.filter(organization_id=org_id)

        # For direct access (e.g., /backends/:id/test/), return backends from user's orgs
        user_org_ids = self.request.user.user_organizations.values_list("organization_id", flat=True)
        return BackendConnection.objects.filter(organization_id__in=user_org_ids)

    def get_serializer_class(self):
        """Use create serializer for POST."""
        if self.action == "create":
            return BackendConnectionCreateSerializer
        return BackendConnectionSerializer

    def get_serializer_context(self):
        """Add organization to serializer context."""
        context = super().get_serializer_context()
        org_id = self.kwargs.get("organization_pk")
        if org_id:
            context["organization"] = get_object_or_404(Organization, id=org_id)
        return context

    def perform_create(self, serializer):
        """Verify user is admin of organization."""
        org_id = self.kwargs.get("organization_pk")
        organization = get_object_or_404(Organization, id=org_id)

        # Verify user is admin (permission class should catch this, but double-check)
        membership, error = require_admin(self.request, organization)
        if error:
            from rest_framework.exceptions import PermissionDenied

            raise PermissionDenied("Only admins can create backend connections")

        serializer.save()

    @action(detail=True, methods=["post"])
    def test(self, request, pk=None):
        """Test backend connection."""
        backend = self.get_object()

        try:
            # Get adapter
            adapter = get_adapter(backend.adapter_type)
            if not adapter:
                return Response(
                    {"success": False, "message": f"Unknown adapter type: {backend.adapter_type}"},
                    status=status.HTTP_400_BAD_REQUEST,
                )

            # Get adapter context
            ctx = get_adapter_context(backend)

            # Test connection based on adapter type
            if backend.adapter_type in [
                BackendConnection.ADAPTER_POSTGRESQL,
                BackendConnection.ADAPTER_MYSQL,
            ]:
                # Direct database adapters have test_connection method
                result = adapter.test_connection(ctx)
                return Response(
                    {
                        "success": result.get("success", False),
                        "message": result.get("message", ""),
                        "capabilities": adapter.get_capabilities(ctx) if result.get("success") else {},
                        "version": result.get("version", ""),
                    }
                )
            else:
                # Supabase - use get_capabilities as test
                capabilities = adapter.get_capabilities(ctx)
                return Response(
                    {
                        "success": True,
                        "message": "Connection successful",
                        "capabilities": capabilities,
                    }
                )

        except Exception as e:
            logger.error(f"Error testing backend connection: {e}")
            return Response(
                {
                    "success": False,
                    "message": str(e),
                },
                status=status.HTTP_400_BAD_REQUEST,
            )

    @action(detail=True, methods=["post"])
    def user_auth(self, request, pk=None):
        """Store user JWT for backend authentication."""
        backend = self.get_object()
        user_jwt = request.data.get("user_jwt")

        if not user_jwt:
            return Response({"error": "user_jwt is required"}, status=status.HTTP_400_BAD_REQUEST)

        # Create or update user auth
        user_auth, created = ProjectUserBackendAuth.objects.update_or_create(
            user=request.user, backend_connection=backend, defaults={}
        )

        user_auth.set_jwt(user_jwt)
        user_auth.save()

        return Response(
            {
                "success": True,
                "message": "User authentication stored",
            }
        )
