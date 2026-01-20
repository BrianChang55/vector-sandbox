"""
CRM views for CustomerGroup CRUD and dashboard statistics.
"""
from decimal import Decimal
from django.db.models import Count, Sum
from django.shortcuts import get_object_or_404
from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.permissions import IsAuthenticated
from rest_framework.response import Response
from rest_framework.views import APIView

from vector_app.models import Organization
from vector_app.permissions import IsOrgEditorOrAbove, get_user_membership
from .models import CustomerGroup, HealthCategory
from .serializers import (
    CustomerGroupSerializer,
    CustomerGroupCreateSerializer,
    CustomerGroupStatsSerializer,
)


class CustomerGroupViewSet(viewsets.ModelViewSet):
    """
    ViewSet for CustomerGroup CRUD operations.

    Endpoints:
    - GET /orgs/{org_id}/customer-groups/ - List all customer groups
    - POST /orgs/{org_id}/customer-groups/ - Create a new customer group
    - GET /customer-groups/{id}/ - Get a customer group
    - PATCH /customer-groups/{id}/ - Update a customer group
    - DELETE /customer-groups/{id}/ - Delete a customer group
    """
    permission_classes = [IsAuthenticated, IsOrgEditorOrAbove]
    serializer_class = CustomerGroupSerializer

    def get_queryset(self):
        """Filter customer groups by organization."""
        org_id = self.kwargs.get('org_id')
        if org_id:
            return CustomerGroup.objects.filter(
                organization_id=org_id
            ).select_related('created_by')

        # For direct access endpoints, filter by the object's org
        pk = self.kwargs.get('pk')
        if pk:
            return CustomerGroup.objects.filter(
                id=pk
            ).select_related('created_by', 'organization')

        return CustomerGroup.objects.none()

    def get_serializer_class(self):
        if self.action == 'create':
            return CustomerGroupCreateSerializer
        return CustomerGroupSerializer

    def get_serializer_context(self):
        context = super().get_serializer_context()
        org_id = self.kwargs.get('org_id')
        if org_id:
            context['organization'] = get_object_or_404(Organization, id=org_id)
        return context

    def perform_create(self, serializer):
        """Set the organization and creator when creating."""
        org_id = self.kwargs.get('org_id')
        organization = get_object_or_404(Organization, id=org_id)
        serializer.save(
            organization=organization,
            created_by=self.request.user
        )


class CustomerGroupStatsView(APIView):
    """
    API view for dashboard statistics.

    GET /orgs/{org_id}/customer-groups/stats/
    Returns aggregated statistics for the dashboard.
    """
    permission_classes = [IsAuthenticated]

    def get(self, request, org_id):
        """Get dashboard statistics for the organization."""
        organization = get_object_or_404(Organization, id=org_id)

        # Check membership
        membership = get_user_membership(request.user, organization)
        if not membership:
            return Response(
                {'error': 'You are not a member of this organization'},
                status=status.HTTP_403_FORBIDDEN
            )

        groups = CustomerGroup.objects.filter(organization=organization)

        # Total counts
        total_groups = groups.count()
        total_value = groups.aggregate(total=Sum('potential_value'))['total'] or Decimal('0')

        # Count by health category
        health_counts = groups.values('health').annotate(count=Count('id'))
        by_health = {item['health']: item['count'] for item in health_counts}

        # Ensure all health categories are present
        for choice in HealthCategory.choices:
            if choice[0] not in by_health:
                by_health[choice[0]] = 0

        # Value by health category
        health_values = groups.values('health').annotate(total=Sum('potential_value'))
        value_by_health = {
            item['health']: item['total'] or Decimal('0')
            for item in health_values
        }

        # Ensure all health categories are present
        for choice in HealthCategory.choices:
            if choice[0] not in value_by_health:
                value_by_health[choice[0]] = Decimal('0')

        # Top groups by value
        top_groups = groups.order_by('-potential_value')[:5]
        top_groups_serializer = CustomerGroupSerializer(top_groups, many=True)

        data = {
            'total_groups': total_groups,
            'total_value': total_value,
            'by_health': by_health,
            'value_by_health': value_by_health,
            'top_groups': top_groups_serializer.data,
        }

        return Response(data)
