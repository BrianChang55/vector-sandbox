"""
Tasklist API views.
"""
from rest_framework import viewsets, status
from rest_framework.permissions import IsAuthenticated
from rest_framework.response import Response
from django.shortcuts import get_object_or_404

from accounts.models import Organization
from vector_app.permissions import IsOrgEditorOrAbove
from .models import Task
from .serializers import TaskSerializer, TaskCreateSerializer, TaskUpdateSerializer


class TaskViewSet(viewsets.ModelViewSet):
    """
    ViewSet for Task model.
    GET /api/v1/orgs/:org_id/tasks/ - List tasks
    POST /api/v1/orgs/:org_id/tasks/ - Create task
    GET /api/v1/orgs/:org_id/tasks/:id/ - Get task
    PATCH /api/v1/orgs/:org_id/tasks/:id/ - Update task
    DELETE /api/v1/orgs/:org_id/tasks/:id/ - Delete task
    """

    permission_classes = [IsAuthenticated, IsOrgEditorOrAbove]
    serializer_class = TaskSerializer

    def get_queryset(self):
        """Filter tasks to organization."""
        org_id = self.kwargs.get("org_id")
        return Task.objects.filter(organization_id=org_id).select_related("created_by")

    def get_serializer_class(self):
        if self.action == "create":
            return TaskCreateSerializer
        if self.action in ("update", "partial_update"):
            return TaskUpdateSerializer
        return TaskSerializer

    def get_serializer_context(self):
        context = super().get_serializer_context()
        org_id = self.kwargs.get("org_id")
        if org_id:
            context["organization"] = get_object_or_404(Organization, id=org_id)
        return context

    def create(self, request, *args, **kwargs):
        serializer = self.get_serializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        self.perform_create(serializer)
        # Return full serialized response
        instance = serializer.instance
        read_serializer = TaskSerializer(instance, context=self.get_serializer_context())
        return Response(read_serializer.data, status=status.HTTP_201_CREATED)

    def update(self, request, *args, **kwargs):
        partial = kwargs.pop("partial", False)
        instance = self.get_object()
        serializer = self.get_serializer(instance, data=request.data, partial=partial)
        serializer.is_valid(raise_exception=True)
        self.perform_update(serializer)
        # Return full serialized response
        read_serializer = TaskSerializer(instance, context=self.get_serializer_context())
        return Response(read_serializer.data)
