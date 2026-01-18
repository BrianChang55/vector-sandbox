"""
Task API views for task list management.
"""
from django.shortcuts import get_object_or_404
from django.utils import timezone
from rest_framework import viewsets, status
from rest_framework.permissions import IsAuthenticated
from rest_framework.response import Response

from tasks.models import Task
from ..models import Organization
from ..serializers import TaskSerializer, TaskCreateSerializer, TaskUpdateSerializer
from ..permissions import IsOrgEditorOrAbove, require_editor_or_above


class TaskViewSet(viewsets.ModelViewSet):
    """
    ViewSet for Task model.
    GET /api/v1/orgs/:org_id/tasks/ - List tasks for organization
    POST /api/v1/orgs/:org_id/tasks/ - Create task
    GET /api/v1/orgs/:org_id/tasks/:id/ - Get task
    PATCH /api/v1/orgs/:org_id/tasks/:id/ - Update task
    DELETE /api/v1/orgs/:org_id/tasks/:id/ - Delete task
    """
    permission_classes = [IsAuthenticated, IsOrgEditorOrAbove]
    serializer_class = TaskSerializer

    def get_queryset(self):
        """Filter tasks to organization."""
        org_id = self.kwargs.get('org_id')
        return Task.objects.filter(organization_id=org_id).select_related('created_by')

    def get_serializer_class(self):
        """Use appropriate serializer based on action."""
        if self.action == 'create':
            return TaskCreateSerializer
        if self.action in ['update', 'partial_update']:
            return TaskUpdateSerializer
        return TaskSerializer

    def get_serializer_context(self):
        """Add organization to serializer context."""
        context = super().get_serializer_context()
        org_id = self.kwargs.get('org_id')
        if org_id:
            context['organization'] = get_object_or_404(Organization, id=org_id)
        return context

    def perform_create(self, serializer):
        """Create a new task."""
        serializer.save()

    def create(self, request, *args, **kwargs):
        """Create task and return full serialized response."""
        serializer = self.get_serializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        self.perform_create(serializer)

        # Return the full task details using the read serializer
        instance = serializer.instance
        read_serializer = TaskSerializer(instance, context=self.get_serializer_context())

        headers = self.get_success_headers(read_serializer.data)
        return Response(read_serializer.data, status=status.HTTP_201_CREATED, headers=headers)

    def perform_update(self, serializer):
        """Update a task, setting completed_at if completing."""
        instance = serializer.instance
        completed_before = instance.completed
        completed_after = serializer.validated_data.get('completed', completed_before)

        # If task is being marked as completed, set completed_at
        if not completed_before and completed_after:
            serializer.save(completed_at=timezone.now())
        # If task is being unmarked as completed, clear completed_at
        elif completed_before and not completed_after:
            serializer.save(completed_at=None)
        else:
            serializer.save()

    def update(self, request, *args, **kwargs):
        """Update task and return full serialized response."""
        partial = kwargs.pop('partial', False)
        instance = self.get_object()
        serializer = self.get_serializer(instance, data=request.data, partial=partial)
        serializer.is_valid(raise_exception=True)
        self.perform_update(serializer)

        # Return the full task details using the read serializer
        read_serializer = TaskSerializer(instance, context=self.get_serializer_context())
        return Response(read_serializer.data)
