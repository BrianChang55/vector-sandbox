"""
Tasks app models - User task management.
"""
from django.conf import settings
from django.db import models

from internal_apps.utils.base_model import DjangoBaseModel


class Task(DjangoBaseModel):
    """
    A task item for the user's task list.
    """
    organization = models.ForeignKey(
        'accounts.Organization',
        on_delete=models.CASCADE,
        related_name="tasks"
    )
    title = models.CharField(max_length=255)
    description = models.TextField(blank=True)
    completed = models.BooleanField(default=False)
    completed_at = models.DateTimeField(null=True, blank=True)
    created_by = models.ForeignKey(
        settings.AUTH_USER_MODEL,
        on_delete=models.CASCADE,
        related_name="tasks"
    )

    class Meta:
        ordering = ["-created_at"]
        indexes = [
            models.Index(fields=["organization", "-created_at"]),
            models.Index(fields=["created_by", "-created_at"]),
        ]

    def __str__(self):
        return self.title
