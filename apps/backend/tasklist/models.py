"""
Tasklist app models - Simple task management.
"""
from django.db import models

from internal_apps.utils.base_model import DjangoBaseModel
from accounts.models import User, Organization


class Task(DjangoBaseModel):
    """
    A task in a user's tasklist.
    Tasks belong to an organization and are created by a user.
    """

    organization = models.ForeignKey(
        Organization,
        on_delete=models.CASCADE,
        related_name="tasks",
    )
    created_by = models.ForeignKey(
        User,
        on_delete=models.CASCADE,
        related_name="tasks",
    )
    title = models.CharField(max_length=500)
    description = models.TextField(blank=True, default="")
    is_completed = models.BooleanField(default=False)
    completed_at = models.DateTimeField(null=True, blank=True)

    class Meta:
        ordering = ["-created_at"]
        indexes = [
            models.Index(fields=["organization", "-created_at"]),
            models.Index(fields=["organization", "is_completed"]),
        ]

    def __str__(self):
        return f"{self.title} ({'done' if self.is_completed else 'pending'})"
