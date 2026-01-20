"""
CRM app models - Customer group tracking for CRM dashboard.
"""
from django.conf import settings
from django.core.validators import MinValueValidator
from django.db import models

from internal_apps.utils.base_model import DjangoBaseModel


class HealthCategory(models.TextChoices):
    """Health classification for customer groups."""
    PROSPECT = 'prospect', 'Prospect'
    GOOD = 'good', 'Good'
    AVERAGE = 'average', 'Average'
    AT_RISK = 'at_risk', 'At Risk'


class CustomerGroup(DjangoBaseModel):
    """
    Tracked customer group for CRM dashboard.
    Each group has a health classification and estimated potential value.
    """
    organization = models.ForeignKey(
        'accounts.Organization',
        on_delete=models.CASCADE,
        related_name='customer_groups'
    )
    name = models.CharField(max_length=255)
    description = models.TextField(blank=True, default='')
    health = models.CharField(
        max_length=20,
        choices=HealthCategory.choices,
        default=HealthCategory.PROSPECT,
    )
    potential_value = models.DecimalField(
        max_digits=15,
        decimal_places=2,
        default=0,
        validators=[MinValueValidator(0)],
        help_text="Estimated potential value (must be non-negative)"
    )
    created_by = models.ForeignKey(
        settings.AUTH_USER_MODEL,
        on_delete=models.SET_NULL,
        null=True,
        related_name='created_customer_groups'
    )

    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['organization', '-created_at']),
            models.Index(fields=['organization', 'health']),
        ]

    def __str__(self):
        return f"{self.name} ({self.organization.name})"
