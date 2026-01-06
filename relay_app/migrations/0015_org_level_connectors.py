# Custom migration for organization-level connector changes
# This replaces per-user connections with organization-level connections

import django.db.models.deletion
import uuid
from django.conf import settings
from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ("relay_app", "0014_add_connectors_integrations_models"),
    ]

    operations = [
        # First, delete the user-level models (order matters for FK constraints)
        migrations.DeleteModel(
            name="UserConnectorLink",
        ),
        migrations.DeleteModel(
            name="MergeIntegrationUser",
        ),
        
        # Add the org-level registered user ID to the provider
        migrations.AddField(
            model_name="mergeintegrationprovider",
            name="merge_registered_user_id",
            field=models.CharField(
                blank=True,
                default="",
                help_text="Organization registered user ID from Merge (set after first link)",
                max_length=255,
            ),
        ),
        
        # Update fields to be optional (credentials now come from env)
        migrations.AlterField(
            model_name="mergeintegrationprovider",
            name="merge_access_key_encrypted",
            field=models.TextField(
                blank=True,
                default="",
                help_text="DEPRECATED: Access key now comes from environment variables",
            ),
        ),
        migrations.AlterField(
            model_name="mergeintegrationprovider",
            name="merge_tool_pack_id",
            field=models.CharField(
                blank=True,
                default="",
                help_text="DEPRECATED: Tool Pack ID now comes from environment variables",
                max_length=255,
            ),
        ),
        
        # Create the organization-level connector link model
        migrations.CreateModel(
            name="OrganizationConnectorLink",
            fields=[
                ("created_at", models.DateTimeField(auto_now_add=True)),
                ("updated_at", models.DateTimeField(auto_now=True)),
                (
                    "id",
                    models.UUIDField(
                        default=uuid.uuid4,
                        editable=False,
                        primary_key=True,
                        serialize=False,
                    ),
                ),
                (
                    "is_connected",
                    models.BooleanField(
                        default=False,
                        help_text="Whether the organization has connected this connector",
                    ),
                ),
                (
                    "connected_at",
                    models.DateTimeField(
                        blank=True,
                        help_text="When the connector was connected",
                        null=True,
                    ),
                ),
                (
                    "connection_metadata",
                    models.JSONField(
                        blank=True,
                        default=dict,
                        help_text="Additional metadata about the connection",
                    ),
                ),
                (
                    "connected_by",
                    models.ForeignKey(
                        blank=True,
                        help_text="User who connected this integration",
                        null=True,
                        on_delete=django.db.models.deletion.SET_NULL,
                        related_name="connected_integrations",
                        to=settings.AUTH_USER_MODEL,
                    ),
                ),
                (
                    "connector",
                    models.ForeignKey(
                        on_delete=django.db.models.deletion.CASCADE,
                        related_name="org_links",
                        to="relay_app.connectorcache",
                    ),
                ),
                (
                    "provider",
                    models.ForeignKey(
                        on_delete=django.db.models.deletion.CASCADE,
                        related_name="connector_links",
                        to="relay_app.mergeintegrationprovider",
                    ),
                ),
            ],
            options={
                "unique_together": {("provider", "connector")},
            },
        ),
    ]

