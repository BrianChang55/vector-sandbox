# Generated migration for adding is_active field to AppVersion

from django.db import migrations, models


def backfill_is_active(apps, schema_editor):
    """
    Backfill is_active for existing versions.
    
    Sets is_active=True for all versions where generation_status='complete'.
    This ensures existing completed versions remain visible in the API.
    """
    AppVersion = apps.get_model('relay_app', 'AppVersion')
    
    # Mark all completed versions as active
    updated_count = AppVersion.objects.filter(
        generation_status='complete'
    ).update(is_active=True)
    
    print(f"Backfilled is_active=True for {updated_count} completed versions")


def reverse_backfill(apps, schema_editor):
    """Reverse migration - set all is_active to False."""
    AppVersion = apps.get_model('relay_app', 'AppVersion')
    AppVersion.objects.all().update(is_active=False)


class Migration(migrations.Migration):

    dependencies = [
        ('relay_app', '0010_version_state_snapshots'),
    ]

    operations = [
        # Add the is_active field with default False
        migrations.AddField(
            model_name='appversion',
            name='is_active',
            field=models.BooleanField(
                default=False,
                help_text='Whether this version is active and should be returned by API. Only set True when generation completes successfully.'
            ),
        ),
        # Add index for efficient filtering by is_active
        migrations.AddIndex(
            model_name='appversion',
            index=models.Index(fields=['internal_app', 'is_active', '-version_number'], name='relay_app_a_interna_active_idx'),
        ),
        # Backfill existing versions
        migrations.RunPython(backfill_is_active, reverse_backfill),
    ]

