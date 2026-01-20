import django.core.validators
import django.db.models.deletion
import uuid
from decimal import Decimal
from django.conf import settings
from django.db import migrations, models


class Migration(migrations.Migration):

    initial = True

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
        ('accounts', '0001_initial'),
    ]

    operations = [
        migrations.CreateModel(
            name='CustomerGroup',
            fields=[
                ('id', models.UUIDField(default=uuid.uuid4, editable=False, primary_key=True, serialize=False)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
                ('name', models.CharField(max_length=255)),
                ('description', models.TextField(blank=True, default='')),
                ('health', models.CharField(
                    choices=[
                        ('prospect', 'Prospect'),
                        ('good', 'Good'),
                        ('average', 'Average'),
                        ('at_risk', 'At Risk')
                    ],
                    default='prospect',
                    max_length=20
                )),
                ('potential_value', models.DecimalField(
                    decimal_places=2,
                    default=0,
                    help_text='Estimated potential value (must be non-negative)',
                    max_digits=15,
                    validators=[django.core.validators.MinValueValidator(0)]
                )),
                ('created_by', models.ForeignKey(
                    null=True,
                    on_delete=django.db.models.deletion.SET_NULL,
                    related_name='created_customer_groups',
                    to=settings.AUTH_USER_MODEL
                )),
                ('organization', models.ForeignKey(
                    on_delete=django.db.models.deletion.CASCADE,
                    related_name='customer_groups',
                    to='accounts.organization'
                )),
            ],
            options={
                'ordering': ['-created_at'],
            },
        ),
        migrations.AddIndex(
            model_name='customergroup',
            index=models.Index(fields=['organization', '-created_at'], name='crm_custome_organiz_a1b2c3_idx'),
        ),
        migrations.AddIndex(
            model_name='customergroup',
            index=models.Index(fields=['organization', 'health'], name='crm_custome_organiz_d4e5f6_idx'),
        ),
    ]
