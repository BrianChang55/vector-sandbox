"""
Seed data for CRM customer groups.
Creates sample data for local development so dashboards render immediately.
"""
from decimal import Decimal
from django.db import migrations


def seed_customer_groups(apps, schema_editor):
    """Create sample customer groups for all existing organizations."""
    CustomerGroup = apps.get_model('crm', 'CustomerGroup')
    Organization = apps.get_model('accounts', 'Organization')

    # Sample data with variety in health and value
    sample_groups = [
        {'name': 'Enterprise Corp', 'description': 'Fortune 500 company', 'health': 'good', 'potential_value': Decimal('250000.00')},
        {'name': 'StartupXYZ', 'description': 'High-growth tech startup', 'health': 'good', 'potential_value': Decimal('75000.00')},
        {'name': 'MidMarket Solutions', 'description': 'Medium-sized B2B company', 'health': 'average', 'potential_value': Decimal('45000.00')},
        {'name': 'RetailChain Inc', 'description': 'Regional retail chain', 'health': 'at_risk', 'potential_value': Decimal('120000.00')},
        {'name': 'TechVentures', 'description': 'New prospect from conference', 'health': 'prospect', 'potential_value': Decimal('50000.00')},
        {'name': 'HealthCare Plus', 'description': 'Healthcare provider network', 'health': 'good', 'potential_value': Decimal('180000.00')},
        {'name': 'SmallBiz LLC', 'description': 'Local small business', 'health': 'average', 'potential_value': Decimal('15000.00')},
        {'name': 'Global Manufacturing', 'description': 'International manufacturing company', 'health': 'at_risk', 'potential_value': Decimal('200000.00')},
        {'name': 'Consulting Partners', 'description': 'Professional services firm', 'health': 'prospect', 'potential_value': Decimal('35000.00')},
        {'name': 'FinServ Group', 'description': 'Financial services company', 'health': 'good', 'potential_value': Decimal('300000.00')},
    ]

    for org in Organization.objects.all():
        for group_data in sample_groups:
            CustomerGroup.objects.get_or_create(
                organization=org,
                name=group_data['name'],
                defaults={
                    'description': group_data['description'],
                    'health': group_data['health'],
                    'potential_value': group_data['potential_value'],
                }
            )


def remove_seed_data(apps, schema_editor):
    """Remove seed data (for rollback)."""
    CustomerGroup = apps.get_model('crm', 'CustomerGroup')
    sample_names = [
        'Enterprise Corp', 'StartupXYZ', 'MidMarket Solutions', 'RetailChain Inc',
        'TechVentures', 'HealthCare Plus', 'SmallBiz LLC', 'Global Manufacturing',
        'Consulting Partners', 'FinServ Group',
    ]
    CustomerGroup.objects.filter(name__in=sample_names).delete()


class Migration(migrations.Migration):

    dependencies = [
        ('crm', '0001_initial'),
    ]

    operations = [
        migrations.RunPython(seed_customer_groups, remove_seed_data),
    ]
