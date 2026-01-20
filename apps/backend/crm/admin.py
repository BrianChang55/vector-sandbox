"""
Django admin configuration for CRM models.
"""
from django.contrib import admin
from .models import CustomerGroup


@admin.register(CustomerGroup)
class CustomerGroupAdmin(admin.ModelAdmin):
    list_display = ['name', 'organization', 'health', 'potential_value', 'created_at']
    list_filter = ['health', 'organization', 'created_at']
    search_fields = ['name', 'description']
    readonly_fields = ['id', 'created_at', 'updated_at']
    ordering = ['-created_at']
