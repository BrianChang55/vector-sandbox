from django.contrib import admin
from .models import Task


@admin.register(Task)
class TaskAdmin(admin.ModelAdmin):
    list_display = ['title', 'organization', 'created_by', 'completed', 'created_at']
    list_filter = ['completed', 'organization']
    search_fields = ['title', 'description']
