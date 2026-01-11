"""
Action Execution Log serializers
"""
from rest_framework import serializers
from ..models import ActionExecutionLog


class ActionExecutionLogSerializer(serializers.ModelSerializer):
    """Serializer for ActionExecutionLog model."""
    status_display = serializers.CharField(source='get_status_display', read_only=True)
    user_email = serializers.EmailField(source='user.email', read_only=True)
    app_name = serializers.CharField(source='internal_app.name', read_only=True)
    
    class Meta:
        model = ActionExecutionLog
        fields = [
            'id',
            'internal_app',
            'app_name',
            'app_version',
            'user',
            'user_email',
            'backend_connection',
            'action_id',
            'resource_id',
            'args_json',
            'status',
            'status_display',
            'error_message',
            'created_at',
        ]
        read_only_fields = ['id', 'created_at']

