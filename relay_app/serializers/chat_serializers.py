"""
Serializers for Chat and Code Generation
"""
from rest_framework import serializers
from ..models import ChatSession, ChatMessage, AgentConfiguration


class ChatSessionSerializer(serializers.ModelSerializer):
    """Serializer for ChatSession model."""
    message_count = serializers.SerializerMethodField()
    created_by_email = serializers.CharField(source='created_by.email', read_only=True)
    
    class Meta:
        model = ChatSession
        fields = [
            'id',
            'internal_app',
            'title',
            'model_id',
            'is_active',
            'created_by',
            'created_by_email',
            'created_at',
            'updated_at',
            'message_count',
        ]
        read_only_fields = ['id', 'created_at', 'updated_at', 'created_by']
    
    def get_message_count(self, obj):
        return obj.messages.count()


class ChatMessageSerializer(serializers.ModelSerializer):
    """Serializer for ChatMessage model."""
    version_number = serializers.SerializerMethodField()
    
    class Meta:
        model = ChatMessage
        fields = [
            'id',
            'session',
            'role',
            'content',
            'status',
            'model_id',
            'token_count_input',
            'token_count_output',
            'duration_ms',
            'generated_spec_json',
            'generated_files',
            'version_created',
            'version_number',
            'error_message',
            'created_at',
        ]
        read_only_fields = [
            'id',
            'created_at',
            'token_count_input',
            'token_count_output',
            'duration_ms',
        ]
    
    def get_version_number(self, obj):
        if obj.version_created:
            return obj.version_created.version_number
        return None


class ChatMessageCreateSerializer(serializers.Serializer):
    """Serializer for creating chat messages."""
    session_id = serializers.UUIDField(required=False, allow_null=True)
    message = serializers.CharField(required=True, min_length=1, max_length=50000)
    model_id = serializers.CharField(required=False, default='anthropic/claude-sonnet-4')
    mode = serializers.ChoiceField(
        choices=['appspec', 'code'],
        required=False,
        default='appspec'
    )
    stream = serializers.BooleanField(required=False, default=True)


class AgentConfigurationSerializer(serializers.ModelSerializer):
    """Serializer for AgentConfiguration model."""
    
    class Meta:
        model = AgentConfiguration
        fields = [
            'id',
            'organization',
            'name',
            'description',
            'default_model',
            'fallback_model',
            'temperature',
            'max_tokens',
            'system_prompt_override',
            'coding_guidelines',
            'enable_streaming',
            'enable_auto_apply',
            'enable_thinking_display',
            'is_default',
            'created_at',
            'updated_at',
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']


class GenerationRequestSerializer(serializers.Serializer):
    """Serializer for generation requests."""
    message = serializers.CharField(required=True, min_length=1)
    session_id = serializers.UUIDField(required=False, allow_null=True)
    model = serializers.CharField(required=False, default='anthropic/claude-sonnet-4')
    mode = serializers.ChoiceField(
        choices=['appspec', 'code'],
        required=False,
        default='appspec'
    )


class ModelInfoSerializer(serializers.Serializer):
    """Serializer for model information."""
    id = serializers.CharField()
    name = serializers.CharField()
    description = serializers.CharField()
    category = serializers.CharField()
    context_length = serializers.IntegerField()
    supports_streaming = serializers.BooleanField()
    recommended_for = serializers.ListField(child=serializers.CharField())
    cost = serializers.DictField()

