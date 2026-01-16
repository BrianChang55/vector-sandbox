from django.contrib import admin

from chat.models import ChatSession, ChatMessage, CodeGenerationJob, QuestioningSession


@admin.register(ChatSession)
class ChatSessionAdmin(admin.ModelAdmin):
    list_display = ("title", "internal_app", "is_active", "created_by", "created_at")
    list_filter = ("is_active",)
    search_fields = ("title", "internal_app__name")


@admin.register(ChatMessage)
class ChatMessageAdmin(admin.ModelAdmin):
    list_display = ("session", "role", "status", "created_at")
    list_filter = ("role", "status")
    search_fields = ("session__title", "content")


@admin.register(CodeGenerationJob)
class CodeGenerationJobAdmin(admin.ModelAdmin):
    list_display = ("id", "internal_app", "status", "created_by", "created_at")
    list_filter = ("status",)
    search_fields = ("internal_app__name",)


@admin.register(QuestioningSession)
class QuestioningSessionAdmin(admin.ModelAdmin):
    list_display = ("id", "chat_session", "status", "question_count", "created_at")
    list_filter = ("status",)
    search_fields = ("chat_session__title", "initial_request")
