"""
Streaming Views for Real-time Code Generation

Implements Server-Sent Events (SSE) for live streaming updates
during AI code generation - similar to Cursor, Lovable, Replit.
"""
import json
import logging
import time
import uuid
from typing import Generator
from django.http import StreamingHttpResponse, JsonResponse
from django.views import View
from django.utils.decorators import method_decorator
from django.views.decorators.csrf import csrf_exempt
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from rest_framework import status as http_status

from ..models import InternalApp, AppVersion, VersionFile
from ..models import ChatSession, ChatMessage, CodeGenerationJob
from ..services.openrouter_service import (
    get_openrouter_service,
    StreamChunk,
    MODEL_CONFIGS,
)
from ..services.validation import AppSpecValidationService
from ..services.codegen import CodegenService

logger = logging.getLogger(__name__)


class AvailableModelsView(APIView):
    """Get list of available AI models."""
    permission_classes = [IsAuthenticated]
    
    def get(self, request):
        """Return available models with their configurations."""
        service = get_openrouter_service()
        models = service.get_available_models()
        
        # Group by category
        grouped = {
            "premium": [],
            "standard": [],
            "economy": [],
        }
        
        for model in models:
            category = model.get("category", "standard")
            if category in grouped:
                grouped[category].append(model)
        
        return Response({
            "models": models,
            "grouped": grouped,
            "default": "anthropic/claude-3.5-sonnet",
        })


class ChatSessionViewSet(APIView):
    """Manage chat sessions for an app."""
    permission_classes = [IsAuthenticated]
    
    def get(self, request, app_id):
        """List chat sessions for an app."""
        try:
            app = InternalApp.objects.get(pk=app_id)
            
            # Verify access
            if not request.user.user_organizations.filter(organization=app.organization).exists():
                return Response(
                    {"error": "Access denied"},
                    status=http_status.HTTP_403_FORBIDDEN
                )
            
            sessions = app.chat_sessions.filter(is_active=True).order_by('-created_at')
            
            return Response({
                "sessions": [
                    {
                        "id": str(s.id),
                        "title": s.title,
                        "model_id": s.model_id,
                        "created_at": s.created_at.isoformat(),
                        "message_count": s.messages.count(),
                    }
                    for s in sessions
                ]
            })
        except InternalApp.DoesNotExist:
            return Response(
                {"error": "App not found"},
                status=http_status.HTTP_404_NOT_FOUND
            )
    
    def post(self, request, app_id):
        """Create a new chat session."""
        try:
            app = InternalApp.objects.get(pk=app_id)
            
            if not request.user.user_organizations.filter(organization=app.organization).exists():
                return Response(
                    {"error": "Access denied"},
                    status=http_status.HTTP_403_FORBIDDEN
                )
            
            session = ChatSession.objects.create(
                internal_app=app,
                title=request.data.get("title", "New Chat"),
                model_id=request.data.get("model_id", "anthropic/claude-3.5-sonnet"),
                created_by=request.user,
            )
            
            return Response({
                "id": str(session.id),
                "title": session.title,
                "model_id": session.model_id,
                "created_at": session.created_at.isoformat(),
            }, status=http_status.HTTP_201_CREATED)
            
        except InternalApp.DoesNotExist:
            return Response(
                {"error": "App not found"},
                status=http_status.HTTP_404_NOT_FOUND
            )


class ChatMessagesView(APIView):
    """Get messages for a chat session."""
    permission_classes = [IsAuthenticated]
    
    def get(self, request, session_id):
        """List messages in a session."""
        try:
            session = ChatSession.objects.select_related(
                'internal_app__organization'
            ).get(pk=session_id)
            
            if not request.user.user_organizations.filter(
                organization=session.internal_app.organization
            ).exists():
                return Response(
                    {"error": "Access denied"},
                    status=http_status.HTTP_403_FORBIDDEN
                )
            
            messages = session.messages.order_by('created_at')
            
            return Response({
                "messages": [
                    {
                        "id": str(m.id),
                        "role": m.role,
                        "content": m.content,
                        "status": m.status,
                        "model_id": m.model_id,
                        "created_at": m.created_at.isoformat(),
                        "generated_spec_json": m.generated_spec_json,
                        "version_created": str(m.version_created.id) if m.version_created else None,
                        "error_message": m.error_message,
                    }
                    for m in messages
                ]
            })
        except ChatSession.DoesNotExist:
            return Response(
                {"error": "Session not found"},
                status=http_status.HTTP_404_NOT_FOUND
            )


def sse_event(event_type: str, data: dict) -> str:
    """Format data as SSE event."""
    json_data = json.dumps(data)
    return f"event: {event_type}\ndata: {json_data}\n\n"


@method_decorator(csrf_exempt, name='dispatch')
class StreamingGenerateView(View):
    """
    SSE endpoint for streaming code generation.
    
    GET /api/v1/apps/:app_id/generate/stream?session_id=...&message=...&model=...
    
    Returns Server-Sent Events with real-time updates.
    """
    
    def get(self, request, app_id):
        """Stream code generation response."""
        # Get parameters
        session_id = request.GET.get('session_id')
        message = request.GET.get('message', '')
        model = request.GET.get('model', 'anthropic/claude-3.5-sonnet')
        mode = request.GET.get('mode', 'appspec')  # 'appspec' or 'code'
        
        if not message:
            return JsonResponse(
                {"error": "Message is required"},
                status=400
            )
        
        # Verify authentication via session or token
        if not request.user.is_authenticated:
            # Try JWT from Authorization header
            auth_header = request.META.get('HTTP_AUTHORIZATION', '')
            if not auth_header:
                return JsonResponse(
                    {"error": "Authentication required"},
                    status=401
                )
        
        try:
            app = InternalApp.objects.select_related(
                'organization',
                'backend_connection'
            ).get(pk=app_id)
        except InternalApp.DoesNotExist:
            return JsonResponse({"error": "App not found"}, status=404)
        
        # Create streaming response
        response = StreamingHttpResponse(
            self._generate_stream(
                app=app,
                message=message,
                model=model,
                mode=mode,
                session_id=session_id,
                user=request.user,
            ),
            content_type='text/event-stream'
        )
        response['Cache-Control'] = 'no-cache'
        response['X-Accel-Buffering'] = 'no'
        return response
    
    def _generate_stream(
        self,
        app: InternalApp,
        message: str,
        model: str,
        mode: str,
        session_id: str,
        user,
    ) -> Generator[str, None, None]:
        """Generate SSE stream for code generation."""
        
        # Send initial connection event
        yield sse_event("connected", {
            "app_id": str(app.id),
            "model": model,
            "mode": mode,
        })
        
        # Get or create session
        try:
            if session_id:
                session = ChatSession.objects.get(pk=session_id)
            else:
                session = ChatSession.objects.create(
                    internal_app=app,
                    title=message[:50] + "..." if len(message) > 50 else message,
                    model_id=model,
                    created_by=user if user.is_authenticated else None,
                )
                yield sse_event("session_created", {
                    "session_id": str(session.id),
                })
        except ChatSession.DoesNotExist:
            yield sse_event("error", {"message": "Session not found"})
            return
        
        # Save user message
        user_message = ChatMessage.objects.create(
            session=session,
            role=ChatMessage.ROLE_USER,
            content=message,
            status=ChatMessage.STATUS_COMPLETE,
        )
        yield sse_event("user_message", {
            "id": str(user_message.id),
            "content": message,
        })
        
        # Create assistant message placeholder
        assistant_message = ChatMessage.objects.create(
            session=session,
            role=ChatMessage.ROLE_ASSISTANT,
            content="",
            status=ChatMessage.STATUS_STREAMING,
            model_id=model,
        )
        yield sse_event("assistant_start", {
            "id": str(assistant_message.id),
            "model": model,
        })
        
        # Get current spec and registry surface
        current_spec = None
        latest_version = AppVersion.objects.filter(
            internal_app=app
        ).order_by('-version_number').first()
        
        if latest_version:
            current_spec = latest_version.spec_json
        
        # Build registry surface
        from ..models import ResourceRegistryEntry
        registry_entries = []
        if app.backend_connection:
            registry_entries = ResourceRegistryEntry.objects.filter(
                backend_connection=app.backend_connection,
                enabled=True
            )
        
        registry_surface = {
            "resources": [
                {
                    "resource_id": entry.resource_id,
                    "resource_name": entry.resource_name,
                    "exposed_fields": entry.exposed_fields_json or [],
                    "allowed_actions": [
                        a.get("action_id") for a in (entry.allowed_actions_json or [])
                    ],
                }
                for entry in registry_entries
            ]
        }
        
        # Get chat history
        chat_history = [
            {"role": m.role, "content": m.content}
            for m in session.messages.order_by('created_at')[:20]
            if m.id != assistant_message.id
        ]
        
        # Get OpenRouter service and stream response
        service = get_openrouter_service()
        accumulated_content = ""
        spec_json = None
        start_time = time.time()
        
        try:
            if mode == "appspec":
                stream = service.generate_app_spec_streaming(
                    intent_message=message,
                    current_spec=current_spec,
                    registry_surface=registry_surface,
                    chat_history=chat_history,
                    model=model,
                )
            else:
                # Get current files for code mode
                current_files = {}
                if latest_version:
                    for vf in latest_version.files.all():
                        current_files[vf.path] = vf.content
                
                stream = service.generate_code_streaming(
                    intent_message=message,
                    current_files=current_files,
                    registry_surface=registry_surface,
                    chat_history=chat_history,
                    model=model,
                )
            
            chunk_count = 0
            for chunk in stream:
                chunk_count += 1
                
                if chunk.type == "content":
                    accumulated_content += chunk.content
                    yield sse_event("content", {
                        "chunk": chunk.content,
                        "accumulated_length": len(accumulated_content),
                    })
                    
                elif chunk.type == "thinking":
                    yield sse_event("thinking", {
                        "content": chunk.content,
                    })
                    
                elif chunk.type == "done":
                    spec_json = chunk.metadata.get("spec_json") if chunk.metadata else None
                    files = chunk.metadata.get("files") if chunk.metadata else None
                    
                    # Update assistant message
                    duration_ms = int((time.time() - start_time) * 1000)
                    assistant_message.content = accumulated_content
                    assistant_message.status = ChatMessage.STATUS_COMPLETE
                    assistant_message.duration_ms = duration_ms
                    
                    if spec_json:
                        assistant_message.generated_spec_json = spec_json
                    if files:
                        assistant_message.generated_files = files
                    
                    assistant_message.save()
                    
                    yield sse_event("generation_complete", {
                        "message_id": str(assistant_message.id),
                        "duration_ms": duration_ms,
                        "chunk_count": chunk_count,
                        "has_spec": spec_json is not None,
                        "has_files": files is not None,
                    })
                    
                    # Auto-create version if we have a valid spec
                    if spec_json and mode == "appspec":
                        try:
                            # Validate spec
                            is_valid, errors = AppSpecValidationService.validate_app_spec(
                                app, spec_json
                            )
                            
                            if is_valid:
                                # Create new version
                                next_version_number = (
                                    latest_version.version_number + 1 if latest_version else 1
                                )
                                
                                version = AppVersion.objects.create(
                                    internal_app=app,
                                    version_number=next_version_number,
                                    source=AppVersion.SOURCE_AI,
                                    spec_json=spec_json,
                                    created_by=user if user.is_authenticated else None,
                                )
                                
                                # Generate files
                                CodegenService.generate_files_from_spec(version)
                                
                                # Link to message
                                assistant_message.version_created = version
                                assistant_message.save()
                                
                                yield sse_event("version_created", {
                                    "version_id": str(version.id),
                                    "version_number": version.version_number,
                                })
                            else:
                                yield sse_event("validation_warning", {
                                    "errors": errors,
                                    "message": "Generated spec has validation warnings",
                                })
                        except Exception as e:
                            logger.error(f"Error creating version: {e}")
                            yield sse_event("version_error", {
                                "error": str(e),
                            })
                    
                elif chunk.type == "error":
                    assistant_message.status = ChatMessage.STATUS_ERROR
                    assistant_message.error_message = chunk.content
                    assistant_message.content = accumulated_content
                    assistant_message.save()
                    
                    yield sse_event("error", {
                        "message": chunk.content,
                    })
                    return
            
            yield sse_event("done", {"success": True})
            
        except Exception as e:
            logger.error(f"Streaming error: {e}")
            assistant_message.status = ChatMessage.STATUS_ERROR
            assistant_message.error_message = str(e)
            assistant_message.content = accumulated_content
            assistant_message.save()
            
            yield sse_event("error", {
                "message": str(e),
            })


class NonStreamingGenerateView(APIView):
    """
    Non-streaming fallback for code generation.
    
    POST /api/v1/apps/:app_id/generate
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request, app_id):
        """Generate code without streaming."""
        try:
            app = InternalApp.objects.select_related(
                'organization',
                'backend_connection'
            ).get(pk=app_id)
            
            if not request.user.user_organizations.filter(
                organization=app.organization
            ).exists():
                return Response(
                    {"error": "Access denied"},
                    status=http_status.HTTP_403_FORBIDDEN
                )
            
            message = request.data.get('message', '')
            model = request.data.get('model', 'anthropic/claude-3.5-sonnet')
            session_id = request.data.get('session_id')
            
            if not message:
                return Response(
                    {"error": "Message is required"},
                    status=http_status.HTTP_400_BAD_REQUEST
                )
            
            # Get or create session
            if session_id:
                session = ChatSession.objects.get(pk=session_id)
            else:
                session = ChatSession.objects.create(
                    internal_app=app,
                    title=message[:50],
                    model_id=model,
                    created_by=request.user,
                )
            
            # Save user message
            user_message = ChatMessage.objects.create(
                session=session,
                role=ChatMessage.ROLE_USER,
                content=message,
            )
            
            # Get context
            current_spec = None
            latest_version = AppVersion.objects.filter(
                internal_app=app
            ).order_by('-version_number').first()
            if latest_version:
                current_spec = latest_version.spec_json
            
            # Build registry surface
            from ..models import ResourceRegistryEntry
            registry_entries = []
            if app.backend_connection:
                registry_entries = ResourceRegistryEntry.objects.filter(
                    backend_connection=app.backend_connection,
                    enabled=True
                )
            
            registry_surface = {
                "resources": [
                    {
                        "resource_id": entry.resource_id,
                        "resource_name": entry.resource_name,
                        "exposed_fields": entry.exposed_fields_json or [],
                    }
                    for entry in registry_entries
                ]
            }
            
            # Generate spec
            service = get_openrouter_service()
            start_time = time.time()
            
            spec_json = service.generate_app_spec(
                intent_message=message,
                current_spec=current_spec,
                registry_surface=registry_surface,
                model=model,
            )
            
            duration_ms = int((time.time() - start_time) * 1000)
            
            # Save assistant message
            assistant_message = ChatMessage.objects.create(
                session=session,
                role=ChatMessage.ROLE_ASSISTANT,
                content=json.dumps(spec_json, indent=2),
                model_id=model,
                duration_ms=duration_ms,
                generated_spec_json=spec_json,
            )
            
            # Create version
            version = None
            is_valid, errors = AppSpecValidationService.validate_app_spec(app, spec_json)
            
            if is_valid:
                next_version_number = (
                    latest_version.version_number + 1 if latest_version else 1
                )
                
                version = AppVersion.objects.create(
                    internal_app=app,
                    version_number=next_version_number,
                    source=AppVersion.SOURCE_AI,
                    spec_json=spec_json,
                    created_by=request.user,
                )
                
                CodegenService.generate_files_from_spec(version)
                
                assistant_message.version_created = version
                assistant_message.save()
            
            return Response({
                "session_id": str(session.id),
                "message_id": str(assistant_message.id),
                "spec_json": spec_json,
                "version_id": str(version.id) if version else None,
                "version_number": version.version_number if version else None,
                "duration_ms": duration_ms,
                "validation_errors": errors if not is_valid else None,
            })
            
        except InternalApp.DoesNotExist:
            return Response(
                {"error": "App not found"},
                status=http_status.HTTP_404_NOT_FOUND
            )
        except Exception as e:
            logger.error(f"Generation error: {e}")
            return Response(
                {"error": str(e)},
                status=http_status.HTTP_500_INTERNAL_SERVER_ERROR
            )


class ApplyGeneratedCodeView(APIView):
    """
    Apply generated code to create a new version.
    
    POST /api/v1/messages/:message_id/apply
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request, message_id):
        """Apply code from a chat message to create a version."""
        try:
            message = ChatMessage.objects.select_related(
                'session__internal_app__organization'
            ).get(pk=message_id)
            
            app = message.session.internal_app
            
            if not request.user.user_organizations.filter(
                organization=app.organization
            ).exists():
                return Response(
                    {"error": "Access denied"},
                    status=http_status.HTTP_403_FORBIDDEN
                )
            
            if message.version_created:
                return Response({
                    "message": "Code already applied",
                    "version_id": str(message.version_created.id),
                    "version_number": message.version_created.version_number,
                })
            
            spec_json = message.generated_spec_json
            if not spec_json:
                return Response(
                    {"error": "No generated spec in this message"},
                    status=http_status.HTTP_400_BAD_REQUEST
                )
            
            # Get latest version
            latest_version = AppVersion.objects.filter(
                internal_app=app
            ).order_by('-version_number').first()
            
            next_version_number = (
                latest_version.version_number + 1 if latest_version else 1
            )
            
            # Create version
            version = AppVersion.objects.create(
                internal_app=app,
                version_number=next_version_number,
                source=AppVersion.SOURCE_AI,
                spec_json=spec_json,
                created_by=request.user,
            )
            
            CodegenService.generate_files_from_spec(version)
            
            message.version_created = version
            message.save()
            
            return Response({
                "version_id": str(version.id),
                "version_number": version.version_number,
                "files_generated": version.files.count(),
            })
            
        except ChatMessage.DoesNotExist:
            return Response(
                {"error": "Message not found"},
                status=http_status.HTTP_404_NOT_FOUND
            )
        except Exception as e:
            logger.error(f"Apply error: {e}")
            return Response(
                {"error": str(e)},
                status=http_status.HTTP_500_INTERNAL_SERVER_ERROR
            )

