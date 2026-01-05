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
from rest_framework_simplejwt.authentication import JWTAuthentication
from rest_framework.request import Request

from ..models import InternalApp, AppVersion, VersionFile, VersionAuditLog
from ..models import ChatSession, ChatMessage, CodeGenerationJob
from ..services.openrouter_service import (
    get_openrouter_service,
    StreamChunk,
    MODEL_CONFIGS,
)
from ..services.validation import AppSpecValidationService
from ..services.codegen import CodegenService
from ..services.agentic_service import get_agentic_service
from ..services.version_service import VersionService
from ..services.snapshot_service import SnapshotService

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
            "default": "anthropic/claude-sonnet-4",
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
            
            sessions = app.chat_sessions.order_by('-created_at')
            
            return Response({
                "sessions": [
                    {
                        "id": str(s.id),
                        "title": s.title,
                        "model_id": s.model_id,
                        "created_at": s.created_at.isoformat(),
                        "created_by": str(s.created_by_id) if s.created_by_id else None,
                        "message_count": s.messages.count(),
                        "last_message_at": (
                            s.messages.order_by('-created_at').first().created_at.isoformat()
                            if s.messages.exists() else None
                        ),
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
                model_id=request.data.get("model_id", "anthropic/claude-sonnet-4"),
                created_by=request.user,
            )
            
            return Response({
                "id": str(session.id),
                "title": session.title,
                "model_id": session.model_id,
                "created_at": session.created_at.isoformat(),
                "created_by": str(session.created_by_id) if session.created_by_id else None,
                "message_count": 0,
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
                        "duration_ms": m.duration_ms,
                        "generated_files": m.generated_files,
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
        model = request.GET.get('model', 'anthropic/claude-sonnet-4')
        mode = request.GET.get('mode', 'appspec')  # 'appspec' or 'code'
        
        if not message:
            return JsonResponse(
                {"error": "Message is required"},
                status=400
            )
        
        # Verify authentication via session or token
        user = request.user
        if not user.is_authenticated:
            # Try JWT from Authorization header
            auth_header = request.META.get('HTTP_AUTHORIZATION', '')
            if not auth_header:
                return JsonResponse(
                    {"error": "Authentication required"},
                    status=401
                )
            
            # Manually authenticate using JWT
            try:
                jwt_auth = JWTAuthentication()
                # Wrap in DRF Request for JWT auth to work
                drf_request = Request(request)
                auth_result = jwt_auth.authenticate(drf_request)
                if auth_result is None:
                    return JsonResponse(
                        {"error": "Invalid authentication token"},
                        status=401
                    )
                user, _ = auth_result
            except Exception as e:
                logger.warning(f"JWT authentication failed: {e}")
                return JsonResponse(
                    {"error": "Authentication failed"},
                    status=401
                )
        
        try:
            app = InternalApp.objects.select_related(
                'organization',
                'backend_connection'
            ).get(pk=app_id)
        except InternalApp.DoesNotExist:
            return JsonResponse({"error": "App not found"}, status=404)
        
        if not user.user_organizations.filter(organization=app.organization).exists():
            return JsonResponse(
                {"error": "Access denied"},
                status=http_status.HTTP_403_FORBIDDEN
            )
        
        # Create streaming response
        response = StreamingHttpResponse(
            self._generate_stream(
                app=app,
                message=message,
                model=model,
                mode=mode,
                session_id=session_id,
                user=user,
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
                session = ChatSession.objects.select_related("internal_app").get(
                    pk=session_id,
                )
                if session.internal_app_id != app.id:
                    yield sse_event("error", {"message": "Session does not belong to this app"})
                    return
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
        if not session.title or session.title.strip() == "" or session.title == "New Chat":
            session.title = message[:50] + "..." if len(message) > 50 else message
            session.save(update_fields=["title", "updated_at"])
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
        # Use the latest STABLE version (complete generation) as the base
        current_spec = None
        latest_stable_version = VersionService.get_latest_stable_version(app)
        
        if latest_stable_version:
            current_spec = latest_stable_version.spec_json
        
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
                                # Create new version using version service for correct numbering
                                next_version_number = VersionService.get_next_version_number(app)
                                
                                version = AppVersion.objects.create(
                                    internal_app=app,
                                    version_number=next_version_number,
                                    source=AppVersion.SOURCE_AI,
                                    spec_json=spec_json,
                                    created_by=user if user.is_authenticated else None,
                                    is_active=False,  # Start inactive until files are generated
                                )
                                
                                # Generate files
                                CodegenService.generate_files_from_spec(version)
                                
                                # Mark as active after successful file generation
                                version.is_active = True
                                version.save(update_fields=['is_active', 'updated_at'])
                                
                                # Create snapshot for revert capability
                                try:
                                    SnapshotService.create_version_snapshot(version)
                                except Exception as snapshot_error:
                                    logger.warning(f"Failed to create snapshot: {snapshot_error}")
                                
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
            model = request.data.get('model', 'anthropic/claude-sonnet-4')
            session_id = request.data.get('session_id')
            
            if not message:
                return Response(
                    {"error": "Message is required"},
                    status=http_status.HTTP_400_BAD_REQUEST
                )
            
            # Get or create session
            try:
                if session_id:
                    session = ChatSession.objects.select_related("internal_app").get(
                        pk=session_id,
                    )
                    if session.internal_app_id != app.id:
                        return Response(
                            {"error": "Session does not belong to this app"},
                            status=http_status.HTTP_400_BAD_REQUEST,
                        )
                else:
                    session = ChatSession.objects.create(
                        internal_app=app,
                        title=message[:50],
                        model_id=model,
                        created_by=request.user,
                    )
            except ChatSession.DoesNotExist:
                return Response(
                    {"error": "Session not found"},
                    status=http_status.HTTP_404_NOT_FOUND,
                )
            
            # Save user message
            user_message = ChatMessage.objects.create(
                session=session,
                role=ChatMessage.ROLE_USER,
                content=message,
            )
            if not session.title or session.title.strip() == "" or session.title == "New Chat":
                session.title = message[:50] + "..." if len(message) > 50 else message
                session.save(update_fields=["title", "updated_at"])
            
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
                    is_active=False,  # Start inactive until files are generated
                )
                
                CodegenService.generate_files_from_spec(version)
                
                # Mark as active after successful file generation
                version.is_active = True
                version.save(update_fields=['is_active', 'updated_at'])
                
                # Create snapshot for revert capability
                try:
                    SnapshotService.create_version_snapshot(version)
                except Exception as snapshot_error:
                    logger.warning(f"Failed to create snapshot: {snapshot_error}")
                
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


@method_decorator(csrf_exempt, name='dispatch')
class AgenticGenerateView(View):
    """
    SSE endpoint for agentic code generation.
    
    GET /api/v1/apps/:app_id/generate/agentic?session_id=...&message=...&model=...
    
    Returns Server-Sent Events with real-time progress updates:
    - agent_start: Generation started
    - phase_change: Research/Plan/Execute/Validate transitions
    - thinking: Agent's reasoning visible to user
    - plan_created: Execution plan with steps
    - step_start/step_complete: Step progress
    - file_generated: Each generated file
    - validation_result: Code validation outcome
    - agent_complete: Generation finished
    - agent_error: Error occurred
    """
    
    def get(self, request, app_id):
        """Start agentic code generation."""
        # Get parameters
        session_id = request.GET.get('session_id')
        message = request.GET.get('message', '')
        model = request.GET.get('model', 'anthropic/claude-sonnet-4')
        
        if not message:
            return JsonResponse(
                {"error": "Message is required"},
                status=400
            )
        
        # Verify authentication
        user = request.user
        if not user.is_authenticated:
            auth_header = request.META.get('HTTP_AUTHORIZATION', '')
            if not auth_header:
                return JsonResponse(
                    {"error": "Authentication required"},
                    status=401
                )
            
            try:
                jwt_auth = JWTAuthentication()
                drf_request = Request(request)
                auth_result = jwt_auth.authenticate(drf_request)
                if auth_result is None:
                    return JsonResponse(
                        {"error": "Invalid authentication token"},
                        status=401
                    )
                user, _ = auth_result
            except Exception as e:
                logger.warning(f"JWT authentication failed: {e}")
                return JsonResponse(
                    {"error": "Authentication failed"},
                    status=401
                )
        
        try:
            app = InternalApp.objects.select_related(
                'organization',
                'backend_connection'
            ).get(pk=app_id)
        except InternalApp.DoesNotExist:
            return JsonResponse({"error": "App not found"}, status=404)
        
        if not user.user_organizations.filter(organization=app.organization).exists():
            return JsonResponse({"error": "Access denied"}, status=403)
        
        # Create streaming response
        response = StreamingHttpResponse(
            self._generate_agentic_stream(
                app=app,
                message=message,
                model=model,
                session_id=session_id,
                user=user,
            ),
            content_type='text/event-stream'
        )
        response['Cache-Control'] = 'no-cache'
        response['X-Accel-Buffering'] = 'no'
        return response
    
    def _generate_agentic_stream(
        self,
        app: InternalApp,
        message: str,
        model: str,
        session_id: str,
        user,
    ) -> Generator[str, None, None]:
        """Generate SSE stream for agentic code generation."""
        
        # Get or create session
        try:
            if session_id:
                session = ChatSession.objects.select_related("internal_app").get(
                    pk=session_id,
                )
                if session.internal_app_id != app.id:
                    yield sse_event("agent_error", {
                        "message": "Session does not belong to this app",
                        "phase": "error",
                        "recoverable": False,
                    })
                    return
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
            yield sse_event("agent_error", {
                "message": "Session not found",
                "phase": "idle",
                "recoverable": False,
            })
            return
        
        # Save user message
        user_message = ChatMessage.objects.create(
            session=session,
            role=ChatMessage.ROLE_USER,
            content=message,
            status=ChatMessage.STATUS_COMPLETE,
        )
        if not session.title or session.title.strip() == "" or session.title == "New Chat":
            session.title = message[:50] + "..." if len(message) > 50 else message
            session.save(update_fields=["title", "updated_at"])
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
        
        # Get current spec and registry surface
        # Use the latest STABLE version (complete generation) as the base
        # This ensures we don't build on top of incomplete/cancelled versions
        current_spec = None
        latest_stable_version = VersionService.get_latest_stable_version(app)
        
        if latest_stable_version:
            current_spec = latest_stable_version.spec_json
        
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
        
        # Run agentic generation with incremental state saving
        start_time = time.time()
        agentic_service = get_agentic_service()
        accumulated_content = ""
        version = None
        current_step_index = 0
        plan_steps = []
        
        try:
            # Create draft version at the start with 'generating' status
            # Use version service to get next version number (includes all versions)
            next_version_number = VersionService.get_next_version_number(app)
            
            # Carry forward the latest known spec so preview has pages/resources
            draft_spec = current_spec or {"generated": True, "agentic": True}
            
            version = AppVersion.objects.create(
                internal_app=app,
                version_number=next_version_number,
                source=AppVersion.SOURCE_AI,
                spec_json=draft_spec,
                created_by=user if user.is_authenticated else None,
                generation_status=AppVersion.GEN_STATUS_GENERATING,
            )
            
            # Link to message immediately
            assistant_message.version_created = version
            assistant_message.save()
            
            # Notify client of draft version
            yield sse_event("version_draft", {
                "version_id": str(version.id),
                "version_number": version.version_number,
                "status": "generating",
            })
            
            for event in agentic_service.generate_app(
                user_message=message,
                current_spec=current_spec,
                registry_surface=registry_surface,
                app_name=app.name,
                model=model,
                app=app,
                version=version,
            ):
                # Stream the event to client
                yield event.to_sse()
                
                # Save plan to version when created
                if event.type == "plan_created":
                    plan_data = event.data.get("plan", {})
                    version.generation_plan_json = plan_data
                    version.save(update_fields=['generation_plan_json', 'updated_at'])
                    plan_steps = plan_data.get("steps", []) or []
                
                # Track step progress
                if event.type == "step_start":
                    current_step_index = event.data.get("step_index", 0)
                    version.generation_current_step = current_step_index
                    version.save(update_fields=['generation_current_step', 'updated_at'])
                
                # Incrementally save files as they're generated
                if event.type == "file_generated":
                    file_data = event.data.get("file", {})
                    path = file_data.get("path", "src/App.tsx")
                    content = file_data.get("content", "")
                    
                    # Use update_or_create to handle duplicates from different steps
                    VersionFile.objects.update_or_create(
                        app_version=version,
                        path=path,
                        defaults={"content": content}
                    )
                    logger.info(f"Saved file incrementally: {path}")
                
            # Generation complete - update version status and mark as active
            duration_ms = int((time.time() - start_time) * 1000)
            
            version.generation_status = AppVersion.GEN_STATUS_COMPLETE
            version.generation_current_step = current_step_index + 1  # Mark all steps done
            version.is_active = True  # Mark as active so it appears in API responses
            version.save(update_fields=['generation_status', 'generation_current_step', 'is_active', 'updated_at'])
            
            # Update assistant message
            assistant_message.status = ChatMessage.STATUS_COMPLETE
            # Do not persist streamed/thinking text; keep assistant content empty
            assistant_message.content = ""
            assistant_message.duration_ms = duration_ms
            
            final_files_payload = [
                {
                    "path": f.path,
                    "content": f.content,
                    "action": "create",
                    "language": f.path.split('.')[-1] if '.' in f.path else "txt"
                }
                for f in version.files.all()
            ]
            assistant_message.generated_files = {
                "files": final_files_payload,
            }
            assistant_message.save()
            
            # Create version snapshot for revert capability
            try:
                snapshot = SnapshotService.create_version_snapshot(version)
                logger.info(f"Created snapshot for version {version.id}: {snapshot.total_tables} tables")
                
                # Log version creation for audit trail
                VersionAuditLog.log_operation(
                    internal_app=app,
                    app_version=version,
                    operation=VersionAuditLog.OPERATION_CREATE,
                    user=user if user.is_authenticated else None,
                    details={
                        'source': 'agentic_generation',
                        'files_generated': len(final_files_payload),
                        'tables_snapshot': snapshot.total_tables,
                        'duration_ms': duration_ms,
                    },
                )
            except Exception as snapshot_error:
                logger.warning(f"Failed to create snapshot for version {version.id}: {snapshot_error}")
            
            # Get final file list
            yield sse_event("preview_ready", {
                "version_id": str(version.id),
                "version_number": version.version_number,
                "preview_url": f"/preview/apps/{app.id}?version={version.id}",
                "files": final_files_payload,
            })
            
            yield sse_event("version_created", {
                "version_id": str(version.id),
                "version_number": version.version_number,
            })
            
            # Emit snapshot_created event for frontend awareness
            yield sse_event("snapshot_created", {
                "version_id": str(version.id),
                "version_number": version.version_number,
            })
            
            yield sse_event("done", {"success": True})
            
        except Exception as e:
            logger.error(f"Agentic generation error: {e}")
            
            # Mark version as errored if it exists
            if version:
                version.generation_status = AppVersion.GEN_STATUS_ERROR
                version.generation_error = str(e)
                version.save(update_fields=['generation_status', 'generation_error', 'updated_at'])
            
            assistant_message.status = ChatMessage.STATUS_ERROR
            assistant_message.error_message = str(e)
            assistant_message.content = accumulated_content
            assistant_message.save()
            
            yield sse_event("agent_error", {
                "message": str(e),
                "phase": "error",
                "recoverable": True,  # Can resume with saved files
                "version_id": str(version.id) if version else None,
            })


class GenerationStateView(APIView):
    """
    Get current generation state for a version.
    
    GET /api/v1/versions/:version_id/generation-state
    
    Used by frontend to resume/restore state after refresh.
    """
    permission_classes = [IsAuthenticated]
    
    def get(self, request, version_id):
        """Get the generation state for a version."""
        try:
            version = AppVersion.objects.select_related(
                'internal_app__organization'
            ).prefetch_related('files').get(pk=version_id)
            
            # Verify access
            if not request.user.user_organizations.filter(
                organization=version.internal_app.organization
            ).exists():
                return Response(
                    {"error": "Access denied"},
                    status=http_status.HTTP_403_FORBIDDEN
                )
            
            # Get files
            files = [
                {
                    "path": f.path,
                    "content": f.content,
                    "action": "create",
                    "language": f.path.split('.')[-1] if '.' in f.path else "txt"
                }
                for f in version.files.all()
            ]
            
            return Response({
                "version_id": str(version.id),
                "version_number": version.version_number,
                "generation_status": version.generation_status,
                "is_active": version.is_active,
                "generation_plan": version.generation_plan_json,
                "current_step": version.generation_current_step,
                "error": version.generation_error,
                "files": files,
                "file_count": len(files),
                "app_id": str(version.internal_app.id),
                "app_name": version.internal_app.name,
                "created_at": version.created_at.isoformat(),
            })
            
        except AppVersion.DoesNotExist:
            return Response(
                {"error": "Version not found"},
                status=http_status.HTTP_404_NOT_FOUND
            )


class LatestGenerationView(APIView):
    """
    Get the latest generation state for an app (in-progress or complete).
    
    GET /api/v1/apps/:app_id/latest-generation
    
    Returns the most recent version and its generation state.
    Also includes the latest stable version info for fallback when generation is incomplete.
    """
    permission_classes = [IsAuthenticated]
    
    def get(self, request, app_id):
        """Get the latest generation for an app."""
        try:
            app = InternalApp.objects.select_related('organization').get(pk=app_id)
            
            # Verify access
            if not request.user.user_organizations.filter(
                organization=app.organization
            ).exists():
                return Response(
                    {"error": "Access denied"},
                    status=http_status.HTTP_403_FORBIDDEN
                )
            
            # Get the latest version (including in-progress ones)
            version = AppVersion.objects.filter(
                internal_app=app
            ).prefetch_related('files').order_by('-created_at').first()
            
            # Also get the latest stable version for fallback
            latest_stable = VersionService.get_latest_stable_version(app)
            
            if not version:
                return Response({
                    "has_generation": False,
                    "message": "No versions found for this app",
                    "latest_stable_version_id": None,
                    "latest_stable_version_number": None,
                })
            
            # Get files
            files = [
                {
                    "path": f.path,
                    "content": f.content,
                    "action": "create",
                    "language": f.path.split('.')[-1] if '.' in f.path else "txt"
                }
                for f in version.files.all()
            ]
            
            # Get stable version files if different
            stable_files = None
            if latest_stable and latest_stable.id != version.id:
                stable_files = [
                    {
                        "path": f.path,
                        "content": f.content,
                        "action": "create",
                        "language": f.path.split('.')[-1] if '.' in f.path else "txt"
                    }
                    for f in latest_stable.files.all()
                ]
            
            return Response({
                "has_generation": True,
                "version_id": str(version.id),
                "version_number": version.version_number,
                "generation_status": version.generation_status,
                "is_active": version.is_active,
                "generation_plan": version.generation_plan_json,
                "current_step": version.generation_current_step,
                "error": version.generation_error,
                "files": files,
                "file_count": len(files),
                "is_complete": version.generation_status == AppVersion.GEN_STATUS_COMPLETE,
                "is_generating": version.generation_status == AppVersion.GEN_STATUS_GENERATING,
                "created_at": version.created_at.isoformat(),
                # Stable version info for fallback
                "latest_stable_version_id": str(latest_stable.id) if latest_stable else None,
                "latest_stable_version_number": latest_stable.version_number if latest_stable else None,
                "latest_stable_files": stable_files,
            })
            
        except InternalApp.DoesNotExist:
            return Response(
                {"error": "App not found"},
                status=http_status.HTTP_404_NOT_FOUND
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
            
            # Create snapshot for revert capability
            try:
                SnapshotService.create_version_snapshot(version)
            except Exception as snapshot_error:
                logger.warning(f"Failed to create snapshot: {snapshot_error}")
            
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


class CancelGenerationView(APIView):
    """
    Cancel an in-progress code generation.
    
    POST /api/v1/versions/:version_id/cancel
    
    This endpoint is called when the user aborts a generation request.
    It cleans up the generating version:
    - If no files were generated, the version is deleted
    - If partial files exist, the version is marked as 'error' 
    
    The frontend can then show the latest stable version instead.
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request, version_id):
        """Cancel a generating version."""
        try:
            version = AppVersion.objects.select_related(
                'internal_app__organization'
            ).get(pk=version_id)
            
            # Verify access
            if not request.user.user_organizations.filter(
                organization=version.internal_app.organization
            ).exists():
                return Response(
                    {"error": "Access denied"},
                    status=http_status.HTTP_403_FORBIDDEN
                )
            
            # Cancel the generation
            result = VersionService.cancel_generating_version(version_id, request.user)
            
            if result['success']:
                # Get the latest stable version to return
                latest_stable = VersionService.get_latest_stable_version(version.internal_app)
                
                return Response({
                    "success": True,
                    "action": result['action'],
                    "cancelled_version_id": version_id,
                    "latest_stable_version_id": str(latest_stable.id) if latest_stable else None,
                    "latest_stable_version_number": latest_stable.version_number if latest_stable else None,
                })
            else:
                return Response(
                    {"error": result.get('error', 'Cancel failed')},
                    status=http_status.HTTP_400_BAD_REQUEST
                )
            
        except AppVersion.DoesNotExist:
            return Response(
                {"error": "Version not found"},
                status=http_status.HTTP_404_NOT_FOUND
            )
        except Exception as e:
            logger.error(f"Cancel error: {e}")
            return Response(
                {"error": str(e)},
                status=http_status.HTTP_500_INTERNAL_SERVER_ERROR
            )

