"""
Streaming Views for Real-time Code Generation

Implements Server-Sent Events (SSE) for live streaming updates
during AI code generation - similar to Cursor, Lovable, Replit.
"""
import json
import logging
import time
import uuid
from typing import Generator, Optional
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

from ..models import (
    AppVersion,
    AppVersionGenerationStatus,
    AppVersionSource,
    ChatMessage,
    ChatMessageRole,
    ChatMessageStatus,
    ChatSession,
    CodeGenerationJob,
    CodeGenerationJobStatus,
    InternalApp,
    QuestioningJob,
    QuestioningJobStatus,
    QuestioningSession,
    QuestioningStatus,
    UserOrganization,
    VersionAuditLog,
    VersionFile,
)
from ..ai.models import AIModel
from ..ai.client import get_llm_client
from ..ai.types import LLMSettings
from ..services.openrouter_service import (
    get_openrouter_service,
    StreamChunk,
    MODEL_CONFIGS,
)
from ..utils.enum_utils import safe_str_enum
from ..services.validation import AppSpecValidationService
from ..services.codegen import CodegenService
from ..services.agentic_service import get_agentic_service
from ..services.version_service import VersionService
from ..services.snapshot_service import SnapshotService
from ..tasks import run_questioning_phase
from ..permissions import require_editor_or_above, require_org_membership

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
            "default": AIModel.CLAUDE_SONNET_4_5.value,
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
                model_id=_parse_model(request.data.get("model_id")),
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


def _parse_model(raw_model: Optional[str]) -> AIModel:
    return safe_str_enum(raw_model, AIModel.CLAUDE_SONNET_4_5, AIModel)


def _build_chat_history(questioning_session: QuestioningSession) -> list:
    """Build chat_history list from ChatMessages in the linked ChatSession.
    
    Returns list of dicts with role and content for each message in order.
    """
    return [
        {"role": msg.role, "content": msg.content}
        for msg in questioning_session.chat_session.messages.order_by('created_at')
    ]


def _authenticate_request(request):
    """Authenticate the request, return user or JsonResponse error.
    
    Supports authentication via:
    1. Django session (request.user already authenticated)
    2. Authorization header (Bearer token)
    3. Query parameter (?token=...) - needed for SSE/EventSource which can't set headers
    """
    user = request.user
    if not user.is_authenticated:
        # Try Authorization header first
        auth_header = request.META.get('HTTP_AUTHORIZATION', '')
        
        # Fall back to query param token (for SSE/EventSource)
        if not auth_header:
            token_param = request.GET.get('token', '')
            if token_param:
                auth_header = f'Bearer {token_param}'
        
        if not auth_header:
            return JsonResponse({"error": "Authentication required"}, status=401)
        
        try:
            jwt_auth = JWTAuthentication()
            drf_request = Request(request)
            # Inject the auth header if it came from query param
            if not request.META.get('HTTP_AUTHORIZATION'):
                drf_request.META['HTTP_AUTHORIZATION'] = auth_header
            auth_result = jwt_auth.authenticate(drf_request)
            if auth_result is None:
                return JsonResponse({"error": "Invalid authentication token"}, status=401)
            user, _ = auth_result
        except Exception as e:
            logger.warning("JWT authentication failed: %s", e)
            return JsonResponse({"error": "Authentication failed"}, status=401)
    return user


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
        model = _parse_model(request.GET.get('model'))
        mode = request.GET.get('mode', 'appspec')  # 'appspec' or 'code'
        
        if not message:
            return JsonResponse(
                {"error": "Message is required"},
                status=400
            )
        
        # Verify authentication (supports header and query param token for SSE)
        user = _authenticate_request(request)
        if isinstance(user, JsonResponse):
            return user
        
        try:
            app = InternalApp.objects.select_related(
                'organization',
            ).get(pk=app_id)
        except InternalApp.DoesNotExist:
            return JsonResponse({"error": "App not found"}, status=404)
        
        # Verify user is editor or above (code generation requires edit permissions)
        try:
            membership = UserOrganization.objects.get(user=user, organization=app.organization)
            if not membership.is_editor_or_above():
                return JsonResponse(
                    {"error": "You must be an editor or admin to generate code"},
                    status=403
                )
        except UserOrganization.DoesNotExist:
            return JsonResponse(
                {"error": "Access denied"},
                status=403
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
        model: AIModel,
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
            role=ChatMessageRole.USER,
            content=message,
            status=ChatMessageStatus.COMPLETE,
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
            role=ChatMessageRole.ASSISTANT,
            content="",
            status=ChatMessageStatus.STREAMING,
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
        
        # No external resource registry in the current backend
        registry_surface = {"resources": []}
        
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
                if latest_stable_version:
                    for vf in latest_stable_version.files.all():
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
                    assistant_message.status = ChatMessageStatus.COMPLETE
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
                                    source=AppVersionSource.AI,
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
                    assistant_message.status = ChatMessageStatus.ERROR
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
            assistant_message.status = ChatMessageStatus.ERROR
            assistant_message.error_message = str(e)
            assistant_message.content = accumulated_content
            assistant_message.save()
            
            yield sse_event("error", {
                "message": str(e),
            })


class NonStreamingGenerateView(APIView):
    """
    Non-streaming fallback for code generation (editor+ only).
    
    POST /api/v1/apps/:app_id/generate
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request, app_id):
        """Generate code without streaming (editor+ only)."""
        try:
            app = InternalApp.objects.select_related(
                'organization',
            ).get(pk=app_id)
            
            # Verify user is editor or above
            membership, error = require_editor_or_above(request, app.organization)
            if error:
                return error
            
            message = request.data.get('message', '')
            model = _parse_model(request.data.get('model'))
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
                role=ChatMessageRole.USER,
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
            
            # No external resource registry in the current backend
            registry_surface = {"resources": []}
            
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
                role=ChatMessageRole.ASSISTANT,
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
                    source=AppVersionSource.AI,
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
    Start agentic code generation via background Celery worker.
    
    POST /api/v1/apps/:app_id/generate/agentic/
    Body: { "message": "...", "model": "...", "session_id": "..." }
    
    Returns: { "job_id": "...", "stream_url": "/api/v1/jobs/{job_id}/stream/" }
    
    The client then connects to the stream URL for SSE events.
    This enables reconnection support - events are stored in DB.
    
    Also supports legacy GET for backward compatibility during transition.
    """
    
    def post(self, request, app_id):
        """Start agentic code generation (new job-based flow).

        Requires a completed questioning phase (QuestioningSession status COMPLETE or SKIPPED).
        """
        try:
            body = json.loads(request.body) if request.body else {}
        except json.JSONDecodeError:
            return JsonResponse({"error": "Invalid JSON body"}, status=400)
        
        message = body.get('message', '')
        model = _parse_model(body.get('model'))
        session_id = body.get('session_id')
        if not session_id:
            return JsonResponse({"error": "session_id is required"}, status=400)
        
        # Verify authentication
        user = self._authenticate(request)
        if isinstance(user, JsonResponse):
            return user
        
        # Get app
        try:
            app = InternalApp.objects.select_related(
                'organization',
            ).get(pk=app_id)
        except InternalApp.DoesNotExist:
            return JsonResponse({"error": "App not found"}, status=404)
        
        # Verify user is editor or above (code generation requires edit permissions)
        try:
            membership = UserOrganization.objects.get(user=user, organization=app.organization)
            if not membership.is_editor_or_above():
                return JsonResponse(
                    {"error": "You must be an editor or admin to generate code"},
                    status=403
                )
        except UserOrganization.DoesNotExist:
            return JsonResponse({"error": "Access denied"}, status=403)
        
        # Load session and completed QuestioningSession
        try:
            session = ChatSession.objects.select_related(
                "internal_app",
            ).get(pk=session_id)
        except ChatSession.DoesNotExist:
            return JsonResponse({"error": "Session not found"}, status=404)
        
        if session.internal_app_id != app.id:
            return JsonResponse({"error": "Session does not belong to this app"}, status=400)

        try:
            questioning_session = session.questioning_session
        except QuestioningSession.DoesNotExist:
            return JsonResponse({"error": "Questioning session not found"}, status=400)

        if questioning_session.status not in [
            QuestioningStatus.COMPLETE,
            QuestioningStatus.SKIPPED,
        ]:
            return JsonResponse(
                {
                    "error": "Questioning phase is not complete",
                    "status": questioning_session.status,
                    "questioning_session_id": str(questioning_session.id),
                },
                status=400,
            )

        if not message:
            message = questioning_session.initial_request

        return self._start_generation_job(app, session, message, model, user, questioning_session)
    
    def _start_generation_job(self, app, session, message, model, user, questioning_session=None):
        """Create and queue a code generation job."""
        from vector_app.tasks import run_agentic_generation
        
        job = CodeGenerationJob.objects.create(
            internal_app=app,
            session=session,
            user_message=message,
            model_id=model,
            created_by=user,
            status=CodeGenerationJobStatus.QUEUED,
        )
        
        # Queue the Celery task
        run_agentic_generation.delay(str(job.id))
        
        response_data = {
            "job_id": str(job.id),
            "stream_url": f"/api/v1/jobs/{job.id}/stream/",
            "status": "queued",
            "action": "generate",
            "session_id": str(session.id),
        }
        
        if questioning_session:
            response_data["questioning_session_id"] = str(questioning_session.id)
            response_data["synthesized_requirements"] = questioning_session.synthesized_requirements
        
        return JsonResponse(response_data, status=201)
    
    def get(self, request, app_id):
        """Legacy GET endpoint - redirects to job-based flow for SSE streaming."""
        # Get parameters from query string
        session_id = request.GET.get('session_id')
        message = request.GET.get('message', '')
        model = _parse_model(request.GET.get('model'))
        if not session_id:
            return JsonResponse({"error": "session_id is required"}, status=400)
        
        # Verify authentication
        user = self._authenticate(request)
        if isinstance(user, JsonResponse):
            return user
        
        try:
            app = InternalApp.objects.select_related(
                'organization',
            ).get(pk=app_id)
        except InternalApp.DoesNotExist:
            return JsonResponse({"error": "App not found"}, status=404)
        
        # Verify user is editor or above (code generation requires edit permissions)
        try:
            membership = UserOrganization.objects.get(user=user, organization=app.organization)
            if not membership.is_editor_or_above():
                return JsonResponse(
                    {"error": "You must be an editor or admin to generate code"},
                    status=403
                )
        except UserOrganization.DoesNotExist:
            return JsonResponse({"error": "Access denied"}, status=403)
        
        # Load session and completed QuestioningSession
        try:
            session = ChatSession.objects.get(pk=session_id)
        except ChatSession.DoesNotExist:
            return JsonResponse({"error": "Session not found"}, status=404)

        if session.internal_app_id != app.id:
            return JsonResponse({"error": "Session does not belong to this app"}, status=400)

        try:
            questioning_session = session.questioning_session
        except QuestioningSession.DoesNotExist:
            return JsonResponse({"error": "Questioning session not found"}, status=400)

        if questioning_session.status not in [
            QuestioningStatus.COMPLETE,
            QuestioningStatus.SKIPPED,
        ]:
            return JsonResponse(
                {"error": "Questioning phase is not complete", "status": questioning_session.status},
                status=400,
            )

        if not message:
            message = questioning_session.initial_request

        # Create the job
        job = CodeGenerationJob.objects.create(
            internal_app=app,
            session=session,
            user_message=message,
            model_id=model,
            created_by=user,
            status=CodeGenerationJobStatus.QUEUED,
        )
        
        # Queue the Celery task
        from vector_app.tasks import run_agentic_generation
        run_agentic_generation.delay(str(job.id))
        
        # For GET requests (legacy), return SSE stream from job
        # This maintains backward compatibility
        response = StreamingHttpResponse(
            self._stream_from_job(job),
            content_type='text/event-stream'
        )
        response['Cache-Control'] = 'no-cache'
        response['X-Accel-Buffering'] = 'no'
        return response
    
    def _authenticate(self, request):
        """Authenticate the request, return user or JsonResponse error."""
        return _authenticate_request(request)
    
    def _stream_from_job(self, job: 'CodeGenerationJob') -> Generator[str, None, None]:
        """Stream events from job's events_json (for legacy GET compatibility)."""
        last_index = 0
        poll_interval = 0.3  # 300ms
        max_wait_for_start = 30  # Wait up to 30s for job to start
        waited = 0
        
        while True:
            job.refresh_from_db()
            events = job.events_json
            
            # Stream any new events
            if len(events) > last_index:
                for event in events[last_index:]:
                    yield sse_event(event['type'], event['data'])
                last_index = len(events)
                waited = 0  # Reset wait counter when we get events
            
            # Check if job is done
            if job.status in [
                CodeGenerationJobStatus.COMPLETE,
                CodeGenerationJobStatus.FAILED,
                CodeGenerationJobStatus.CANCELLED,
            ]:
                break
            
            # Wait for more events
            time.sleep(poll_interval)
            waited += poll_interval
            
            # Timeout if job never starts
            if job.status == CodeGenerationJobStatus.QUEUED and waited > max_wait_for_start:
                yield sse_event("agent_error", {
                    "message": "Job timed out waiting to start. The background worker may need to be restarted.",
                    "phase": "error",
                    "recoverable": True,
                    "details": {
                        "timeout_seconds": max_wait_for_start,
                        "suggestion": "Try refreshing the page. If the issue persists, the worker may need to be restarted.",
                    },
                })
                break


@method_decorator(csrf_exempt, name='dispatch')
class QuestioningStartView(View):
    """
    Start a questioning job for a new requirements session.
    
    POST /api/v1/questioning/start/
    Body: { "app_id": "...", "message": "...", "model": "...", "session_id": "..." }
    """
    
    def post(self, request):
        try:
            body = json.loads(request.body) if request.body else {}
        except json.JSONDecodeError:
            return JsonResponse({"error": "Invalid JSON body"}, status=400)
        
        app_id = body.get("app_id")
        message = body.get("message", "")
        model = _parse_model(body.get("model"))
        session_id = body.get("session_id")
        
        if not app_id:
            return JsonResponse({"error": "app_id is required"}, status=400)
        if not message:
            return JsonResponse({"error": "Message is required"}, status=400)
        
        user = _authenticate_request(request)
        if isinstance(user, JsonResponse):
            return user
        
        try:
            app = InternalApp.objects.select_related("organization").get(pk=app_id)
        except InternalApp.DoesNotExist:
            return JsonResponse({"error": "App not found"}, status=404)
        
        try:
            membership = UserOrganization.objects.get(user=user, organization=app.organization)
            if not membership.is_editor_or_above():
                return JsonResponse(
                    {"error": "You must be an editor or admin to start questioning"},
                    status=403,
                )
        except UserOrganization.DoesNotExist:
            return JsonResponse({"error": "Access denied"}, status=403)
        
        if session_id:
            try:
                session = ChatSession.objects.get(pk=session_id)
            except ChatSession.DoesNotExist:
                return JsonResponse({"error": "Session not found"}, status=404)
            if session.internal_app_id != app.id:
                return JsonResponse({"error": "Session does not belong to this app"}, status=400)
            try:
                existing = session.questioning_session
                return JsonResponse(
                    {
                        "error": "Questioning session already exists",
                        "questioning_session_id": str(existing.id),
                    },
                    status=400,
                )
            except QuestioningSession.DoesNotExist:
                pass
        else:
            session = ChatSession.objects.create(
                internal_app=app,
                title=message[:50] + "..." if len(message) > 50 else message,
                model_id=model,
                created_by=user,
            )
        
        questioning_session = QuestioningSession.objects.create(
            chat_session=session,
            initial_request=message,
            status=QuestioningStatus.IN_PROGRESS,
            question_count=0,
        )
        
        job = QuestioningJob.objects.create(
            questioning_session=questioning_session,
            status=QuestioningJobStatus.QUEUED,
            current_question="",
            user_answer=message,
            created_by=user,
        )
        
        run_questioning_phase.delay(str(job.id))
        
        return JsonResponse(
            {
                "job_id": str(job.id),
                "stream_url": f"/api/v1/questioning/{job.id}/stream/",
                "status": job.status,
                "session_id": str(session.id),
                "questioning_session_id": str(questioning_session.id),
            },
            status=201,
        )


@method_decorator(csrf_exempt, name='dispatch')
class QuestioningStreamView(View):
    """
    SSE endpoint for streaming questioning job events.
    
    GET /api/v1/questioning/:job_id/stream/?last_index=0
    """
    
    def get(self, request, job_id):
        last_index = int(request.GET.get("last_index", 0))
        
        user = _authenticate_request(request)
        if isinstance(user, JsonResponse):
            return user
        
        try:
            job = QuestioningJob.objects.select_related(
                "questioning_session__chat_session__internal_app__organization"
            ).get(pk=job_id)
        except QuestioningJob.DoesNotExist:
            return JsonResponse({"error": "Job not found"}, status=404)
        
        app = job.questioning_session.chat_session.internal_app
        if not user.user_organizations.filter(organization=app.organization).exists():
            return JsonResponse({"error": "Access denied"}, status=403)
        
        response = StreamingHttpResponse(
            self._stream_events(job, last_index),
            content_type="text/event-stream",
        )
        response["Cache-Control"] = "no-cache"
        response["X-Accel-Buffering"] = "no"
        return response
    
    def _stream_events(self, job: QuestioningJob, last_index: int) -> Generator[str, None, None]:
        poll_interval = 0.3
        max_wait_for_start = 30
        waited = 0
        
        while True:
            job.refresh_from_db()
            events = job.events_json
            
            if len(events) > last_index:
                for event in events[last_index:]:
                    yield sse_event(event["type"], event["data"])
                last_index = len(events)
                waited = 0
            
            if job.status in [
                QuestioningJobStatus.COMPLETE,
                QuestioningJobStatus.SKIPPED,
                QuestioningJobStatus.FAILED,
            ]:
                break
            
            time.sleep(poll_interval)
            waited += poll_interval
            
            if job.status == QuestioningJobStatus.QUEUED and waited > max_wait_for_start:
                yield sse_event(
                    "error",
                    {
                        "message": "Questioning job timed out waiting to start.",
                        "recoverable": True,
                    },
                )
                break


@method_decorator(csrf_exempt, name='dispatch')
class QuestioningAnswerView(View):
    """
    Handle user answers during the questioning phase.
    
    POST /api/v1/questioning/:job_id/answer/
    Body: { "message": "user's answer" }
    
    Queues the next questioning phase task for the job.
    """
    
    def post(self, request, job_id):
        """Queue the user's answer for processing."""
        try:
            body = json.loads(request.body) if request.body else {}
        except json.JSONDecodeError:
            return JsonResponse({"error": "Invalid JSON body"}, status=400)
        
        # Accept both 'answer' (frontend) and 'message' (legacy) keys
        message = body.get('answer') or body.get('message', '')
        if not message:
            return JsonResponse({"error": "Answer is required"}, status=400)
        
        user = _authenticate_request(request)
        if isinstance(user, JsonResponse):
            return user
        
        try:
            job = QuestioningJob.objects.select_related(
                "questioning_session__chat_session__internal_app__organization"
            ).get(pk=job_id)
        except QuestioningJob.DoesNotExist:
            return JsonResponse({"error": "Job not found"}, status=404)
        
        questioning_session = job.questioning_session
        session = questioning_session.chat_session
        app = session.internal_app
        
        try:
            membership = UserOrganization.objects.get(user=user, organization=app.organization)
            if not membership.is_editor_or_above():
                return JsonResponse(
                    {"error": "You must be an editor or admin to answer questions"},
                    status=403,
                )
        except UserOrganization.DoesNotExist:
            return JsonResponse({"error": "Access denied"}, status=403)
        
        if questioning_session.status in [QuestioningStatus.COMPLETE, QuestioningStatus.SKIPPED]:
            return JsonResponse(
                {"error": "Questioning phase is already complete", "status": questioning_session.status},
                status=400,
            )
        
        if job.status == QuestioningJobStatus.PROCESSING:
            return JsonResponse({"error": "Questioning job is still processing"}, status=409)

        if job.status == QuestioningJobStatus.FAILED:
            return JsonResponse({"error": "Questioning job failed"}, status=400)
        
        if job.status in [QuestioningJobStatus.COMPLETE, QuestioningJobStatus.SKIPPED]:
            return JsonResponse(
                {"error": "Questioning job is already complete", "status": job.status},
                status=400,
            )

        if job.status != QuestioningJobStatus.WAITING_FOR_ANSWER:
            return JsonResponse(
                {"error": "No question is awaiting an answer", "status": job.status},
                status=400,
            )
        
        job.user_answer = message
        job.current_question = ""
        job.status = QuestioningJobStatus.QUEUED
        job.save(update_fields=["user_answer", "current_question", "status", "updated_at"])
        
        run_questioning_phase.delay(str(job.id))
        
        return JsonResponse(
            {
                "status": "queued",
                "job_id": str(job.id),
                "questioning_session_id": str(questioning_session.id),
                "stream_url": f"/api/v1/questioning/{job.id}/stream/",
            },
            status=202,
        )


@method_decorator(csrf_exempt, name='dispatch')
class QuestioningSkipView(View):
    """
    Skip remaining questions for a job.
    
    POST /api/v1/questioning/:job_id/skip/
    """
    
    def post(self, request, job_id):
        user = _authenticate_request(request)
        if isinstance(user, JsonResponse):
            return user
        
        try:
            job = QuestioningJob.objects.select_related(
                "questioning_session__chat_session__internal_app__organization"
            ).get(pk=job_id)
        except QuestioningJob.DoesNotExist:
            return JsonResponse({"error": "Job not found"}, status=404)
        
        questioning_session = job.questioning_session
        app = questioning_session.chat_session.internal_app
        
        try:
            membership = UserOrganization.objects.get(user=user, organization=app.organization)
            if not membership.is_editor_or_above():
                return JsonResponse(
                    {"error": "You must be an editor or admin to skip questions"},
                    status=403,
                )
        except UserOrganization.DoesNotExist:
            return JsonResponse({"error": "Access denied"}, status=403)
        
        if questioning_session.status in [QuestioningStatus.COMPLETE, QuestioningStatus.SKIPPED]:
            return JsonResponse(
                {"error": "Questioning phase is already complete", "status": questioning_session.status},
                status=400,
            )
        
        if job.status == QuestioningJobStatus.PROCESSING:
            return JsonResponse({"error": "Questioning job is still processing"}, status=409)
        
        if job.status == QuestioningJobStatus.FAILED:
            return JsonResponse({"error": "Questioning job failed"}, status=400)

        if job.status in [QuestioningJobStatus.COMPLETE, QuestioningJobStatus.SKIPPED]:
            return JsonResponse(
                {"error": "Questioning job is already complete", "status": job.status},
                status=400,
            )
        
        job.user_answer = "skip"
        job.current_question = ""
        job.status = QuestioningJobStatus.QUEUED
        job.save(update_fields=["user_answer", "current_question", "status", "updated_at"])
        
        run_questioning_phase.delay(str(job.id))
        
        return JsonResponse(
            {
                "status": "queued",
                "job_id": str(job.id),
                "questioning_session_id": str(questioning_session.id),
                "stream_url": f"/api/v1/questioning/{job.id}/stream/",
            },
            status=202,
        )


@method_decorator(csrf_exempt, name='dispatch')
class QuestioningStatusView(View):
    """
    Get current status for a questioning job.
    
    GET /api/v1/questioning/:job_id/status/
    """
    
    def get(self, request, job_id):
        user = _authenticate_request(request)
        if isinstance(user, JsonResponse):
            return user
        
        try:
            job = QuestioningJob.objects.select_related(
                "questioning_session__chat_session__internal_app__organization"
            ).get(pk=job_id)
        except QuestioningJob.DoesNotExist:
            return JsonResponse({"error": "Job not found"}, status=404)
        
        questioning_session = job.questioning_session
        session = questioning_session.chat_session
        app = session.internal_app
        
        if not user.user_organizations.filter(organization=app.organization).exists():
            return JsonResponse({"error": "Access denied"}, status=403)
        
        return JsonResponse(
            {
                "job_id": str(job.id),
                "status": job.status,
                "current_question": job.current_question,
                "events_count": len(job.events_json),
                "questioning_session_id": str(questioning_session.id),
                "session_id": str(session.id),
                "questioning_status": questioning_session.status,
                "question_count": questioning_session.question_count,
                "synthesized_requirements": questioning_session.synthesized_requirements,
            }
        )


@method_decorator(csrf_exempt, name='dispatch')
class QuestioningStateView(View):
    """
    Get the current questioning state for an app.
    
    GET /api/v1/apps/:app_id/questioning/state/
    
    Returns the latest questioning session state including:
    - status (idle/questioning/complete)
    - messages (Q&A chat history)
    - facts (extracted requirements)
    - job_id (if in progress)
    
    This enables the frontend to restore state when navigating back
    to the requirements tab.
    """
    
    def get(self, request, app_id):
        user = _authenticate_request(request)
        if isinstance(user, JsonResponse):
            return user
        
        try:
            app = InternalApp.objects.get(pk=app_id)
        except InternalApp.DoesNotExist:
            return JsonResponse({"error": "App not found"}, status=404)
        
        if not user.user_organizations.filter(organization=app.organization).exists():
            return JsonResponse({"error": "Access denied"}, status=403)
        
        # Find the latest questioning session for this app
        questioning_session = QuestioningSession.objects.filter(
            chat_session__internal_app=app
        ).select_related("chat_session").order_by("-created_at").first()
        
        if not questioning_session:
            # No questioning session exists - return idle state
            return JsonResponse({
                "status": "idle",
                "messages": [],
                "facts": None,
                "job_id": None,
                "question_count": 0,
            })
        
        # Get the chat messages for this session
        messages = ChatMessage.objects.filter(
            session=questioning_session.chat_session
        ).order_by("created_at").values("id", "role", "content", "created_at")
        
        # Convert to list and format timestamps
        messages_list = [
            {
                "id": str(msg["id"]),
                "role": msg["role"],
                "content": msg["content"],
                "timestamp": msg["created_at"].isoformat(),
            }
            for msg in messages
        ]
        
        # Get the latest job for this session (if any)
        latest_job = QuestioningJob.objects.filter(
            questioning_session=questioning_session
        ).order_by("-created_at").first()
        
        # Map questioning session status to frontend status
        # Backend: IN_PROGRESS, COMPLETE, SKIPPED
        # Frontend: idle, questioning, complete
        status_map = {
            QuestioningStatus.IN_PROGRESS: "questioning",
            QuestioningStatus.COMPLETE: "complete",
            QuestioningStatus.SKIPPED: "complete",  # Skipped = complete from frontend perspective
        }
        # Convert stored status string to enum for lookup
        session_status = safe_str_enum(
            questioning_session.status,
            QuestioningStatus.IN_PROGRESS,
            QuestioningStatus,
        )
        frontend_status = status_map.get(session_status, "idle")
        
        # Extract facts from synthesized_requirements
        facts = None
        if questioning_session.synthesized_requirements:
            facts = questioning_session.synthesized_requirements.get("facts")
        
        return JsonResponse({
            "status": frontend_status,
            "messages": messages_list,
            "facts": facts,
            "job_id": str(latest_job.id) if latest_job else None,
            "job_status": latest_job.status if latest_job else None,
            "current_question": latest_job.current_question if latest_job else None,
            "question_count": questioning_session.question_count,
            "initial_request": questioning_session.initial_request,
            "session_id": str(questioning_session.chat_session.id),
            "questioning_session_id": str(questioning_session.id),
        })


@method_decorator(csrf_exempt, name='dispatch')
class JobStreamView(View):
    """
    SSE endpoint to stream events from a CodeGenerationJob.
    
    GET /api/v1/jobs/:job_id/stream/?last_index=0
    
    Streams events from the job's events_json, enabling:
    - Real-time progress updates during generation
    - Reconnection support (replay all events then continue)
    - Browser switch/refresh without losing progress
    """
    
    def get(self, request, job_id):
        """Stream events from a generation job."""
        last_index = int(request.GET.get('last_index', 0))
        
        # Verify authentication (supports header and query param token for SSE)
        user = _authenticate_request(request)
        if isinstance(user, JsonResponse):
            return user
        
        try:
            job = CodeGenerationJob.objects.select_related(
                'internal_app__organization'
            ).get(pk=job_id)
        except CodeGenerationJob.DoesNotExist:
            return JsonResponse({"error": "Job not found"}, status=404)
        
        # Verify access
        if not user.user_organizations.filter(organization=job.internal_app.organization).exists():
            return JsonResponse({"error": "Access denied"}, status=403)
        
        response = StreamingHttpResponse(
            self._stream_events(job, last_index),
            content_type='text/event-stream'
        )
        response['Cache-Control'] = 'no-cache'
        response['X-Accel-Buffering'] = 'no'
        return response
    
    def _stream_events(self, job: CodeGenerationJob, last_index: int) -> Generator[str, None, None]:
        """Stream events from the job's events_json."""
        poll_interval = 0.3  # 300ms
        max_wait_for_start = 30  # Wait up to 30s for job to start
        waited = 0
        
        while True:
            job.refresh_from_db()
            events = job.events_json
            
            # Stream any new events
            if len(events) > last_index:
                for event in events[last_index:]:
                    yield sse_event(event['type'], event['data'])
                last_index = len(events)
                waited = 0  # Reset wait counter when we get events
            
            # Check if job is done
            if job.status in [
                CodeGenerationJobStatus.COMPLETE,
                CodeGenerationJobStatus.FAILED,
                CodeGenerationJobStatus.CANCELLED,
            ]:
                # Emit final done event with job status
                yield sse_event("done", {
                    "success": job.status == CodeGenerationJobStatus.COMPLETE,
                    "status": job.status,
                    "version_id": str(job.version_id) if job.version_id else None,
                })
                break
            
            # Wait for more events
            time.sleep(poll_interval)
            waited += poll_interval
            
            # Timeout if job never starts
            if job.status == CodeGenerationJobStatus.QUEUED and waited > max_wait_for_start:
                yield sse_event("agent_error", {
                    "message": "Job timed out waiting to start. The background worker may need to be restarted.",
                    "phase": "error",
                    "recoverable": True,
                    "details": {
                        "timeout_seconds": max_wait_for_start,
                        "suggestion": "Try refreshing the page. If the issue persists, the worker may need to be restarted.",
                    },
                })
                # Also emit done event for timeout
                yield sse_event("done", {
                    "success": False,
                    "status": "timeout",
                })
                break


class JobStatusView(APIView):
    """
    Get the status of a generation job.
    
    GET /api/v1/jobs/:job_id/
    """
    permission_classes = [IsAuthenticated]
    
    def get(self, request, job_id):
        """Get job status and metadata."""
        try:
            job = CodeGenerationJob.objects.select_related(
                'internal_app__organization',
                'version',
            ).get(pk=job_id)
        except CodeGenerationJob.DoesNotExist:
            return Response({"error": "Job not found"}, status=http_status.HTTP_404_NOT_FOUND)
        
        # Verify access
        if not request.user.user_organizations.filter(
            organization=job.internal_app.organization
        ).exists():
            return Response({"error": "Access denied"}, status=http_status.HTTP_403_FORBIDDEN)
        
        return Response({
            "job_id": str(job.id),
            "app_id": str(job.internal_app_id),
            "status": job.status,
            "version_id": str(job.version_id) if job.version_id else None,
            "event_count": len(job.events_json),
            "created_at": job.created_at.isoformat(),
            "started_at": job.started_at.isoformat() if job.started_at else None,
            "completed_at": job.completed_at.isoformat() if job.completed_at else None,
            "error_message": job.error_message,
            "is_active": job.status in [
                CodeGenerationJobStatus.QUEUED,
                CodeGenerationJobStatus.PROCESSING,
                CodeGenerationJobStatus.STREAMING,
            ],
        })


class LatestJobView(APIView):
    """
    Get the latest active or recent job for an app.
    
    GET /api/v1/apps/:app_id/latest-job/
    
    Returns the most recent job, prioritizing active jobs.
    Used for reconnection after page refresh.
    """
    permission_classes = [IsAuthenticated]
    
    def get(self, request, app_id):
        """Get the latest job for an app."""
        try:
            app = InternalApp.objects.get(pk=app_id)
        except InternalApp.DoesNotExist:
            return Response({"error": "App not found"}, status=http_status.HTTP_404_NOT_FOUND)
        
        # Verify access
        if not request.user.user_organizations.filter(organization=app.organization).exists():
            return Response({"error": "Access denied"}, status=http_status.HTTP_403_FORBIDDEN)
        
        # First look for active jobs
        active_job = CodeGenerationJob.objects.filter(
                    internal_app=app,
            status__in=[
                CodeGenerationJobStatus.QUEUED,
                CodeGenerationJobStatus.PROCESSING,
                CodeGenerationJobStatus.STREAMING,
            ]
        ).order_by('-created_at').first()
        
        if active_job:
            return Response({
                "has_active_job": True,
                "job_id": str(active_job.id),
                "status": active_job.status,
                "version_id": str(active_job.version_id) if active_job.version_id else None,
                "event_count": len(active_job.events_json),
                "created_at": active_job.created_at.isoformat(),
            })
        
        # No active job - return info about the latest completed job
        latest_job = CodeGenerationJob.objects.filter(
            internal_app=app
        ).order_by('-created_at').first()
        
        if latest_job:
            return Response({
                "has_active_job": False,
                "job_id": str(latest_job.id),
                "status": latest_job.status,
                "version_id": str(latest_job.version_id) if latest_job.version_id else None,
                "completed_at": latest_job.completed_at.isoformat() if latest_job.completed_at else None,
            })
        
        return Response({
            "has_active_job": False,
            "job_id": None,
        })


class JobCancelView(APIView):
    """
    Cancel a running generation job.
    
    POST /api/v1/jobs/:job_id/cancel/
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request, job_id):
        """Cancel a generation job."""
        try:
            job = CodeGenerationJob.objects.select_related(
                'internal_app__organization'
            ).get(pk=job_id)
        except CodeGenerationJob.DoesNotExist:
            return Response({"error": "Job not found"}, status=http_status.HTTP_404_NOT_FOUND)
        
        # Verify access
        if not request.user.user_organizations.filter(
            organization=job.internal_app.organization
        ).exists():
            return Response({"error": "Access denied"}, status=http_status.HTTP_403_FORBIDDEN)
        
        # Only cancel if still running
        if job.status in [CodeGenerationJobStatus.QUEUED, CodeGenerationJobStatus.PROCESSING, CodeGenerationJobStatus.STREAMING]:
            job.status = CodeGenerationJobStatus.CANCELLED
            job.save(update_fields=['status', 'updated_at'])
            
            # Cancel the version if it exists
            if job.version:
                job.version.generation_status = AppVersionGenerationStatus.ERROR
                job.version.generation_error = "Cancelled by user"
                job.version.save(update_fields=['generation_status', 'generation_error', 'updated_at'])
            
            return Response({
                "status": "cancelled",
                "job_id": str(job.id),
            })
        
        return Response({
            "status": job.status,
            "message": "Job is not cancellable in its current state",
            "job_id": str(job.id),
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
                "is_complete": version.generation_status == AppVersionGenerationStatus.COMPLETE,
                "is_generating": version.generation_status == AppVersionGenerationStatus.GENERATING,
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
    Apply generated code to create a new version (editor+ only).
    
    POST /api/v1/messages/:message_id/apply
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request, message_id):
        """Apply code from a chat message to create a version (editor+ only)."""
        try:
            message = ChatMessage.objects.select_related(
                'session__internal_app__organization'
            ).get(pk=message_id)
            
            app = message.session.internal_app
            
            # Verify user is editor or above
            membership, error = require_editor_or_above(request, app.organization)
            if error:
                return error
            
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
                source=AppVersionSource.AI,
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
    Cancel an in-progress code generation (editor+ only).
    
    POST /api/v1/versions/:version_id/cancel
    
    This endpoint is called when the user aborts a generation request.
    It cleans up the generating version:
    - If no files were generated, the version is deleted
    - If partial files exist, the version is marked as 'error' 
    
    The frontend can then show the latest stable version instead.
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request, version_id):
        """Cancel a generating version (editor+ only)."""
        try:
            version = AppVersion.objects.select_related(
                'internal_app__organization'
            ).get(pk=version_id)
            
            # Verify user is editor or above
            membership, error = require_editor_or_above(request, version.internal_app.organization)
            if error:
                return error
            
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


@method_decorator(csrf_exempt, name='dispatch')
class FixErrorsView(View):
    """
    SSE endpoint for fixing bundler errors detected by the frontend.
    
    GET /api/v1/versions/:version_id/fix-errors?errors=base64_json&model=...
    
    This is a fallback for errors that the backend TypeScript validation
    didn't catch. Returns SSE stream with Live Activity events showing
    fix progress.
    
    Maximum 2 fix attempts. Agent is constrained to ONLY fix errors,
    not change core functionality.
    """
    MAX_FIX_ATTEMPTS = 2
    
    def get(self, request, version_id):
        """Start error fixing stream."""
        import base64
        
        # Get parameters
        errors_b64 = request.GET.get('errors', '')
        model = _parse_model(request.GET.get('model'))
        attempt = int(request.GET.get('attempt', '1'))
        
        if not errors_b64:
            return JsonResponse(
                {"error": "No errors provided"},
                status=400
            )
        
        # Verify authentication (supports header and query param token for SSE)
        user = _authenticate_request(request)
        if isinstance(user, JsonResponse):
            return user
        
        # Get version and verify access
        try:
            version = AppVersion.objects.select_related(
                'internal_app__organization'
            ).prefetch_related('files').get(pk=version_id)
        except AppVersion.DoesNotExist:
            return JsonResponse({"error": "Version not found"}, status=404)
        
        # Verify user is editor or above (fixing errors requires edit permissions)
        try:
            membership = UserOrganization.objects.get(user=user, organization=version.internal_app.organization)
            if not membership.is_editor_or_above():
                return JsonResponse(
                    {"error": "You must be an editor or admin to fix errors"},
                    status=403
                )
        except UserOrganization.DoesNotExist:
            return JsonResponse({"error": "Access denied"}, status=403)
        
        # Decode errors
        try:
            errors_json = base64.b64decode(errors_b64).decode('utf-8')
            errors = json.loads(errors_json)
        except Exception as e:
            logger.error(f"Failed to decode errors: {e}")
            return JsonResponse({"error": "Invalid errors format"}, status=400)
        
        # Check attempt limit
        if attempt > self.MAX_FIX_ATTEMPTS:
            return JsonResponse({
                "error": f"Maximum fix attempts ({self.MAX_FIX_ATTEMPTS}) exceeded",
                "max_attempts": self.MAX_FIX_ATTEMPTS,
            }, status=400)
        
        # Create streaming response
        response = StreamingHttpResponse(
            self._fix_errors_stream(
                version=version,
                errors=errors,
                model=model,
                attempt=attempt,
                user=user,
            ),
            content_type='text/event-stream'
        )
        response['Cache-Control'] = 'no-cache'
        response['X-Accel-Buffering'] = 'no'
        return response
    
    def _fix_errors_stream(
        self,
        version: AppVersion,
        errors: list,
        model: AIModel,
        attempt: int,
        user,
    ) -> Generator[str, None, None]:
        """Generate SSE stream for error fixing."""
        from ..services.error_fix_service import get_error_fix_service
        from ..services.agentic_service import FileChange
        
        # Send initial connection event
        yield sse_event("connected", {
            "version_id": str(version.id),
            "error_count": len(errors),
            "attempt": attempt,
            "max_attempts": self.MAX_FIX_ATTEMPTS,
        })
        
        # Get current files from version
        files = [
            FileChange(
                path=f.path,
                action='modify',
                language=f.path.split('.')[-1] if '.' in f.path else 'tsx',
                content=f.content,
                previous_content=f.content,
            )
            for f in version.files.all()
        ]
        
        try:
            fix_service = get_error_fix_service()
            
            # Fix bundler errors
            fix_gen = fix_service.fix_bundler_errors(
                files=files,
                bundler_errors=errors,
                model=model,
                attempt=attempt,
            )
            
            fixed_files_by_path = {}
            
            # Consume events and collect fixed files
            while True:
                try:
                    event = next(fix_gen)
                    yield event.to_sse()
                    
                    # Capture fixed files
                    if event.type == "file_generated":
                        file_data = event.data.get("file", {})
                        if file_data.get("path"):
                            fixed_files_by_path[file_data["path"]] = {
                                "path": file_data["path"],
                                "content": file_data.get("content", ""),
                            }
                except StopIteration:
                    break
            
            # Save fixed files to version
            if fixed_files_by_path:
                for path, file_data in fixed_files_by_path.items():
                    VersionFile.objects.update_or_create(
                        app_version=version,
                        path=path,
                        defaults={"content": file_data["content"]}
                    )
                
                logger.info(f"Saved {len(fixed_files_by_path)} fixed files to version {version.id}")
                
                # Update version validation status
                version.validation_status = 'pending'  # Needs re-validation
                version.save(update_fields=['validation_status', 'updated_at'])
                
                yield sse_event("fix_complete", {
                    "success": True,
                    "fix_attempts": attempt,
                    "files_fixed": list(fixed_files_by_path.keys()),
                })
            else:
                yield sse_event("fix_failed", {
                    "remaining_errors": len(errors),
                    "fix_attempts": attempt,
                    "message": "No fixes could be applied",
                })
            
            yield sse_event("done", {"success": True})
            
        except Exception as e:
            logger.error(f"Error fix stream error: {e}")
            yield sse_event("agent_error", {
                "message": str(e),
                "phase": "error",
                "recoverable": True,
            })


class GenerateAppTitleView(APIView):
    """
    Generate a short title and description from a prompt using GPT mini.
    This is a quick, lightweight call for naming apps.
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request):
        """
        Generate app title and description from prompt.
        
        Request body:
            prompt: str - The user's app description/prompt
            
        Response:
            title: str - Short title (max 40 chars)
            description: str - Short description (max 60 chars)
        """
        prompt = request.data.get('prompt', '').strip()
        
        if not prompt:
            return Response(
                {"error": "prompt is required"},
                status=http_status.HTTP_400_BAD_REQUEST
            )
        
        try:
            client = get_llm_client()
            if not client.api_key:
                # Fallback: use prompt as title
                return Response({
                    "title": prompt[:40],
                    "description": "",
                    "fallback": True,
                })

            # Use GPT-4o-mini for fast, cheap title generation
            system_prompt = """You are a helpful assistant that generates short, catchy app titles and descriptions.
Given a user's prompt describing what they want to build, generate:
1. A short title (max 40 characters) - concise, descriptive app name
2. A short description (max 60 characters) - brief summary of the app

Respond in JSON format: {"title": "...", "description": "..."}

Examples:
- Prompt: "Build a dashboard to manage user subscriptions" -> {"title": "Subscription Manager", "description": "Track and manage user subscriptions"}
- Prompt: "Create an order tracking system with refunds" -> {"title": "Order Tracker", "description": "Track orders and process refunds"}
"""

            result = client.run(
                system_prompt=system_prompt,
                user_prompt=f"Prompt: {prompt}",
                llm_settings=LLMSettings(
                    model="openai/gpt-4o-mini",
                    temperature=0.3,
                    max_tokens=100,
                    timeout=15.0,
                ),
                json_mode=True,
            )
            parsed = result.validated(json.loads, default={})

            title = parsed.get("title", prompt[:40])[:40]
            description = parsed.get("description", "")[:60]

            return Response({
                "title": title,
                "description": description,
                "fallback": False,
            })
                
        except Exception as e:
            logger.warning(f"Failed to generate app title via GPT: {e}")
            # Fallback: use prompt as title
            return Response({
                "title": prompt[:40] if len(prompt) <= 40 else prompt[:37] + "...",
                "description": "",
                "fallback": True,
            })

