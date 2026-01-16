"""
Celery Tasks for Vector Internal Apps

Background tasks for code generation that can survive connection drops.
Events are stored in the database for replay on reconnection.
"""
import logging
import time
from celery import shared_task
from django.utils import timezone

from chat.models import ChatMessage, QuestioningSession
from chat.types import ChatMessageRole, ChatMessageStatus, QuestioningStatus
from vector_app.models import (
    AppVersion,
    AppVersionGenerationStatus,
    AppVersionSource,
    AppVersionValidationStatus,
    ChatSession,
    CodeGenerationJob,
    CodeGenerationJobStatus,
    VersionAuditLog,
    VersionAuditOperation,
    VersionFile,
)
from vector_app.services.agentic_service import get_agentic_service
from vector_app.services.main_agent_service import get_main_agent_service
from vector_app.services.snapshot_service import SnapshotService
from vector_app.services.version_service import VersionService
from vector_app.utils.job_utils import append_job_event


# Alias for backward compatibility within this module
_append_event = append_job_event


logger = logging.getLogger(__name__)


@shared_task(bind=True, max_retries=0, soft_time_limit=25 * 60, time_limit=30 * 60)
def run_agentic_generation(self, job_id: str):
    """
    Run agentic code generation in the background.
    
    This task:
    1. Loads the job and creates necessary objects (message, version)
    2. Runs agentic_service.generate_app()
    3. Appends each event to job.events_json for client streaming
    4. Updates job status on completion/error
    
    The client connects to JobStreamView SSE endpoint to receive events
    from the database, enabling reconnection without losing progress.
    """
    try:
        job = CodeGenerationJob.objects.select_related(
            'internal_app',
            'session',
            'created_by',
            'questioning_session',
        ).get(pk=job_id)
    except CodeGenerationJob.DoesNotExist:
        logger.error(f"Job {job_id} not found")
        return

    # Check if already cancelled before starting
    if job.status == CodeGenerationJobStatus.CANCELLED:
        logger.info(f"Job {job_id} was cancelled before starting")
        return

    # Get extracted facts from questioning phase (if any)
    extracted_facts = None
    if job.questioning_session and job.questioning_session.synthesized_requirements:
        extracted_facts = job.questioning_session.synthesized_requirements
        logger.info(f"Using extracted facts from questioning: {list(extracted_facts.keys())}")

    # Mark as processing
    job.status = CodeGenerationJobStatus.PROCESSING
    job.started_at = timezone.now()
    job.save(update_fields=['status', 'started_at', 'updated_at'])
    
    app = job.internal_app
    user = job.created_by
    message = job.user_message
    model = job.model_id
    session = job.session
    
    # Get or create session if not provided
    if not session:
        session = ChatSession.objects.create(
            internal_app=app,
            title=message[:50] + "..." if len(message) > 50 else message,
            model_id=model,
            created_by=user,
        )
        job.session = session
        job.save(update_fields=['session', 'updated_at'])
        _append_event(job, "session_created", {"session_id": str(session.id)})
    
    # Create user message (so it persists and shows on refresh)
    user_chat_message = ChatMessage.objects.create(
        session=session,
        role=ChatMessageRole.USER,
        content=message,
        status=ChatMessageStatus.COMPLETE,
    )
    
    # Update session title if needed
    if not session.title or session.title.strip() == "" or session.title == "New Chat":
        session.title = message[:50] + "..." if len(message) > 50 else message
        session.save(update_fields=["title", "updated_at"])
    
    _append_event(job, "user_message", {"id": str(user_chat_message.id), "content": message})
    
    # Create assistant message
    assistant_message = ChatMessage.objects.create(
        session=session,
        role=ChatMessageRole.ASSISTANT,
        content="",
        status=ChatMessageStatus.STREAMING,
        model_id=model,
    )
    job.chat_message = assistant_message
    job.save(update_fields=['chat_message', 'updated_at'])
    
    # Get current spec and registry surface
    current_spec = None
    latest_stable_version = VersionService.get_latest_stable_version(app)
    
    if latest_stable_version:
        current_spec = latest_stable_version.spec_json
    
    # No external resource registry in the current backend
    registry_surface = {"resources": []}
    
    # Create draft version
    start_time = time.time()
    version = None
    current_step_index = 0
    
    try:
        next_version_number = VersionService.get_next_version_number(app)
        draft_spec = current_spec or {"generated": True, "agentic": True}
        
        version = AppVersion.objects.create(
            internal_app=app,
            version_number=next_version_number,
            source=AppVersionSource.AI,
            spec_json=draft_spec,
            created_by=user,
            generation_status=AppVersionGenerationStatus.GENERATING,
        )
        
        # Link version to job
        job.version = version
        job.status = CodeGenerationJobStatus.STREAMING
        job.save(update_fields=['version', 'status', 'updated_at'])
        
        # Copy existing files from latest stable version
        if latest_stable_version:
            copied_count = 0
            for existing_file in latest_stable_version.files.all():
                VersionFile.objects.create(
                    app_version=version,
                    path=existing_file.path,
                    content=existing_file.content or "",
                )
                copied_count += 1
            if copied_count > 0:
                logger.info(f"Copied {copied_count} files from version {latest_stable_version.version_number}")
        
        # Link to message
        assistant_message.version_created = version
        assistant_message.save(update_fields=['version_created', 'updated_at'])
        
        # Notify client of draft version
        _append_event(job, "version_draft", {
            "version_id": str(version.id),
            "version_number": version.version_number,
            "status": "generating",
        })
        
        # Run agentic generation
        agentic_service = get_agentic_service()

        for event in agentic_service.generate_app(
            user_message=message,
            current_spec=current_spec,
            registry_surface=registry_surface,
            app_name=app.name,
            model=model,
            app=app,
            version=version,
            session=session,
            extracted_facts=extracted_facts,
        ):
            # Debug log: intercept all Agent Events
            logger.debug(
                f"[AGENT_EVENT] Job {job_id} - Event type: {event.type}, "
                f"Data: {event.data}"
            )
            
            # Check for cancellation periodically
            job.refresh_from_db(fields=['status'])
            if job.status == CodeGenerationJobStatus.CANCELLED:
                logger.info(f"Job {job_id} cancelled during generation")
                version.generation_status = AppVersionGenerationStatus.ERROR
                version.generation_error = "Generation cancelled by user"
                version.save(update_fields=['generation_status', 'generation_error', 'updated_at'])
                _append_event(job, "agent_error", {
                    "message": "Generation cancelled",
                    "phase": "cancelled",
                    "recoverable": False,
                })
                return
            
            # Append event to job for client streaming
            _append_event(job, event.type, event.data)
            
            # Save plan to version when created
            if event.type == "plan_created":
                plan_data = event.data.get("plan", {})
                version.generation_plan_json = plan_data
                version.save(update_fields=['generation_plan_json', 'updated_at'])
            
            # Track step progress
            if event.type == "step_start":
                current_step_index = event.data.get("step_index", 0)
                version.generation_current_step = current_step_index
                version.save(update_fields=['generation_current_step', 'updated_at'])
            
            # Incrementally save files
            if event.type == "file_generated":
                file_data = event.data.get("file", {})
                path = file_data.get("path", "src/App.tsx")
                content = file_data.get("content", "")
                
                VersionFile.objects.update_or_create(
                    app_version=version,
                    path=path,
                    defaults={"content": content}
                )
        
        # Generation complete
        duration_ms = int((time.time() - start_time) * 1000)
        
        version.generation_status = AppVersionGenerationStatus.COMPLETE
        version.generation_current_step = current_step_index + 1
        version.is_active = True
        version.validation_status = AppVersionValidationStatus.PASSED
        version.save(update_fields=[
            'generation_status', 'generation_current_step', 'is_active',
            'validation_status', 'updated_at'
        ])
        
        # Update assistant message
        assistant_message.status = ChatMessageStatus.COMPLETE
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
        assistant_message.generated_files = {"files": final_files_payload}
        assistant_message.save()
        
        # Create snapshot
        try:
            snapshot = SnapshotService.create_version_snapshot(version)
            logger.info(f"Created snapshot for version {version.id}: {snapshot.total_tables} tables")
            
            VersionAuditLog.log_operation(
                internal_app=app,
                app_version=version,
                operation=VersionAuditOperation.CREATE,
                user=user,
                details={
                    'source': 'agentic_generation_background',
                    'files_generated': len(final_files_payload),
                    'tables_snapshot': snapshot.total_tables,
                    'duration_ms': duration_ms,
                    'job_id': str(job.id),
                },
            )
        except Exception as snapshot_error:
            logger.warning(f"Failed to create snapshot: {snapshot_error}")
        
        # Send completion events
        _append_event(job, "preview_ready", {
            "version_id": str(version.id),
            "version_number": version.version_number,
            "preview_url": f"/preview/apps/{app.id}?version={version.id}",
            "files": final_files_payload,
        })
        
        _append_event(job, "version_created", {
            "version_id": str(version.id),
            "version_number": version.version_number,
        })
        
        _append_event(job, "snapshot_created", {
            "version_id": str(version.id),
            "version_number": version.version_number,
        })
        
        _append_event(job, "done", {"success": True})
        
        # Mark job as complete
        job.status = CodeGenerationJobStatus.COMPLETE
        job.completed_at = timezone.now()
        job.save(update_fields=['status', 'completed_at', 'updated_at'])
        
        logger.info(f"Job {job_id} completed successfully in {duration_ms}ms")
        
    except Exception as e:
        logger.error(f"Job {job_id} failed: {e}", exc_info=True)
        
        # Mark version as errored
        if version:
            version.generation_status = AppVersionGenerationStatus.ERROR
            version.generation_error = str(e)
            version.save(update_fields=['generation_status', 'generation_error', 'updated_at'])
        
        # Update assistant message
        assistant_message.status = ChatMessageStatus.ERROR
        assistant_message.error_message = str(e)
        assistant_message.save(update_fields=['status', 'error_message', 'updated_at'])
        
        # Append error event
        _append_event(job, "agent_error", {
            "message": str(e),
            "phase": "error",
            "recoverable": True,
            "version_id": str(version.id) if version else None,
        })
        
        # Mark job as failed
        job.status = CodeGenerationJobStatus.FAILED
        job.error_message = str(e)
        job.completed_at = timezone.now()
        job.save(update_fields=['status', 'error_message', 'completed_at', 'updated_at'])


@shared_task(bind=True, max_retries=0, soft_time_limit=5 * 60, time_limit=6 * 60)
def run_questioning_phase(self, job_id: str, user_response: str = None):
    """
    Run questioning phase - one turn at a time.

    This task:
    1. Loads the job and gets/creates QuestioningSession
    2. Processes the user response (or initial request if first turn)
    3. Either emits a question and ends, OR proceeds to generation

    The key insight: task ENDS after emitting question.
    User response triggers a NEW task invocation.
    All state lives in DB (survives worker restarts).
    """
    try:
        job = CodeGenerationJob.objects.select_related(
            'session', 'internal_app'
        ).get(pk=job_id)
    except CodeGenerationJob.DoesNotExist:
        logger.error(f"Job {job_id} not found")
        return

    # Check if already cancelled before starting
    if job.status == CodeGenerationJobStatus.CANCELLED:
        logger.info(f"Job {job_id} was cancelled before questioning started")
        return

    session = job.session

    # Get or create questioning session
    if not hasattr(session, 'questioning_session') or session.questioning_session is None:
        questioning_session = QuestioningSession.objects.create(
            chat_session=session,
            initial_request=job.user_message,
            status=QuestioningStatus.IN_PROGRESS,
        )
        # Link questioning session to job
        job.questioning_session = questioning_session
        job.save(update_fields=['questioning_session', 'updated_at'])
    else:
        questioning_session = session.questioning_session

    # Emit start event (only on first turn)
    if questioning_session.question_count == 0 and user_response is None:
        _append_event(job, "questioning_started", {
            "job_id": str(job.id),
            "session_id": str(questioning_session.id),
            "initial_request": job.user_message,
        })

    # Build chat history from session messages
    chat_history = [
        {"role": m.role, "content": m.content}
        for m in session.messages.order_by('created_at')
    ]

    # Get the message to process (user_response or initial_request)
    message_to_process = user_response or job.user_message

    # Call MainAgentService
    agent = get_main_agent_service()
    decision = agent.process_user_message(
        session=questioning_session,
        user_message=message_to_process,
        chat_history=chat_history,
    )

    # Handle decision
    if decision.action == "ask_question":
        # Emit question event
        _append_event(job, "question_asked", {
            "question": decision.question,
            "question_number": questioning_session.question_count + 1,
        })

        # Save question as assistant message
        ChatMessage.objects.create(
            session=session,
            role=ChatMessageRole.ASSISTANT,
            content=decision.question,
            status=ChatMessageStatus.COMPLETE,
        )

        # Increment question count
        questioning_session.question_count += 1
        questioning_session.save(update_fields=['question_count', 'updated_at'])

        # Task ends here - user response will trigger new task
        return {"status": "awaiting_response", "question": decision.question}

    elif decision.action in ("proceed", "skip"):
        # Store extracted facts
        questioning_session.synthesized_requirements = decision.extraction.facts
        questioning_session.status = (
            QuestioningStatus.SKIPPED if decision.action == "skip"
            else QuestioningStatus.COMPLETE
        )
        questioning_session.save()

        # Link questioning session to job (if not already)
        if not job.questioning_session:
            job.questioning_session = questioning_session
            job.save(update_fields=['questioning_session', 'updated_at'])

        # Emit completion event
        _append_event(job, "questioning_complete", {
            "facts": decision.extraction.facts,
            "question_count": decision.extraction.question_count,
            "was_skipped": decision.action == "skip",
        })

        # Transition to generation
        run_agentic_generation.delay(str(job.id))
        return {"status": "proceeding_to_generation"}
