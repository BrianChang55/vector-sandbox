"""
Celery Tasks for Vector Internal Apps

Background tasks for code generation that can survive connection drops.
Events are stored in the database for replay on reconnection.
"""
import logging
import time
from celery import shared_task
from django.utils import timezone
from vector_app.models import VersionAuditOperation
from vector_app.models import VersionAuditOperation



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
    from vector_app.models import (
        AppVersion,
        AppVersionGenerationStatus,
        AppVersionSource,
        AppVersionValidationStatus,
        ChatMessage,
        ChatMessageRole,
        ChatMessageSource,
        ChatMessageStatus,
        ChatSession,
        CodeGenerationJob,
        CodeGenerationJobStatus,
        VersionAuditLog,
        VersionFile,
    )
    from vector_app.services.agentic_service import get_agentic_service
    from vector_app.services.version_service import VersionService
    from vector_app.services.snapshot_service import SnapshotService
    
    try:
        job = CodeGenerationJob.objects.select_related(
            'internal_app',
            'session',
            'created_by',
        ).get(pk=job_id)
    except CodeGenerationJob.DoesNotExist:
        logger.error(f"Job {job_id} not found")
        return
    
    # Check if already cancelled before starting
    if job.status == CodeGenerationJobStatus.CANCELLED:
        logger.info(f"Job {job_id} was cancelled before starting")
        return
    
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
        source=ChatMessageSource.BUILD,
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
        source=ChatMessageSource.BUILD,
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


@shared_task(bind=True, max_retries=0, soft_time_limit=5 * 60, time_limit=10 * 60)
def run_questioning_phase(self, job_id: str):
    """
    Run a single turn of the questioning phase in the background.

    This task:
    1. Loads the QuestioningJob and related session
    2. Saves the user's message to the chat history
    3. Calls MainAgentService.process_user_message()
    4. Appends events to job.events_json for SSE replay
    """
    logger.info("Starting questioning phase task for job %s", job_id)
    
    from vector_app.models import (
        ChatMessage,
        ChatMessageRole,
        ChatMessageSource,
        ChatMessageStatus,
        QuestioningJob,
        QuestioningJobStatus,
        QuestioningStatus,
    )
    from vector_app.services.main_agent_service import get_main_agent_service

    try:
        job = QuestioningJob.objects.select_related(
            "questioning_session__chat_session__internal_app",
            "created_by",
        ).get(pk=job_id)
    except QuestioningJob.DoesNotExist:
        logger.error("Questioning job %s not found", job_id)
        return

    if job.status in [
        QuestioningJobStatus.COMPLETE,
        QuestioningJobStatus.SKIPPED,
        QuestioningJobStatus.FAILED,
    ]:
        return

    user_message = (job.user_answer or "").strip()
    if not user_message:
        job.status = QuestioningJobStatus.FAILED
        job.save(update_fields=["status", "updated_at"])
        job.append_event("error", {"message": "No user message provided"})
        return

    # Mark as processing
    job.status = QuestioningJobStatus.PROCESSING
    job.save(update_fields=["status", "updated_at"])

    questioning_session = job.questioning_session
    chat_session = questioning_session.chat_session

    try:
        # Save the user's message to the chat session
        ChatMessage.objects.create(
            session=chat_session,
            role=ChatMessageRole.USER,
            content=user_message,
            status=ChatMessageStatus.COMPLETE,
            source=ChatMessageSource.QUESTIONING,
        )

        # Build chat history for decision-making
        chat_history = [
            {"role": msg.role, "content": msg.content}
            for msg in chat_session.messages.order_by("created_at")
        ]

        logger.info("Calling MainAgentService for job %s with %d messages in history", job_id, len(chat_history))
        
        agent_service = get_main_agent_service()
        decision = agent_service.process_user_message(
            session=questioning_session,
            user_message=user_message,
            chat_history=chat_history,
        )
        
        logger.info("Agent decision for job %s: action=%s", job_id, decision.action)

        if decision.action == "ask_question":
            if not decision.question:
                raise ValueError("Agent returned ask_question without a question")

            # Store the question as an assistant message
            ChatMessage.objects.create(
                session=chat_session,
                role=ChatMessageRole.ASSISTANT,
                content=decision.question,
                status=ChatMessageStatus.COMPLETE,
                source=ChatMessageSource.QUESTIONING,
            )

            # Increment question count
            questioning_session.question_count += 1
            questioning_session.save(update_fields=["question_count", "updated_at"])

            job.current_question = decision.question
            job.status = QuestioningJobStatus.WAITING_FOR_ANSWER
            job.user_answer = ""
            job.save(update_fields=["current_question", "status", "user_answer", "updated_at"])

            logger.info("Job %s: Generated question #%d", job_id, questioning_session.question_count)
            job.append_event(
                "question",
                {
                    "question": decision.question,
                    "question_count": questioning_session.question_count,
                    "questioning_session_id": str(questioning_session.id),
                    "session_id": str(chat_session.id),
                },
            )
            return

        # Action is "proceed" or "skip" - mark questioning complete
        questioning_session.status = (
            QuestioningStatus.COMPLETE
            if decision.action == "proceed"
            else QuestioningStatus.SKIPPED
        )
        if decision.extraction:
            questioning_session.synthesized_requirements = {
                "facts": decision.extraction.facts,
                "reasoning": decision.extraction.reasoning,
                "question_count": decision.extraction.question_count,
            }
        questioning_session.save(update_fields=["status", "synthesized_requirements", "updated_at"])

        job.status = (
            QuestioningJobStatus.COMPLETE
            if decision.action == "proceed"
            else QuestioningJobStatus.SKIPPED
        )
        job.current_question = ""
        job.user_answer = ""
        job.save(update_fields=["current_question", "status", "user_answer", "updated_at"])

        event_type = "extraction_complete" if decision.action == "proceed" else "skip"
        logger.info("Job %s: Questioning complete with action=%s, emitting %s event", job_id, decision.action, event_type)
        job.append_event(
            event_type,
            {
                "questioning_session_id": str(questioning_session.id),
                "status": questioning_session.status,
                "synthesized_requirements": questioning_session.synthesized_requirements,
            },
        )

    except Exception as e:
        logger.error("Questioning job %s failed: %s", job_id, e, exc_info=True)
        job.status = QuestioningJobStatus.FAILED
        job.save(update_fields=["status", "updated_at"])
        job.append_event("error", {"message": str(e)})


def _append_event(job, event_type: str, data: dict):
    """
    Append an event to the job's events_json.
    
    Uses a separate function to ensure atomic DB updates.
    """
    from vector_app.models import CodeGenerationJob
    
    event = {
        'type': event_type,
        'data': data,
        'timestamp': time.time(),
        'index': len(job.events_json),
    }
    job.events_json.append(event)
    job.chunk_count = len(job.events_json)
    job.save(update_fields=['events_json', 'chunk_count', 'updated_at'])

