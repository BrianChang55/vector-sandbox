# Architecture

## Overview

Django REST Framework backend with layered architecture separating HTTP handling, business logic, and data access.

## Architectural Pattern

**Layered Architecture with Service Layer**

```
Request → ViewSet → Serializer → Service → Model → Response
```

## Conceptual Layers

### Layer 1: HTTP Entry Points (`vector_app/views/`)

ViewSets handle HTTP concerns only - routing, authentication, permissions, response formatting.

| File | Responsibility |
|------|----------------|
| `organization_views.py` | Organization CRUD, logo management |
| `auth_views.py` | Login, signup, OAuth, magic link |
| `internal_app_views.py` | App lifecycle management |
| `version_views.py` | Version control, rollback, diffs |
| `streaming_views.py` | SSE streaming for code generation |
| `connector_views.py` | Third-party integrations |
| `member_views.py` | Organization membership |
| `app_data_views.py` | Data table CRUD |

### Layer 2: API Request/Response (`vector_app/serializers/`)

Serializers handle validation and data transformation.

| File | Responsibility |
|------|----------------|
| `auth_serializers.py` | User, authentication data |
| `internal_app.py` | App metadata |
| `version.py` | Version state, audit logs |
| `chat_serializers.py` | Chat sessions and messages |
| `connector_serializers.py` | Integration configs |
| `member_serializers.py` | Members and invites |
| `app_data.py` | Data tables, rows, queries |

### Layer 3: Business Logic (`vector_app/services/`)

Services contain all business logic - views and models are logic-free.

**Questioning & Decision Services (New):**
- `main_agent_service.py` - Central orchestrator for questioning flow decisions
- `questioning_service.py` - Pure distillation layer for fact extraction (no inference)

**Core Orchestration:**
- `agentic_service.py` - AI code generation orchestrator (legacy backbone)
- `intent_router.py` - Routes classified intents to specialized handlers
- `intent_classifier.py` - Classifies user requests into intent categories
- `planning_service.py` - Generates execution plans with dependency resolution
- `context_analyzer.py` - Deep analysis of existing app state

**Validation & Error Handling:**
- `validation_service.py` - TypeScript compilation validation, error filtering
- `error_fix_service.py` - Automatic error fixing via LLM (max 2 attempts)
- `diff.py` - Diff generation and application
- `diff_application_service.py` - Applies diffs from LLM output

**Code Generation:**
- `enhanced_codegen.py` - Advanced code generation
- `react_codegen.py` - React component generation
- `typescript_types_generator.py` - Auto-generates TS types from table schemas

**Data & Integration:**
- `cloud_storage_service.py` - File storage
- `merge_service.py` - Merge Agent Handler API
- `snapshot_service.py` - State snapshots
- `version_service.py` - Version operations
- `app_data_service.py` - Table/schema management and queries

### Layer 4: Data Models (`vector_app/models.py`)

All models inherit from `BaseModel` providing `id`, `created_at`, `updated_at`.

**Model Categories:**
- **User & Auth**: `User`, `MagicLinkToken`, `ChatSession`, `ChatMessage`
- **Organization**: `Organization`, `UserOrganization`, `OrganizationInvite`
- **Apps**: `InternalApp`, `AppVersion`, `VersionFile`, `AppFavorite`
- **Execution**: `ActionExecutionLog`, `CodeGenerationJob`
- **Data Store**: `AppDataTable`, `AppDataRow`, `AppDataQuery`
- **Audit**: `VersionAuditLog`, `VersionStateSnapshot`, `AIUsageLog`
- **Integration**: `BackendConnection`, `MergeIntegrationProvider`, `ConnectorCache`
- **Questioning**: `QuestioningSession` (in `chat/models.py`) - Tracks questioning phase state

## Data Flow Patterns

### Questioning Flow (New)

```
User sends initial request
  → MainAgentService.process_user_message() orchestrates
  → Checks for skip request ("pass", "skip", "go ahead")
  → If continuing: _should_continue_questioning() uses LLM to decide
  → If more questions needed: QuestioningService.generate_question()
  → Returns AgentDecision with question or extraction

When questioning ends (skip or complete):
  → _cross_extraction_boundary() called ONCE
  → QuestioningService.extract_facts() extracts ONLY explicit statements
  → ExtractionResult created (chat_history_destroyed=True)
  → Downstream stages receive ONLY extracted facts
```

**Key Architectural Principles:**
- **Distillation vs Decision**: QuestioningService only extracts facts (no inference), MainAgentService makes all control-flow decisions
- **Extraction Boundary**: Facts extracted exactly once; high-entropy context (chat history) destroyed after extraction
- **No Inference Guarantee**: QuestioningService extracts ONLY what user explicitly stated

### Code Generation Request (AI-Powered)

```
Frontend sends message to /api/v1/streaming/generation/stream
  → Backend creates CodeGenerationJob
  → Celery task run_agentic_generation spawned
  → AgenticService.generate_app() processes request
  → Intent classification → routing to specialized handler
  → Handler executes: Plan → Execute → Validate → Fix
  → LLM calls via LLMClient (OpenRouter)
  → Events appended to job.events_json
  → SSE endpoint streams events to frontend
  → Final code saved to AppVersion + VersionFile
```

### CRUD Operations

```
HTTP request to /api/v1/{endpoint}
  → DRF ViewSet handles request
  → Serializer validates/transforms data
  → Service layer processes business logic
  → Models persisted via Django ORM
  → Serialized response returned
```

## Key Abstractions

### Service Pattern

All business logic in `services/` directory. Services are stateless.

```python
class AgenticService:
    def generate_app(self, job, app, user_message) -> event_stream:
        # Business logic here
        pass

def get_agentic_service() -> AgenticService:
    return AgenticService()
```

### ViewSet Pattern

DRF ModelViewSets with custom actions via `@action` decorator.

```python
class OrganizationViewSet(ModelViewSet):
    @action(detail=True, methods=['post'])
    def switch(self, request, pk=None):
        # Custom action logic
        pass
```

### Handler Pattern

Domain-specific handlers in `services/handlers/`:
- `BaseHandler` - Abstract base
- `GenerateHandler` - Code generation
- `EditHandler` - Code editing
- `FeatureHandler` - Feature addition
- `SchemaHandler` - Schema operations

### Service Locator Pattern (Singleton)

Factory functions for dependency injection:
- `get_main_agent_service()` - Questioning orchestrator
- `get_questioning_service()` - Fact extraction
- `get_agentic_service()` - Code generation orchestrator
- `get_intent_classifier()` - Intent classification
- `get_intent_router()` - Intent routing
- `get_planning_service()` - Plan generation
- `get_context_analyzer()` - App state analysis
- `get_validation_service()` - Code validation
- `get_error_fix_service()` - Error auto-fixing
- `get_llm_client()` - LLM communication

### Questioning Architecture

**State Machine:**
```python
class AgentState(StrEnum):
    IDLE = "idle"
    QUESTIONING = "questioning"
    EXTRACTED = "extracted"
    READY = "ready"
```

**Key Data Structures:**
```python
@dataclass
class ExtractionResult:
    facts: Dict[str, Any]
    reasoning: str
    question_count: int
    chat_history_destroyed: bool = True  # Marks context destroyed
    initial_request_destroyed: bool = True

@dataclass
class AgentDecision:
    action: str  # "ask_question" | "skip" | "proceed"
    next_state: AgentState
    question: Optional[str] = None
    extraction: Optional[ExtractionResult] = None
    reasoning: str = ""
```

**QuestioningSession Model:**
```python
class QuestioningSession(DjangoBaseModel):
    chat_session: OneToOneField → ChatSession
    status: CharField (IN_PROGRESS | COMPLETE | SKIPPED)
    synthesized_requirements: JSONField
    question_count: IntegerField
    initial_request: TextField
```

## Entry Points

| Entry Point | File | Purpose |
|-------------|------|---------|
| CLI | `manage.py` | Django management commands |
| WSGI | `internal_apps/wsgi.py` | Production server |
| ASGI | `internal_apps/asgi.py` | Async server |
| Celery | `internal_apps/celery.py` | Background worker |
| URL Root | `internal_apps/urls.py` | Main URL config |
| App URLs | `vector_app/urls.py` | API endpoints |
| Settings | `internal_apps/settings.py` | Django config |

## API Versioning

All endpoints prefixed with `/api/v1/`:
- `GET /api/v1/orgs` - List organizations
- `POST /api/v1/orgs/{org_id}/apps` - Create app
- `PATCH /api/v1/apps/{app_id}/versions/{version_id}/rollback` - Rollback
- `POST /api/v1/streaming/generation/stream` - SSE code generation
