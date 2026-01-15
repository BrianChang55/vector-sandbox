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

**Core Services:**
- `agentic_service.py` - AI code generation orchestrator
- `validation_service.py` - Schema, code, spec validation
- `diff.py` - Diff generation and application
- `error_fix_service.py` - Automatic error fixing via LLM
- `intent_router.py` - Intent classification routing
- `planning_service.py` - Generation planning

**Code Generation:**
- `enhanced_codegen.py` - Advanced code generation
- `react_codegen.py` - React component generation
- `typescript_types_generator.py` - Type inference

**Data & Integration:**
- `cloud_storage_service.py` - File storage
- `merge_service.py` - Merge Agent Handler API
- `snapshot_service.py` - State snapshots
- `version_service.py` - Version operations

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

## Data Flow Patterns

### Code Generation Request (AI-Powered)

```
Frontend sends message to /api/v1/streaming/generation/stream
  → Backend creates CodeGenerationJob
  → Celery task run_agentic_generation spawned
  → AgenticService.generate_app() processes request
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

### Service Locator Pattern

Factory functions for dependency injection:
- `get_agentic_service()`
- `get_llm_client()`
- `get_react_codegen_service()`
- `get_execution_scope_classifier()`

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
