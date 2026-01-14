# Architecture

**Analysis Date:** 2026-01-14

## Pattern Overview

**Overall:** Full-Stack AI-Powered Platform with Layered Service Architecture

**Key Characteristics:**
- Multi-tier separation (HTTP → Service → Data)
- AI-centric code generation with streaming SSE
- Async task processing via Celery
- Multi-tenant organization model
- Version-controlled app state with diff/rollback

## Layers

### Backend Layers (Python/Django)

**HTTP Entry Layer (`vector_app/views/`):**
- Purpose: Handle HTTP requests, authentication, response serialization
- Contains: DRF ViewSets, custom API views, SSE streaming endpoints
- Key files: `organization_views.py`, `auth_views.py`, `streaming_views.py`, `version_views.py`
- Depends on: Serializers, Services, Models
- Used by: Frontend API calls

**Serialization Layer (`vector_app/serializers/`):**
- Purpose: Input validation and data transformation
- Contains: DRF serializers for all models
- Key files: `auth_serializers.py`, `internal_app.py`, `version.py`, `chat_serializers.py`
- Depends on: Models
- Used by: Views

**Service Layer (`vector_app/services/`):**
- Purpose: Business logic, LLM integration, code generation
- Contains: Stateless service classes and functions
- Key files: `agentic_service.py`, `enhanced_codegen.py`, `validation_service.py`, `intent_router.py`
- Depends on: Models, Prompts, External APIs
- Used by: Views, Celery Tasks

**Handler Layer (`vector_app/services/handlers/`):**
- Purpose: Domain-specific action implementations
- Contains: Generate, Edit, Feature, Schema handlers
- Key files: `generate_handler.py`, `edit_handler.py`, `feature_handler.py`, `base_handler.py`
- Depends on: Services, Models, Context builders
- Used by: AgenticService

**Data Layer (`vector_app/models.py`):**
- Purpose: Data schema, relationships, persistence
- Contains: 30+ Django models inheriting from BaseModel
- Depends on: BaseModel (provides `created_at`, `updated_at`), encryption utils
- Used by: All layers

### Frontend Layers (TypeScript/React)

**Page Layer (`src/pages/`):**
- Purpose: Full-page route components
- Contains: AppsPage, AppBuilderPage, SettingsPage, ResourcesPage
- Depends on: Components, Hooks, Services
- Used by: Router

**Component Layer (`src/components/`):**
- Purpose: Reusable UI components
- Contains: Builder components, UI primitives, Auth guards
- Key dirs: `builder/`, `ui/`, `auth/`, `settings/`
- Depends on: Hooks, Types
- Used by: Pages

**Service Layer (`src/services/`):**
- Purpose: API communication, external integrations
- Contains: apiService, authService, agentService, analyticsService
- Key files: `apiService.ts`, `authService.ts`, `loggingService.ts`
- Depends on: Types, Utils
- Used by: Hooks, Components

**State Layer (`src/store/`, `src/hooks/`):**
- Purpose: Application state management
- Contains: Redux slices (`authSlice`, `uiSlice`), custom hooks
- Key files: `useApps.ts`, `useOrganizations.ts`, `useMembers.ts`
- Depends on: Services, Types
- Used by: Components

## Data Flow

**Code Generation Request (SSE Streaming):**

1. User sends message via `AgenticChatPanel`
2. POST to `/api/v1/streaming/generation/stream`
3. Backend creates `CodeGenerationJob`
4. Celery task `run_agentic_generation` spawned
5. `AgenticService.generate_app()` processes request
6. LLM calls via `openrouter_service` or direct
7. Events appended to `job.events_json`
8. SSE endpoint streams events to frontend
9. Frontend subscribes via EventSource, updates UI
10. Final code version saved to `AppVersion` model

**CRUD Operations:**

1. Frontend hook (e.g., `useApps`) triggers action
2. `apiService.ts` makes HTTP request to `/api/v1/{endpoint}`
3. DRF ViewSet handles request
4. Serializer validates/transforms data
5. Service layer processes business logic
6. Models persisted via Django ORM
7. Serialized response sent to frontend
8. Redux/React Query updates local state

**Version Control Flow:**

1. User edits code in editor
2. OnChange triggers intent classification
3. `IntentRouter` determines edit type
4. `CodeGenerationJob` created
5. `AgenticService` applies diff or generates new code
6. `VersionFile` created with new code
7. Diff calculated and stored
8. `VersionAuditLog` records change
9. Frontend shows version timeline

**State Management:**
- UI State: Redux (`authSlice`, `uiSlice`)
- Server State: React Query (caching, refetching)
- Local State: React useState for component state

## Key Abstractions

**Service Pattern:**
- Purpose: Encapsulate business logic
- Examples: `AgenticService`, `ValidationService`, `CloudStorageService`
- Pattern: Stateless functions or classes, factory getters

**ViewSet Pattern (DRF):**
- Purpose: Automatic CRUD endpoint generation
- Examples: `OrganizationViewSet`, `AppVersionViewSet`, `InternalAppViewSet`
- Pattern: Inherit `ModelViewSet`, custom actions via `@action`

**Handler Pattern:**
- Purpose: Domain-specific action processing
- Examples: `GenerateHandler`, `EditHandler`, `FeatureHandler`
- Pattern: Inherit `BaseHandler`, implement `handle()` method

**Intent Classification:**
- Purpose: Route user requests to appropriate handler
- Implementation: `IntentClassifier` uses LLM to analyze intent
- Reduces token usage by pre-classifying requests

## Entry Points

**Backend:**
- CLI: `manage.py` - Django management commands
- WSGI: `internal_apps/wsgi.py` - Production server
- ASGI: `internal_apps/asgi.py` - Async server
- Celery: `internal_apps/celery.py` - Background worker
- URLs: `internal_apps/urls.py` → `vector_app/urls.py`

**Frontend:**
- Bootstrap: `src/main.tsx` - React root with service init
- Router: `src/App.tsx` - Route definitions and auth guards

## Error Handling

**Strategy:** Throw exceptions in services, catch at view boundaries

**Patterns:**
- Services throw custom exceptions with context
- Views catch and return appropriate HTTP responses
- Celery tasks log errors and update job status
- Frontend shows toast notifications for errors

## Cross-Cutting Concerns

**Logging:**
- Backend: Python `logging` module, structured logs
- Frontend: `loggingService.ts` with Sentry integration

**Validation:**
- Backend: DRF serializers at API boundary
- Frontend: TypeScript strict mode, form validation

**Authentication:**
- Backend: JWT via SimpleJWT, session-based fallback
- Frontend: Token stored in localStorage, auto-refresh

**Monitoring:**
- Sentry for error tracking (both backend and frontend)
- Mixpanel for product analytics

---

*Architecture analysis: 2026-01-14*
*Update when major patterns change*
