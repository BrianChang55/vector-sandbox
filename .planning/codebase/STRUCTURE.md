# Codebase Structure

**Analysis Date:** 2026-01-14

## Directory Layout

```
internal-apps-backend/
├── manage.py                          # Django CLI entry point
├── conftest.py                        # Pytest configuration
├── requirements.txt                   # Python dependencies
├── requirements/                      # Additional requirement files
│   └── static.txt                     # Static analysis tools
│
├── internal_apps/                     # Django project settings
│   ├── settings.py                    # Database, middleware, apps config
│   ├── urls.py                        # Root URL configuration
│   ├── wsgi.py                        # Production WSGI entry
│   ├── asgi.py                        # Async entry point
│   ├── celery.py                      # Celery task broker config
│   └── utils/                         # Shared utilities
│       ├── base_model.py              # Abstract model with timestamps
│       ├── encryption.py              # Field encryption/decryption
│       └── decorators.py              # Auth decorators
│
├── vector_app/                        # Main Django app
│   ├── models.py                      # 30+ data models (2100+ lines)
│   ├── urls.py                        # API route definitions
│   ├── permissions.py                 # Custom DRF permissions
│   ├── tasks.py                       # Celery async tasks
│   ├── admin.py                       # Django admin config
│   │
│   ├── views/                         # API ViewSets
│   ├── serializers/                   # DRF Serializers
│   ├── services/                      # Business Logic Layer
│   ├── prompts/                       # LLM Prompt Templates
│   ├── action_classification/         # Action classification module
│   ├── adapters/                      # External database adapters
│   ├── migrations/                    # Database migrations
│   └── tests/                         # App tests
│
├── scripts/                           # Utility scripts
├── tests/                             # Root-level tests
└── venv/                              # Python virtual environment
```

```
internal-apps-web-app/
├── src/
│   ├── main.tsx                       # React bootstrap
│   ├── App.tsx                        # Main router component
│   │
│   ├── pages/                         # Page-level components
│   ├── components/                    # Reusable components
│   │   ├── builder/                   # IDE components
│   │   ├── ui/                        # Base Radix UI primitives
│   │   ├── auth/                      # Authentication guards
│   │   ├── data/                      # Data management
│   │   └── settings/                  # Settings panels
│   │
│   ├── services/                      # API & business logic
│   ├── hooks/                         # Custom React hooks
│   ├── store/                         # Redux state management
│   │   └── slices/                    # Redux slices
│   ├── types/                         # TypeScript definitions
│   ├── lib/                           # Utility libraries
│   └── utils/                         # Utility functions
│
├── public/                            # Static assets
├── vite.config.ts                     # Vite build config
├── tsconfig.json                      # TypeScript config
├── tailwind.config.ts                 # Tailwind CSS config
└── package.json                       # Node dependencies
```

## Directory Purposes

### Backend (`internal-apps-backend/`)

**internal_apps/**
- Purpose: Django project configuration
- Contains: Settings, URL routing, WSGI/ASGI entry, Celery config
- Key files: `settings.py` (1200+ lines), `celery.py`

**vector_app/views/**
- Purpose: API endpoint handlers
- Contains: DRF ViewSets and custom API views
- Key files: `organization_views.py`, `auth_views.py`, `streaming_views.py`, `version_views.py`, `internal_app_views.py`, `connector_views.py`

**vector_app/serializers/**
- Purpose: Request/response data transformation
- Contains: DRF serializers for all models
- Key files: `auth_serializers.py`, `internal_app.py`, `version.py`, `chat_serializers.py`, `connector_serializers.py`

**vector_app/services/**
- Purpose: Business logic layer
- Contains: All business logic, LLM integration, code generation
- Key files: `agentic_service.py`, `enhanced_codegen.py`, `validation_service.py`, `intent_router.py`, `diff.py`, `merge_service.py`
- Subdirectories: `handlers/` (action handlers), `datastore/` (data table operations)

**vector_app/prompts/**
- Purpose: LLM prompt templates
- Contains: Prompt definitions for AI features
- Key files: `agentic.py` (2000+ lines), `intent_classification.py`, `error_fix.py`

**vector_app/action_classification/**
- Purpose: User intent analysis
- Contains: Action classifier, tool matcher
- Key files: `action_classifier.py`, `tool_matcher.py`, `types.py`

**vector_app/adapters/**
- Purpose: External database adapters
- Contains: PostgreSQL, MySQL, Supabase adapters
- Key files: `postgresql.py`, `mysql.py`, `supabase.py`

### Frontend (`internal-apps-web-app/`)

**src/pages/**
- Purpose: Route-level page components
- Contains: One component per route
- Key files: `AppsPage.tsx`, `AppBuilderPage.tsx`, `SettingsPage.tsx`, `ResourcesPage.tsx`

**src/components/builder/**
- Purpose: IDE and code editing components
- Contains: Chat panel, code editor, preview, versions
- Key files: `AgenticChatPanel.tsx` (2000+ lines), `SandpackPreview.tsx`, `VersionsPanel.tsx`, `ModelSelector.tsx`

**src/components/ui/**
- Purpose: Base UI primitives
- Contains: Radix UI wrapped components
- Key files: `button.tsx`, `dialog.tsx`, `input.tsx`, `select.tsx`, `toast.tsx`

**src/services/**
- Purpose: API communication and external integrations
- Contains: HTTP clients, authentication, analytics
- Key files: `apiService.ts` (1000+ lines), `authService.ts`, `agentService.ts`, `loggingService.ts`

**src/hooks/**
- Purpose: Custom React hooks for data fetching
- Contains: Domain-specific hooks
- Key files: `useApps.ts`, `useOrganizations.ts`, `useMembers.ts`, `useIntegrations.ts`, `useDataStore.ts`

**src/store/slices/**
- Purpose: Redux state management
- Contains: Auth and UI state slices
- Key files: `authSlice.ts`, `uiSlice.ts`

**src/types/**
- Purpose: TypeScript type definitions
- Contains: Shared types for models and API
- Key files: `models.ts`, `dataStore.ts`, `agent.ts`, `auth.ts`

## Key File Locations

**Entry Points:**
- `manage.py` - Django CLI entry
- `internal_apps/wsgi.py` - Production WSGI server
- `internal_apps/celery.py` - Celery worker
- `../internal-apps-web-app/src/main.tsx` - React bootstrap

**Configuration:**
- `internal_apps/settings.py` - Django settings
- `../internal-apps-web-app/vite.config.ts` - Vite build
- `../internal-apps-web-app/tsconfig.json` - TypeScript
- `.env` / `.env.example` - Environment variables

**Core Logic:**
- `vector_app/services/agentic_service.py` - AI orchestration
- `vector_app/services/handlers/generate_handler.py` - Code generation
- `vector_app/views/streaming_views.py` - SSE streaming
- `../internal-apps-web-app/src/services/apiService.ts` - API client

**Testing:**
- `vector_app/tests/` - Django app tests
- `tests/` - Root-level integration tests
- `conftest.py` - Pytest configuration

## Naming Conventions

**Files (Python):**
- snake_case for all files: `user_service.py`, `auth_views.py`
- Test files: `test_*.py`

**Files (TypeScript):**
- PascalCase for components: `Button.tsx`, `AgenticChatPanel.tsx`
- camelCase for services/hooks: `apiService.ts`, `useApps.ts`
- camelCase for types: `models.ts`, `auth.ts`

**Directories:**
- snake_case for Python: `action_classification/`, `internal_apps/`
- kebab-case or camelCase for TypeScript: `builder/`, `ui/`
- Plural for collections: `views/`, `serializers/`, `hooks/`

## Where to Add New Code

**New Feature (Backend):**
- Models: `vector_app/models.py`
- API endpoint: `vector_app/views/{domain}_views.py`
- Serializer: `vector_app/serializers/{domain}.py`
- Business logic: `vector_app/services/{feature}_service.py`
- Tests: `vector_app/tests/test_{feature}.py`

**New Feature (Frontend):**
- Page component: `src/pages/{Feature}Page.tsx`
- Components: `src/components/{feature}/`
- API calls: Add to `src/services/apiService.ts`
- Hook: `src/hooks/use{Feature}.ts`
- Types: `src/types/{feature}.ts`

**New API Endpoint:**
- ViewSet: `vector_app/views/{domain}_views.py`
- URL registration: `vector_app/urls.py`
- Serializer: `vector_app/serializers/{domain}.py`

**New LLM Prompt:**
- Prompt definition: `vector_app/prompts/{feature}.py`

## Special Directories

**vector_app/migrations/**
- Purpose: Django database migrations
- Source: Auto-generated by `makemigrations`
- Committed: Yes (track schema changes)

**.planning/**
- Purpose: Project planning documents (GSD workflow)
- Source: Created by planning tools
- Committed: Typically yes

**venv/**
- Purpose: Python virtual environment
- Source: Created by `python -m venv venv`
- Committed: No (in .gitignore)

**node_modules/**
- Purpose: Node.js dependencies
- Source: Installed by `npm install`
- Committed: No (in .gitignore)

---

*Structure analysis: 2026-01-14*
*Update when directory structure changes*
