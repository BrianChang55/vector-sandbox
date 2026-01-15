# Directory Structure

```
internal-apps-backend/
├── manage.py                          # Django CLI entry point
├── conftest.py                        # Pytest configuration
├── requirements.txt                   # Main dependencies
├── requirements/
│   └── static.txt                     # Static analysis tools
│
├── internal_apps/                     # Django project settings
│   ├── settings.py                    # Database, middleware, apps config
│   ├── urls.py                        # Root URL config (/api/v1 → vector_app.urls)
│   ├── wsgi.py                        # Production WSGI entry
│   ├── asgi.py                        # Async entry point
│   ├── celery.py                      # Celery task broker config
│   └── utils/
│       ├── base_model.py              # Abstract model with timestamps
│       ├── encryption.py              # Field encryption/decryption
│       ├── enum.py                    # Enum utilities
│       └── decorators.py              # Auth decorators
│
├── vector_app/                        # Main Django app
│   ├── models.py                      # All data models (30+)
│   ├── urls.py                        # API route definitions
│   ├── permissions.py                 # Custom DRF permissions
│   ├── tasks.py                       # Celery async tasks
│   ├── admin.py                       # Django admin config
│   │
│   ├── views/                         # API ViewSets
│   │   ├── organization_views.py
│   │   ├── auth_views.py
│   │   ├── internal_app_views.py
│   │   ├── version_views.py
│   │   ├── streaming_views.py
│   │   ├── connector_views.py
│   │   ├── member_views.py
│   │   ├── backend_connection_views.py
│   │   ├── app_data_views.py
│   │   ├── data_runtime_views.py
│   │   ├── action_views.py
│   │   └── publish_views.py
│   │
│   ├── serializers/                   # DRF Serializers
│   │   ├── auth_serializers.py
│   │   ├── organization.py
│   │   ├── internal_app.py
│   │   ├── version.py
│   │   ├── chat_serializers.py
│   │   ├── connector_serializers.py
│   │   ├── member_serializers.py
│   │   ├── app_data.py
│   │   └── backend_connection.py
│   │
│   ├── services/                      # Business Logic Layer
│   │   ├── agentic_service.py         # AI code generation orchestrator
│   │   ├── enhanced_codegen.py        # Advanced code generation
│   │   ├── react_codegen.py           # React component generation
│   │   ├── validation_service.py      # Schema/code validation
│   │   ├── intent_router.py           # Intent classification routing
│   │   ├── intent_classifier.py       # LLM-based intent analysis
│   │   ├── planning_service.py        # Generation planning
│   │   ├── error_fix_service.py       # Automatic error correction
│   │   ├── diff.py                    # Diff generation
│   │   ├── diff_application_service.py
│   │   ├── execution_scope_classifier.py
│   │   ├── merge_service.py           # Merge Agent Handler API
│   │   ├── snapshot_service.py        # State snapshots
│   │   ├── version_service.py         # Version operations
│   │   ├── cloud_storage_service.py   # Cloud storage
│   │   ├── image_upload_service.py    # S3/R2 uploads
│   │   ├── app_data_service.py        # Data table service
│   │   ├── connectors_context.py      # Integration context
│   │   │
│   │   ├── handlers/                  # Domain-specific handlers
│   │   │   ├── base_handler.py
│   │   │   ├── generate_handler.py
│   │   │   ├── edit_handler.py
│   │   │   ├── feature_handler.py
│   │   │   ├── schema_handler.py
│   │   │   └── parallel_executor.py
│   │   │
│   │   └── datastore/                 # Data table operations
│   │       ├── context_builder.py
│   │       ├── fetcher.py
│   │       ├── parser.py
│   │       ├── schema.py
│   │       ├── table_creator.py
│   │       └── validator.py
│   │
│   ├── ai/                            # LLM Client Module
│   │   ├── client.py                  # LLMClient class
│   │   ├── types.py                   # LLMSettings, ChatResult
│   │   ├── models.py                  # AIModel enum
│   │   └── exceptions.py              # ChatFunctionError, APIError
│   │
│   ├── prompts/                       # LLM Prompt Templates
│   │   ├── agentic.py                 # Main agentic prompts
│   │   ├── intent_classification.py
│   │   ├── error_fix.py
│   │   ├── execution_scope.py
│   │   └── ai.py
│   │
│   ├── action_classification/         # Action classification module
│   │   ├── action_classifier.py
│   │   ├── types.py                   # ActionType enum
│   │   ├── tool_matcher.py
│   │   ├── build_mcp_context.py
│   │   └── prompts.py
│   │
│   ├── adapters/                      # Database adapters
│   │   ├── postgresql.py
│   │   ├── mysql.py
│   │   └── supabase.py
│   │
│   ├── utils/                         # Utility functions
│   │   ├── encryption.py
│   │   └── pydantic_utils.py          # StrictBaseModel
│   │
│   ├── migrations/                    # Database migrations
│   │
│   ├── tests/                         # App tests
│   │   ├── test_integration.py
│   │   ├── test_adapters.py
│   │   ├── test_diff_application_service.py
│   │   └── ...
│   │
│   └── templates/
│       └── reception/                 # Email templates
│
├── .cursor/
│   └── rules/                         # Cursor AI rules
│       ├── style.mdc                  # Backend style guide
│       └── calling-llms.mdc           # LLM calling conventions
│
├── .planning/
│   └── codebase/                      # Codebase documentation
│
├── Makefile                           # Build commands
├── mypy.ini                           # Type checking config
├── prospector.yml                     # Linting config
└── venv/                              # Virtual environment
```

## Key File Locations

### Entry Points
- **CLI**: `manage.py`
- **WSGI**: `internal_apps/wsgi.py`
- **Celery**: `internal_apps/celery.py`

### Models
- All models: `vector_app/models.py` (30+ models, 2100+ lines)

### Business Logic
- Core services: `vector_app/services/`
- Handlers: `vector_app/services/handlers/`
- AI client: `vector_app/ai/client.py`

### API Endpoints
- Routes: `vector_app/urls.py`
- ViewSets: `vector_app/views/`

### Configuration
- Django settings: `internal_apps/settings.py`
- Type checking: `mypy.ini`
- Linting: `prospector.yml`
- Style rules: `.cursor/rules/style.mdc`

## Module Boundaries

| Module | Responsibility | Dependencies |
|--------|----------------|--------------|
| `views/` | HTTP handling, response serialization | serializers, services |
| `serializers/` | Input validation, transformation | models |
| `services/` | Business logic, external APIs | models, prompts, ai |
| `models.py` | Data schema, relationships | BaseModel |
| `prompts/` | LLM prompt templates | (none) |
| `ai/` | LLM client abstraction | types, exceptions |
| `handlers/` | Domain action implementations | services, models |
| `tasks.py` | Celery background tasks | services, models |
