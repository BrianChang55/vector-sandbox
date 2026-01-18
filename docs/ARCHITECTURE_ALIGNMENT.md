# Architecture Alignment Document

> **Status:** Draft  
> **Last Updated:** January 2026  
> **Purpose:** Define the future architecture as Vector pivots to a new infrastructure model

---

## Executive Summary

Vector is transitioning from an internal app builder to a more flexible platform supporting:
- **Multiple agentic workflows** (starting with code generation)
- **App hosting & deployment** (preview + production)
- **Flexible code storage** (GitHub or internal, abstracted)

This document outlines what stays, what goes, and what's new.

---

## Current State Overview

### Backend Structure (Before Pivot)

```
apps/backend/
├── accounts/        # User identity, organizations, auth
├── apps/            # InternalApp, AppVersion, VersionFile ← REMOVING
├── audit/           # Version snapshots, audit logs
├── chat/            # Chat sessions, messages, generation jobs
├── data_store/      # App data tables and rows
├── integrations/    # MCP connectors (Merge)
├── internal_apps/   # Django config, utilities, AI client, Celery
└── vector_app/      # All services, views, prompts ← REMOVING/REPLACING
```

### Frontend Structure
```
apps/web/            # React/TypeScript frontend ← KEEPING (may need updates later)
```

---

## Modules: Keep, Remove, Adapt

### ✅ KEEP (No Significant Changes)

| Module | Purpose | Notes |
|--------|---------|-------|
| `accounts/` | Users, Organizations, Auth | Core identity - no changes needed |
| `integrations/` | MCP connectors, external tools | Connector model stays; may add new integrations |
| `internal_apps/` | Django config, utilities, AI client, Celery | Rename consideration: `core/` or keep as-is |

### 🔄 ADAPT (Keep but Modify)

| Module | Current Purpose | Adaptation Needed |
|--------|-----------------|-------------------|
| `audit/` | Version snapshots tied to `AppVersion` | Generalize to work with new `Project`/`CodeVersion` model |
| `chat/` | Chat sessions tied to `InternalApp` | Generalize to work with `Project`; may become `agent_sessions/` |
| `data_store/` | Data tables tied to `InternalApp` | Generalize to `Project`; becomes "runtime data" for generated apps |

### ❌ REMOVE (Replace with New)

| Module | What It Does | Replacement |
|--------|--------------|-------------|
| `apps/` | `InternalApp`, `AppVersion`, `VersionFile` | → `projects/` |
| `vector_app/` | All code generation services, views, serializers | → `agents/`, `deployment/`, `storage/` |

---

## New Module Architecture

### Proposed Structure

```
apps/backend/
├── accounts/           # ✅ KEEP - Users, Orgs, Auth
├── internal_apps/      # ✅ KEEP - Django config, utilities, AI client
│
├── projects/           # 🆕 NEW - Replaces 'apps/'
│   ├── models.py       # Project, CodeVersion, SourceFile
│   ├── services/
│   ├── serializers/
│   └── views/
│
├── agents/             # 🆕 NEW - Agentic workflow orchestration
│   ├── models.py       # AgentSession, AgentJob, AgentStep
│   ├── codegen/        # Code generation agent
│   │   ├── services/
│   │   └── prompts/
│   └── (future agents...)
│
├── deployment/         # 🆕 NEW - Hosting & preview infrastructure
│   ├── models.py       # Deployment, Environment, DeploymentLog
│   ├── services/
│   │   ├── preview_service.py      # Ephemeral preview environments
│   │   ├── hosting_service.py      # Production deployment
│   │   └── providers/              # Railway, Render, Docker abstractions
│   └── views/
│
├── storage/            # 🆕 NEW - Code storage abstraction
│   ├── models.py       # StorageBackend config
│   ├── services/
│   │   ├── base.py                 # Abstract storage interface
│   │   ├── github_service.py       # GitHub integration
│   │   ├── internal_service.py     # DB/local storage
│   │   └── (future: S3, etc.)
│   └── views/
│
├── chat/               # 🔄 ADAPT → possibly rename to 'sessions/'
│   ├── models.py       # AgentChatSession, ChatMessage
│   └── ...
│
├── audit/              # 🔄 ADAPT - Generalized auditing
│   ├── models.py       # ActivityLog, VersionSnapshot (generic)
│   └── ...
│
├── data_store/         # 🔄 ADAPT - Runtime data for generated apps
│   ├── models.py       # DataTable, DataRow (tied to Project)
│   └── ...
│
└── integrations/       # ✅ KEEP - MCP connectors
    └── ...
```

---

## Entity Model

### New Hierarchy

```
Organization
    └── Project (replaces InternalApp)
            ├── CodeVersion (replaces AppVersion)
            │       └── SourceFile (replaces VersionFile)
            │
            ├── DataTable (from data_store, scoped to Project)
            │       └── DataRow
            │
            ├── AgentSession (from chat, scoped to Project)
            │       ├── ChatMessage
            │       └── AgentJob
            │
            ├── Deployment
            │       └── DeploymentLog
            │
            └── AuditLog (activity trail)
```

### Key Model Definitions

#### `projects/models.py`

```python
class Project(DjangoBaseModel):
    """
    A user project containing generated code.
    Replaces InternalApp.
    """
    organization = models.ForeignKey('accounts.Organization', on_delete=models.CASCADE)
    name = models.CharField(max_length=255)
    slug = models.SlugField(max_length=255)
    description = models.TextField(blank=True)
    
    # Storage configuration - abstracted
    storage_backend = models.CharField(max_length=50)  # 'github', 'internal'
    storage_config = models.JSONField(default=dict)    # Backend-specific config
    
    # Current state
    current_version = models.ForeignKey('CodeVersion', null=True, on_delete=models.SET_NULL)
    published_version = models.ForeignKey('CodeVersion', null=True, on_delete=models.SET_NULL)
    
    created_by = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.SET_NULL, null=True)


class CodeVersion(DjangoBaseModel):
    """
    Immutable snapshot of project code.
    Replaces AppVersion.
    """
    project = models.ForeignKey(Project, on_delete=models.CASCADE, related_name='versions')
    version_number = models.PositiveIntegerField()
    parent_version = models.ForeignKey('self', null=True, on_delete=models.SET_NULL)
    
    # Metadata
    commit_message = models.TextField(blank=True)  # User or AI-generated description
    source = models.CharField(max_length=20)       # 'ai_generation', 'manual_edit', 'import'
    
    # Storage reference (either internal files or external commit)
    storage_ref = models.CharField(max_length=255, blank=True)  # e.g., commit SHA or internal ID
    
    # Generation tracking
    generation_status = models.CharField(max_length=20)
    validation_status = models.CharField(max_length=20)
    
    created_by = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.SET_NULL, null=True)


class SourceFile(DjangoBaseModel):
    """
    Individual file in a code version.
    Only used when storage_backend='internal'.
    For GitHub, files live in the repo.
    """
    code_version = models.ForeignKey(CodeVersion, on_delete=models.CASCADE, related_name='files')
    path = models.CharField(max_length=500)
    content = models.TextField()
    content_hash = models.CharField(max_length=64)
```

#### `agents/models.py`

```python
class AgentSession(DjangoBaseModel):
    """
    A session where an agent works on a project.
    Evolves from ChatSession.
    """
    project = models.ForeignKey('projects.Project', on_delete=models.CASCADE)
    agent_type = models.CharField(max_length=50)  # 'codegen', future types
    title = models.CharField(max_length=255)
    model_id = models.CharField(max_length=100)
    
    created_by = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.SET_NULL, null=True)


class AgentJob(DjangoBaseModel):
    """
    A discrete unit of agent work.
    Evolves from CodeGenerationJob.
    """
    session = models.ForeignKey(AgentSession, on_delete=models.CASCADE, related_name='jobs')
    project = models.ForeignKey('projects.Project', on_delete=models.CASCADE)
    
    # Job specification
    job_type = models.CharField(max_length=50)     # 'generate', 'edit', 'fix'
    input_message = models.TextField()
    
    # Execution state
    status = models.CharField(max_length=20)       # 'queued', 'running', 'complete', 'failed'
    plan_json = models.JSONField(null=True)        # Agent's execution plan
    events_json = models.JSONField(default=list)   # Streaming events for replay
    
    # Result
    result_version = models.ForeignKey('projects.CodeVersion', null=True, on_delete=models.SET_NULL)
    error_message = models.TextField(null=True)
    
    # Timing
    started_at = models.DateTimeField(null=True)
    completed_at = models.DateTimeField(null=True)
```

#### `deployment/models.py`

```python
class Deployment(DjangoBaseModel):
    """
    A deployment of a project version to infrastructure.
    """
    project = models.ForeignKey('projects.Project', on_delete=models.CASCADE)
    code_version = models.ForeignKey('projects.CodeVersion', on_delete=models.CASCADE)
    
    # Deployment type
    environment = models.CharField(max_length=20)  # 'preview', 'production'
    provider = models.CharField(max_length=50)     # 'railway', 'render', 'docker', etc.
    
    # State
    status = models.CharField(max_length=20)       # 'pending', 'building', 'live', 'failed', 'destroyed'
    url = models.URLField(blank=True)              # Deployed URL when live
    
    # Provider-specific
    provider_deployment_id = models.CharField(max_length=255, blank=True)
    provider_config = models.JSONField(default=dict)
    
    # Lifecycle (for previews)
    expires_at = models.DateTimeField(null=True)   # Auto-destroy time for previews
    
    created_by = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.SET_NULL, null=True)


class DeploymentLog(DjangoBaseModel):
    """
    Build/deployment logs and events.
    """
    deployment = models.ForeignKey(Deployment, on_delete=models.CASCADE, related_name='logs')
    level = models.CharField(max_length=20)        # 'info', 'warn', 'error'
    message = models.TextField()
    metadata = models.JSONField(default=dict)
```

#### `storage/services/base.py`

```python
from abc import ABC, abstractmethod
from dataclasses import dataclass


@dataclass
class FileContent:
    path: str
    content: str
    hash: str


class StorageBackend(ABC):
    """
    Abstract interface for code storage.
    Implementations: GitHubStorage, InternalStorage
    """
    
    @abstractmethod
    def save_version(self, project, files: list[FileContent], message: str) -> str:
        """Save files as a new version. Returns storage_ref."""
        pass
    
    @abstractmethod
    def get_files(self, project, storage_ref: str) -> list[FileContent]:
        """Retrieve all files for a version."""
        pass
    
    @abstractmethod
    def get_file(self, project, storage_ref: str, path: str) -> FileContent | None:
        """Retrieve a single file."""
        pass
    
    @abstractmethod
    def list_files(self, project, storage_ref: str) -> list[str]:
        """List file paths in a version."""
        pass
    
    @abstractmethod
    def diff_versions(self, project, ref_a: str, ref_b: str) -> dict:
        """Compare two versions."""
        pass
```

---

## Key Abstractions & Design Decisions

### 1. Storage Backend Abstraction

**Requirement:** Source of truth can be GitHub OR internal DB - must be swappable.

**Approach:**
- `Project.storage_backend` field indicates which backend
- `StorageBackend` abstract class defines interface
- `GitHubStorageService` and `InternalStorageService` implementations
- Services call `get_storage_backend(project)` factory function

**Migration path:** Start with internal storage, add GitHub later without model changes.

### 2. Deployment Provider Abstraction

**Requirement:** Deploy to Railway, Render, Docker, etc. - keep generic.

**Approach:**
- `Deployment.provider` field indicates target
- `DeploymentProvider` abstract class defines interface
- Provider implementations: `RailwayProvider`, `RenderProvider`, `DockerProvider`
- Factory pattern: `get_deployment_provider(provider_name)`

### 3. Agent Extensibility

**Requirement:** Code generation now, anything later.

**Approach:**
- `AgentSession.agent_type` and `AgentJob.job_type` fields
- Agent implementations in `agents/{type}/` subdirectories
- Shared infrastructure in `agents/common/`:
  - LLM client
  - Tool execution
  - Event streaming
  - Plan execution

### 4. Preview vs Production

**Requirement:** Ephemeral previews for compile check + visual preview.

**Approach:**
- `Deployment.environment = 'preview'` with `expires_at` set
- Background job to clean up expired previews
- Preview builds may use lighter infrastructure (no persistent DB, etc.)

---

## Open Questions / Future Decisions

| Question | Current Assumption | Decide By |
|----------|-------------------|-----------|
| GitHub: org-hosted repos or user repos? | Org-hosted initially | Before GitHub integration |
| Build pipeline: Vector builds or platform builds? | TBD | Before deployment work |
| Agent runtime: local or dev container? | TBD | Before agent v2 |
| Monorepo vs separate repos? | Flexible, start with separate | Before GitHub integration |
| How to handle data_store schema migrations? | TBD | During data_store adaptation |

---

## Migration Strategy

### Phase 1: Foundation
1. Create `projects/` module with `Project`, `CodeVersion`, `SourceFile`
2. Create `storage/` module with internal storage backend
3. Create `deployment/` module with preview service (minimal)
4. Create `agents/` module - move code generation logic from `vector_app/`

### Phase 2: Adaptation
1. Update `chat/` to reference `Project` instead of `InternalApp`
2. Update `data_store/` to reference `Project`
3. Update `audit/` to be generic

### Phase 3: Enhancement
1. Add GitHub storage backend
2. Add production deployment providers (Railway, Render)
3. Add new agent types as needed

### Phase 4: Cleanup
1. Remove `apps/` module
2. Remove `vector_app/` module
3. Update frontend (separate effort)

---

## Appendix: Module Dependencies

```
accounts ─────────────────────────────────────────┐
                                                  │
internal_apps (core utilities) ───────────────────┤
                                                  │
projects ─────────────────────────────────────────┤
    ↑                                             │
    │                                             │
agents ───────────────────────────────────────────┤
    │                                             │
    ├──→ storage                                  │
    │                                             │
    └──→ deployment                               │
                                                  │
chat ─────────────→ projects ─────────────────────┤
                                                  │
data_store ───────→ projects ─────────────────────┤
                                                  │
audit ────────────→ projects ─────────────────────┤
                                                  │
integrations ─────→ accounts ─────────────────────┘
```

---

## Document History

| Date | Author | Changes |
|------|--------|---------|
| Jan 2026 | - | Initial draft |
