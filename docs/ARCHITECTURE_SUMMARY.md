# Architecture Summary

## Modules Overview

### `accounts/`
User identity and organization management. Handles authentication (email/password, Google OAuth, magic links), user profiles, organizations, team membership with roles (admin/editor/viewer), and organization invites.

### `projects/`
**NEW** — Replaces the current `apps/` module.

The central entity users interact with. A Project is a codebase that users create and iterate on with AI assistance. Contains:
- **Project** — Name, description, ownership, current/published version pointers
- **CodeVersion** — Immutable snapshots of project code, with version numbers and parent references for history
- **SourceFile** — Individual files within a version (only used when storing code internally vs GitHub)

### `agents/`
**NEW** — Replaces most of `vector_app/`.

Orchestrates AI agent workflows. Starting with code generation, but designed to support future agent types. Contains:
- **Session & Job tracking** — AgentSession groups related work, AgentJob tracks individual generation tasks with status, plans, and streaming events
- **codegen/** — The code generation agent: prompts, planning logic, diff application, error fixing, TypeScript validation
- **common/** — Shared infrastructure: LLM client wrapper, tool execution, event streaming, plan execution patterns

### `deployment/`
**NEW** — Handles getting generated code running on infrastructure.

Two modes: ephemeral previews (compile check + visual preview before publishing) and production deployments. Contains:
- **Deployment tracking** — Status, URLs, provider-specific IDs, expiration for previews
- **providers/** — Abstracted implementations for Railway, Render, Docker, etc. Easy to add new targets.
- **Preview lifecycle** — Spin up, health check, auto-destroy expired previews

### `storage/`
**NEW** — Abstracts where code actually lives.

Source of truth can be either GitHub or internal database storage. The abstraction lets us start simple and migrate later. Contains:
- **Storage interface** — Save version, get files, diff versions, list files
- **github/** — GitHub integration: commit, push, pull, branch management, potentially PR workflows
- **internal/** — Database-backed storage using SourceFile model

### `chat/`
**ADAPT** — Currently tied to InternalApp, will point to Project instead.

Manages conversational AI sessions. Users chat with agents here, ask questions, provide feedback. Contains:
- **ChatSession** — A conversation thread tied to a project
- **ChatMessage** — Individual messages (user, assistant, system) with metadata
- **QuestioningSession** — Multi-turn requirement gathering before generation

### `data_store/`
**ADAPT** — Currently tied to InternalApp, will point to Project instead.

Runtime data storage for generated apps. When a generated app needs to store data (like a todo list or user records), it uses these tables. Contains:
- **DataTable** — Schema definition (columns, types) for a project's data
- **DataRow** — Actual data rows, stored as JSON matching the schema
- **Schema management** — Validation, migrations when schema changes

### `audit/`
**ADAPT** — Needs to generalize beyond the current AppVersion model.

Activity logging and version snapshots for compliance and debugging. Contains:
- **AuditLog** — Who did what, when, from where (operations, user, IP, timestamp)
- **VersionSnapshot** — Point-in-time captures of project state for rollback capability

### `integrations/`
**KEEP** — No significant changes needed.

External tool connections via MCP (Merge). Lets generated apps interact with services like Jira, Slack, Linear, etc. Contains:
- **Connector management** — Available connectors, which are linked/authenticated
- **Tool catalog** — Cached tools from each connector with schemas and action types
- **Execution logging** — Audit trail of connector tool calls

### `internal_apps/`
**KEEP** — Core Django infrastructure.

Project configuration, shared utilities, and foundational services. Contains:
- **ai/** — LLM client for OpenRouter API calls
- **utils/** — Base model class, encryption helpers, enum utilities
- **Celery config** — Async task queue for background jobs
