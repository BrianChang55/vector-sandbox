# Vector Backend

Django + Celery backend for the Vector platform.

## Quick Start

From the **repository root** (not this directory):

```bash
# Install dependencies
mise run setup

# Start all services (docker, frontend, backend, celery, flower)
mise run dev
```

## Prerequisites

- [mise](https://mise.run) - Runtime manager
- Docker - For Redis and PostgreSQL

## Environment Setup

```bash
# Copy env files
cp .env.example .env                           # Root: Docker port config
cp apps/backend/.env.example apps/backend/.env  # Backend config
```

Required variables in `apps/backend/.env`:
- `OPENROUTER_API_KEY` - For AI features
- `ENCRYPTION_KEY` - For data encryption

## Development Commands

From the repository root:

| Command | Description |
|---------|-------------|
| `mise run dev` | Start all services |
| `mise run be` | Backend only (port 8001) |
| `mise run celery` | Celery worker |
| `mise run flower` | Celery monitor (port 5555) |
| `mise run docker` | Start Docker services |
| `mise run migrate` | Run migrations |
| `mise run test` | Run tests |
| `mise run shell` | Django shell_plus |

## Running Services Individually

```bash
cd apps/backend

# Django
uv run python manage.py runserver 8001

# Celery
uv run celery -A internal_apps worker --loglevel=info

# Flower
uv run celery -A internal_apps flower --port=5555
```

## Flower (Celery Monitor)

[Flower](https://flower.readthedocs.io/) provides real-time visibility into Celery tasks.

Open http://localhost:5555 after starting with `mise run flower`.

**Features:**
- Dashboard: Overview of workers, tasks, and queues
- Tasks: View running, completed, and failed tasks
- Workers: Monitor worker status and resource usage
- Real-time updates and persistent history

Task results are stored in the Django database via `django-celery-results`.

## Project Structure

```
apps/backend/
├── accounts/        # User & organization models
├── apps/            # InternalApp, AppVersion models
├── audit/           # Version audit logging
├── chat/            # Chat sessions & code generation
├── data_store/      # App data tables
├── integrations/    # External service connectors
├── internal_apps/   # Django project settings
└── vector_app/      # Core app logic & services
```
