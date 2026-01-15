# Vector

AI-powered internal application builder.

## Project Structure

```
vector/
├── apps/
│   ├── backend/    # Django + DRF + Celery (Python 3.11)
│   └── web/        # React + TypeScript + Vite (Node 24)
└── docs/
```

## Quick Start

```bash
# Install mise if you haven't
curl https://mise.run | sh

# Install dependencies
mise run setup

# Start all services
mise run dev
```

## Development

| Command | Description |
|---------|-------------|
| `mise run dev` | Start all services (frontend, backend, celery) |
| `mise run fe` | Frontend only (port 5176) |
| `mise run be` | Backend only (port 8001) |
| `mise run celery` | Celery worker |
| `mise run flower` | Celery monitor (port 5555) |
| `mise run migrate` | Run Django migrations |
| `mise run test` | Run backend tests |
| `mise run test-fe` | Run frontend lint + build |

## Environment Setup

Copy the example env file and configure:

```bash
cp apps/backend/.env.example apps/backend/.env
```

Required variables:
- `OPENROUTER_API_KEY` - AI model access
- `ENCRYPTION_KEY` - Data encryption (generate with Fernet)
- `FRONTEND_URL` - Frontend base URL (http://localhost:5176)

## Tech Stack

**Backend:**
- Django 4.2 + Django REST Framework
- Celery + Redis
- PostgreSQL (prod) / SQLite (dev)
- OpenRouter API for AI models

**Frontend:**
- React 19 + TypeScript
- Vite + TailwindCSS
- Redux Toolkit + React Query
- Sandpack for live preview
