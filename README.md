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
| `mise run dev` | Start all services (docker, frontend, backend, celery, flower) |
| `mise run fe` | Frontend only (port 5176) |
| `mise run be` | Backend only (port 8001) |
| `mise run celery` | Celery worker |
| `mise run flower` | Celery monitor (port 5555) |
| `mise run docker` | Start Docker services (Redis, PostgreSQL) |
| `mise run docker-down` | Stop Docker services |
| `mise run migrate` | Run Django migrations |
| `mise run test` | Run backend tests |

## Environment Setup

Copy the example env files and configure:

```bash
cp .env.example .env                           # Docker port config
cp apps/backend/.env.example apps/backend/.env  # Backend config
```

Configure Docker ports in root `.env` to avoid conflicts:
- `REDIS_PORT` - default 6379
- `POSTGRES_PORT` - default 5432

Required variables:
- `OPENROUTER_API_KEY` - AI model access
- `ENCRYPTION_KEY` - Data encryption (generate with Fernet)
- `FRONTEND_URL` - Frontend base URL (http://localhost:5176)

## GitHub Codespaces

To use this project in a Codespace, first configure your secrets:

1. Go to **GitHub → Settings → Codespaces → Secrets** (or repo Settings → Secrets and variables → Codespaces)
2. Add the following secrets:

| Secret | Required | Description |
|--------|----------|-------------|
| `OPENROUTER_API_KEY` | Yes | AI model access - get from [openrouter.ai/keys](https://openrouter.ai/keys) |
| `ENCRYPTION_KEY` | Yes | Data encryption key - generate with `python -c 'from cryptography.fernet import Fernet; print(Fernet.generate_key().decode())'` |
| `DJANGO_SECRET_KEY` | Yes | Django secret key - use a long random string |
| `GOOGLE_OAUTH_CLIENT_ID` | No | For Google OAuth login |
| `GOOGLE_OAUTH_CLIENT_SECRET` | No | For Google OAuth login |

3. Create or rebuild your Codespace - secrets will be automatically injected as environment variables.

### Sync Local Env to GitHub Codespaces

Sync your local environment secrets to GitHub Codespaces:

```bash
gh secret set OPENROUTER_API_KEY --user --body "$OPENROUTER_API_KEY"
gh secret set GOOGLE_OAUTH_CLIENT_ID --user --body "$GOOGLE_OAUTH_CLIENT_ID"
gh secret set GOOGLE_OAUTH_CLIENT_SECRET --user --body "$GOOGLE_OAUTH_CLIENT_SECRET"
```

**Note:** For Google OAuth in Codespaces, add your Codespace URL to Google Cloud Console once per Codespace:

```bash
# Print the redirect URI to add to Google Console
echo $GOOGLE_OAUTH_REDIRECT_URI
```

Add this URL to [Google Cloud Console → Credentials](https://console.cloud.google.com/apis/credentials) → your OAuth 2.0 Client → **Authorized redirect URIs**.

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
