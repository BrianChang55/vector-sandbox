# Technology Stack

## Languages & Runtime

| Technology | Version | Purpose |
|------------|---------|---------|
| Python | 3.13.5 | Backend runtime |
| Django | 4.2.x | Web framework |
| Django REST Framework | 3.15.x | API framework |

## Package Management

- **pip** - Python package manager
- **requirements.txt** - Main dependencies
- **requirements/static.txt** - Static analysis tools

## Database & ORM

| Technology | Purpose |
|------------|---------|
| PostgreSQL | Production database (via `DATABASE_URL`) |
| SQLite | Development default |
| MySQL | Supported via PyMySQL 1.1.0+ |
| Django ORM | Database abstraction |
| SQLAlchemy 2.0.0+ | Database introspection |

## Authentication

- **djangorestframework-simplejwt 5.3.x** - JWT token auth (7-day access, 30-day refresh)
- **Django Sessions** - 30-day expiry
- **Google OAuth 2.0** - Social login
- **Magic Link Auth** - Email-based passwordless

## Async Processing

| Technology | Purpose |
|------------|---------|
| Celery 5.3.0+ | Task queue |
| Redis 5.0.0+ | Message broker and result backend |

## AI/LLM Integration

- **OpenRouter** - Primary AI API via `vector_app/ai/client.py`
  - Claude Opus 4.5 (200K context)
  - Claude Sonnet 4.5 (200K context)
  - Claude Haiku 4.5 (200K context)
  - GPT-5.1 (128K context)
  - Gemini 3 Pro (1M context)
- **OpenAI** - Fallback provider

## HTTP Clients

| Library | Purpose |
|---------|---------|
| httpx 0.27.0+ | Async HTTP client for streaming |
| requests 2.32.x | Sync HTTP requests |

## Cloud Storage

- **boto3 1.34.0+** - S3-compatible client
- **Cloudflare R2** - Object storage (production)
- **Local filesystem** - Development fallback

## Image & Document Processing

| Library | Purpose |
|---------|---------|
| Pillow 12.1.x | Image processing |
| cairosvg 2.7.0+ | SVG to PNG conversion |
| ReportLab 4.0.0+ | PDF generation |
| python-pptx 0.6.21+ | PowerPoint generation |

## Security

| Library | Purpose |
|---------|---------|
| cryptography 41.0.0+ | Fernet encryption for secrets |
| django-cors-headers 4.0.0+ | CORS handling |
| WhiteNoise 6.6.0 | Static file serving |

## Monitoring & Analytics

| Service | Library | Purpose |
|---------|---------|---------|
| Sentry | sentry-sdk[django] 2.0.0+ | Error tracking |
| Mixpanel | mixpanel 5.0.0 | Product analytics |

## Development Tools

| Tool | Version | Purpose |
|------|---------|---------|
| black | 24.10.0 | Code formatting |
| isort | 5.13.2 | Import sorting |
| mypy | 1.13.0 | Type checking |
| prospector | 1.12.0 | Linting |
| django-stubs | 5.1.1 | Django type stubs |
| djangorestframework-stubs | 3.15.1 | DRF type stubs |

## Utilities

| Library | Purpose |
|---------|---------|
| python-dotenv 1.0.x | Environment variables |
| django-environ 0.11.x | Django env handling |
| jsonschema 4.20.0+ | JSON schema validation |
| whatthepatch 1.0.7 | Unified diff parsing |
| django-extensions 3.2.0+ | Enhanced Django shell |
| IPython 8.12.0+ | Interactive shell |

## External APIs

| Integration | Library |
|-------------|---------|
| Merge Agent Handler | Custom service (`merge_service.py`) |
| Supabase | supabase 2.0.0+, postgrest 0.13.0+ |
