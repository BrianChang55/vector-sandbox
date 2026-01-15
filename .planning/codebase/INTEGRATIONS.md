# External Integrations

## AI/LLM Services

### OpenRouter (Primary)

Primary AI API supporting multiple models.

| File | Purpose |
|------|---------|
| `vector_app/ai/client.py` | LLMClient class |
| `vector_app/ai/models.py` | AIModel enum |
| `vector_app/ai/types.py` | LLMSettings, ChatResult |
| `vector_app/services/openrouter_service.py` | OpenRouter API integration |

**Models Supported:**
- Claude Opus 4.5 (200K context)
- Claude Sonnet 4.5 (200K context)
- Claude Haiku 4.5 (200K context)
- GPT-5.1 (128K context)
- Gemini 3 Pro (1M context)

**Configuration:**
- `OPENROUTER_API_KEY`
- `OPENROUTER_APP_NAME`

**Features:** Streaming support, JSON mode, multi-model routing

### OpenAI (Fallback)

Fallback if OpenRouter not configured.

**Configuration:** `OPENAI_API_KEY`

## Cloud Storage

### Cloudflare R2

S3-compatible object storage for production.

| File | Purpose |
|------|---------|
| `vector_app/services/cloud_storage_service.py` | Storage abstraction |
| `vector_app/services/image_upload_service.py` | Image uploads |

**Library:** boto3 1.34.0+ (S3-compatible)

**Configuration:**
- `R2_ACCESS_KEY_ID`
- `R2_SECRET_ACCESS_KEY`
- `R2_BUCKET_NAME`
- `R2_ENDPOINT_URL`
- `R2_PRESIGNED_URL_EXPIRY` (1 hour default)
- `R2_PRESIGNED_URL_EXPIRY_API` (24 hours default)

**Features:** Private bucket with presigned URLs, local fallback

## Error Tracking & Monitoring

### Sentry

Error tracking and performance monitoring.

**Library:** sentry-sdk[django] 2.0.0+

**Configuration:**
- `SENTRY_DSN`
- `SENTRY_ENVIRONMENT`

**Location:** `internal_apps/settings.py` (lines 325-356)

## Product Analytics

### Mixpanel

Product and behavioral analytics.

**Library:** mixpanel 5.0.0

**Configuration:** `MIXPANEL_TOKEN`

## Email Services

### Resend

Email delivery via SMTP.

**Configuration:**
- `EMAIL_HOST=smtp.resend.com`
- `EMAIL_PORT=465`
- `EMAIL_HOST_PASSWORD` (Resend API key)
- `EMAIL_HOST_USER=resend`
- `DEFAULT_FROM_EMAIL`

## Async Task Processing

### Celery

Distributed task queue.

**Configuration:** `internal_apps/settings.py` (lines 359-383)

| Setting | Value |
|---------|-------|
| Broker | Redis (`CELERY_BROKER_URL`) |
| Result Backend | Redis (`CELERY_RESULT_BACKEND`) |
| Serialization | JSON |
| Task Timeout | 30 minutes |

**Default:** `redis://localhost:6379/0`

## API Integrations

### Merge Agent Handler API

Third-party API connector platform.

| File | Purpose |
|------|---------|
| `vector_app/services/merge_service.py` | API client |

**Configuration:**
- `MERGE_TOOL_PACK_ID`
- `MERGE_ACCESS_KEY`
- `MERGE_API_TIMEOUT` (30 seconds default)

**API Docs:** https://docs.ah.merge.dev/api-reference/overview

**Features:**
- Connector detection (Linear, Jira, Slack, etc.)
- Tool action execution
- Live MCP tool fetching

**Models:** `MergeIntegrationProvider`, `ConnectorCache`, `ConnectorToolAction`

### Database Adapters

Multi-database support via adapters.

| File | Database |
|------|----------|
| `vector_app/adapters/postgresql.py` | PostgreSQL |
| `vector_app/adapters/mysql.py` | MySQL |
| `vector_app/adapters/supabase.py` | Supabase |

**Libraries:**
- supabase 2.0.0+
- postgrest 0.13.0+

## Authentication

### Google OAuth 2.0

Social login integration.

**Callback:** `/api/v1/auth/google/callback`

**Configuration:**
- `GOOGLE_OAUTH_CLIENT_ID`
- `GOOGLE_OAUTH_CLIENT_SECRET`
- `GOOGLE_OAUTH_REDIRECT_URI`

## Environment Configuration

### Backend (.env.example)

```bash
# Django
DJANGO_SECRET_KEY=
DEBUG=

# Database
DATABASE_URL=

# AI/LLM
OPENROUTER_API_KEY=
OPENROUTER_APP_NAME=
OPENAI_API_KEY=

# Storage
R2_ACCESS_KEY_ID=
R2_SECRET_ACCESS_KEY=
R2_BUCKET_NAME=
R2_ENDPOINT_URL=
ENCRYPTION_KEY=

# Email
EMAIL_HOST=
EMAIL_PORT=
EMAIL_HOST_PASSWORD=
EMAIL_HOST_USER=
DEFAULT_FROM_EMAIL=

# OAuth
GOOGLE_OAUTH_CLIENT_ID=
GOOGLE_OAUTH_CLIENT_SECRET=
GOOGLE_OAUTH_REDIRECT_URI=

# Celery/Redis
CELERY_BROKER_URL=
CELERY_RESULT_BACKEND=

# Monitoring
SENTRY_DSN=
SENTRY_ENVIRONMENT=
MIXPANEL_TOKEN=

# Merge
MERGE_TOOL_PACK_ID=
MERGE_ACCESS_KEY=

# Deployment
RENDER_EXTERNAL_HOSTNAME=
API_DOMAIN=
```

## Infrastructure Notes

- **Development Server:** Port 8001
- **CORS:** Configured for multiple dev ports + production domains
- **Media Storage:** Local development with `MEDIA_ROOT` fallback, R2 for production
