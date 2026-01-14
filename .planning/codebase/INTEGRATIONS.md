# External Integrations

**Analysis Date:** 2026-01-14

## APIs & External Services

**AI/LLM Services:**

- **OpenRouter** - Primary AI API supporting multiple models
  - Service File: `vector_app/services/openrouter_service.py`
  - Client File: `vector_app/ai/client.py`
  - Models: Claude Opus 4.5, Claude Sonnet 4.5, Claude Haiku 4.5, GPT-5.1, Gemini 3 Pro
  - Config: `OPENROUTER_API_KEY`, `OPENROUTER_APP_NAME` - `internal_apps/settings.py`
  - Features: Streaming support, JSON mode, multi-model routing

- **OpenAI** - Fallback LLM provider
  - Config: `OPENAI_API_KEY` - `internal_apps/settings.py`
  - Used as: Fallback if OpenRouter not configured

**Email Services:**

- **Resend** - Email delivery service
  - Config: SMTP-based configuration
    - `EMAIL_HOST=smtp.resend.com`
    - `EMAIL_PORT=465`
    - `EMAIL_HOST_PASSWORD` (Resend API key)
    - `EMAIL_HOST_USER=resend`
    - `DEFAULT_FROM_EMAIL`
  - Details: `.env.example`

## Data Storage

**Databases:**

- **PostgreSQL** - Production database
  - Connection: `DATABASE_URL` env var
  - Client: Django ORM
  - Config: `internal_apps/settings.py`

- **SQLite** - Development fallback
  - Connection: `db.sqlite3` file
  - Used when: No `DATABASE_URL` configured

**File Storage:**

- **Cloudflare R2** - S3-compatible object storage
  - Service File: `vector_app/services/cloud_storage_service.py`
  - Image Upload: `vector_app/services/image_upload_service.py`
  - SDK: boto3 1.34.0+ (S3-compatible)
  - Config:
    - `R2_ACCESS_KEY_ID`
    - `R2_SECRET_ACCESS_KEY`
    - `R2_BUCKET_NAME`
    - `R2_ENDPOINT_URL`
    - `R2_PRESIGNED_URL_EXPIRY` (1 hour default)
    - `R2_PRESIGNED_URL_EXPIRY_API` (24 hours default)
  - Features: Private bucket with presigned URLs, local fallback

**Caching:**

- **Redis** - Message broker and cache
  - Connection: `CELERY_BROKER_URL`, `CELERY_RESULT_BACKEND`
  - Default: `redis://localhost:6379/0`
  - Used for: Celery task queue, result backend

## Authentication & Identity

**Auth Provider:**

- **Django REST Framework SimpleJWT** - JWT authentication
  - Config: `internal_apps/settings.py`
  - Access token: 7-day expiry
  - Refresh token: 30-day expiry
  - Token storage: Frontend localStorage

**OAuth Integrations:**

- **Google OAuth 2.0** - Social login
  - Frontend Utility: `../internal-apps-web-app/src/utils/googleOAuth.ts`
  - Backend Callback: `/api/v1/auth/google/callback`
  - Config:
    - `GOOGLE_OAUTH_CLIENT_ID`
    - `GOOGLE_OAUTH_CLIENT_SECRET`
    - `GOOGLE_OAUTH_REDIRECT_URI`
  - Scopes: email, profile

**Magic Link Auth:**

- Email-based passwordless authentication
  - Service: `authService.ts`
  - Token model: `MagicLinkToken` in `models.py`

## Monitoring & Observability

**Error Tracking:**

- **Sentry** - Error tracking and performance
  - Backend: sentry-sdk[django] 2.0.0+ - `requirements.txt`
  - Frontend: @sentry/react 8.0.0 - `../internal-apps-web-app/package.json`
  - Backend config: `internal_apps/settings.py` (lines 325-356)
  - Frontend service: `../internal-apps-web-app/src/services/loggingService.ts`
  - Config:
    - `SENTRY_DSN` (backend)
    - `VITE_SENTRY_DSN` (frontend)
    - `SENTRY_ENVIRONMENT` / `VITE_SENTRY_ENVIRONMENT`
  - Features: Error capture, performance profiling (10% sample), session replays

**Analytics:**

- **Mixpanel** - Product analytics
  - Backend: mixpanel 5.0.0 - `requirements.txt`
  - Frontend: mixpanel-browser 2.73.0 - `../internal-apps-web-app/package.json`
  - Frontend service: `../internal-apps-web-app/src/services/analyticsService.ts`
  - Config:
    - `MIXPANEL_TOKEN` (backend)
    - `VITE_MIXPANEL_TOKEN` (frontend)
  - Features: User identification, event tracking, session analytics

**Logs:**

- **Vercel logs** - stdout/stderr (production)
- **Python logging** - Backend structured logging
- **Console + Sentry** - Frontend logging

## CI/CD & Deployment

**Hosting:**

- **Render** - Backend hosting
  - Detection: `RENDER_EXTERNAL_HOSTNAME`, `RENDER_DISK_MOUNT_PATH`
  - Deployment: Automatic on push
  - Env vars: Configured in Render dashboard

**Frontend:**

- Deployment target not explicitly configured in codebase
- Build: `npm run build` produces static files

## Environment Configuration

**Development:**

- Required backend env vars:
  - `SECRET_KEY`
  - `DATABASE_URL` (optional, falls back to SQLite)
  - `OPENROUTER_API_KEY` or `OPENAI_API_KEY`
- Required frontend env vars:
  - `VITE_API_BASE_URL`
- Secrets location: `.env.local` (gitignored)
- Mock services: Local database, no external services required

**Staging:**

- Uses separate environment variables
- Same infrastructure, different credentials

**Production:**

- Secrets management: Render/Vercel environment variables
- Database: Production PostgreSQL
- Storage: Cloudflare R2
- Monitoring: Sentry with production DSN

## Webhooks & Callbacks

**Incoming:**

- None currently configured

**Outgoing:**

- None currently configured

## API Integrations & Connectors

**Merge Agent Handler API:**

- Service: `vector_app/services/merge_service.py`
- Purpose: Third-party API connector platform
- Config:
  - `MERGE_TOOL_PACK_ID`
  - `MERGE_ACCESS_KEY`
  - `MERGE_API_TIMEOUT` (30 seconds default)
- API Docs: https://docs.ah.merge.dev/api-reference/overview
- Features:
  - Connector detection (Linear, Jira, Slack, etc.)
  - Tool action execution
  - Live MCP tool fetching
- Models: `MergeIntegrationProvider`, `ConnectorCache`, `OrganizationConnectorLink`

**Database Adapters:**

- Location: `vector_app/adapters/`
- Supported:
  - PostgreSQL: `postgresql.py`
  - MySQL: `mysql.py`
  - Supabase: `supabase.py`
- Supabase client: supabase 2.0.0+, postgrest 0.13.0+

## Code Editors & Runtime

**Monaco Editor:**

- Package: @monaco-editor/react 4.6.0
- Used for: Code editing in IDE

**CodeSandbox Sandpack:**

- Package: @codesandbox/sandpack-react 2.19.8
- Used for: Code preview and execution
- CORS: Configured for codesandbox.io domains

## Third-Party SDKs

**Frontend:**

- **@pipedream/sdk** 2.3.5 - Workflow automation
- **@mergeapi/react-agent-handler-link** 0.0.5 - Merge integration UI

---

*Integration audit: 2026-01-14*
*Update when adding/removing external services*
