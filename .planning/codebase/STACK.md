# Technology Stack

**Analysis Date:** 2026-01-14

## Languages

**Primary:**
- Python 3.13.5 - All backend application code - `manage.py`
- TypeScript 5.9.3 - All frontend application code - `../internal-apps-web-app/package.json`

**Secondary:**
- JavaScript (ES2022) - Target compilation for frontend - `../internal-apps-web-app/tsconfig.app.json`

## Runtime

**Environment:**
- Python 3.13+ - Backend runtime
- Node.js - Frontend runtime (lockfile present)

**Package Manager:**
- pip - Python package manager - `requirements.txt`
- npm - Frontend package manager - `../internal-apps-web-app/package.json`
- Lockfile: `package-lock.json` present for frontend

## Frameworks

**Core:**
- Django 4.2.x - Backend REST API framework - `requirements.txt`
- Django REST Framework 3.15.x - API serialization and viewsets - `requirements.txt`
- React 19.2.0 - Frontend UI framework - `../internal-apps-web-app/package.json`
- React Router 7.10.1 - Client-side routing - `../internal-apps-web-app/package.json`

**Testing:**
- Django TestCase - Built-in Django testing framework
- pytest - Python testing (via `conftest.py` configuration)
- ESLint - Frontend static analysis (no test framework configured)

**Build/Dev:**
- Vite 7.2.4 - Frontend build tool and dev server - `../internal-apps-web-app/vite.config.ts`
- TypeScript 5.9.3 - Type checking and compilation
- Celery 5.3.0+ - Async task queue - `requirements.txt`

## Key Dependencies

**Critical (Backend):**
- djangorestframework-simplejwt 5.3.x - JWT authentication - `requirements.txt`, `internal_apps/settings.py`
- httpx 0.27.0+ - HTTP client for streaming - `requirements.txt`
- boto3 1.34.0+ - S3-compatible storage (Cloudflare R2) - `requirements.txt`
- cryptography 41.0.0+ - Encryption for secrets - `requirements.txt`
- Pillow 12.1.x - Image processing - `requirements.txt`

**Critical (Frontend):**
- Redux Toolkit 2.11.0 - Client-side UI state - `../internal-apps-web-app/package.json`
- TanStack React Query 5.59.0 - Server state and caching - `../internal-apps-web-app/package.json`
- Axios 1.13.2 - HTTP client library - `../internal-apps-web-app/package.json`

**UI Components:**
- Radix UI - Headless component library (accordion, dialog, dropdown, etc.)
- Tailwind CSS 3.4.18 - Utility-first CSS framework
- Tailwind Merge 3.4.0 - Utility class merging
- Class Variance Authority 0.7.1 - Component variant system
- Lucide React 0.556.0 - Icon library
- Framer Motion 12.23.25 - Animation library

**Infrastructure:**
- Redis 5.0.0+ - Message broker for Celery - `requirements.txt`
- PostgreSQL - Production database (SQLite for development) - `internal_apps/settings.py`
- WhiteNoise 6.6.0 - Static file serving - `requirements.txt`

**Code Editors & Development:**
- @codesandbox/sandpack-react 2.19.8 - Code editor & execution
- @monaco-editor/react 4.6.0 - Monaco editor integration

## Configuration

**Environment:**
- python-dotenv 1.0.x - Environment variable management - `requirements.txt`
- django-environ 0.11.x - Django-specific environment handling - `requirements.txt`
- Vite environment variables - Prefixed with `VITE_` - `../internal-apps-web-app/src/services/api.ts`
- `.env` files - Local configuration (gitignored), `.env.example` files present

**Build:**
- `vite.config.ts` - Vite bundler configuration
- `tsconfig.json`, `tsconfig.app.json` - TypeScript configuration
- `tailwind.config.js` - Tailwind CSS configuration
- `internal_apps/settings.py` - Django settings

## Platform Requirements

**Development:**
- macOS/Linux/Windows (any platform with Python and Node.js)
- Redis for Celery task queue
- PostgreSQL recommended (SQLite works for simple testing)

**Production:**
- Render (detected by `RENDER_EXTERNAL_HOSTNAME`, `RENDER_DISK_MOUNT_PATH`)
- Custom domains supported via `API_DOMAIN`
- Cloudflare R2 for file storage

**Ports:**
- Frontend dev server: 5176
- Backend dev server: 8001
- Redis: 6379

---

*Stack analysis: 2026-01-14*
*Update after major dependency changes*
