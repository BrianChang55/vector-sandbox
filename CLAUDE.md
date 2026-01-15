# Vector - Internal Apps Platform

AI-powered application builder for internal business tools.

## Project Structure

- `apps/backend/` - Django + DRF backend (Python 3.11, uv)
- `apps/web/` - React + TypeScript frontend (Node 24, Vite)

## Quick Start

```bash
mise run setup      # Install dependencies
mise run dev        # Start all services
mise run fe         # Frontend only (port 5176)
mise run be         # Backend only (port 8001)
```

## Agent Guidelines

For detailed context:
- Backend patterns: see `apps/backend/CLAUDE.md`
- Frontend patterns: see `apps/web/CLAUDE.md`

## Anti-Over-Engineering Rules

- Only implement what is explicitly requested
- No error handling for impossible scenarios
- No helpers/utilities for one-time operations
- Reuse existing code (DRY principle)
