# Internal Apps Web App

React frontend for the Internal Apps platform.

## Quick Start

> **Note:** This frontend requires the backend to be running. For the full development setup, see the **[Backend README](../internal-apps-backend/README.md)**.

### Full Stack Setup (Recommended)

If you haven't set up the project yet, go to the backend repo and run:

```bash
cd ../internal-apps-backend
./scripts/setup_localdev.sh
```

This sets up both frontend and backend automatically.

### Starting Development

From the workspace root (`internal-apps/`):

```bash
./dev.sh --start
```

This starts Django, Celery, and the React frontend with:
- Hot module replacement (HMR) for instant updates
- Auto-login with a dev user
- Chrome opens automatically

---

## Development Commands

From the workspace root:

```bash
./dev.sh --start    # Start all services + auto-login
./dev.sh --stop     # Stop all services
./dev.sh --restart  # Restart all services
./dev.sh --logs     # View live logs
```

### Viewing Frontend Logs

```bash
./dev.sh --logs                # All logs
tail -f ../logs/frontend.log   # Frontend only
```

---

## Running Frontend Only

If you just need to run the frontend (backend already running elsewhere):

```bash
npm install
npm run dev
```

The app will be available at `http://localhost:5176`.

---

## Git Hooks

This repository includes Git hooks to enforce code quality.

### Available Hooks

| Hook | Description |
|------|-------------|
| `pre-push` | Runs `npm ci` and `npm run build` before pushing. Blocks push if build fails. |
| `pre-commit` | Currently empty. Can be extended for linting, formatting, etc. |

### Installing Hooks

**Option 1: Configure Git hooks path (recommended)**

```bash
git config core.hooksPath hooks
```

**Option 2: Copy hooks manually**

```bash
cp hooks/pre-push .git/hooks/pre-push
cp hooks/pre-commit .git/hooks/pre-commit
chmod +x .git/hooks/pre-push .git/hooks/pre-commit
```

---

## Project Structure

```
src/
├── components/     # Reusable UI components
├── pages/          # Page components (routes)
├── hooks/          # Custom React hooks
├── services/       # API service functions
├── store/          # Redux store and slices
├── types/          # TypeScript type definitions
└── utils/          # Utility functions
```
