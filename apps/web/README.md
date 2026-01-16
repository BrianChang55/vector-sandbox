# Vector Web App

React frontend for the Vector platform.

## Quick Start

From the **repository root** (not this directory):

```bash
# Install dependencies
mise run setup

# Start all services (docker, frontend, backend, celery, flower)
mise run dev
```

The app will be available at http://localhost:5176

## Prerequisites

- [mise](https://mise.run) - Runtime manager

## Development Commands

From the repository root:

| Command | Description |
|---------|-------------|
| `mise run dev` | Start all services |
| `mise run fe` | Frontend only (port 5176) |

## Running Frontend Only

If you need to run just the frontend (backend already running):

```bash
cd apps/web
npm install
npm run dev
```

## Git Hooks

This repository includes Git hooks to enforce code quality.

### Available Hooks

| Hook | Description |
|------|-------------|
| `pre-push` | Runs `npm ci` and `npm run build` before pushing. Blocks push if build fails. |
| `pre-commit` | Currently empty. Can be extended for linting, formatting, etc. |

### Installing Hooks

```bash
git config core.hooksPath hooks
```

Or manually:

```bash
cp hooks/pre-push .git/hooks/pre-push
chmod +x .git/hooks/pre-push
```

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
