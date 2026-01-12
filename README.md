# Vector Web App

## Getting Started

To run the app from scratch:

1. Install dependencies:
   ```bash
   npm install
   ```

2. Start the development server:
   ```bash
   npm run dev
   ```

The app will be available at `http://localhost:5176`.

## Git Hooks

This repository includes Git hooks to enforce code quality and prevent broken code from being committed or pushed.

### Available Hooks

#### Pre-push Hook

The `pre-push` hook runs before code is pushed to the remote repository. It performs a build gate check to ensure that:
- Dependencies are installed correctly (`npm ci`)
- The project builds successfully (`npm run build`)

If either step fails, the push is blocked and the error output is displayed. This prevents broken code from being pushed to the repository.

#### Pre-commit Hook

The `pre-commit` hook runs before code is committed to the repository. Currently empty, but can be extended with custom checks such as:
- Linting (`npm run lint`)
- Formatting checks
- Unit tests
- Type checking

### Installation

To install the Git hooks, you have two options:

#### Option 1: Copy hooks manually

```bash
cp hooks/pre-push .git/hooks/pre-push
cp hooks/pre-commit .git/hooks/pre-commit
chmod +x .git/hooks/pre-push
chmod +x .git/hooks/pre-commit
```

#### Option 2: Use Git hooks path (recommended)

Configure Git to use the hooks directory directly:

```bash
git config core.hooksPath hooks
```

This way, the hooks will be automatically used from the `hooks/` directory without needing to copy them.

### Hook Files

- `hooks/pre-push` - Build gate check before pushing
- `hooks/pre-commit` - Pre-commit checks (currently empty)
