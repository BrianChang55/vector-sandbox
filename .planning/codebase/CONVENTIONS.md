# Coding Conventions

**Analysis Date:** 2026-01-14

## Naming Patterns

**Files:**
- Python: snake_case for all files (`user_service.py`, `auth_views.py`)
- TypeScript components: PascalCase (`Button.tsx`, `AgenticChatPanel.tsx`)
- TypeScript services/hooks: camelCase (`apiService.ts`, `useApps.ts`)
- Test files: `test_*.py` (Python), none yet (frontend)

**Functions:**
- Python: snake_case (`get_user_profile`, `create_organization`)
- TypeScript: camelCase (`getRolePermissions`, `createOrganization`)
- Async: No special prefix for async functions
- Handlers: `handle{Event}` for event handlers

**Variables:**
- Python: snake_case for variables, UPPER_SNAKE_CASE for constants
- TypeScript: camelCase for variables, UPPER_SNAKE_CASE for constants
- Private: `_leading_underscore` in Python (no underscore in TS)

**Types:**
- Python classes: PascalCase (`UserService`, `Organization`)
- TypeScript interfaces: PascalCase, no `I` prefix (`User`, `Organization`)
- TypeScript types: PascalCase (`UserConfig`, `ResponseData`)
- Enums: PascalCase name, UPPER_CASE values

## Code Style

**Python Formatting:**
- Formatter: `black` (line length 110) - `requirements/static.txt`
- Import sorter: `isort` (profile: black) - `requirements/static.txt`
- Configuration: `Makefile` commands (`make format`, `make format-all`)
- Indentation: 4 spaces (PEP 8)
- Quotes: Double quotes (enforced by Black)
- Line length: Max 110 characters

**TypeScript Formatting:**
- ESLint config: `eslint.config.js`
- TypeScript strict mode: Enabled (`tsconfig.app.json`)
- Indentation: 2 spaces
- Quotes: Single quotes (React convention)
- Semicolons: Not required

**Linting:**
- Python: `prospector` 1.12.0 with pylint, pyflakes, pycodestyle - `prospector.yml`
- Python type checking: `mypy` 1.13.0 with Django stubs - `mypy.ini`
- TypeScript: ESLint 9.39.1 with TypeScript-ESLint recommended
- Commands: `make lint` (Python), `npm run lint` (TypeScript)

## Import Organization

**Python:**
1. Standard library imports
2. Third-party library imports
3. Local imports (from internal_apps)
4. Relative imports

**TypeScript:**
1. React and React-related imports
2. Third-party library imports
3. Internal imports (components, hooks, utils, types)
4. Relative imports (same directory)
5. CSS/asset imports

**Path Aliases:**
- TypeScript: `@/` maps to `src/` (configured in `tsconfig.json`)

## Error Handling

**Python Patterns:**
- Services throw custom exceptions with context
- Views catch and return appropriate HTTP responses
- Use specific exception types where possible
- Log error with context before throwing

**TypeScript Patterns:**
- try/catch at service boundaries
- Toast notifications for user-facing errors
- Error boundary components for React crashes

**Error Types:**
- Throw on: invalid input, missing dependencies, invariant violations
- Log with context: `logger.error({ context }, 'Message')`

## Logging

**Python Framework:**
- Python `logging` module
- Configured per-module: `logger = logging.getLogger(__name__)`
- Levels: DEBUG, INFO, WARNING, ERROR

**TypeScript Framework:**
- `loggingService.ts` with Sentry integration
- Levels: debug, info, warn, error
- Production: Sentry capture, development: console

**Patterns:**
- Log at service boundaries
- Include context objects in logs
- No `print()` statements in committed code

## Comments

**When to Comment:**
- Explain why, not what
- Document business rules and edge cases
- Explain non-obvious algorithms
- Avoid obvious comments

**Python Docstrings:**
- Google/NumPy style (Args/Returns/Raises sections)
- Required for public functions and classes
- Module docstrings at top of file

**TypeScript JSDoc:**
- JSDoc for complex public functions
- Optional for internal functions with clear signatures
- Use `@param`, `@returns`, `@throws` tags

**TODO Comments:**
- Format: `# TODO: description` (Python), `// TODO: description` (TS)
- Link to issue if exists: `# TODO: Fix race condition (issue #123)`

## Function Design

**Size:**
- Keep under 50 lines where practical
- Extract helpers for complex logic
- One level of abstraction per function

**Parameters:**
- Max 3-4 parameters
- Use options object/dict for more
- Type hints required on all Python functions

**Return Values:**
- Explicit return statements
- Return early for guard clauses
- Consistent return types

## Module Design

**Python:**
- One class per file for large classes
- Related functions can share a file
- `__init__.py` for package exports

**TypeScript:**
- Named exports preferred
- Default exports only for React components/pages
- Barrel exports from index.ts where appropriate

**ViewSets (DRF):**
- Inherit `ModelViewSet` for CRUD
- Custom actions via `@action` decorator
- Override `get_queryset()` for filtering

**Services:**
- Stateless functions or classes
- Factory getters: `get_service_name()`
- Business logic only, no HTTP concerns

---

*Convention analysis: 2026-01-14*
*Update when patterns change*
