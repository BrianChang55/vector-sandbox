# Testing Patterns

**Analysis Date:** 2026-01-14

## Test Framework

**Backend Runner:**
- Django TestCase (`django.test`) - Primary framework
- pytest - Compatible, configured in `conftest.py`
- Config: `conftest.py` in project root

**Assertion Library:**
- Django `self.assertEqual`, `self.assertIn`, etc.
- pytest `assert` statements also work

**Run Commands:**
```bash
# Activate venv first
source venv/bin/activate

# Run all tests
python manage.py test

# Run specific test file
python manage.py test vector_app.tests.test_views

# Run specific test class/method
python manage.py test vector_app.tests.test_views.UserViewSetTestCase.test_create_user

# With pytest (if installed)
pytest
pytest vector_app/tests/test_integration.py
```

**Frontend:**
- No test framework configured
- ESLint and TypeScript strict mode for static analysis
- Build-time type checking: `npm run build`

## Test File Organization

**Backend Location:**
- App tests: `vector_app/tests/test_*.py`
- Root tests: `tests/test_*.py`
- No separate tests/ tree mirroring source

**Naming:**
- All tests: `test_*.py` (e.g., `test_integration.py`, `test_adapters.py`)
- Test classes: `*TestCase` (e.g., `IntegrationTestCase`)
- Test methods: `test_*` (e.g., `test_create_organization`)

**Structure:**
```
internal-apps-backend/
├── vector_app/
│   └── tests/
│       ├── test_adapters.py
│       ├── test_app_data_store.py
│       ├── test_diff_application_service.py
│       ├── test_agent_optimizations.py
│       ├── test_ai_generation.py
│       ├── test_integration.py
│       ├── test_publish.py
│       ├── test_schema_locking.py
│       ├── test_typescript_types_generator.py
│       └── test_version_stability.py
│
└── tests/
    └── test_organization_logo_api.py
```

## Test Structure

**Suite Organization:**
```python
from django.test import TestCase, Client
from django.contrib.auth import get_user_model

User = get_user_model()

class IntegrationTestCase(TestCase):
    """End-to-end integration tests."""

    def setUp(self):
        """Set up test data."""
        self.client = Client()
        self.user = User.objects.create_user(
            username='testuser',
            email='test@example.com',
            password='testpass123'
        )
        self.client.force_login(self.user)

    def test_create_organization(self):
        """Test creating an organization."""
        response = self.client.post(
            '/api/v1/orgs/',
            data=json.dumps({'name': 'New Org', 'slug': 'new-org'}),
            content_type='application/json',
        )
        self.assertEqual(response.status_code, 201)
        data = json.loads(response.content)
        self.assertEqual(data['name'], 'New Org')
```

**Patterns:**
- `setUp()` for per-test setup
- `tearDown()` for cleanup (when needed)
- Descriptive docstrings on test methods
- One assertion focus per test

## Mocking

**Framework:**
- `unittest.mock` (Python standard library)
- `MagicMock` for complex mocks

**Patterns:**
```python
from unittest.mock import MagicMock, patch

class AdapterTestCase(TestCase):
    def test_database_connection(self):
        # Mock the connection and cursor
        mock_cursor = MagicMock()
        mock_cursor.fetchone.return_value = ("PostgreSQL 14.11",)
        mock_conn = MagicMock()
        mock_conn.cursor.return_value = mock_cursor

        # Test with mocked connection
        with patch('psycopg2.connect', return_value=mock_conn):
            result = adapter.get_version()
            self.assertEqual(result, "PostgreSQL 14.11")
```

**What to Mock:**
- External APIs (OpenRouter, OpenAI)
- Database connections (for adapter tests)
- File system operations
- Time/dates

**What NOT to Mock:**
- Django ORM (use TestCase with test database)
- Internal pure functions
- Business logic (test the real implementation)

## Fixtures and Factories

**Test Data Pattern:**
```python
def setUp(self):
    """Set up test data."""
    self.user = User.objects.create_user(
        username='testuser',
        email='test@example.com',
        password='testpass123'
    )
    self.org = Organization.objects.create(
        name='Test Org',
        slug='test-org'
    )
    UserOrganization.objects.create(
        user=self.user,
        organization=self.org,
        role=UserOrganization.ROLE_ADMIN
    )
```

**Location:**
- Test data created in `setUp()` methods
- No separate fixtures directory
- Complex fixtures inline in test file

## Coverage

**Requirements:**
- No enforced coverage target
- Coverage tracked for awareness
- Focus on critical paths (services, views)

**Tools:**
- Not currently configured
- Can add `pytest-cov` for coverage reports

**View Coverage:**
```bash
# If pytest-cov installed
pytest --cov=vector_app --cov-report=html
open htmlcov/index.html
```

## Test Types

**Unit Tests:**
- Test single function/class in isolation
- Mock external dependencies
- Fast: each test <1s
- Examples: `test_adapters.py`, `test_diff_application_service.py`

**Integration Tests:**
- Test multiple modules together
- Use Django test client for API tests
- Real database (test DB)
- Examples: `test_integration.py`

**Manual Test Scripts:**
- `tests/test_organization_logo_api.py` - Custom test runner pattern
- `scripts/test_data_store_e2e.py` - E2E data store testing

**E2E Tests:**
- Not currently implemented
- CLI integration tested manually

## Common Patterns

**API Endpoint Testing:**
```python
def test_create_organization(self):
    """Test creating an organization."""
    response = self.client.post(
        '/api/v1/orgs/',
        data=json.dumps({
            'name': 'New Org',
            'slug': 'new-org',
        }),
        content_type='application/json',
    )
    self.assertEqual(response.status_code, 201)
    data = json.loads(response.content)
    self.assertEqual(data['name'], 'New Org')
```

**Authenticated Requests:**
```python
def setUp(self):
    self.client = Client()
    self.user = User.objects.create_user(...)
    self.client.force_login(self.user)  # Authenticate for all tests
```

**Error Testing:**
```python
def test_invalid_input(self):
    """Test validation error response."""
    response = self.client.post(
        '/api/v1/orgs/',
        data=json.dumps({'name': ''}),  # Invalid: empty name
        content_type='application/json',
    )
    self.assertEqual(response.status_code, 400)
```

## Pytest Configuration

**Location:** `conftest.py`

```python
import os
import django

def pytest_configure():
    os.environ.setdefault("DJANGO_SETTINGS_MODULE", "internal_apps.settings")
    django.setup()
```

- Django settings: `internal_apps.settings`
- Pytest cache: `.pytest_cache/`
- No pytest.ini (configuration via conftest.py)

## Static Analysis Tools

**Python:**
- `black` 24.10.0 - Code formatter
- `isort` 5.13.2 - Import sorter
- `mypy` 1.13.0 - Type checker
- `prospector` 1.12.0 - Linter suite

**TypeScript:**
- ESLint 9.39.1 - Linter
- TypeScript 5.9.3 - Type checker (strict mode)

**Commands:**
```bash
# Python
make format       # Format changed files
make lint         # Lint changed files
make static       # Type check changed files
make check        # All checks

# TypeScript
npm run lint      # ESLint
npm run build     # Type check via tsc
```

---

*Testing analysis: 2026-01-14*
*Update when test patterns change*
