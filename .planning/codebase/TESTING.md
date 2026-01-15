# Testing

## Test Framework

- **Primary**: Django TestCase (`django.test`)
- **Alternative**: Pytest (compatible via `conftest.py`)
- **Configuration**: `conftest.py`

## Test Locations

| Type | Location |
|------|----------|
| Unit tests | `vector_app/tests/` |
| Integration tests | `vector_app/tests/test_integration.py` |
| Root tests | `test_*.py` in root directory |

## Running Tests

```bash
# Activate virtual environment first
source venv/bin/activate

# Run all tests
python manage.py test

# Run specific test file
python manage.py test vector_app.tests.test_views

# Run specific test class/method
python manage.py test vector_app.tests.test_views.UserViewSetTestCase.test_create_user
```

## Test Structure

### Class-Based Tests

```python
from django.test import TestCase, Client
from vector_app.models import Organization, User

class OrganizationTestCase(TestCase):
    """Test organization CRUD operations."""

    def setUp(self):
        """Set up test data."""
        self.client = Client()
        self.user = User.objects.create_user(
            username='testuser',
            email='test@example.com',
            password='testpass123'
        )
        self.org = Organization.objects.create(name='Test Org', slug='test-org')
        self.client.force_login(self.user)

    def test_create_organization(self):
        """Test creating an organization."""
        response = self.client.post(
            '/api/v1/orgs/',
            data={'name': 'New Org', 'slug': 'new-org'},
            content_type='application/json',
        )
        self.assertEqual(response.status_code, 201)
```

### Test Naming

- File: `test_*.py`
- Class: `{Feature}TestCase`
- Methods: `test_{action}_{expected_result}`

## Existing Test Files

| File | Purpose |
|------|---------|
| `test_integration.py` | End-to-end API workflows |
| `test_adapters.py` | Database adapter functionality |
| `test_diff_application_service.py` | Diff application logic |
| `test_agent_optimizations.py` | Agent optimization |
| `test_ai_generation.py` | AI generation |
| `test_publish.py` | Publishing functionality |
| `test_schema_locking.py` | Schema locking |
| `test_typescript_types_generator.py` | Type generation |
| `test_version_stability.py` | Version stability |

## Test Patterns

### Setup Pattern

```python
def setUp(self):
    """Set up test data."""
    self.client = Client()
    self.user = User.objects.create_user(...)
    self.org = Organization.objects.create(...)
    UserOrganization.objects.create(
        user=self.user,
        organization=self.org,
        role=UserOrganization.ROLE_ADMIN
    )
    self.client.force_login(self.user)
```

### API Test Pattern

```python
def test_create_resource(self):
    """Test creating a resource."""
    response = self.client.post(
        '/api/v1/endpoint/',
        data=json.dumps({'key': 'value'}),
        content_type='application/json',
    )
    self.assertEqual(response.status_code, 201)
    data = json.loads(response.content)
    self.assertEqual(data['key'], 'value')
```

### Mocking External Services

```python
from unittest.mock import MagicMock, patch

def test_with_mock_ai(self):
    """Test with mocked AI service."""
    mock_response = MagicMock()
    mock_response.content = '{"result": "success"}'

    with patch('vector_app.ai.client.get_llm_client') as mock_client:
        mock_client.return_value.run.return_value = mock_response
        # Test code here
```

## Pytest Configuration

```python
# conftest.py
def pytest_configure():
    os.environ.setdefault("DJANGO_SETTINGS_MODULE", "internal_apps.settings")
    django.setup()
```

## Static Analysis

| Tool | Command | Config |
|------|---------|--------|
| Formatter | `make format` | black + isort |
| Linting | `make lint` | `prospector.yml` |
| Type checking | `make static` | `mypy.ini` |

## Type Checking Configuration

```ini
# mypy.ini
[mypy]
python_version = 3.13
strict_equality = True
warn_return_any = True
plugins = mypy_django_plugin.main, mypy_drf_plugin.main
```

## Linting Configuration

```yaml
# prospector.yml
strictness: high
max-line-length: 110

mccabe:
  max-complexity: 15
```

## Pre-Commit Checks

Run before committing:

```bash
make check  # Format check + lint + type check
```
