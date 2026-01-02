# Django Backend Style Guide

This document defines the coding standards, best practices, and patterns for the Internal Apps Django backend. **All developers and AI agents must refer to this guide when building features.**

## Table of Contents

1. [Technology Stack](#technology-stack)
2. [Project Structure](#project-structure)
3. [Python Style Guidelines](#python-style-guidelines)
4. [Django Best Practices](#django-best-practices)
5. [Models](#models)
6. [Views and Viewsets](#views-and-viewsets)
7. [Serializers](#serializers)
8. [Services](#services)
9. [Utilities](#utilities)
10. [Error Handling](#error-handling)
11. [Testing](#testing)
12. [Performance](#performance)
13. [Security](#security)

---

## Technology Stack

### Core Technologies
- **Python 3.10+** - Programming language
- **Django 4.2+** - Web framework
- **Django REST Framework** - API framework
- **PostgreSQL** (production) / **SQLite** (development) - Database
- **Celery** - Async task queue
- **Redis** - Cache and Celery broker

### Key Libraries
- `django-environ` - Environment variable management
- `djangorestframework-simplejwt` - JWT authentication
- `psycopg2-binary` - PostgreSQL adapter
- `whitenoise` - Static file serving
- `gunicorn` - WSGI server
- `sentry-sdk` - Error tracking

---

## Project Structure

```
internal_apps/
├── internal_apps/           # Django project settings
│   ├── settings.py
│   ├── urls.py
│   ├── wsgi.py
│   ├── asgi.py
│   ├── celery.py
│   └── utils/               # Shared utilities
│       ├── base_model.py
│       ├── decorators.py
│       └── enum.py
├── internal_apps_app/       # Django app (create when needed)
│   ├── models.py
│   ├── views/
│   ├── serializers/
│   ├── services/
│   ├── urls.py
│   ├── admin.py
│   └── migrations/
├── manage.py
└── requirements.txt
```

---

## Python Style Guidelines

### Code Style

- **Follow PEP 8** - Python style guide
- **Use type hints** - For function parameters and return values
- **Docstrings** - Use for classes, functions, and modules
- **Line length** - Maximum 100 characters (configurable)
- **Import order** - Standard library, third-party, local imports

### Naming Conventions

```python
# Classes: PascalCase
class UserProfile(BaseModel):
    pass

# Functions and variables: snake_case
def get_user_profile(user_id: int) -> User:
    pass

# Constants: UPPER_SNAKE_CASE
MAX_RETRY_ATTEMPTS = 3
DEFAULT_TIMEOUT = 30

# Private methods: _leading_underscore
def _internal_helper(self):
    pass

# Module names: lowercase_with_underscores
# File: user_service.py
```

### Type Hints

```python
from typing import Optional, List, Dict, Any
from django.db.models import QuerySet

# ✅ Good - Type hints for parameters and return values
def get_user(user_id: int) -> Optional[User]:
    try:
        return User.objects.get(id=user_id)
    except User.DoesNotExist:
        return None

def process_users(users: List[User]) -> Dict[str, Any]:
    return {
        'count': len(users),
        'ids': [user.id for user in users]
    }

# ❌ Bad - No type hints
def get_user(user_id):
    return User.objects.get(id=user_id)
```

### Docstrings

```python
def create_user(email: str, name: str) -> User:
    """
    Create a new user with the given email and name.
    
    Args:
        email: User's email address (must be unique)
        name: User's full name
        
    Returns:
        User instance that was created
        
    Raises:
        IntegrityError: If email already exists
        ValidationError: If email or name is invalid
    """
    # Implementation
    pass
```

---

## Django Best Practices

### Settings Management

- **Use environment variables** - Never hardcode secrets
- **Use `django-environ`** - For managing environment variables
- **Separate settings** - Use different settings for development/production if needed

```python
# settings.py
import environ
from pathlib import Path

BASE_DIR = Path(__file__).resolve().parent.parent

env = environ.Env()
environ.Env.read_env(BASE_DIR / '.env')

SECRET_KEY = env('DJANGO_SECRET_KEY', default='django-insecure-change-me')
DEBUG = env.bool('DEBUG', default=False)
DATABASES = {
    'default': env.db('DATABASE_URL', default='sqlite:///db.sqlite3')
}
```

### URL Patterns

```python
# urls.py
from django.urls import path, include
from rest_framework.routers import DefaultRouter
from .views import UserViewSet

router = DefaultRouter()
router.register(r'users', UserViewSet, basename='user')

urlpatterns = [
    path('api/v1/', include(router.urls)),
    path('api/v1/auth/', include('internal_apps_app.auth.urls')),
]
```

### Apps Organization

- **One app per feature domain** - Keep related models, views, serializers together
- **Reusable apps** - Create reusable apps for shared functionality
- **App naming** - Use descriptive names (e.g., `users`, `authentication`, `billing`)

---

## Models

### Base Model Pattern

```python
# Always inherit from BaseModel for created_at/updated_at
from django.db import models
from internal_apps.utils.base_model import BaseModel

class User(BaseModel):
    email = models.EmailField(unique=True)
    name = models.CharField(max_length=255)
    
    class Meta:
        db_table = 'users'
        ordering = ['-created_at']
        
    def __str__(self) -> str:
        return self.email
```

### Field Best Practices

```python
class Product(BaseModel):
    # ✅ Good - Explicit max_length, help_text
    name = models.CharField(
        max_length=255,
        help_text='Product name'
    )
    
    # ✅ Good - Use appropriate field types
    price = models.DecimalField(
        max_digits=10,
        decimal_places=2,
        help_text='Price in USD'
    )
    
    # ✅ Good - ForeignKey with related_name
    category = models.ForeignKey(
        'Category',
        on_delete=models.CASCADE,
        related_name='products',
        help_text='Product category'
    )
    
    # ✅ Good - Boolean with default
    is_active = models.BooleanField(
        default=True,
        help_text='Whether product is active'
    )
    
    # ✅ Good - DateTime with auto_now_add
    published_at = models.DateTimeField(
        null=True,
        blank=True,
        help_text='When product was published'
    )
```

### Relationships

```python
# ✅ Good - Explicit related_name
class Organization(BaseModel):
    name = models.CharField(max_length=255)

class User(BaseModel):
    organization = models.ForeignKey(
        Organization,
        on_delete=models.CASCADE,
        related_name='members'  # Explicit related_name
    )

# Usage: organization.members.all()

# ✅ Good - ManyToMany with through model for extra fields
class UserOrganization(BaseModel):
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    organization = models.ForeignKey(Organization, on_delete=models.CASCADE)
    role = models.CharField(max_length=50)
    
    class Meta:
        unique_together = ['user', 'organization']
```

### Model Methods

```python
class User(BaseModel):
    email = models.EmailField(unique=True)
    first_name = models.CharField(max_length=100)
    last_name = models.CharField(max_length=100)
    
    @property
    def full_name(self) -> str:
        """Return user's full name."""
        return f"{self.first_name} {self.last_name}"
    
    def get_display_name(self) -> str:
        """Return display name for user."""
        return self.full_name or self.email
    
    def activate(self) -> None:
        """Activate user account."""
        self.is_active = True
        self.save(update_fields=['is_active'])
```

### Query Optimization

```python
# ✅ Good - Use select_related for ForeignKey
users = User.objects.select_related('organization').all()

# ✅ Good - Use prefetch_related for ManyToMany/Reverse FK
organizations = Organization.objects.prefetch_related('members').all()

# ✅ Good - Use only()/defer() for large models
users = User.objects.only('id', 'email', 'name')

# ✅ Good - Use exists() instead of count() when checking existence
if User.objects.filter(email=email).exists():
    # User exists
    pass

# ❌ Bad - N+1 queries
users = User.objects.all()
for user in users:
    print(user.organization.name)  # N+1 query!
```

---

## Views and Viewsets

### Viewset Pattern

```python
# views/user_views.py
from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.shortcuts import get_object_or_404

from .serializers import UserSerializer, UserCreateSerializer
from .services import UserService
from internal_apps_app.models import User

class UserViewSet(viewsets.ModelViewSet):
    """
    ViewSet for User model.
    Provides list, create, retrieve, update, destroy actions.
    """
    queryset = User.objects.all()
    serializer_class = UserSerializer
    permission_classes = [IsAuthenticated]
    
    def get_serializer_class(self):
        """Return appropriate serializer based on action."""
        if self.action == 'create':
            return UserCreateSerializer
        return UserSerializer
    
    def get_queryset(self):
        """Filter queryset based on user permissions."""
        queryset = super().get_queryset()
        # Add filtering logic here
        return queryset
    
    @action(detail=True, methods=['post'])
    def activate(self, request, pk=None):
        """Custom action to activate user."""
        user = self.get_object()
        user.activate()
        return Response({'status': 'user activated'})
    
    def perform_create(self, serializer):
        """Override create to add custom logic."""
        serializer.save(created_by=self.request.user)
```

### Function-Based Views (when needed)

```python
# views/user_views.py
from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import IsAuthenticated
from rest_framework.response import Response
from rest_framework import status

@api_view(['GET'])
@permission_classes([IsAuthenticated])
def user_stats(request):
    """Get user statistics."""
    stats = UserService.get_user_stats(request.user)
    return Response(stats, status=status.HTTP_200_OK)
```

---

## Serializers

### Serializer Pattern

```python
# serializers/user_serializers.py
from rest_framework import serializers
from internal_apps_app.models import User

class UserSerializer(serializers.ModelSerializer):
    """Serializer for User model."""
    
    # ✅ Good - Read-only fields
    full_name = serializers.CharField(source='get_display_name', read_only=True)
    
    class Meta:
        model = User
        fields = [
            'id',
            'email',
            'first_name',
            'last_name',
            'full_name',
            'is_active',
            'created_at',
            'updated_at',
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']
    
    def validate_email(self, value: str) -> str:
        """Validate email is unique."""
        if self.instance and self.instance.email == value:
            return value
        if User.objects.filter(email=value).exists():
            raise serializers.ValidationError("Email already exists.")
        return value

class UserCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating users."""
    
    password = serializers.CharField(write_only=True, min_length=8)
    
    class Meta:
        model = User
        fields = ['email', 'first_name', 'last_name', 'password']
    
    def create(self, validated_data: dict) -> User:
        """Create user with hashed password."""
        password = validated_data.pop('password')
        user = User.objects.create(**validated_data)
        user.set_password(password)
        user.save()
        return user
```

---

## Services

### Service Layer Pattern

```python
# services/user_service.py
from typing import Optional, List
from django.db.models import QuerySet
from django.core.exceptions import ValidationError

from internal_apps_app.models import User
from internal_apps.utils.decorators import retry_with_backoff

class UserService:
    """Service class for user-related business logic."""
    
    @staticmethod
    def get_user(user_id: int) -> Optional[User]:
        """
        Get user by ID.
        
        Args:
            user_id: User ID
            
        Returns:
            User instance or None if not found
        """
        try:
            return User.objects.get(id=user_id)
        except User.DoesNotExist:
            return None
    
    @staticmethod
    def get_user_by_email(email: str) -> Optional[User]:
        """Get user by email."""
        try:
            return User.objects.get(email=email)
        except User.DoesNotExist:
            return None
    
    @staticmethod
    def create_user(email: str, name: str, password: str) -> User:
        """
        Create a new user.
        
        Args:
            email: User email
            name: User name
            password: User password
            
        Returns:
            Created User instance
            
        Raises:
            ValidationError: If user creation fails
        """
        if User.objects.filter(email=email).exists():
            raise ValidationError("User with this email already exists.")
        
        user = User.objects.create_user(
            email=email,
            name=name,
            password=password
        )
        return user
    
    @staticmethod
    @retry_with_backoff(max_retries=3)
    def sync_user_with_external_service(user: User) -> dict:
        """
        Sync user with external service (example with retry).
        
        Args:
            user: User instance
            
        Returns:
            Sync result dictionary
        """
        # Implementation with retry logic
        pass
```

### When to Use Services

- **Business logic** - Complex operations that don't belong in models or views
- **External API calls** - Integration with third-party services
- **Reusable logic** - Logic used by multiple views or tasks
- **Transaction management** - Operations that need database transactions

---

## Utilities

### Decorators

```python
# Use existing retry_with_backoff decorator
from internal_apps.utils.decorators import retry_with_backoff

@retry_with_backoff(max_retries=3, initial_delay=1.0)
def call_external_api():
    # Implementation
    pass
```

### Enum Helpers

```python
# Use enum helpers for model choices
from enum import StrEnum
from internal_apps.utils.enum import choices

class UserStatus(StrEnum):
    ACTIVE = 'active'
    INACTIVE = 'inactive'
    SUSPENDED = 'suspended'

class User(BaseModel):
    status = models.CharField(
        max_length=20,
        choices=choices(UserStatus),
        default=UserStatus.ACTIVE
    )
```

---

## Error Handling

### Exception Handling

```python
# ✅ Good - Specific exception handling
from django.core.exceptions import ValidationError
from rest_framework.exceptions import NotFound, ValidationError as DRFValidationError

def get_user_safely(user_id: int) -> User:
    try:
        return User.objects.get(id=user_id)
    except User.DoesNotExist:
        raise NotFound(f"User with ID {user_id} not found")
    except Exception as e:
        logger.error(f"Error fetching user {user_id}: {str(e)}")
        raise

# ✅ Good - Use DRF exceptions in views
from rest_framework.exceptions import ValidationError, PermissionDenied

class UserViewSet(viewsets.ModelViewSet):
    def create(self, request, *args, **kwargs):
        try:
            return super().create(request, *args, **kwargs)
        except ValidationError as e:
            return Response(
                {'error': str(e)},
                status=status.HTTP_400_BAD_REQUEST
            )
```

### Logging

```python
import logging

logger = logging.getLogger(__name__)

def process_user_data(user_id: int):
    logger.info(f"Processing user data for user {user_id}")
    try:
        # Process data
        logger.info(f"Successfully processed user {user_id}")
    except Exception as e:
        logger.error(f"Error processing user {user_id}: {str(e)}", exc_info=True)
        raise
```

---

## Testing

### Test Structure

```python
# tests/test_user_views.py
from django.test import TestCase
from rest_framework.test import APIClient
from rest_framework import status

from internal_apps_app.models import User

class UserViewSetTestCase(TestCase):
    def setUp(self):
        """Set up test data."""
        self.client = APIClient()
        self.user = User.objects.create_user(
            email='test@example.com',
            password='testpass123'
        )
        self.client.force_authenticate(user=self.user)
    
    def test_list_users(self):
        """Test listing users."""
        response = self.client.get('/api/v1/users/')
        self.assertEqual(response.status_code, status.HTTP_200_OK)
    
    def test_create_user(self):
        """Test creating a user."""
        data = {
            'email': 'newuser@example.com',
            'name': 'New User',
            'password': 'securepass123'
        }
        response = self.client.post('/api/v1/users/', data)
        self.assertEqual(response.status_code, status.HTTP_201_CREATED)
```

---

## Performance

### Database Optimization

```python
# ✅ Good - Use select_related and prefetch_related
users = User.objects.select_related('organization').prefetch_related('permissions').all()

# ✅ Good - Use values()/values_list() when you don't need full objects
user_ids = User.objects.values_list('id', flat=True)

# ✅ Good - Use iterator() for large querysets
for user in User.objects.iterator():
    process_user(user)

# ✅ Good - Use count() efficiently
user_count = User.objects.count()  # Uses COUNT(*) SQL
```

### Caching

```python
from django.core.cache import cache

def get_user_cached(user_id: int) -> Optional[User]:
    cache_key = f'user:{user_id}'
    user = cache.get(cache_key)
    
    if user is None:
        user = User.objects.get(id=user_id)
        cache.set(cache_key, user, timeout=300)  # 5 minutes
    
    return user
```

---

## Security

### Authentication and Authorization

```python
# ✅ Good - Always use permission classes
from rest_framework.permissions import IsAuthenticated, IsAdminUser

class UserViewSet(viewsets.ModelViewSet):
    permission_classes = [IsAuthenticated]
    
    @action(detail=False, permission_classes=[IsAdminUser])
    def admin_only_action(self, request):
        # Admin-only action
        pass
```

### Input Validation

```python
# ✅ Good - Validate all user input
from rest_framework import serializers

class UserCreateSerializer(serializers.ModelSerializer):
    email = serializers.EmailField()
    password = serializers.CharField(min_length=8)
    
    def validate_password(self, value: str) -> str:
        if len(value) < 8:
            raise serializers.ValidationError("Password must be at least 8 characters.")
        return value
```

### SQL Injection Prevention

- **Always use ORM** - Django ORM prevents SQL injection
- **Never use raw SQL with user input** - If you must, use parameterized queries

```python
# ✅ Good - Use ORM
users = User.objects.filter(email=user_email)

# ❌ Bad - Raw SQL with user input (DANGEROUS!)
users = User.objects.raw(f"SELECT * FROM users WHERE email = '{user_email}'")
```

---

## References

- [Django Documentation](https://docs.djangoproject.com/)
- [Django REST Framework Documentation](https://www.django-rest-framework.org/)
- [PEP 8 - Python Style Guide](https://pep8.org/)
- [Django Best Practices](https://docs.djangoproject.com/en/stable/misc/design-philosophies/)

---

**Remember**: Consistency is key. When in doubt, refer to existing code patterns and this guide.

