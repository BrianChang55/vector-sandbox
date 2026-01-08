import environ
from pathlib import Path

BASE_DIR = Path(__file__).resolve().parent.parent

env = environ.Env()
environ.Env.read_env(BASE_DIR / '.env')

SECRET_KEY = env('DJANGO_SECRET_KEY', default='django-insecure-change-this-in-production')
DEBUG = env.bool('DEBUG', default=True)
BASE_URL = env('BASE_URL', default='http://localhost:8001')

ALLOWED_HOSTS = ['*']

RENDER_EXTERNAL_HOSTNAME = env('RENDER_EXTERNAL_HOSTNAME', default=None)
if RENDER_EXTERNAL_HOSTNAME:
    ALLOWED_HOSTS.append(RENDER_EXTERNAL_HOSTNAME)

# Custom User Model
AUTH_USER_MODEL = 'relay_app.User'

INSTALLED_APPS = [
    'django.contrib.admin',
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    'corsheaders',
    'rest_framework',
    'relay_app',
]

MIDDLEWARE = [
    'corsheaders.middleware.CorsMiddleware',
    'django.middleware.security.SecurityMiddleware',
    'whitenoise.middleware.WhiteNoiseMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.common.CommonMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    'django.middleware.clickjacking.XFrameOptionsMiddleware',
]

ROOT_URLCONF = 'internal_apps.urls'

TEMPLATES = [
    {
        'BACKEND': 'django.template.backends.django.DjangoTemplates',
        'DIRS': [],
        'APP_DIRS': True,
        'OPTIONS': {
            'context_processors': [
                'django.template.context_processors.debug',
                'django.template.context_processors.request',
                'django.contrib.auth.context_processors.auth',
                'django.contrib.messages.context_processors.messages',
            ],
        },
    },
]

WSGI_APPLICATION = 'internal_apps.wsgi.application'

# Database
DATABASES = {
    'default': env.db('DATABASE_URL', default='sqlite:///db.sqlite3')
}

# Password validation
AUTH_PASSWORD_VALIDATORS = [
    {
        'NAME': 'django.contrib.auth.password_validation.UserAttributeSimilarityValidator',
    },
    {
        'NAME': 'django.contrib.auth.password_validation.MinimumLengthValidator',
    },
    {
        'NAME': 'django.contrib.auth.password_validation.CommonPasswordValidator',
    },
    {
        'NAME': 'django.contrib.auth.password_validation.NumericPasswordValidator',
    },
]

# Internationalization
LANGUAGE_CODE = 'en-us'
TIME_ZONE = 'UTC'
USE_I18N = True
USE_TZ = True

# Static files
STATIC_URL = '/static/'
STATIC_ROOT = BASE_DIR / 'staticfiles'
STATICFILES_STORAGE = 'whitenoise.storage.CompressedManifestStaticFilesStorage'

DEFAULT_AUTO_FIELD = 'django.db.models.BigAutoField'

# Email Configuration
EMAIL_BACKEND = env('EMAIL_BACKEND', default='django.core.mail.backends.smtp.EmailBackend')
EMAIL_HOST = env('EMAIL_HOST', default='smtp.resend.com')
EMAIL_PORT = env.int('EMAIL_PORT', default=465)
EMAIL_USE_TLS = env.bool('EMAIL_USE_TLS', default=False)
EMAIL_USE_SSL = env.bool('EMAIL_USE_SSL', default=True)
EMAIL_HOST_USER = env('EMAIL_HOST_USER', default='resend')
EMAIL_HOST_PASSWORD = env('EMAIL_HOST_PASSWORD', default='')
DEFAULT_FROM_EMAIL = env('DEFAULT_FROM_EMAIL', default='Internal Apps <noreply@internalapps.com>')

# REST Framework
REST_FRAMEWORK = {
    'DEFAULT_PERMISSION_CLASSES': ['rest_framework.permissions.IsAuthenticated'],
    'DEFAULT_AUTHENTICATION_CLASSES': [
        'rest_framework_simplejwt.authentication.JWTAuthentication',
        'rest_framework.authentication.SessionAuthentication',
        'rest_framework.authentication.BasicAuthentication',
    ],
}

# JWT Configuration
from datetime import timedelta
SIMPLE_JWT = {
    'ACCESS_TOKEN_LIFETIME': timedelta(days=7),
    'REFRESH_TOKEN_LIFETIME': timedelta(days=30),
    'ROTATE_REFRESH_TOKENS': True,
    'BLACKLIST_AFTER_ROTATION': True,
    'UPDATE_LAST_LOGIN': True,
    'ALGORITHM': 'HS256',
    'SIGNING_KEY': SECRET_KEY,
    'AUTH_HEADER_TYPES': ('Bearer',),
    'AUTH_HEADER_NAME': 'HTTP_AUTHORIZATION',
    'USER_ID_FIELD': 'id',
    'USER_ID_CLAIM': 'user_id',
}

# Authentication Backends
AUTHENTICATION_BACKENDS = [
    'django.contrib.auth.backends.ModelBackend',  # Email/password
]

# Google OAuth Configuration
GOOGLE_OAUTH_CLIENT_ID = env('GOOGLE_OAUTH_CLIENT_ID', default='')
GOOGLE_OAUTH_CLIENT_SECRET = env('GOOGLE_OAUTH_CLIENT_SECRET', default='')
GOOGLE_OAUTH_REDIRECT_URI = env('GOOGLE_OAUTH_REDIRECT_URI', default='')

# If no explicit redirect URI is set, auto-generate based on environment
if not GOOGLE_OAUTH_REDIRECT_URI:
    if RENDER_EXTERNAL_HOSTNAME:
        GOOGLE_OAUTH_REDIRECT_URI = f'https://{RENDER_EXTERNAL_HOSTNAME}/api/v1/auth/google/callback'
    else:
        GOOGLE_OAUTH_REDIRECT_URI = 'http://localhost:8001/api/v1/auth/google/callback'

# Session Configuration
SESSION_COOKIE_AGE = 60 * 60 * 24 * 30  # 30 days
SESSION_EXPIRE_AT_BROWSER_CLOSE = False
SESSION_SAVE_EVERY_REQUEST = True
SESSION_COOKIE_HTTPONLY = True

# Cross-Site Cookie Settings for Render (Frontend & Backend on different subdomains)
if RENDER_EXTERNAL_HOSTNAME:
    SESSION_COOKIE_SAMESITE = 'None'
    SESSION_COOKIE_SECURE = True
    CSRF_COOKIE_SAMESITE = 'None'
    CSRF_COOKIE_SECURE = True
    # Trust the X-Forwarded-Proto header from Render's load balancer
    SECURE_PROXY_SSL_HEADER = ('HTTP_X_FORWARDED_PROTO', 'https')
else:
    SESSION_COOKIE_SAMESITE = 'Lax'
    SESSION_COOKIE_SECURE = False

# CORS settings
CORS_ALLOWED_ORIGINS = [
    'http://localhost:3000',
    'http://localhost:3001',
    'http://localhost:5173',
    'http://localhost:5174',
    'http://localhost:5175',
    'http://localhost:5176',
    'https://localhost:3000',
    'https://localhost:3001',
    # Production custom domain
    'https://relayapps.ai',
    'https://www.relayapps.ai',
]

# Allow Sandpack/CodeSandbox iframe origins for the preview runtime
# These are dynamically generated subdomains, so we use a regex pattern
CORS_ALLOWED_ORIGIN_REGEXES = [
    r'^https://.*\.codesandbox\.io$',
    r'^https://.*-sandpack\.codesandbox\.io$',
    r'^https://internal-apps-frontend.*\.onrender\.com$',
    r'^https://.*\.relayapps\.ai$',  # All subdomains of relayapps.ai
]

FRONTEND_URL = env('FRONTEND_URL', default='http://localhost:5176')

if FRONTEND_URL:
    CORS_ALLOWED_ORIGINS.append(FRONTEND_URL)

CORS_ALLOW_CREDENTIALS = True
CORS_ALLOW_METHODS = [
    'DELETE',
    'GET',
    'OPTIONS',
    'PATCH',
    'POST',
    'PUT',
]

# Allow Private Network Access - required for Sandpack iframes (public origin)
# to access localhost (private network) during development
# See: https://developer.chrome.com/blog/private-network-access-preflight/
CORS_ALLOW_PRIVATE_NETWORK = True

# CSRF settings
CSRF_TRUSTED_ORIGINS = [
    'http://localhost:5173',
    'http://localhost:5174',
    'http://localhost:5175',
    'http://localhost:3000',
    'http://localhost:3001',
    'https://localhost:3000',
    'https://localhost:3001',
    # Production custom domain
    'https://relayapps.ai',
    'https://www.relayapps.ai',
    'https://app.relayapps.ai',
    'https://api.relayapps.ai',
]

if RENDER_EXTERNAL_HOSTNAME:
    CSRF_TRUSTED_ORIGINS.append(f'https://{RENDER_EXTERNAL_HOSTNAME}')

# Support custom API domain
API_DOMAIN = env('API_DOMAIN', default=None)
if API_DOMAIN:
    CSRF_TRUSTED_ORIGINS.append(f'https://{API_DOMAIN}')
    ALLOWED_HOSTS.append(API_DOMAIN)

# External API Configuration (configure as needed)
OPENAI_API_KEY = env('OPENAI_API_KEY', default='')
MIXPANEL_TOKEN = env('MIXPANEL_TOKEN', default='')

# OpenRouter Configuration for AI Code Generation
OPENROUTER_API_KEY = env('OPENROUTER_API_KEY', default='')
OPENROUTER_APP_NAME = env('OPENROUTER_APP_NAME', default='Internal Apps Builder')

# If OpenRouter key not set, fall back to OpenAI key
if not OPENROUTER_API_KEY and OPENAI_API_KEY:
    OPENROUTER_API_KEY = OPENAI_API_KEY

# Encryption Key for secrets (generate with: python -c 'from cryptography.fernet import Fernet; print(Fernet.generate_key().decode())')
ENCRYPTION_KEY = env('ENCRYPTION_KEY', default=None)

# Merge Agent Handler Configuration
# These credentials are used for all organizations (platform-wide integration)
MERGE_TOOL_PACK_ID = env('MERGE_TOOL_PACK_ID', default='')
MERGE_ACCESS_KEY = env('MERGE_ACCESS_KEY', default='')
MERGE_API_TIMEOUT = env.float('MERGE_API_TIMEOUT', default=30.0)

# Sentry Error Tracking
SENTRY_DSN = env('SENTRY_DSN', default='')
SENTRY_ENVIRONMENT = env('SENTRY_ENVIRONMENT', default='production' if not DEBUG else 'development')

# Media Files
MEDIA_URL = '/media/'

# Render Persistent Disk Configuration
RENDER_DISK_MOUNT_PATH = env('RENDER_DISK_MOUNT_PATH', default=None)

if RENDER_DISK_MOUNT_PATH:
    # If running on Render with a disk attached, store media there
    MEDIA_ROOT = Path(RENDER_DISK_MOUNT_PATH) / 'media'
else:
    # Local development
    MEDIA_ROOT = BASE_DIR / 'media'

# Logging Configuration
LOGGING = {
    'version': 1,
    'disable_existing_loggers': False,
    'formatters': {
        'verbose': {
            'format': '{levelname} {asctime} {module} {process:d} {thread:d} {message}',
            'style': '{',
        },
        'simple': {
            'format': '{levelname} {asctime} {name} {message}',
            'style': '{',
        },
    },
    'handlers': {
        'console': {
            'class': 'logging.StreamHandler',
            'formatter': 'simple',
        },
    },
    'root': {
        'handlers': ['console'],
        'level': 'INFO',
    },
    'loggers': {
        'django': {
            'handlers': ['console'],
            'level': 'INFO',
            'propagate': False,
        },
    },
}

# Configure Sentry for error tracking
if SENTRY_DSN:
    import logging
    import sentry_sdk
    from sentry_sdk.integrations.django import DjangoIntegration
    from sentry_sdk.integrations.logging import LoggingIntegration
    
    # Configure Sentry
    sentry_sdk.init(
        dsn=SENTRY_DSN,
        environment=SENTRY_ENVIRONMENT,
        integrations=[
            DjangoIntegration(
                transaction_style='url',
                middleware_spans=True,
                signals_spans=True,
                cache_spans=True,
            ),
            LoggingIntegration(
                level=logging.INFO,        # Capture info and above as breadcrumbs
                event_level=logging.ERROR  # Send errors and above as events
            ),
        ],
        traces_sample_rate=0.1 if DEBUG else 0.1,
        profiles_sample_rate=0.1 if DEBUG else 0.1,
        send_default_pii=True,
        attach_stacktrace=True,
        ignore_errors=[
            'django.http.Http404',
            'django.http.response.Http404',
        ],
    )

# =============================================================================
# Celery Configuration
# =============================================================================
# Redis broker for async task processing
CELERY_BROKER_URL = env('CELERY_BROKER_URL', default='redis://localhost:6379/0')
CELERY_RESULT_BACKEND = env('CELERY_RESULT_BACKEND', default='redis://localhost:6379/0')

# Celery settings
CELERY_ACCEPT_CONTENT = ['json']
CELERY_TASK_SERIALIZER = 'json'
CELERY_RESULT_SERIALIZER = 'json'
CELERY_TIMEZONE = 'UTC'

# Task settings
CELERY_TASK_TRACK_STARTED = True
CELERY_TASK_TIME_LIMIT = 30 * 60  # 30 minutes max per task
CELERY_TASK_SOFT_TIME_LIMIT = 25 * 60  # 25 minutes soft limit

# Worker settings
CELERY_WORKER_PREFETCH_MULTIPLIER = 1  # Process one task at a time for heavy tasks
CELERY_WORKER_CONCURRENCY = 2  # Number of concurrent workers

# Task retry settings
CELERY_TASK_ACKS_LATE = True  # Acknowledge task after completion (for reliability)
CELERY_TASK_REJECT_ON_WORKER_LOST = True  # Requeue task if worker dies

