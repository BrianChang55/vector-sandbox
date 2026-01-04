"""
URL configuration for relay_app.
"""
from django.urls import path, include
from rest_framework.routers import DefaultRouter

from .views import (
    organization_views,
    backend_connection_views,
    resource_registry_views,
    internal_app_views,
    version_views,
    runtime_views,
    action_views,
    publish_views,
    auth_views,
    streaming_views,
)

# Router for viewsets
router = DefaultRouter()
router.register(r'orgs', organization_views.OrganizationViewSet, basename='organization')
router.register(
    r'orgs/(?P<organization_pk>[^/.]+)/members',
    organization_views.UserOrganizationViewSet,
    basename='organization-members'
)
router.register(
    r'orgs/(?P<organization_pk>[^/.]+)/backends',
    backend_connection_views.BackendConnectionViewSet,
    basename='backend-connection'
)
router.register(
    r'backends/(?P<backend_connection_pk>[^/.]+)/resources',
    resource_registry_views.ResourceRegistryEntryViewSet,
    basename='resource-registry'
)
router.register(
    r'orgs/(?P<organization_pk>[^/.]+)/apps',
    internal_app_views.InternalAppViewSet,
    basename='internal-app'
)
router.register(
    r'apps/(?P<internal_app_pk>[^/.]+)/versions',
    version_views.AppVersionViewSet,
    basename='app-version'
)

urlpatterns = [
    # Router URLs
    path('', include(router.urls)),
    
    # Authentication endpoints
    path('auth/signup', auth_views.SignUpView.as_view(), name='signup'),
    path('auth/login', auth_views.LoginView.as_view(), name='login'),
    path('auth/refresh', auth_views.RefreshTokenView.as_view(), name='refresh-token'),
    path('auth/me', auth_views.AuthMeView.as_view(), name='auth-me'),
    
    # Magic Link Authentication
    path('auth/magic-link/request', auth_views.MagicLinkRequestView.as_view(), name='magic-link-request'),
    path('auth/magic-link/verify', auth_views.MagicLinkVerifyView.as_view(), name='magic-link-verify'),
    
    # Google OAuth
    path('auth/google', auth_views.GoogleOAuthView.as_view(), name='google-oauth'),
    path('auth/google/callback', auth_views.GoogleOAuthCallbackView.as_view(), name='google-oauth-callback'),
    
    # Custom action endpoints
    path('backends/<uuid:pk>/discover/', resource_registry_views.discover_resources, name='backend-discover'),
    path('backends/<uuid:pk>/test/', backend_connection_views.BackendConnectionViewSet.as_view({'post': 'test'}), name='backend-test'),
    path('backends/<uuid:pk>/user-auth/', backend_connection_views.BackendConnectionViewSet.as_view({'post': 'user_auth'}), name='backend-user-auth'),
    path('orgs/<uuid:pk>/switch/', organization_views.OrganizationViewSet.as_view({'post': 'switch'}), name='org-switch'),
    path('apps/<uuid:pk>/publish/', publish_views.publish_app, name='app-publish'),
    
    # Action allowlist
    path('actions/allowlist/', action_views.ActionAllowlistView.as_view(), name='action-allowlist'),
    path('actions/<str:action_id>/', action_views.ActionAllowlistView.as_view(), name='action-delete'),
    
    # Runtime proxy
    path('runtime/query/', runtime_views.RuntimeQueryView.as_view(), name='runtime-query'),
    path('runtime/action/', runtime_views.RuntimeActionView.as_view(), name='runtime-action'),
    
    # Direct access endpoints (without org prefix)
    path('apps/<uuid:pk>/', internal_app_views.InternalAppViewSet.as_view({'get': 'retrieve', 'patch': 'partial_update'}), name='app-detail'),
    path('versions/<uuid:pk>/', version_views.AppVersionViewSet.as_view({'get': 'retrieve'}), name='version-detail'),
    path('versions/<uuid:pk>/rollback/', version_views.AppVersionViewSet.as_view({'post': 'rollback'}), name='version-rollback'),
    path('versions/<uuid:pk>/save-files/', version_views.AppVersionViewSet.as_view({'post': 'save_files'}), name='version-save-files'),
    path('resources/<uuid:pk>/', resource_registry_views.ResourceRegistryEntryViewSet.as_view({'get': 'retrieve', 'patch': 'partial_update'}), name='resource-detail'),
    
    # AI/Code Generation endpoints
    path('models/', streaming_views.AvailableModelsView.as_view(), name='available-models'),
    path('apps/<uuid:app_id>/chat-sessions/', streaming_views.ChatSessionViewSet.as_view(), name='chat-sessions'),
    path('chat-sessions/<uuid:session_id>/messages/', streaming_views.ChatMessagesView.as_view(), name='chat-messages'),
    path('apps/<uuid:app_id>/generate/', streaming_views.NonStreamingGenerateView.as_view(), name='generate'),
    path('apps/<uuid:app_id>/generate/stream/', streaming_views.StreamingGenerateView.as_view(), name='generate-stream'),
    path('apps/<uuid:app_id>/generate/agentic/', streaming_views.AgenticGenerateView.as_view(), name='generate-agentic'),
    path('apps/<uuid:app_id>/latest-generation/', streaming_views.LatestGenerationView.as_view(), name='latest-generation'),
    path('versions/<uuid:version_id>/generation-state/', streaming_views.GenerationStateView.as_view(), name='generation-state'),
    path('messages/<uuid:message_id>/apply/', streaming_views.ApplyGeneratedCodeView.as_view(), name='apply-generated'),
]
