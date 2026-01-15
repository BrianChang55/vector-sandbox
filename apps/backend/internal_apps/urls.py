from django.contrib import admin
from django.urls import path, include, re_path
from django.views.generic import TemplateView
from django.conf import settings
from django.conf.urls.static import static
from django.views.static import serve

from vector_app.views.preview_views import AppPreviewView

urlpatterns = [
    path('admin/', admin.site.urls),
    path('api/v1/', include('vector_app.urls')),
    
    # Preview endpoint (outside /api/v1/)
    path('preview/apps/<uuid:app_id>/', AppPreviewView.as_view(), name='app-preview'),
]

# Serve media files (logos, uploads, etc.)
urlpatterns += static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)
# Explicit serve endpoint for production
urlpatterns += [
    re_path(r'^media/(?P<path>.*)$', serve, {'document_root': settings.MEDIA_ROOT}),
]

