from django.contrib import admin
from django.urls import path, include, re_path
from django.views.generic import TemplateView
from django.conf import settings
from django.conf.urls.static import static
from django.views.static import serve

from relay_app.views.preview_views import AppPreviewView

urlpatterns = [
    path('admin/', admin.site.urls),
    path('api/v1/', include('relay_app.urls')),
    
    # Preview endpoint (outside /api/v1/)
    path('preview/apps/<uuid:app_id>/', AppPreviewView.as_view(), name='app-preview'),
]

if settings.DEBUG:
    urlpatterns += static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)
    # Serve media files in production
    urlpatterns += [
        re_path(r'^media/(?P<path>.*)$', serve, {'document_root': settings.MEDIA_ROOT}),
    ]

