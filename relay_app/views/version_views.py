"""
App Version API views
"""
import logging
from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.shortcuts import get_object_or_404
from django.db import transaction

from ..models import InternalApp, AppVersion, VersionFile, Organization
from ..serializers import (
    AppVersionSerializer,
    AppVersionListSerializer,
    AppVersionCreateSerializer,
    VersionFileSerializer,
    CodeEditSerializer,
)
from ..services.validation import AppSpecValidationService
from ..services.codegen import CodegenService
from ..services.ai_service import AIService

logger = logging.getLogger(__name__)


class AppVersionViewSet(viewsets.ReadOnlyModelViewSet):
    """
    ViewSet for AppVersion model (read-only, use custom actions for creation).
    GET /api/v1/apps/:app_id/versions - List versions
    GET /api/v1/versions/:id - Get version
    POST /api/v1/apps/:app_id/versions/ai-edit - Create version from AI edit
    POST /api/v1/apps/:app_id/versions/code-edit - Create version from code edit
    POST /api/v1/versions/:id/rollback - Rollback to version
    """
    permission_classes = [IsAuthenticated]
    serializer_class = AppVersionSerializer
    
    def get_queryset(self):
        """Filter versions to app."""
        app_id = self.kwargs.get('internal_app_pk')
        return AppVersion.objects.filter(internal_app_id=app_id).select_related('created_by')
    
    def get_serializer_class(self):
        """
        Use lightweight serializer for list to avoid shipping full file blobs.
        
        The detail endpoint still returns the complete payload (including files)
        for callers that need code content (e.g., builder autosave).
        """
        if self.action == 'list':
            include_files = False
            if self.request:
                flag = self.request.query_params.get('include_files')
                include_files = str(flag).lower() in ('1', 'true', 'yes')
            return AppVersionSerializer if include_files else AppVersionListSerializer
        return super().get_serializer_class()
    
    @action(detail=False, methods=['post'], url_path='ai-edit')
    def ai_edit(self, request, internal_app_pk=None):
        """Create new version from AI edit."""
        app = get_object_or_404(InternalApp, pk=internal_app_pk)
        
        # Verify access
        if not request.user.user_organizations.filter(organization=app.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        serializer = AppVersionCreateSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        
        intent_message = serializer.validated_data.get('intent_message', '')
        
        # Get current spec (from latest version if exists)
        latest_version = AppVersion.objects.filter(internal_app=app).order_by('-version_number').first()
        current_spec = latest_version.spec_json if latest_version else None
        
        # Build registry surface for AI
        from ..models import ResourceRegistryEntry
        registry_entries = ResourceRegistryEntry.objects.filter(
            backend_connection=app.backend_connection,
            enabled=True
        )
        registry_surface = {
            'resources': [
                {
                    'resource_id': entry.resource_id,
                    'resource_name': entry.resource_name,
                    'exposed_fields': entry.exposed_fields_json or [],
                    'allowed_actions': [a.get('action_id') for a in (entry.allowed_actions_json or [])],
                }
                for entry in registry_entries
            ]
        }
        
        # Generate AppSpec using AI
        try:
            ai_service = AIService()
            spec_json = ai_service.generate_app_spec(intent_message, current_spec, registry_surface)
        except Exception as e:
            logger.error(f"AI generation failed: {e}")
            return Response(
                {'error': f'AI generation failed: {str(e)}'},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )
        
        # Validate AppSpec
        is_valid, errors = AppSpecValidationService.validate_app_spec(app, spec_json)
        if not is_valid:
            return Response(
                {'error': 'AppSpec validation failed', 'errors': errors},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get next version number
        latest_version = AppVersion.objects.filter(internal_app=app).order_by('-version_number').first()
        next_version_number = (latest_version.version_number + 1) if latest_version else 1
        
        with transaction.atomic():
            # Create version
            version = AppVersion.objects.create(
                internal_app=app,
                version_number=next_version_number,
                source=AppVersion.SOURCE_AI,
                spec_json=spec_json,
                created_by=request.user,
            )
            
            # Generate files
            CodegenService.generate_files_from_spec(version)
        
        return Response(
            AppVersionSerializer(version).data,
            status=status.HTTP_201_CREATED
        )
    
    @action(detail=False, methods=['post'], url_path='code-edit')
    def code_edit(self, request, internal_app_pk=None):
        """Create new version from code edit."""
        app = get_object_or_404(InternalApp, pk=internal_app_pk)
        
        # Verify access
        if not request.user.user_organizations.filter(organization=app.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Get latest version to base edit on
        latest_version = AppVersion.objects.filter(internal_app=app).order_by('-version_number').first()
        if not latest_version:
            return Response(
                {'error': 'No base version found. Create a version with AI edit first.'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Validate file path is allowlisted
        file_path = request.data.get('file_path')
        if file_path not in CodegenService.ALLOWLISTED_PATHS:
            return Response(
                {'error': f'File path not allowlisted: {file_path}'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get next version number
        next_version_number = latest_version.version_number + 1
        
        with transaction.atomic():
            # Create new version with same spec (code edits don't change spec)
            version = AppVersion.objects.create(
                internal_app=app,
                version_number=next_version_number,
                source=AppVersion.SOURCE_CODE,
                spec_json=latest_version.spec_json,
                created_by=request.user,
            )
            
            # Copy all files from parent, then update the edited file
            for parent_file in latest_version.files.all():
                VersionFile.objects.create(
                    app_version=version,
                    path=parent_file.path,
                    content=parent_file.content,
                    content_hash=parent_file.content_hash,
                )
            
            # Update edited file
            edited_file = version.files.get(path=file_path)
            edited_file.content = request.data.get('content', '')
            edited_file.save()
        
        return Response(
            AppVersionSerializer(version).data,
            status=status.HTTP_201_CREATED
        )
    
    @action(detail=True, methods=['post'], url_path='save-files')
    def save_files(self, request, pk=None, internal_app_pk=None):
        """Save edited files to an existing version."""
        version = get_object_or_404(AppVersion, pk=pk)
        app = version.internal_app
        
        # Verify access
        if not request.user.user_organizations.filter(organization=app.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        files = request.data.get('files', [])
        if not files:
            return Response(
                {'error': 'No files provided'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        with transaction.atomic():
            for file_data in files:
                path = file_data.get('path', '')
                content = file_data.get('content', '')
                
                if not path:
                    continue
                
                # Normalize path - remove leading slash if present
                if path.startswith('/'):
                    path = path[1:]
                
                # Update or create the file
                VersionFile.objects.update_or_create(
                    app_version=version,
                    path=path,
                    defaults={'content': content}
                )
        
        return Response({
            'success': True,
            'message': f'Saved {len(files)} files',
            'version_id': str(version.id),
        })
    
    @action(detail=True, methods=['post'])
    def rollback(self, request, pk=None):
        """Rollback to a previous version."""
        version = self.get_object()
        app = version.internal_app
        
        # Verify access
        if not request.user.user_organizations.filter(organization=app.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Get latest version number
        latest_version = AppVersion.objects.filter(internal_app=app).order_by('-version_number').first()
        next_version_number = latest_version.version_number + 1 if latest_version else 1
        
        with transaction.atomic():
            # Create new version as rollback (use publish as generic source for now)
            rollback_version = AppVersion.objects.create(
                internal_app=app,
                version_number=next_version_number,
                source=AppVersion.SOURCE_PUBLISH,  # Using publish as rollback indicator
                spec_json=version.spec_json,
                created_by=request.user,
            )
            
            # Copy all files from rollback target
            for source_file in version.files.all():
                VersionFile.objects.create(
                    app_version=rollback_version,
                    path=source_file.path,
                    content=source_file.content,
                    content_hash=source_file.content_hash,
                )
        
        return Response(
            AppVersionSerializer(rollback_version).data,
            status=status.HTTP_201_CREATED
        )

