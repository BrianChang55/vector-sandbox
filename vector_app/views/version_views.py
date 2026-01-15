"""
App Version API views

Provides endpoints for managing app versions including:
- Listing versions with snapshots
- Creating versions from AI/code edits
- Enhanced rollback with preview and schema revert
- Version diff comparison
- Audit trail access
"""
import logging
from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.shortcuts import get_object_or_404
from django.db import transaction

from ..models import InternalApp, AppVersion, VersionFile, Organization, VersionAuditLog
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
from ..services.version_service import VersionService
from ..services.snapshot_service import SnapshotService
from ..services.schema_migration_service import SchemaMigrationService
from ..permissions import IsOrgEditorOrAbove, require_editor_or_above

logger = logging.getLogger(__name__)


class AppVersionViewSet(viewsets.ReadOnlyModelViewSet):
    """
    ViewSet for AppVersion model (read-only, use custom actions for creation).
    GET /api/v1/apps/:app_id/versions - List versions (any member)
    GET /api/v1/versions/:id - Get version (any member)
    POST /api/v1/apps/:app_id/versions/ai-edit - Create version from AI edit (editor+)
    POST /api/v1/apps/:app_id/versions/code-edit - Create version from code edit (editor+)
    POST /api/v1/versions/:id/rollback - Rollback to version (editor+)
    POST /api/v1/versions/:id/save-files - Save files to version (editor+)
    """
    permission_classes = [IsAuthenticated, IsOrgEditorOrAbove]
    serializer_class = AppVersionSerializer
    
    def get_queryset(self):
        """
        Filter versions to app, or return all accessible versions for direct access.
        
        When accessed via /apps/{app_id}/versions/ - filter by app
        When accessed via /versions/{version_id}/ (direct) - return all versions
        
        By default, only returns active versions (is_active=True) to ensure users
        only see completed generations. Use ?include_inactive=true to include all versions.
        """
        app_id = self.kwargs.get('internal_app_pk')
        
        # Check if we should include inactive versions
        include_inactive = False
        if self.request:
            flag = self.request.query_params.get('include_inactive')
            include_inactive = str(flag).lower() in ('1', 'true', 'yes')
        
        if app_id:
            queryset = AppVersion.objects.filter(internal_app_id=app_id).select_related('created_by')
            # Filter to only active versions by default for list views
            if not include_inactive and self.action == 'list':
                queryset = queryset.filter(is_active=True)
            return queryset
        
        # Direct access - return all versions (will be filtered by pk lookup)
        # For direct access (detail view), don't filter by is_active to allow accessing specific versions
        return AppVersion.objects.all().select_related('created_by')
    
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
        """Create new version from AI edit (editor+ only)."""
        app = get_object_or_404(InternalApp, pk=internal_app_pk)
        
        # Verify editor or above access
        membership, error = require_editor_or_above(request, app.organization)
        if error:
            return error
        
        serializer = AppVersionCreateSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        
        intent_message = serializer.validated_data.get('intent_message', '')
        
        # Get current spec from latest STABLE version (complete generation)
        # This ensures we don't build on top of incomplete/cancelled versions
        latest_stable_version = VersionService.get_latest_stable_version(app)
        current_spec = latest_stable_version.spec_json if latest_stable_version else None
        
        # No external resource registry in the current backend
        registry_surface = {'resources': []}
        
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
        
        # Get next version number using version service
        next_version_number = VersionService.get_next_version_number(app)
        
        with transaction.atomic():
            # Create version - parent is the stable version we based the spec on
            version = AppVersion.objects.create(
                internal_app=app,
                version_number=next_version_number,
                parent_version=latest_stable_version,
                source=AppVersion.SOURCE_AI_EDIT,
                intent_message=intent_message,
                spec_json=spec_json,
                created_by=request.user,
                is_active=False,  # Start inactive until files are generated
            )
        
        # Generate files (potentially long-running, outside transaction)
        CodegenService.generate_files_from_spec(version)
        
        # Mark as active after successful file generation
        version.is_active = True
        version.save(update_fields=['is_active', 'updated_at'])
        
        return Response(
            AppVersionSerializer(version).data,
            status=status.HTTP_201_CREATED
        )
    
    @action(detail=False, methods=['post'], url_path='code-edit')
    def code_edit(self, request, internal_app_pk=None):
        """Create new version from code edit (editor+ only)."""
        app = get_object_or_404(InternalApp, pk=internal_app_pk)
        
        # Verify editor or above access
        membership, error = require_editor_or_above(request, app.organization)
        if error:
            return error
        
        # Get latest STABLE version to base edit on
        # Code edits should only be made on complete versions
        latest_stable = VersionService.get_latest_stable_version(app)
        if not latest_stable:
            return Response(
                {'error': 'No stable version found. Create a complete version with AI edit first.'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Validate file path is allowlisted
        file_path = request.data.get('file_path')
        if file_path not in CodegenService.ALLOWLISTED_PATHS:
            return Response(
                {'error': f'File path not allowlisted: {file_path}'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get next version number using version service
        next_version_number = VersionService.get_next_version_number(app)
        
        with transaction.atomic():
            # Create new version with same spec (code edits don't change spec)
            version = AppVersion.objects.create(
                internal_app=app,
                version_number=next_version_number,
                parent_version=latest_stable,
                source=AppVersion.SOURCE_CODE_EDIT,
                spec_json=latest_stable.spec_json,
                created_by=request.user,
                is_active=False,  # Start inactive until files are copied
            )
            
            # Copy all files from parent, then update the edited file
            for parent_file in latest_stable.files.all():
                VersionFile.objects.create(
                    app_version=version,
                    path=parent_file.path,
                    content=parent_file.content,
                    content_hash=parent_file.content_hash or VersionFile.compute_hash(parent_file.content),
                )
            
            # Update edited file
            edited_file = version.files.get(path=file_path)
            edited_file.content = request.data.get('content', '')
            edited_file.content_hash = VersionFile.compute_hash(edited_file.content)
            edited_file.save()
            
            # Mark as active after successful file operations
            version.is_active = True
            version.save(update_fields=['is_active', 'updated_at'])
        
        return Response(
            AppVersionSerializer(version).data,
            status=status.HTTP_201_CREATED
        )
    
    @action(detail=True, methods=['post'], url_path='save-files')
    def save_files(self, request, pk=None, internal_app_pk=None):
        """Save edited files to an existing version (editor+ only)."""
        version = get_object_or_404(AppVersion, pk=pk)
        app = version.internal_app
        
        # Verify editor or above access
        membership, error = require_editor_or_above(request, app.organization)
        if error:
            return error
        
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
                    defaults={
                        'content': content,
                        'content_hash': VersionFile.compute_hash(content),
                    }
                )
        
        return Response({
            'success': True,
            'message': f'Saved {len(files)} files',
            'version_id': str(version.id),
        })
    
    @action(detail=True, methods=['post'])
    def rollback(self, request, pk=None):
        """
        Enhanced rollback to a previous version (editor+ only).
        
        Supports:
        - dry_run=True for preview mode (shows what would change)
        - include_schema=True to revert schema changes
        - Full audit logging
        - Append-only version history (rollback creates new version)
        """
        version = self.get_object()
        app = version.internal_app
        
        # Verify editor or above access
        membership, error = require_editor_or_above(request, app.organization)
        if error:
            return error
        
        # Get options from request
        dry_run = request.data.get('dry_run', False)
        include_schema = request.data.get('include_schema', True)
        
        # Get current stable version for comparison
        current_version = VersionService.get_latest_stable_version(app)
        
        # If dry_run, return preview of what would change
        if dry_run:
            if current_version:
                preview = SnapshotService.preview_revert(current_version, version)
                
                # Add schema compatibility info if reverting schema
                if include_schema:
                    try:
                        current_snapshot = SnapshotService.ensure_snapshot_exists(current_version)
                        target_snapshot = SnapshotService.ensure_snapshot_exists(version)
                        
                        schema_compatibility = SchemaMigrationService.check_table_compatibility(
                            current_snapshot.tables_json,
                            target_snapshot.tables_json
                        )
                        preview['schema_compatibility'] = schema_compatibility
                        
                        # Add migration hints
                        if schema_compatibility['risk_level'] != 'safe':
                            hints = []
                            for table in schema_compatibility['tables']:
                                if table.get('compatibility'):
                                    from_schema = table['compatibility'].get('from_schema', {})
                                    to_schema = table['compatibility'].get('to_schema', {})
                                    table_hints = SchemaMigrationService.generate_migration_hints(
                                        from_schema, to_schema
                                    )
                                    hints.extend(table_hints)
                            preview['migration_hints'] = hints
                    except Exception as e:
                        logger.warning(f"Failed to get schema compatibility: {e}")
                        preview['schema_compatibility'] = None
            else:
                preview = {
                    'diff': None,
                    'warnings': [],
                    'can_revert': True,
                    'target_version': {
                        'id': str(version.id),
                        'version_number': version.version_number,
                        'source': version.source,
                        'created_at': version.created_at.isoformat(),
                        'intent_message': version.intent_message,
                    },
                }
            
            # Log preview access for audit
            VersionAuditLog.log_operation(
                internal_app=app,
                app_version=version,
                operation=VersionAuditLog.OPERATION_PREVIEW,
                user=request.user,
                details={'action': 'rollback_preview', 'include_schema': include_schema},
                ip_address=self._get_client_ip(request),
                user_agent=request.META.get('HTTP_USER_AGENT'),
            )
            
            return Response(preview)
        
        # Execute the rollback
        try:
            rollback_version = SnapshotService.execute_rollback(
                target_version=version,
                user=request.user,
                include_schema=include_schema,
                ip_address=self._get_client_ip(request),
                user_agent=request.META.get('HTTP_USER_AGENT'),
            )
            
            return Response(
                AppVersionSerializer(rollback_version).data,
                status=status.HTTP_201_CREATED
            )
        except Exception as e:
            logger.error(f"Rollback failed: {e}")
            
            # Log failed operation
            VersionAuditLog.log_operation(
                internal_app=app,
                app_version=version,
                operation=VersionAuditLog.OPERATION_ROLLBACK,
                user=request.user,
                details={'action': 'rollback_failed', 'include_schema': include_schema},
                ip_address=self._get_client_ip(request),
                user_agent=request.META.get('HTTP_USER_AGENT'),
                success=False,
                error_message=str(e),
            )
            
            return Response(
                {'error': f'Rollback failed: {str(e)}'},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )
    
    @action(detail=True, methods=['get'])
    def snapshot(self, request, pk=None):
        """
        Get the state snapshot for a version.
        
        Returns the complete state captured at this version including
        tables, resources, and file counts.
        """
        version = self.get_object()
        app = version.internal_app
        
        # Verify access
        if not request.user.user_organizations.filter(organization=app.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Ensure snapshot exists (create if needed for backfill)
        snapshot = SnapshotService.ensure_snapshot_exists(version)
        
        return Response({
            'version_id': str(version.id),
            'version_number': version.version_number,
            'tables': snapshot.tables_json,
            'total_tables': snapshot.total_tables,
            'total_rows': snapshot.total_rows,
            'file_count': snapshot.file_count,
            'created_at': snapshot.created_at.isoformat(),
        })
    
    @action(detail=True, methods=['get'], url_path='diff/(?P<target_id>[^/.]+)')
    def diff(self, request, pk=None, target_id=None):
        """
        Compare this version with another version.
        
        Returns detailed diff including files, tables, and resources.
        """
        from_version = self.get_object()
        app = from_version.internal_app
        
        # Verify access
        if not request.user.user_organizations.filter(organization=app.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Get target version
        try:
            to_version = AppVersion.objects.get(pk=target_id, internal_app=app)
        except AppVersion.DoesNotExist:
            return Response(
                {'error': 'Target version not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        # Get the diff
        diff = SnapshotService.get_snapshot_diff(from_version, to_version)
        
        return Response(diff)
    
    @action(detail=False, methods=['get'], url_path='audit-trail')
    def audit_trail(self, request, internal_app_pk=None):
        """
        Get the audit trail for this app's versions.
        
        Returns history of all version operations for compliance.
        """
        app = get_object_or_404(InternalApp, pk=internal_app_pk)
        
        # Verify access
        if not request.user.user_organizations.filter(organization=app.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Get limit from query params
        limit = min(int(request.query_params.get('limit', 50)), 200)
        
        audit_trail = SnapshotService.get_audit_trail(app, limit)
        
        return Response({
            'app_id': str(app.id),
            'app_name': app.name,
            'entries': audit_trail,
            'count': len(audit_trail),
        })
    
    @action(detail=False, methods=['get'], url_path='history')
    def history(self, request, internal_app_pk=None):
        """
        Get version history with optional snapshot data.
        
        Query params:
        - include_snapshots: Include full snapshot data (default: false)
        - limit: Max versions to return (default: 50, max: 100)
        """
        app = get_object_or_404(InternalApp, pk=internal_app_pk)
        
        # Verify access
        if not request.user.user_organizations.filter(organization=app.organization).exists():
            return Response(
                {'error': 'Access denied'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Get options
        include_snapshots = request.query_params.get('include_snapshots', 'false').lower() == 'true'
        limit = min(int(request.query_params.get('limit', 50)), 100)
        
        history = SnapshotService.get_version_history(app, limit, include_snapshots)
        
        return Response({
            'app_id': str(app.id),
            'app_name': app.name,
            'versions': history,
            'count': len(history),
        })
    
    def _get_client_ip(self, request) -> str:
        """Extract client IP from request."""
        x_forwarded_for = request.META.get('HTTP_X_FORWARDED_FOR')
        if x_forwarded_for:
            return x_forwarded_for.split(',')[0].strip()
        return request.META.get('REMOTE_ADDR')

