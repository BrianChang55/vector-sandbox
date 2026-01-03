"""
AppSpec validation service
"""
import logging
from typing import Dict, Any, List, Tuple
from ..models import InternalApp, ResourceRegistryEntry

logger = logging.getLogger(__name__)


class AppSpecValidationService:
    """Service for validating AppSpec JSON against Resource Registry."""
    
    @staticmethod
    def validate_app_spec(app: InternalApp, spec_json: Dict[str, Any]) -> Tuple[bool, List[str]]:
        """
        Validate AppSpec against the resource registry.
        
        Args:
            app: InternalApp instance
            spec_json: AppSpec JSON to validate
            
        Returns:
            Tuple of (is_valid, list_of_errors)
        """
        errors = []
        
        # Validate structure
        if not isinstance(spec_json, dict):
            errors.append("spec_json must be a dictionary")
            return False, errors
        
        if 'appName' not in spec_json:
            errors.append("spec_json must have 'appName' field")
        
        if 'pages' not in spec_json:
            errors.append("spec_json must have 'pages' field")
        
        if not isinstance(spec_json.get('pages', []), list):
            errors.append("'pages' must be a list")
        
        if errors:
            return False, errors
        
        # Get enabled resources for the app's backend
        enabled_resources = ResourceRegistryEntry.objects.filter(
            backend_connection=app.backend_connection,
            enabled=True
        )
        resource_ids = {r.resource_id for r in enabled_resources}
        
        # Validate each page
        pages = spec_json.get('pages', [])
        for i, page in enumerate(pages):
            page_errors = AppSpecValidationService._validate_page(
                page, resource_ids, enabled_resources, i
            )
            errors.extend(page_errors)
        
        return len(errors) == 0, errors
    
    @staticmethod
    def _validate_page(
        page: Dict[str, Any],
        resource_ids: set,
        enabled_resources: List[ResourceRegistryEntry],
        page_index: int
    ) -> List[str]:
        """Validate a single page spec."""
        errors = []
        prefix = f"pages[{page_index}]"
        
        # Validate required fields
        if 'primaryResource' not in page:
            errors.append(f"{prefix}: missing 'primaryResource'")
            return errors
        
        primary_resource = page['primaryResource']
        
        # Validate primary resource is enabled
        if primary_resource not in resource_ids:
            errors.append(f"{prefix}: primaryResource '{primary_resource}' is not enabled")
            return errors
        
        # Get resource registry entry
        resource_entry = next(
            (r for r in enabled_resources if r.resource_id == primary_resource),
            None
        )
        
        if not resource_entry:
            errors.append(f"{prefix}: resource '{primary_resource}' not found in registry")
            return errors
        
        # Validate view structure
        if 'view' not in page:
            errors.append(f"{prefix}: missing 'view'")
            return errors
        
        view = page['view']
        
        # Validate table view
        if 'table' in view:
            table_errors = AppSpecValidationService._validate_table_view(
                view['table'], resource_entry, prefix
            )
            errors.extend(table_errors)
        
        # Validate detail drawer
        if 'detailDrawer' in view:
            drawer_errors = AppSpecValidationService._validate_detail_drawer(
                view['detailDrawer'], resource_entry, prefix
            )
            errors.extend(drawer_errors)
        
        return errors
    
    @staticmethod
    def _validate_table_view(
        table: Dict[str, Any],
        resource_entry: ResourceRegistryEntry,
        prefix: str
    ) -> List[str]:
        """Validate table view configuration."""
        errors = []
        exposed_fields = set(resource_entry.exposed_fields_json or [])
        allowed_actions = {a.get('action_id') for a in (resource_entry.allowed_actions_json or [])}
        
        # Validate columns
        columns = table.get('columns', [])
        for i, col in enumerate(columns):
            field = col.get('field')
            if field and field not in exposed_fields:
                errors.append(f"{prefix}.view.table.columns[{i}]: field '{field}' is not exposed")
        
        # Validate filterable fields
        filterable_fields = table.get('filterableFields', [])
        for field in filterable_fields:
            if field not in exposed_fields:
                errors.append(f"{prefix}.view.table.filterableFields: field '{field}' is not exposed")
        
        # Validate searchable fields
        searchable_fields = table.get('searchableFields', [])
        for field in searchable_fields:
            if field not in exposed_fields:
                errors.append(f"{prefix}.view.table.searchableFields: field '{field}' is not exposed")
        
        # Validate row actions
        row_actions = table.get('rowActions', [])
        for i, action in enumerate(row_actions):
            action_id = action.get('actionId')
            if action_id and action_id not in allowed_actions:
                errors.append(f"{prefix}.view.table.rowActions[{i}]: action '{action_id}' is not allowlisted")
        
        # Validate bulk actions
        bulk_actions = table.get('bulkActions', [])
        for i, action in enumerate(bulk_actions):
            action_id = action.get('actionId')
            if action_id and action_id not in allowed_actions:
                errors.append(f"{prefix}.view.table.bulkActions[{i}]: action '{action_id}' is not allowlisted")
        
        return errors
    
    @staticmethod
    def _validate_detail_drawer(
        drawer: Dict[str, Any],
        resource_entry: ResourceRegistryEntry,
        prefix: str
    ) -> List[str]:
        """Validate detail drawer configuration."""
        errors = []
        exposed_fields = set(resource_entry.exposed_fields_json or [])
        allowed_actions = {a.get('action_id') for a in (resource_entry.allowed_actions_json or [])}
        
        # Validate fields
        fields = drawer.get('fields', [])
        for i, field_spec in enumerate(fields):
            field = field_spec.get('field')
            if field and field not in exposed_fields:
                errors.append(f"{prefix}.view.detailDrawer.fields[{i}]: field '{field}' is not exposed")
        
        # Validate actions
        actions = drawer.get('actions', [])
        for i, action in enumerate(actions):
            action_id = action.get('actionId')
            if action_id and action_id not in allowed_actions:
                errors.append(f"{prefix}.view.detailDrawer.actions[{i}]: action '{action_id}' is not allowlisted")
        
        return errors

