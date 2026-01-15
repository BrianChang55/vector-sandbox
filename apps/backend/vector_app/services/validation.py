"""
AppSpec validation service
"""
import logging
from typing import Dict, Any, List, Tuple
from ..models import InternalApp

logger = logging.getLogger(__name__)


class AppSpecValidationService:
    """Service for validating AppSpec JSON structure."""
    
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
        
        # Validate each page has expected structure
        pages = spec_json.get('pages', [])
        for i, page in enumerate(pages):
            page_errors = AppSpecValidationService._validate_page(page, i)
            errors.extend(page_errors)

        return len(errors) == 0, errors
    
    @staticmethod
    def _validate_page(page: Dict[str, Any], page_index: int) -> List[str]:
        """Validate a single page spec."""
        errors = []
        prefix = f"pages[{page_index}]"
        
        # Validate required fields
        if 'primaryResource' not in page:
            errors.append(f"{prefix}: missing 'primaryResource'")
            return errors
        
        # Validate view structure
        if 'view' not in page:
            errors.append(f"{prefix}: missing 'view'")
            return errors
        
        view = page['view']
        
        return errors

