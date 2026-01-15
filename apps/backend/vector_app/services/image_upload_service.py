"""
Image Upload Service

Abstracted image upload service that handles both local storage (development)
and Cloudflare R2 cloud storage (production).

Environment Behavior:
- Development (R2 not configured): Stores images locally in MEDIA_ROOT
- Production (R2 configured): Uploads images to Cloudflare R2

Usage:
    from vector_app.services.image_upload_service import ImageUploadService
    
    # Upload an image file (from Django UploadedFile)
    result = ImageUploadService.upload_image(
        uploaded_file=request.FILES['image'],
        folder='org_logos',
        filename_prefix='org_123'
    )
    
    if result['success']:
        # Store result['storage_key'] in database
        # Use result['url'] to display the image
        
    # Get URL for a stored image
    url = ImageUploadService.get_image_url(storage_key)
    
    # Delete an image
    ImageUploadService.delete_image(storage_key)
"""
import os
import uuid
import logging
import mimetypes
from typing import Optional, Dict, Any, Union
from io import BytesIO

from django.conf import settings
from django.core.files.uploadedfile import UploadedFile

from .cloud_storage_service import CloudStorageService

logger = logging.getLogger(__name__)


class ImageUploadService:
    """
    Service for uploading, retrieving, and managing images.
    
    Provides a unified interface that automatically routes to either
    local storage or cloud storage (Cloudflare R2) based on configuration.
    
    Storage Keys:
    - Local storage: 'local://relative/path/to/file.png'
    - Cloud storage: 'r2://folder/unique_filename.png'
    
    The storage key format allows the service to know where the image
    is stored and how to generate URLs for it.
    """
    
    # Allowed image content types
    ALLOWED_CONTENT_TYPES = {
        'image/jpeg',
        'image/jpg',
        'image/png',
        'image/gif',
        'image/webp',
        'image/svg+xml',
    }
    
    # Maximum file size (5 MB)
    MAX_FILE_SIZE = 5 * 1024 * 1024
    
    # Default folders for different image types
    FOLDER_ORG_LOGOS = 'org_logos'
    FOLDER_USER_AVATARS = 'user_avatars'
    FOLDER_UPLOADS = 'uploads'
    
    @classmethod
    def upload_image(
        cls,
        uploaded_file: UploadedFile,
        folder: str = FOLDER_UPLOADS,
        filename_prefix: Optional[str] = None,
        validate: bool = True,
    ) -> Dict[str, Any]:
        """
        Upload an image file and return storage information.
        
        Args:
            uploaded_file: Django UploadedFile object (from request.FILES)
            folder: Folder/category for the image (e.g., 'org_logos', 'user_avatars')
            filename_prefix: Optional prefix for the filename (e.g., org ID)
            validate: Whether to validate file type and size
            
        Returns:
            Dict with keys:
                - success: bool - Whether upload succeeded
                - storage_key: str - Key to store in database (e.g., 'r2://folder/file.png')
                - url: str - URL to access the image
                - error: str - Error message if failed (only if success=False)
        """
        try:
            # Validate file if requested
            if validate:
                validation_error = cls._validate_image(uploaded_file)
                if validation_error:
                    return {'success': False, 'error': validation_error}
            
            # Read file content
            content = uploaded_file.read()
            uploaded_file.seek(0)  # Reset file pointer
            
            # Get content type
            content_type = uploaded_file.content_type or 'application/octet-stream'
            
            # Generate filename
            original_name = uploaded_file.name or 'image'
            extension = cls._get_extension(original_name, content_type)
            unique_id = uuid.uuid4().hex[:12]
            
            if filename_prefix:
                filename = f"{filename_prefix}_{unique_id}{extension}"
            else:
                filename = f"{unique_id}{extension}"
            
            # Try cloud storage first
            if CloudStorageService.is_configured():
                result = cls._upload_to_cloud(content, filename, content_type, folder)
            else:
                result = cls._upload_to_local(content, filename, folder)
            
            return result
            
        except Exception as e:
            logger.error(f"Error uploading image: {e}")
            return {'success': False, 'error': str(e)}
    
    @classmethod
    def upload_image_from_bytes(
        cls,
        content: bytes,
        filename: str,
        content_type: str = 'image/png',
        folder: str = FOLDER_UPLOADS,
        filename_prefix: Optional[str] = None,
    ) -> Dict[str, Any]:
        """
        Upload image from raw bytes.
        
        Args:
            content: Image content as bytes
            filename: Original filename (used for extension detection)
            content_type: MIME type of the image
            folder: Storage folder
            filename_prefix: Optional prefix for the filename
            
        Returns:
            Dict with success, storage_key, url, or error
        """
        try:
            # Generate unique filename
            extension = cls._get_extension(filename, content_type)
            unique_id = uuid.uuid4().hex[:12]
            
            if filename_prefix:
                final_filename = f"{filename_prefix}_{unique_id}{extension}"
            else:
                final_filename = f"{unique_id}{extension}"
            
            # Route to appropriate storage
            if CloudStorageService.is_configured():
                return cls._upload_to_cloud(content, final_filename, content_type, folder)
            else:
                return cls._upload_to_local(content, final_filename, folder)
                
        except Exception as e:
            logger.error(f"Error uploading image from bytes: {e}")
            return {'success': False, 'error': str(e)}
    
    @classmethod
    def get_image_url(cls, storage_key: str) -> Optional[str]:
        """
        Get a URL for accessing an image by its storage key.
        
        Args:
            storage_key: Storage key returned from upload_image()
                        (e.g., 'r2://folder/file.png' or 'local://path/file.png')
                        
        Returns:
            URL to access the image, or None if invalid
        """
        if not storage_key:
            return None
        
        try:
            # Cloud storage (R2)
            if storage_key.startswith('r2://'):
                object_key = storage_key[5:]  # Remove 'r2://' prefix
                return CloudStorageService.get_presigned_url(object_key, for_api=False)
            
            # Local storage
            if storage_key.startswith('local://'):
                relative_path = storage_key[8:]  # Remove 'local://' prefix
                base_url = getattr(settings, 'BASE_URL', '').rstrip('/')
                media_url = settings.MEDIA_URL.rstrip('/')
                return f"{base_url}{media_url}/{relative_path}"
            
            # Legacy format: assume it's a relative path (local storage)
            if not storage_key.startswith('http'):
                base_url = getattr(settings, 'BASE_URL', '').rstrip('/')
                media_url = settings.MEDIA_URL.rstrip('/')
                return f"{base_url}{media_url}/{storage_key}"
            
            # Already a URL
            return storage_key
            
        except Exception as e:
            logger.error(f"Error getting image URL for {storage_key}: {e}")
            return None
    
    @classmethod
    def delete_image(cls, storage_key: str) -> bool:
        """
        Delete an image by its storage key.
        
        Args:
            storage_key: Storage key returned from upload_image()
            
        Returns:
            True if deleted successfully, False otherwise
        """
        if not storage_key:
            return False
        
        try:
            # Cloud storage (R2)
            if storage_key.startswith('r2://'):
                object_key = storage_key[5:]  # Remove 'r2://' prefix
                return CloudStorageService.delete_file(object_key)
            
            # Local storage
            if storage_key.startswith('local://'):
                relative_path = storage_key[8:]  # Remove 'local://' prefix
                full_path = os.path.join(settings.MEDIA_ROOT, relative_path)
                if os.path.exists(full_path):
                    os.remove(full_path)
                    logger.info(f"Deleted local image: {relative_path}")
                    return True
                return False
            
            # Legacy format: try to delete as local file
            if not storage_key.startswith('http'):
                full_path = os.path.join(settings.MEDIA_ROOT, storage_key)
                if os.path.exists(full_path):
                    os.remove(full_path)
                    logger.info(f"Deleted local image: {storage_key}")
                    return True
            
            return False
            
        except Exception as e:
            logger.error(f"Error deleting image {storage_key}: {e}")
            return False
    
    @classmethod
    def is_cloud_storage_enabled(cls) -> bool:
        """Check if cloud storage is configured and enabled."""
        return CloudStorageService.is_configured()
    
    @classmethod
    def _upload_to_cloud(
        cls,
        content: bytes,
        filename: str,
        content_type: str,
        folder: str,
    ) -> Dict[str, Any]:
        """Upload image to Cloudflare R2."""
        object_key = CloudStorageService.upload_file(
            content=content,
            filename=filename,
            content_type=content_type,
            folder=folder,
        )
        
        if not object_key:
            logger.warning("Cloud upload failed, falling back to local storage")
            return cls._upload_to_local(content, filename, folder)
        
        # Generate presigned URL for immediate use
        url = CloudStorageService.get_presigned_url(object_key, for_api=False)
        
        if not url:
            logger.warning("Failed to generate presigned URL after upload")
            return {'success': False, 'error': 'Failed to generate access URL'}
        
        storage_key = f"r2://{object_key}"
        
        logger.info(f"Uploaded image to cloud storage: {storage_key}")
        
        return {
            'success': True,
            'storage_key': storage_key,
            'url': url,
            'storage_type': 'cloud',
        }
    
    @classmethod
    def _upload_to_local(
        cls,
        content: bytes,
        filename: str,
        folder: str,
    ) -> Dict[str, Any]:
        """Upload image to local MEDIA_ROOT."""
        # Build path
        relative_path = os.path.join(folder, filename)
        full_path = os.path.join(settings.MEDIA_ROOT, relative_path)
        
        # Ensure directory exists
        os.makedirs(os.path.dirname(full_path), exist_ok=True)
        
        # Write file
        with open(full_path, 'wb') as f:
            f.write(content)
        
        # Build URL
        base_url = getattr(settings, 'BASE_URL', '').rstrip('/')
        media_url = settings.MEDIA_URL.rstrip('/')
        url = f"{base_url}{media_url}/{relative_path}"
        
        storage_key = f"local://{relative_path}"
        
        logger.info(f"Uploaded image to local storage: {storage_key} ({len(content)} bytes)")
        
        return {
            'success': True,
            'storage_key': storage_key,
            'url': url,
            'storage_type': 'local',
        }
    
    @classmethod
    def _validate_image(cls, uploaded_file: UploadedFile) -> Optional[str]:
        """
        Validate an uploaded image file.
        
        Returns:
            Error message if validation fails, None if valid
        """
        # Check content type
        content_type = uploaded_file.content_type
        if content_type not in cls.ALLOWED_CONTENT_TYPES:
            return f"Invalid file type: {content_type}. Allowed types: {', '.join(cls.ALLOWED_CONTENT_TYPES)}"
        
        # Check file size
        if uploaded_file.size > cls.MAX_FILE_SIZE:
            max_mb = cls.MAX_FILE_SIZE / (1024 * 1024)
            return f"File too large. Maximum size is {max_mb:.1f} MB"
        
        return None
    
    @classmethod
    def _get_extension(cls, filename: str, content_type: str) -> str:
        """Get file extension from filename or content type."""
        # Try to get from filename
        if '.' in filename:
            ext = os.path.splitext(filename)[1].lower()
            if ext in ['.jpg', '.jpeg', '.png', '.gif', '.webp', '.svg']:
                return ext
        
        # Infer from content type
        content_type_map = {
            'image/jpeg': '.jpg',
            'image/jpg': '.jpg',
            'image/png': '.png',
            'image/gif': '.gif',
            'image/webp': '.webp',
            'image/svg+xml': '.svg',
        }
        
        return content_type_map.get(content_type, '.png')

