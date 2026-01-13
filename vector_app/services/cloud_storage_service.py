"""
Cloud Storage Service

Provides cloud storage functionality using Cloudflare R2 (S3-compatible).
Uses presigned URLs for secure, time-limited access to private buckets.

Environment Behavior:
- Development (R2 not configured): Falls back to local MEDIA_ROOT storage
- Production (R2 configured): Uploads to Cloudflare R2 with presigned URLs

Security:
- Bucket remains private (no public access)
- Presigned URLs expire after set duration
- Access control can be verified before URL generation
- URLs are cryptographically signed, preventing tampering

Setup:
1. Create a Cloudflare R2 bucket at https://dash.cloudflare.com/
2. Keep bucket PRIVATE (do not enable public access)
3. Create API tokens with read/write access
4. Set environment variables (see settings.py)

Usage:
    from vector_app.services.cloud_storage_service import CloudStorageService
    
    # Upload file - returns object key (stored in database)
    object_key = CloudStorageService.upload_file(
        content=file_bytes,
        filename="image.png",
        content_type="image/png",
        folder="org_logos"
    )
    
    # Generate presigned URL for user access (1 hour expiry)
    url = CloudStorageService.get_presigned_url(object_key)
"""
import os
import logging
import hashlib
import uuid
from typing import Optional
from django.conf import settings

logger = logging.getLogger(__name__)

# Try to import boto3 for S3-compatible storage
try:
    import boto3
    from botocore.exceptions import ClientError
    HAS_BOTO3 = True
except ImportError:
    HAS_BOTO3 = False
    logger.warning("boto3 not installed - cloud storage disabled. Install with: pip install boto3")


class CloudStorageService:
    """
    Service for uploading files to Cloudflare R2 (S3-compatible storage).
    
    Uses presigned URLs for secure access to private buckets.
    
    R2 provides:
    - Private bucket storage (secure by default)
    - Presigned URLs for time-limited access
    - S3-compatible API (works with boto3)
    - Generous free tier (10GB storage, 10M requests/month)
    - No egress fees
    
    Falls back to local storage if R2 is not configured.
    """
    
    _client = None
    _is_configured = None
    
    @classmethod
    def is_configured(cls) -> bool:
        """Check if cloud storage is properly configured."""
        if cls._is_configured is not None:
            return cls._is_configured
        
        if not HAS_BOTO3:
            cls._is_configured = False
            return False
        
        required_settings = [
            'R2_ACCESS_KEY_ID',
            'R2_SECRET_ACCESS_KEY',
            'R2_BUCKET_NAME',
            'R2_ENDPOINT_URL',
        ]
        
        cls._is_configured = all(
            getattr(settings, setting, None)
            for setting in required_settings
        )
        
        if cls._is_configured:
            logger.info("Cloud storage (R2) is configured and ready (using presigned URLs)")
        else:
            missing = [s for s in required_settings if not getattr(settings, s, None)]
            logger.debug(f"Cloud storage not configured. Missing: {missing}")
        
        return cls._is_configured
    
    @classmethod
    def _get_client(cls):
        """Get or create boto3 S3 client for R2."""
        if cls._client is not None:
            return cls._client
        
        if not HAS_BOTO3:
            raise RuntimeError("boto3 is not installed. Install with: pip install boto3")
        
        if not cls.is_configured():
            raise RuntimeError("Cloud storage is not configured. Check R2 settings.")
        
        cls._client = boto3.client(
            's3',
            endpoint_url=settings.R2_ENDPOINT_URL,
            aws_access_key_id=settings.R2_ACCESS_KEY_ID,
            aws_secret_access_key=settings.R2_SECRET_ACCESS_KEY,
            region_name='auto',  # R2 uses 'auto' region
        )
        
        return cls._client
    
    @classmethod
    def upload_file(
        cls,
        content: bytes,
        filename: str,
        content_type: str = 'application/octet-stream',
        folder: str = 'uploads',
    ) -> Optional[str]:
        """
        Upload a file to R2 and return the object key.
        
        The object key should be stored in your database. To get a URL,
        use get_presigned_url() with the returned key.
        
        Args:
            content: File content as bytes
            filename: Filename (will be prefixed with unique ID)
            content_type: MIME type of the file
            folder: Folder path within the bucket (e.g., 'org_logos')
            
        Returns:
            Object key (path) for the uploaded file, or None if upload fails
            Example: 'org_logos/abc123_image.png'
        """
        if not cls.is_configured():
            logger.warning("Cloud storage not configured, cannot upload")
            return None
        
        try:
            client = cls._get_client()
            
            # Generate unique key with folder structure
            unique_id = uuid.uuid4().hex[:12]
            key = f"{folder}/{unique_id}_{filename}".lstrip('/')
            
            # Upload to R2 (private bucket)
            client.put_object(
                Bucket=settings.R2_BUCKET_NAME,
                Key=key,
                Body=content,
                ContentType=content_type,
            )
            
            logger.info(f"Successfully uploaded to R2: {key} ({len(content)} bytes)")
            
            # Return object key (not URL) - URLs are generated on-demand via presigned URLs
            return key
            
        except ClientError as e:
            logger.error(f"R2 upload failed: {e}")
            return None
        except Exception as e:
            logger.error(f"Unexpected error uploading to R2: {e}")
            return None
    
    @classmethod
    def get_presigned_url(
        cls,
        object_key: str,
        for_api: bool = False,
    ) -> Optional[str]:
        """
        Generate a presigned (signed) URL for accessing a file in R2.
        
        Presigned URLs are:
        - Time-limited (expire after set duration)
        - Cryptographically signed (cannot be forged)
        - Only work for the specific object key
        
        Args:
            object_key: The R2 object key (path) returned from upload_file()
            for_api: If True, use longer expiry for external API access (default: False)
                    - False: User access (1 hour default)
                    - True: API access (24 hours default)
        
        Returns:
            Presigned URL that can be used to access the file, or None if generation fails
        """
        if not cls.is_configured():
            return None
        
        if not object_key:
            logger.warning("Object key is required for presigned URL generation")
            return None
        
        try:
            client = cls._get_client()
            
            # Get expiry time from settings
            if for_api:
                expiry = getattr(settings, 'R2_PRESIGNED_URL_EXPIRY_API', 86400)  # 24 hours default
            else:
                expiry = getattr(settings, 'R2_PRESIGNED_URL_EXPIRY', 3600)  # 1 hour default
            
            # Generate presigned URL
            url = client.generate_presigned_url(
                'get_object',
                Params={
                    'Bucket': settings.R2_BUCKET_NAME,
                    'Key': object_key,
                },
                ExpiresIn=expiry,
            )
            
            logger.debug(f"Generated presigned URL for {object_key} (expires in {expiry}s)")
            return url
            
        except ClientError as e:
            logger.error(f"Failed to generate presigned URL for {object_key}: {e}")
            return None
        except Exception as e:
            logger.error(f"Unexpected error generating presigned URL: {e}")
            return None
    
    @classmethod
    def extract_object_key_from_url(cls, url: str) -> Optional[str]:
        """
        Extract R2 object key from a URL (presigned or public).
        
        Useful for migrating from public URLs or handling legacy data.
        
        Args:
            url: Full URL (can be presigned or public URL)
            
        Returns:
            Object key if URL is from R2, None otherwise
        """
        if not url:
            return None
        
        # Handle presigned URLs from boto3
        endpoint_url = getattr(settings, 'R2_ENDPOINT_URL', '')
        bucket_name = getattr(settings, 'R2_BUCKET_NAME', '')
        
        if endpoint_url and endpoint_url in url:
            try:
                from urllib.parse import urlparse, unquote
                parsed = urlparse(url)
                # URL-decode the path to get the actual object key
                path = unquote(parsed.path.lstrip('/'))
                
                if path:
                    # Remove bucket name from path if present (path-style URLs)
                    if bucket_name and path.startswith(bucket_name + '/'):
                        key = path[len(bucket_name) + 1:]
                        return key if key else None
                    elif bucket_name and path.startswith(bucket_name):
                        key = path[len(bucket_name):].lstrip('/')
                        return key if key else None
                    else:
                        # Path is the object key directly (virtual-hosted style)
                        return path
            except Exception:
                pass
        
        # Handle public URLs (if R2_PUBLIC_URL was set)
        public_url = getattr(settings, 'R2_PUBLIC_URL', None)
        if public_url and url.startswith(public_url):
            from urllib.parse import unquote
            key = url[len(public_url):].lstrip('/').split('?')[0]
            return unquote(key)
        
        return None
    
    @classmethod
    def delete_file(cls, object_key_or_url: str) -> bool:
        """
        Delete a file from R2 by its object key or URL.
        
        Args:
            object_key_or_url: Object key (e.g., 'org_logos/image.png') or full URL
            
        Returns:
            True if deleted successfully, False otherwise
        """
        if not cls.is_configured():
            return False
        
        try:
            client = cls._get_client()
            
            # Extract object key (handle both keys and URLs)
            if '/' in object_key_or_url and ('http' in object_key_or_url or '?' in object_key_or_url):
                # Looks like a URL, extract key
                key = cls.extract_object_key_from_url(object_key_or_url)
                if not key:
                    logger.warning(f"Could not extract object key from URL: {object_key_or_url}")
                    return False
            else:
                # Assume it's already an object key
                key = object_key_or_url
            
            client.delete_object(
                Bucket=settings.R2_BUCKET_NAME,
                Key=key,
            )
            
            logger.info(f"Successfully deleted from R2: {key}")
            return True
            
        except Exception as e:
            logger.error(f"Failed to delete from R2: {e}")
            return False
    
    @classmethod
    def ensure_user_url(cls, url_or_key: str) -> str:
        """
        Ensure a URL or object key is converted to a presigned URL suitable for user access.
        
        This function handles:
        - R2 object keys (format: "r2://object-key") → converts to presigned URL with user expiry
        - Existing R2 presigned URLs → regenerates fresh presigned URL
        - Other URLs (localhost, external) → returns as-is
        
        Use this when returning URLs to users via API responses.
        
        Args:
            url_or_key: URL string or R2 object key reference
            
        Returns:
            URL that users can access (presigned URL for R2, original URL otherwise)
        """
        if not url_or_key:
            return url_or_key
        
        # Check if it's an R2 object key reference (format: "r2://object-key")
        if url_or_key.startswith('r2://'):
            object_key = url_or_key[5:]  # Remove "r2://" prefix
            presigned_url = cls.get_presigned_url(object_key, for_api=False)
            if presigned_url:
                return presigned_url
            else:
                logger.warning(f"Failed to generate presigned URL for {object_key}, returning original")
                return url_or_key
        
        # Check if it's already an R2 presigned URL - regenerate fresh URL
        if url_or_key.startswith('http://') or url_or_key.startswith('https://'):
            # Try to extract object key from R2 presigned URL
            object_key = cls.extract_object_key_from_url(url_or_key)
            if object_key:
                # It's an R2 URL - regenerate fresh presigned URL
                presigned_url = cls.get_presigned_url(object_key, for_api=False)
                if presigned_url:
                    return presigned_url
                else:
                    logger.warning(f"Failed to regenerate presigned URL for {object_key}, returning original")
            # Not an R2 URL or regeneration failed - return as-is
            return url_or_key
        
        # Might be a plain object key (without r2:// prefix)
        if cls.is_configured():
            presigned_url = cls.get_presigned_url(url_or_key, for_api=False)
            if presigned_url:
                return presigned_url
        
        # Fallback: return original
        return url_or_key
    
    @classmethod
    def _get_extension_from_content_type(cls, content_type: str) -> str:
        """Get file extension from content type."""
        content_type_map = {
            'image/png': '.png',
            'image/jpeg': '.jpg',
            'image/jpg': '.jpg',
            'image/gif': '.gif',
            'image/webp': '.webp',
            'image/svg+xml': '.svg',
        }
        
        # Clean content type (remove parameters)
        clean_type = content_type.split(';')[0].strip().lower()
        return content_type_map.get(clean_type, '.bin')

