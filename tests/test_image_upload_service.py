"""
Comprehensive tests for the Image Upload Service.

Tests both local storage (development) and cloud storage (production) modes.

Usage:
    cd /Users/mac/Engineering/internal-apps/internal-apps-backend
    source venv/bin/activate
    python tests/test_image_upload_service.py
"""
import os
import sys
import tempfile
import uuid
from io import BytesIO
from PIL import Image

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Setup Django settings
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'internal_apps.settings')

import django
django.setup()

from django.conf import settings
from django.core.files.uploadedfile import InMemoryUploadedFile
from vector_app.services.cloud_storage_service import CloudStorageService
from vector_app.services.image_upload_service import ImageUploadService


class TestImageUploadService:
    """Test suite for ImageUploadService."""
    
    def __init__(self):
        self.passed = 0
        self.failed = 0
        self.errors = []
    
    def create_test_image(self, format='PNG', size=(100, 100), color='red'):
        """Create a test image file."""
        image = Image.new('RGB', size, color)
        buffer = BytesIO()
        image.save(buffer, format=format)
        buffer.seek(0)
        return buffer
    
    def create_uploaded_file(self, content, name='test.png', content_type='image/png'):
        """Create an InMemoryUploadedFile for testing."""
        return InMemoryUploadedFile(
            file=content,
            field_name='file',
            name=name,
            content_type=content_type,
            size=len(content.getvalue()),
            charset=None
        )
    
    def test(self, name, condition, message=""):
        """Run a single test."""
        try:
            if condition:
                self.passed += 1
                print(f"  ✅ {name}")
            else:
                self.failed += 1
                error_msg = f"  ❌ {name}: {message}"
                print(error_msg)
                self.errors.append(error_msg)
        except Exception as e:
            self.failed += 1
            error_msg = f"  ❌ {name}: Exception - {str(e)}"
            print(error_msg)
            self.errors.append(error_msg)
    
    def run_all_tests(self):
        """Run all test cases."""
        print("\n" + "="*60)
        print("Image Upload Service Test Suite")
        print("="*60)
        
        # Check storage configuration
        is_cloud = CloudStorageService.is_configured()
        print(f"\nStorage Mode: {'Cloud (Cloudflare R2)' if is_cloud else 'Local (MEDIA_ROOT)'}")
        print(f"MEDIA_ROOT: {settings.MEDIA_ROOT}")
        if is_cloud:
            print(f"R2 Bucket: {getattr(settings, 'R2_BUCKET_NAME', 'N/A')}")
        
        print("\n--- Test Group: Basic Functionality ---")
        self.test_storage_mode_detection()
        
        print("\n--- Test Group: Image Upload from File ---")
        self.test_upload_image_png()
        self.test_upload_image_jpeg()
        
        print("\n--- Test Group: Image Upload from Bytes ---")
        self.test_upload_from_bytes()
        
        print("\n--- Test Group: URL Generation ---")
        self.test_get_image_url_local()
        if is_cloud:
            self.test_get_image_url_cloud()
        
        print("\n--- Test Group: Image Deletion ---")
        self.test_delete_image()
        
        print("\n--- Test Group: Validation ---")
        self.test_validation_file_size()
        self.test_validation_file_type()
        
        print("\n--- Test Group: Integration ---")
        self.test_full_workflow()
        
        # Summary
        print("\n" + "="*60)
        print(f"Results: {self.passed} passed, {self.failed} failed")
        print("="*60)
        
        if self.errors:
            print("\nErrors:")
            for error in self.errors:
                print(error)
        
        return self.failed == 0
    
    def test_storage_mode_detection(self):
        """Test that storage mode is correctly detected."""
        is_cloud = ImageUploadService.is_cloud_storage_enabled()
        self.test(
            "is_cloud_storage_enabled returns boolean",
            isinstance(is_cloud, bool),
            f"Expected bool, got {type(is_cloud)}"
        )
    
    def test_upload_image_png(self):
        """Test uploading a PNG image."""
        image_buffer = self.create_test_image(format='PNG', color='blue')
        uploaded_file = self.create_uploaded_file(
            image_buffer, name='test_blue.png', content_type='image/png'
        )
        
        result = ImageUploadService.upload_image(
            uploaded_file=uploaded_file,
            folder='test_uploads',
            filename_prefix='test_png',
        )
        
        self.test(
            "PNG upload returns success=True",
            result.get('success') is True,
            f"Result: {result}"
        )
        
        self.test(
            "PNG upload returns storage_key",
            bool(result.get('storage_key')),
            f"Missing storage_key in {result}"
        )
        
        self.test(
            "PNG upload returns url",
            bool(result.get('url')),
            f"Missing url in {result}"
        )
        
        # Cleanup
        if result.get('storage_key'):
            ImageUploadService.delete_image(result['storage_key'])
    
    def test_upload_image_jpeg(self):
        """Test uploading a JPEG image."""
        image_buffer = self.create_test_image(format='JPEG', color='green')
        uploaded_file = self.create_uploaded_file(
            image_buffer, name='test_green.jpg', content_type='image/jpeg'
        )
        
        result = ImageUploadService.upload_image(
            uploaded_file=uploaded_file,
            folder='test_uploads',
            filename_prefix='test_jpg',
        )
        
        self.test(
            "JPEG upload returns success=True",
            result.get('success') is True,
            f"Result: {result}"
        )
        
        # Cleanup
        if result.get('storage_key'):
            ImageUploadService.delete_image(result['storage_key'])
    
    def test_upload_from_bytes(self):
        """Test uploading image from raw bytes."""
        image_buffer = self.create_test_image(format='PNG', color='yellow')
        content = image_buffer.getvalue()
        
        result = ImageUploadService.upload_image_from_bytes(
            content=content,
            filename='test_bytes.png',
            content_type='image/png',
            folder='test_uploads',
            filename_prefix='bytes_test',
        )
        
        self.test(
            "Upload from bytes returns success=True",
            result.get('success') is True,
            f"Result: {result}"
        )
        
        # Cleanup
        if result.get('storage_key'):
            ImageUploadService.delete_image(result['storage_key'])
    
    def test_get_image_url_local(self):
        """Test URL generation for local storage."""
        # Test local:// prefix handling
        storage_key = "local://test_folder/test_image.png"
        url = ImageUploadService.get_image_url(storage_key)
        
        self.test(
            "get_image_url handles local:// prefix",
            url is not None and 'test_folder/test_image.png' in url,
            f"URL: {url}"
        )
        
        # Test legacy path handling (without prefix)
        legacy_key = "test_folder/test_image.png"
        url = ImageUploadService.get_image_url(legacy_key)
        
        self.test(
            "get_image_url handles legacy paths",
            url is not None and 'test_folder/test_image.png' in url,
            f"URL: {url}"
        )
    
    def test_get_image_url_cloud(self):
        """Test URL generation for cloud storage (if configured)."""
        # This will only generate a URL if cloud is configured
        storage_key = "r2://test_folder/test_image.png"
        url = ImageUploadService.get_image_url(storage_key)
        
        if CloudStorageService.is_configured():
            self.test(
                "get_image_url generates presigned URL for r2:// prefix",
                url is not None and 'r2.cloudflarestorage.com' in url,
                f"URL: {url}"
            )
        else:
            self.test(
                "get_image_url returns original for r2:// when not configured",
                url == storage_key or url is None,
                f"URL: {url}"
            )
    
    def test_delete_image(self):
        """Test image deletion."""
        # First upload an image
        image_buffer = self.create_test_image(format='PNG', color='purple')
        uploaded_file = self.create_uploaded_file(
            image_buffer, name='to_delete.png', content_type='image/png'
        )
        
        result = ImageUploadService.upload_image(
            uploaded_file=uploaded_file,
            folder='test_uploads',
            filename_prefix='delete_test',
        )
        
        if not result.get('success'):
            self.test("Delete test - upload failed", False, str(result))
            return
        
        storage_key = result['storage_key']
        
        # Now delete it
        deleted = ImageUploadService.delete_image(storage_key)
        
        self.test(
            "delete_image returns True for existing image",
            deleted is True,
            f"storage_key: {storage_key}"
        )
        
        # Verify it's gone (URL should fail or return None for cloud)
        # For local storage, we can check the file doesn't exist
        if storage_key.startswith('local://'):
            relative_path = storage_key[8:]
            full_path = os.path.join(settings.MEDIA_ROOT, relative_path)
            self.test(
                "Local file is actually deleted",
                not os.path.exists(full_path),
                f"File still exists at {full_path}"
            )
    
    def test_validation_file_size(self):
        """Test file size validation."""
        # Create a mock large file (we'll fake the size)
        image_buffer = self.create_test_image(format='PNG')
        
        class MockLargeFile:
            def __init__(self, buffer):
                self._buffer = buffer
                self.name = 'large.png'
                self.content_type = 'image/png'
                self.size = 10 * 1024 * 1024  # 10MB (over 5MB limit)
            
            def read(self):
                return self._buffer.getvalue()
            
            def seek(self, pos):
                self._buffer.seek(pos)
        
        large_file = MockLargeFile(image_buffer)
        
        result = ImageUploadService.upload_image(
            uploaded_file=large_file,
            folder='test_uploads',
            validate=True,
        )
        
        self.test(
            "Rejects files over size limit",
            result.get('success') is False and 'large' in result.get('error', '').lower(),
            f"Result: {result}"
        )
    
    def test_validation_file_type(self):
        """Test file type validation."""
        # Create a fake PDF file
        pdf_content = BytesIO(b'%PDF-1.4 fake pdf content')
        
        class MockPDFFile:
            def __init__(self, buffer):
                self._buffer = buffer
                self.name = 'document.pdf'
                self.content_type = 'application/pdf'
                self.size = len(buffer.getvalue())
            
            def read(self):
                return self._buffer.getvalue()
            
            def seek(self, pos):
                self._buffer.seek(pos)
        
        pdf_file = MockPDFFile(pdf_content)
        
        result = ImageUploadService.upload_image(
            uploaded_file=pdf_file,
            folder='test_uploads',
            validate=True,
        )
        
        self.test(
            "Rejects non-image files",
            result.get('success') is False,
            f"Result: {result}"
        )
    
    def test_full_workflow(self):
        """Test complete upload -> get URL -> delete workflow."""
        # Create image
        image_buffer = self.create_test_image(format='PNG', color='cyan', size=(200, 200))
        uploaded_file = self.create_uploaded_file(
            image_buffer, name='workflow_test.png', content_type='image/png'
        )
        
        # Upload
        upload_result = ImageUploadService.upload_image(
            uploaded_file=uploaded_file,
            folder=ImageUploadService.FOLDER_ORG_LOGOS,
            filename_prefix='org_test123',
        )
        
        self.test(
            "Full workflow: Upload succeeds",
            upload_result.get('success') is True,
            f"Result: {upload_result}"
        )
        
        if not upload_result.get('success'):
            return
        
        storage_key = upload_result['storage_key']
        storage_type = upload_result.get('storage_type', 'unknown')
        
        self.test(
            "Full workflow: Storage type is reported",
            storage_type in ['local', 'cloud'],
            f"Storage type: {storage_type}"
        )
        
        # Get URL
        url = ImageUploadService.get_image_url(storage_key)
        
        self.test(
            "Full workflow: Can get URL from storage key",
            url is not None and url.startswith('http'),
            f"URL: {url}"
        )
        
        # Test URL format based on storage type
        if storage_type == 'cloud':
            self.test(
                "Full workflow: Cloud URL is presigned",
                'X-Amz-' in url or 'r2.cloudflarestorage.com' in url,
                f"URL: {url}"
            )
        else:
            self.test(
                "Full workflow: Local URL contains media path",
                '/media/' in url,
                f"URL: {url}"
            )
        
        # Delete
        deleted = ImageUploadService.delete_image(storage_key)
        
        self.test(
            "Full workflow: Delete succeeds",
            deleted is True,
            f"storage_key: {storage_key}"
        )
        
        print(f"\n  ℹ️  Workflow completed with storage_type: {storage_type}")


def main():
    """Run all tests."""
    test_suite = TestImageUploadService()
    success = test_suite.run_all_tests()
    
    print("\n" + "="*60)
    if success:
        print("🎉 All tests passed!")
    else:
        print("❌ Some tests failed. See errors above.")
    print("="*60 + "\n")
    
    return 0 if success else 1


if __name__ == '__main__':
    sys.exit(main())

