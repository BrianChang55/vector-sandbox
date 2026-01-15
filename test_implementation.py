#!/usr/bin/env python
"""
Test script to verify core implementation works.
"""
import os
import sys
import django

# Setup Django
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'internal_apps.settings')
sys.path.insert(0, os.path.dirname(__file__))
django.setup()

from vector_app.models import User, Organization, UserOrganization, InternalApp
from vector_app.utils.encryption import encrypt_string, decrypt_string, encrypt_json, decrypt_json
from vector_app.services.validation import AppSpecValidationService
from vector_app.services.codegen import CodegenService

def test_encryption():
    """Test encryption utilities."""
    print("Testing encryption utilities...")
    
    # Test string encryption
    plaintext = "test_secret_key_12345"
    encrypted = encrypt_string(plaintext)
    decrypted = decrypt_string(encrypted)
    assert decrypted == plaintext, f"Encryption failed: {decrypted} != {plaintext}"
    print("✓ String encryption/decryption works")
    
    # Test JSON encryption
    test_json = {"key": "value", "number": 123}
    encrypted_json = encrypt_json(test_json)
    decrypted_json = decrypt_json(encrypted_json)
    assert decrypted_json == test_json, f"JSON encryption failed"
    print("✓ JSON encryption/decryption works")
    
    print("Encryption tests passed!\n")

def test_models():
    """Test model creation."""
    print("Testing models...")
    
    # Test User
    user = User(email="test@example.com")
    user.set_password("testpass123")
    user.save()
    print(f"✓ Created user: {user.email}")
    
    # Test Organization
    org = Organization(name="Test Org", slug="test-org")
    org.save()
    print(f"✓ Created organization: {org.name}")
    
    # Test UserOrganization
    user_org = UserOrganization(user=user, organization=org, role=UserOrganization.ROLE_ADMIN)
    user_org.save()
    print(f"✓ Created membership: {user.email} - {org.name}")
    
    # Test InternalApp
    app = InternalApp(
        organization=org,
        name="Test App",
        created_by=user
    )
    app.save()
    print(f"✓ Created internal app: {app.name}")
    
    print("Model tests passed!\n")
    
    # Cleanup
    app.delete()
    user_org.delete()
    org.delete()
    user.delete()

def test_validation():
    """Test AppSpec validation."""
    print("Testing AppSpec validation...")
    
    # Create minimal setup
    user = User.objects.create_user(email="valtest@example.com", password="test")
    org = Organization.objects.create(name="Val Org", slug="val-org")
    UserOrganization.objects.create(user=user, organization=org, role=UserOrganization.ROLE_ADMIN)
    app = InternalApp.objects.create(
        organization=org,
        name="Val App",
        created_by=user
    )
    
    # Test invalid spec (missing fields)
    invalid_spec = {}
    is_valid, errors = AppSpecValidationService.validate_app_spec(app, invalid_spec)
    assert not is_valid
    assert len(errors) > 0
    print(f"✓ Invalid spec correctly rejected ({len(errors)} errors)")
    
    # Cleanup
    app.delete()
    UserOrganization.objects.filter(organization=org).delete()
    org.delete()
    user.delete()
    
    print("Validation tests passed!\n")

def test_codegen():
    """Test code generation service."""
    print("Testing code generation...")
    
    assert len(CodegenService.ALLOWLISTED_PATHS) > 0
    print(f"✓ Allowlisted paths: {len(CodegenService.ALLOWLISTED_PATHS)} paths")
    
    runtime_client = CodegenService._generate_runtime_client()
    assert "runtimeQuery" in runtime_client
    assert "runtimeAction" in runtime_client
    print("✓ Runtime client generation works")
    
    types_content = CodegenService._generate_types()
    assert "AppSpec" in types_content
    print("✓ Types generation works")
    
    print("Codegen tests passed!\n")

if __name__ == "__main__":
    print("=" * 60)
    print("Testing Vector Internal Apps Implementation")
    print("=" * 60)
    print()
    
    try:
        test_encryption()
        test_models()
        test_validation()
        test_codegen()
        
        print("=" * 60)
        print("✓ All tests passed!")
        print("=" * 60)
        
    except Exception as e:
        print(f"\n✗ Test failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

