"""
Encryption utilities for secrets (service role keys, user JWTs).
Uses Fernet symmetric encryption from cryptography library.
"""

import base64
import os
from typing import Optional
from cryptography.fernet import Fernet
from django.conf import settings

# Get encryption key from settings (should be set in environment)
# Fallback to a generated key for development (not secure for production!)
_encryption_key: Optional[bytes] = None


def get_encryption_key() -> bytes:
    """
    Get the encryption key from settings or generate one.

    In production, ENCRYPTION_KEY should be set in environment variables.
    For development, this will generate a key (not secure!).
    """
    global _encryption_key

    if _encryption_key is not None:
        return _encryption_key

    key_str = getattr(settings, "ENCRYPTION_KEY", None)

    if key_str:
        # Key should be base64-encoded Fernet key
        try:
            _encryption_key = key_str.encode() if isinstance(key_str, str) else key_str
            # Validate it's a valid Fernet key
            Fernet(_encryption_key)
            return _encryption_key
        except Exception:
            raise ValueError("Invalid ENCRYPTION_KEY format. Must be a valid Fernet key.")
    else:
        # Development fallback - generate a key
        # WARNING: This is not secure for production!
        if getattr(settings, "DEBUG", False):
            # For development, derive from SECRET_KEY
            import hashlib

            key_bytes = hashlib.sha256(settings.SECRET_KEY.encode()).digest()
            _encryption_key = base64.urlsafe_b64encode(key_bytes)
            return _encryption_key
        else:
            raise ValueError(
                "ENCRYPTION_KEY must be set in production. "
                "Generate one with: python -c 'from cryptography.fernet import Fernet; print(Fernet.generate_key().decode())'"
            )


def encrypt_string(plaintext: str) -> str:
    """
    Encrypt a string and return base64-encoded ciphertext.

    Args:
        plaintext: String to encrypt

    Returns:
        Base64-encoded encrypted string
    """
    if not plaintext:
        return ""

    key = get_encryption_key()
    fernet = Fernet(key)
    encrypted = fernet.encrypt(plaintext.encode())
    return base64.urlsafe_b64encode(encrypted).decode()


def decrypt_string(ciphertext: str) -> str:
    """
    Decrypt a base64-encoded ciphertext and return plaintext.

    Args:
        ciphertext: Base64-encoded encrypted string

    Returns:
        Decrypted string

    Raises:
        ValueError: If decryption fails
    """
    if not ciphertext:
        return ""

    try:
        key = get_encryption_key()
        fernet = Fernet(key)
        encrypted_bytes = base64.urlsafe_b64decode(ciphertext.encode())
        decrypted = fernet.decrypt(encrypted_bytes)
        return decrypted.decode()
    except Exception as e:
        raise ValueError(f"Failed to decrypt: {str(e)}")


def encrypt_json(data: dict) -> str:
    """
    Encrypt a JSON-serializable dictionary.

    Args:
        data: Dictionary to encrypt

    Returns:
        Base64-encoded encrypted JSON string
    """
    import json

    json_str = json.dumps(data)
    return encrypt_string(json_str)


def decrypt_json(ciphertext: str) -> dict:
    """
    Decrypt and parse JSON.

    Args:
        ciphertext: Base64-encoded encrypted JSON string

    Returns:
        Decrypted dictionary

    Raises:
        ValueError: If decryption or JSON parsing fails
    """
    import json

    json_str = decrypt_string(ciphertext)
    return json.loads(json_str)
