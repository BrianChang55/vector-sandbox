"""
Type definitions for the accounts app.
"""
from enum import StrEnum


class UserOrganizationRole(StrEnum):
    ADMIN = "admin"
    EDITOR = "editor"
    VIEWER = "viewer"
