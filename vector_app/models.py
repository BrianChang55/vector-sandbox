"""
Vector Internal Apps - Data Models

MIGRATION NOTICE: Models have moved to domain-specific apps.
This file re-exports them for backward compatibility.

New locations:
- accounts: User, Organization, UserOrganization, OrganizationInvite, MagicLinkToken
- apps: InternalApp, AppFavorite, AppVersion, VersionFile
- chat: ChatSession, ChatMessage, CodeGenerationJob
- data_store: AppDataTable, AppDataRow, AppDataTableSnapshot
- integrations: MergeIntegrationProvider, ConnectorCache, OrganizationConnectorLink, 
                ConnectorToolAction, ConnectorExecutionLog
- audit: VersionStateSnapshot, VersionAuditLog

Import from the new locations for new code.
"""

# Re-export all models for backward compatibility
# accounts app
from accounts.models import (
    User,
    Organization,
    UserOrganization,
    OrganizationInvite,
    MagicLinkToken,
)
from accounts.types import UserOrganizationRole

# apps app
from apps.models import (
    InternalApp,
    AppFavorite,
    AppVersion,
    VersionFile,
)
from apps.types import (
    InternalAppStatus,
    AppVersionSource,
    AppVersionGenerationStatus,
    AppVersionValidationStatus,
)

# chat app
from chat.models import (
    ChatSession,
    ChatMessage,
    CodeGenerationJob,
)
from chat.types import (
    ChatMessageRole,
    ChatMessageStatus,
    CodeGenerationJobStatus,
)

# Note: CodeGenerationJobStatus is in chat.types (not apps.types) 
# because the CodeGenerationJob model is in the chat app

# data_store app
from data_store.models import (
    AppDataTable,
    AppDataRow,
    AppDataTableSnapshot,
)
from data_store.types import AppDataTableSnapshotOperation

# integrations app
from integrations.models import (
    MergeIntegrationProvider,
    ConnectorCache,
    OrganizationConnectorLink,
    ConnectorToolAction,
    ConnectorExecutionLog,
)
from integrations.types import ConnectorExecutionStatus

# audit app
from audit.models import (
    VersionStateSnapshot,
    VersionAuditLog,
)
from audit.types import VersionAuditOperation

# Also re-export ActionType for backward compatibility
from apps.action_classification.types import ActionType

__all__ = [
    # accounts
    "User",
    "Organization", 
    "UserOrganization",
    "OrganizationInvite",
    "MagicLinkToken",
    "UserOrganizationRole",
    # apps
    "InternalApp",
    "AppFavorite",
    "AppVersion",
    "VersionFile",
    "InternalAppStatus",
    "AppVersionSource",
    "AppVersionGenerationStatus",
    "AppVersionValidationStatus",
    # chat
    "ChatSession",
    "ChatMessage",
    "CodeGenerationJob",
    "ChatMessageRole",
    "ChatMessageStatus",
    "CodeGenerationJobStatus",
    # data_store
    "AppDataTable",
    "AppDataRow",
    "AppDataTableSnapshot",
    "AppDataTableSnapshotOperation",
    # integrations
    "MergeIntegrationProvider",
    "ConnectorCache",
    "OrganizationConnectorLink",
    "ConnectorToolAction",
    "ConnectorExecutionLog",
    "ConnectorExecutionStatus",
    # audit
    "VersionStateSnapshot",
    "VersionAuditLog",
    "VersionAuditOperation",
    # action_classification
    "ActionType",
]
