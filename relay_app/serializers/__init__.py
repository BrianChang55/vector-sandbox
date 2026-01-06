"""
DRF Serializers for Relay Internal Apps
"""
from .organization import OrganizationSerializer, OrganizationCreateSerializer, UserOrganizationSerializer
from .backend_connection import BackendConnectionSerializer, BackendConnectionCreateSerializer, BackendConnectionTestSerializer
from .resource_registry import ResourceRegistryEntrySerializer
from .internal_app import InternalAppSerializer, InternalAppCreateSerializer, AppFavoriteSerializer
from .version import (
    AppVersionSerializer,
    AppVersionListSerializer,
    AppVersionCreateSerializer,
    VersionFileSerializer,
    CodeEditSerializer,
    VersionStateSnapshotSerializer,
    VersionStateSnapshotSummarySerializer,
    AppVersionWithSnapshotSerializer,
    VersionAuditLogSerializer,
    VersionAuditLogSummarySerializer,
    VersionDiffSerializer,
    RollbackPreviewSerializer,
    RollbackRequestSerializer,
)
from .action_log import ActionExecutionLogSerializer
from .auth_serializers import UserSerializer, SignUpSerializer, LoginSerializer, MagicLinkRequestSerializer, MagicLinkVerifySerializer
from .member_serializers import (
    OrgMemberSerializer,
    OrgMemberUpdateSerializer,
    OrgInviteSerializer,
    OrgInviteCreateSerializer,
    InviteAcceptSerializer,
    InviteVerifySerializer,
    InviteDetailsSerializer,
    MembersListResponseSerializer,
)

__all__ = [
    'OrganizationSerializer',
    'OrganizationCreateSerializer',
    'UserOrganizationSerializer',
    'BackendConnectionSerializer',
    'BackendConnectionCreateSerializer',
    'BackendConnectionTestSerializer',
    'ResourceRegistryEntrySerializer',
    'InternalAppSerializer',
    'InternalAppCreateSerializer',
    'AppFavoriteSerializer',
    'AppVersionSerializer',
    'AppVersionListSerializer',
    'AppVersionCreateSerializer',
    'VersionFileSerializer',
    'CodeEditSerializer',
    'VersionStateSnapshotSerializer',
    'VersionStateSnapshotSummarySerializer',
    'AppVersionWithSnapshotSerializer',
    'VersionAuditLogSerializer',
    'VersionAuditLogSummarySerializer',
    'VersionDiffSerializer',
    'RollbackPreviewSerializer',
    'RollbackRequestSerializer',
    'ActionExecutionLogSerializer',
    'UserSerializer',
    'SignUpSerializer',
    'LoginSerializer',
    'MagicLinkRequestSerializer',
    'MagicLinkVerifySerializer',
    'OrgMemberSerializer',
    'OrgMemberUpdateSerializer',
    'OrgInviteSerializer',
    'OrgInviteCreateSerializer',
    'InviteAcceptSerializer',
    'InviteVerifySerializer',
    'InviteDetailsSerializer',
    'MembersListResponseSerializer',
]

