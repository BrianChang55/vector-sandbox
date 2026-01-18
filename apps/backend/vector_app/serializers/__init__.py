"""
DRF Serializers for Vector Internal Apps
"""
from .organization import OrganizationSerializer, OrganizationCreateSerializer, UserOrganizationSerializer
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
from .task_serializers import (
    TaskSerializer,
    TaskCreateSerializer,
    TaskUpdateSerializer,
)

__all__ = [
    'OrganizationSerializer',
    'OrganizationCreateSerializer',
    'UserOrganizationSerializer',
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
    'TaskSerializer',
    'TaskCreateSerializer',
    'TaskUpdateSerializer',
]

