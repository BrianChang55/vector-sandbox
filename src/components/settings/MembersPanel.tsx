/**
 * MembersPanel - Organization member management panel for settings.
 *
 * Features:
 * - List all organization members with roles
 * - Show pending invitations
 * - Invite new members (admin only)
 * - Update member roles (admin only)
 * - Remove members (admin only)
 * - Resend/cancel invitations (admin only)
 */
import { useState } from 'react'
import {
  Users,
  UserPlus,
  Mail,
  MoreVertical,
  Loader2,
  Check,
  X,
  Clock,
  Shield,
  PencilLine,
  Eye,
  Trash2,
  RefreshCw,
  AlertCircle,
} from 'lucide-react'
import { Button } from '../ui/button'
import {
  Dialog,
  DialogBody,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '../ui/dialog'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '../ui/dropdown-menu'
import { useDialog } from '../ui/dialog-provider'
import {
  useOrgMembers,
  useUpdateMemberRole,
  useRemoveMember,
  useInviteMember,
  useCancelInvite,
  useResendInvite,
} from '../../hooks/useMembers'
import type { OrgMember, OrgInvite, OrgRole } from '../../types/models'
import { formatDistanceToNow } from 'date-fns'

interface MembersPanelProps {
  orgId: string
}

const ROLE_OPTIONS: { value: OrgRole; label: string; description: string; icon: React.ElementType }[] = [
  {
    value: 'admin',
    label: 'Admin',
    description: 'Manage members, integrations, and apps',
    icon: Shield,
  },
  {
    value: 'editor',
    label: 'Editor',
    description: 'Edit and build apps',
    icon: PencilLine,
  },
  {
    value: 'viewer',
    label: 'Viewer',
    description: 'View and run published apps',
    icon: Eye,
  },
]

export function MembersPanel({ orgId }: MembersPanelProps) {
  const { data: membersData, isLoading, error } = useOrgMembers(orgId)
  const { confirm, alert } = useDialog()

  const [inviteDialogOpen, setInviteDialogOpen] = useState(false)
  const [inviteEmail, setInviteEmail] = useState('')
  const [inviteRole, setInviteRole] = useState<OrgRole>('editor')

  const updateRole = useUpdateMemberRole()
  const removeMember = useRemoveMember()
  const inviteMember = useInviteMember()
  const cancelInvite = useCancelInvite()
  const resendInvite = useResendInvite()

  const members = membersData?.members ?? []
  const pendingInvites = membersData?.pending_invites ?? []
  const canManage = membersData?.can_manage_members ?? false
  const currentUserRole = membersData?.current_user_role

  const handleInvite = async () => {
    if (!inviteEmail.trim()) return

    try {
      await inviteMember.mutateAsync({
        orgId,
        email: inviteEmail.trim().toLowerCase(),
        role: inviteRole,
      })
      setInviteDialogOpen(false)
      setInviteEmail('')
      setInviteRole('editor')
      await alert({
        title: 'Invitation Sent',
        description: `An invitation has been sent to ${inviteEmail}.`,
        variant: 'success',
      })
    } catch (error: any) {
      await alert({
        title: 'Failed to Send Invitation',
        description: error?.response?.data?.email?.[0] || error?.response?.data?.error || 'Please try again.',
        variant: 'destructive',
      })
    }
  }

  const handleRoleChange = async (member: OrgMember, newRole: OrgRole) => {
    try {
      await updateRole.mutateAsync({
        orgId,
        memberId: member.id,
        role: newRole,
      })
    } catch (error: any) {
      await alert({
        title: 'Failed to Update Role',
        description: error?.response?.data?.role?.[0] || error?.response?.data?.error || 'Please try again.',
        variant: 'destructive',
      })
    }
  }

  const handleRemoveMember = async (member: OrgMember) => {
    const confirmed = await confirm({
      title: 'Remove Member',
      description: `Are you sure you want to remove ${member.email} from the organization?`,
      confirmText: 'Remove',
      variant: 'destructive',
    })

    if (confirmed) {
      try {
        await removeMember.mutateAsync({
          orgId,
          memberId: member.id,
        })
      } catch (error: any) {
        await alert({
          title: 'Failed to Remove Member',
          description: error?.response?.data?.error || 'Please try again.',
          variant: 'destructive',
        })
      }
    }
  }

  const handleCancelInvite = async (invite: OrgInvite) => {
    const confirmed = await confirm({
      title: 'Cancel Invitation',
      description: `Cancel the invitation to ${invite.email}?`,
      confirmText: 'Cancel Invitation',
      variant: 'destructive',
    })

    if (confirmed) {
      try {
        await cancelInvite.mutateAsync({
          orgId,
          inviteId: invite.id,
        })
      } catch (error: any) {
        await alert({
          title: 'Failed to Cancel Invitation',
          description: error?.response?.data?.error || 'Please try again.',
          variant: 'destructive',
        })
      }
    }
  }

  const handleResendInvite = async (invite: OrgInvite) => {
    try {
      await resendInvite.mutateAsync({
        orgId,
        inviteId: invite.id,
      })
      await alert({
        title: 'Invitation Resent',
        description: `A new invitation has been sent to ${invite.email}.`,
        variant: 'success',
      })
    } catch (error: any) {
      await alert({
        title: 'Failed to Resend Invitation',
        description: error?.response?.data?.error || 'Please try again.',
        variant: 'destructive',
      })
    }
  }

  if (isLoading) {
    return (
      <div className="flex flex-col items-center justify-center py-16">
        <Loader2 className="h-6 w-6 animate-spin text-gray-400" />
        <p className="mt-3 text-sm text-gray-500">Loading members...</p>
      </div>
    )
  }

  if (error) {
    return (
      <div className="flex flex-col items-center justify-center py-16">
        <AlertCircle className="h-8 w-8 text-red-400 mb-2" />
        <p className="text-sm text-gray-700 font-medium">Failed to load members</p>
        <p className="text-sm text-gray-500 mt-1">Please try refreshing the page.</p>
      </div>
    )
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-lg font-medium text-gray-900">Team Members</h2>
          <p className="text-sm text-gray-500 mt-0.5">
            {members.length} member{members.length !== 1 ? 's' : ''}
            {pendingInvites.length > 0 && ` · ${pendingInvites.length} pending`}
          </p>
        </div>
        {canManage && (
          <Button onClick={() => setInviteDialogOpen(true)}>
            <UserPlus className="h-4 w-4 mr-2" />
            Invite
          </Button>
        )}
      </div>

      {/* Members List */}
      <div className="bg-white rounded-lg border border-gray-200 divide-y divide-gray-200">
        {members.map((member) => (
          <MemberRow
            key={member.id}
            member={member}
            canManage={canManage}
            isCurrentUser={currentUserRole === member.role && members.length === 1}
            onRoleChange={(role) => handleRoleChange(member, role)}
            onRemove={() => handleRemoveMember(member)}
            isUpdating={updateRole.isPending}
          />
        ))}

        {members.length === 0 && (
          <div className="p-8 text-center">
            <Users className="h-8 w-8 text-gray-300 mx-auto mb-2" />
            <p className="text-sm text-gray-500">No members yet</p>
          </div>
        )}
      </div>

      {/* Pending Invitations */}
      {pendingInvites.length > 0 && (
        <div className="space-y-3">
          <h3 className="text-sm font-medium text-gray-900">Pending Invitations</h3>
          <div className="bg-white rounded-lg border border-gray-200 divide-y divide-gray-200">
            {pendingInvites.map((invite) => (
              <InviteRow
                key={invite.id}
                invite={invite}
                canManage={canManage}
                onCancel={() => handleCancelInvite(invite)}
                onResend={() => handleResendInvite(invite)}
                isResending={resendInvite.isPending}
              />
            ))}
          </div>
        </div>
      )}


      {/* Invite Dialog */}
      <Dialog open={inviteDialogOpen} onOpenChange={setInviteDialogOpen}>
        <DialogContent showCloseButton className="max-w-md">
          <form
            onSubmit={(e) => {
              e.preventDefault()
              handleInvite()
            }}
          >
            <DialogHeader>
              <DialogTitle>Invite Team Member</DialogTitle>
              <DialogDescription>
                Send an invitation to join your organization.
              </DialogDescription>
            </DialogHeader>
            <DialogBody>
              <div className="space-y-4">
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-2">
                    Email address
                  </label>
                  <input
                    type="email"
                    value={inviteEmail}
                    onChange={(e) => setInviteEmail(e.target.value)}
                    placeholder="colleague@company.com"
                    className="w-full px-3 py-2 border border-gray-200 rounded-md text-sm text-gray-900 placeholder:text-gray-400 focus:outline-none focus:border-gray-400"
                    autoFocus
                  />
                </div>

                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-2">
                    Role
                  </label>
                  <div className="space-y-2">
                    {ROLE_OPTIONS.map((role) => {
                      const Icon = role.icon
                      const isSelected = inviteRole === role.value
                      return (
                        <label
                          key={role.value}
                          className={`flex items-center gap-3 p-3 rounded-md border cursor-pointer transition-all ${
                            isSelected
                              ? 'border-gray-900 bg-gray-50'
                              : 'border-gray-200 hover:border-gray-300'
                          }`}
                        >
                          <input
                            type="radio"
                            name="role"
                            value={role.value}
                            checked={isSelected}
                            onChange={() => setInviteRole(role.value)}
                            className="sr-only"
                          />
                          <div className={`h-8 w-8 rounded-md flex items-center justify-center flex-shrink-0 ${
                            isSelected ? 'bg-gray-200 text-gray-700' : 'bg-gray-100 text-gray-500'
                          }`}>
                            <Icon className="h-4 w-4" />
                          </div>
                          <div className="flex-1">
                            <span className="text-sm font-medium text-gray-900">
                              {role.label}
                            </span>
                            <p className="text-xs text-gray-500">
                              {role.description}
                            </p>
                          </div>
                          {isSelected && (
                            <Check className="h-5 w-5 text-green-600 flex-shrink-0 mr-1" />
                          )}
                        </label>
                      )
                    })}
                  </div>
                </div>
              </div>
            </DialogBody>
            <DialogFooter>
              <Button
                type="button"
                variant="outline"
                onClick={() => setInviteDialogOpen(false)}
                disabled={inviteMember.isPending}
              >
                Cancel
              </Button>
              <Button
                type="submit"
                disabled={!inviteEmail.trim() || inviteMember.isPending}
              >
                {inviteMember.isPending ? (
                  <>
                    <Loader2 className="h-4 w-4 mr-2 animate-spin" />
                    Sending...
                  </>
                ) : (
                  <>
                    <Mail className="h-4 w-4 mr-2" />
                    Send Invitation
                  </>
                )}
              </Button>
            </DialogFooter>
          </form>
        </DialogContent>
      </Dialog>
    </div>
  )
}

// ============================================================================
// Sub-components
// ============================================================================

interface MemberRowProps {
  member: OrgMember
  canManage: boolean
  isCurrentUser: boolean
  onRoleChange: (role: OrgRole) => void
  onRemove: () => void
  isUpdating: boolean
}

function MemberRow({
  member,
  canManage,
  isCurrentUser,
  onRoleChange,
  onRemove,
  isUpdating,
}: MemberRowProps) {
  const roleOption = ROLE_OPTIONS.find((r) => r.value === member.role)
  const RoleIcon = roleOption?.icon ?? Eye

  const displayName =
    member.first_name || member.last_name
      ? `${member.first_name} ${member.last_name}`.trim()
      : member.email

  return (
    <div className="flex items-center gap-4 p-4">
      {/* Avatar */}
      <div className="h-10 w-10 rounded-full bg-gray-200 flex items-center justify-center text-gray-600 font-medium flex-shrink-0">
        {member.profile_image_url ? (
          <img
            src={member.profile_image_url}
            alt=""
            className="h-10 w-10 rounded-full object-cover"
          />
        ) : (
          displayName.charAt(0).toUpperCase()
        )}
      </div>

      {/* Info */}
      <div className="flex-1 min-w-0">
        <div className="flex items-center gap-2">
          <span className="text-sm font-medium text-gray-900 truncate">
            {displayName}
          </span>
          {isCurrentUser && (
            <span className="text-xs text-gray-400">(you)</span>
          )}
        </div>
        <div className="text-sm text-gray-500 truncate">{member.email}</div>
      </div>

      {/* Role Badge */}
      <div className="flex items-center gap-1.5 px-2.5 py-1 rounded-full bg-gray-100 text-gray-700">
        <RoleIcon className="h-3.5 w-3.5" />
        <span className="text-xs font-medium">{member.role_display}</span>
      </div>

      {/* Actions */}
      {canManage && !isCurrentUser && (
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <button className="p-1.5 rounded-md text-gray-400 hover:text-gray-600 hover:bg-gray-100 transition-colors">
              <MoreVertical className="h-4 w-4" />
            </button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end" sideOffset={4}>
            {ROLE_OPTIONS.filter((r) => r.value !== member.role).map((role) => {
              const Icon = role.icon
              return (
                <DropdownMenuItem
                  key={role.value}
                  onSelect={() => onRoleChange(role.value)}
                  className="gap-2"
                >
                  <Icon className="h-4 w-4 text-gray-500" />
                  Change to {role.label}
                </DropdownMenuItem>
              )
            })}
            <DropdownMenuSeparator />
            <DropdownMenuItem
              onSelect={onRemove}
              className="gap-2 text-red-600 focus:bg-red-50"
            >
              <Trash2 className="h-4 w-4" />
              Remove from organization
            </DropdownMenuItem>
          </DropdownMenuContent>
        </DropdownMenu>
      )}
    </div>
  )
}

interface InviteRowProps {
  invite: OrgInvite
  canManage: boolean
  onCancel: () => void
  onResend: () => void
  isResending: boolean
}

function InviteRow({
  invite,
  canManage,
  onCancel,
  onResend,
  isResending,
}: InviteRowProps) {
  const roleOption = ROLE_OPTIONS.find((r) => r.value === invite.role)
  const RoleIcon = roleOption?.icon ?? Eye

  return (
    <div className="flex items-center gap-4 p-4">
      {/* Icon */}
      <div className="h-10 w-10 rounded-full bg-gray-100 flex items-center justify-center flex-shrink-0">
        <Clock className="h-5 w-5 text-gray-400" />
      </div>

      {/* Info */}
      <div className="flex-1 min-w-0">
        <div className="text-sm font-medium text-gray-900 truncate">
          {invite.email}
        </div>
        <div className="flex items-center gap-2 text-xs text-gray-500">
          <span>
            Invited {formatDistanceToNow(new Date(invite.created_at), { addSuffix: true })}
          </span>
          {invite.is_expired && (
            <span className="flex items-center gap-1 text-red-600">
              <AlertCircle className="h-3 w-3" />
              Expired
            </span>
          )}
        </div>
      </div>

      {/* Role Badge */}
      <div className="flex items-center gap-1.5 px-2.5 py-1 rounded-full bg-gray-100 text-gray-600">
        <RoleIcon className="h-3.5 w-3.5" />
        <span className="text-xs font-medium">{invite.role_display}</span>
      </div>

      {/* Actions */}
      {canManage && (
        <div className="flex items-center gap-1">
          <button
            onClick={onResend}
            disabled={isResending}
            className="p-1.5 rounded-md text-gray-400 hover:text-gray-600 hover:bg-gray-100 transition-colors disabled:opacity-50"
            title="Resend invitation"
          >
            {isResending ? (
              <Loader2 className="h-4 w-4 animate-spin" />
            ) : (
              <RefreshCw className="h-4 w-4" />
            )}
          </button>
          <button
            onClick={onCancel}
            className="p-1.5 rounded-md text-gray-400 hover:text-red-600 hover:bg-red-50 transition-colors"
            title="Cancel invitation"
          >
            <X className="h-4 w-4" />
          </button>
        </div>
      )}
    </div>
  )
}

export default MembersPanel

