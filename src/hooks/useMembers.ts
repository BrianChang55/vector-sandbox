/**
 * React Query hooks for Organization Members & Invitations
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { api } from '../services/api'
import type {
  OrgMember,
  OrgInvite,
  OrgRole,
  MembersListResponse,
  InviteDetails,
  InviteAcceptResponse,
} from '../types/models'

// ============================================================================
// Members Hooks
// ============================================================================

/**
 * Fetch organization members and pending invites
 */
export function useOrgMembers(orgId: string | null) {
  return useQuery({
    queryKey: ['org-members', orgId],
    queryFn: async () => {
      if (!orgId) return null
      const response = await api.get<MembersListResponse>(`/orgs/${orgId}/members/`)
      return response.data
    },
    enabled: !!orgId,
  })
}

/**
 * Update a member's role
 */
export function useUpdateMemberRole() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({
      orgId,
      memberId,
      role,
    }: {
      orgId: string
      memberId: string
      role: OrgRole
    }) => {
      const response = await api.patch<{ member: OrgMember; message: string }>(
        `/orgs/${orgId}/members/${memberId}/`,
        { role }
      )
      return response.data
    },
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['org-members', variables.orgId] })
    },
  })
}

/**
 * Remove a member from the organization
 */
export function useRemoveMember() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({
      orgId,
      memberId,
    }: {
      orgId: string
      memberId: string
    }) => {
      const response = await api.delete<{ message: string }>(
        `/orgs/${orgId}/members/${memberId}/`
      )
      return response.data
    },
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['org-members', variables.orgId] })
    },
  })
}

// ============================================================================
// Invitation Hooks
// ============================================================================

/**
 * Send an invitation to join the organization
 */
export function useInviteMember() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({
      orgId,
      email,
      role,
    }: {
      orgId: string
      email: string
      role: OrgRole
    }) => {
      const response = await api.post<{ invite: OrgInvite; message: string }>(
        `/orgs/${orgId}/invites/`,
        { email, role }
      )
      return response.data
    },
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['org-members', variables.orgId] })
    },
  })
}

/**
 * Cancel a pending invitation
 */
export function useCancelInvite() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({
      orgId,
      inviteId,
    }: {
      orgId: string
      inviteId: string
    }) => {
      const response = await api.delete<{ message: string }>(
        `/orgs/${orgId}/invites/${inviteId}/`
      )
      return response.data
    },
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['org-members', variables.orgId] })
    },
  })
}

/**
 * Resend an invitation email
 */
export function useResendInvite() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({
      orgId,
      inviteId,
    }: {
      orgId: string
      inviteId: string
    }) => {
      const response = await api.post<{ invite: OrgInvite; message: string }>(
        `/orgs/${orgId}/invites/${inviteId}/resend/`
      )
      return response.data
    },
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['org-members', variables.orgId] })
    },
  })
}

// ============================================================================
// Public Invitation Hooks (no auth required)
// ============================================================================

/**
 * Verify an invitation token and get details
 */
export function useVerifyInvite(token: string | null) {
  return useQuery({
    queryKey: ['invite-verify', token],
    queryFn: async () => {
      if (!token) return null
      const response = await api.post<InviteDetails>('/auth/invite/verify', { token })
      return response.data
    },
    enabled: !!token,
    retry: false,
  })
}

/**
 * Accept an invitation
 */
export function useAcceptInvite() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async (token: string) => {
      const response = await api.post<InviteAcceptResponse>('/auth/invite/accept', { token })
      return response.data
    },
    onSuccess: () => {
      // Invalidate user organizations since they may have a new one
      queryClient.invalidateQueries({ queryKey: ['auth-me'] })
      queryClient.invalidateQueries({ queryKey: ['organizations'] })
    },
  })
}

// ============================================================================
// Utility Hooks
// ============================================================================

/**
 * Get the current user's role in an organization
 */
export function useCurrentUserRole(orgId: string | null) {
  const { data: membersData, isLoading } = useOrgMembers(orgId)

  return {
    role: membersData?.current_user_role ?? null,
    canManageMembers: membersData?.can_manage_members ?? false,
    isLoading,
  }
}

