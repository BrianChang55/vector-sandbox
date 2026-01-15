/**
 * React Query hooks for Organization Members & Invitations
 * 
 * Uses centralized apiService for all API calls.
 * @see {@link @/services/apiService} for API documentation.
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { membersApi } from '@/services/apiService'
import type { OrgRole } from '../types/models'

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
      return membersApi.list(orgId)
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
      return membersApi.updateRole(orgId, memberId, role)
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
      return membersApi.remove(orgId, memberId)
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
      return membersApi.invite(orgId, email, role)
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
      return membersApi.cancelInvite(orgId, inviteId)
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
      return membersApi.resendInvite(orgId, inviteId)
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
      return membersApi.verifyInvite(token)
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
      return membersApi.acceptInvite(token)
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
