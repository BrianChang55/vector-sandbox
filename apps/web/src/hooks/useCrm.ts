/**
 * React Query hooks for CRM Customer Groups
 *
 * Uses centralized apiService for all API calls.
 * @see {@link @/services/apiService} for API documentation.
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { crmApi } from '@/services/apiService'
import type { CustomerGroupInput } from '../types/models'

/**
 * Fetch all customer groups for an organization
 */
export function useCustomerGroups(orgId: string | null) {
  return useQuery({
    queryKey: ['customer-groups', orgId],
    queryFn: async () => {
      if (!orgId) return []
      return crmApi.list(orgId)
    },
    enabled: !!orgId,
  })
}

/**
 * Fetch a single customer group by ID
 */
export function useCustomerGroup(groupId: string | null) {
  return useQuery({
    queryKey: ['customer-group', groupId],
    queryFn: async () => {
      if (!groupId) return null
      return crmApi.get(groupId)
    },
    enabled: !!groupId,
  })
}

/**
 * Fetch dashboard statistics for an organization
 */
export function useCustomerGroupStats(orgId: string | null) {
  return useQuery({
    queryKey: ['customer-group-stats', orgId],
    queryFn: async () => {
      if (!orgId) return null
      return crmApi.getStats(orgId)
    },
    enabled: !!orgId,
  })
}

/**
 * Create a new customer group
 */
export function useCreateCustomerGroup() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({ orgId, data }: { orgId: string; data: CustomerGroupInput }) => {
      return crmApi.create(orgId, data)
    },
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['customer-groups', variables.orgId] })
      queryClient.invalidateQueries({ queryKey: ['customer-group-stats', variables.orgId] })
    },
  })
}

/**
 * Update an existing customer group
 */
export function useUpdateCustomerGroup() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({
      groupId,
      data,
    }: {
      groupId: string
      orgId: string
      data: Partial<CustomerGroupInput>
    }) => {
      return crmApi.update(groupId, data)
    },
    onSuccess: (data, variables) => {
      queryClient.invalidateQueries({ queryKey: ['customer-group', data.id] })
      queryClient.invalidateQueries({ queryKey: ['customer-groups', variables.orgId] })
      queryClient.invalidateQueries({ queryKey: ['customer-group-stats', variables.orgId] })
    },
  })
}

/**
 * Delete a customer group
 */
export function useDeleteCustomerGroup() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({ groupId }: { groupId: string; orgId: string }) => {
      await crmApi.delete(groupId)
      return groupId
    },
    onSuccess: (groupId, variables) => {
      queryClient.invalidateQueries({ queryKey: ['customer-group', groupId] })
      queryClient.invalidateQueries({ queryKey: ['customer-groups', variables.orgId] })
      queryClient.invalidateQueries({ queryKey: ['customer-group-stats', variables.orgId] })
    },
  })
}
