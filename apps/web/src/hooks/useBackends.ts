/**
 * React Query hooks for Backend Connections
 * 
 * Uses centralized apiService for all API calls.
 * @see {@link @/services/apiService} for API documentation.
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { backendsApi } from '@/services/apiService'
import type { BackendConnection } from '../types/models'

export function useBackends(orgId: string | null) {
  return useQuery({
    queryKey: ['backends', orgId],
    queryFn: async () => {
      if (!orgId) return []
      return backendsApi.list(orgId)
    },
    enabled: !!orgId,
  })
}

export function useCreateBackend() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ orgId, data }: { orgId: string; data: Partial<BackendConnection> }) => {
      return backendsApi.create(orgId, data)
    },
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['backends', variables.orgId] })
    },
  })
}

export function useTestBackend() {
  return useMutation({
    mutationFn: async (backendId: string) => {
      return backendsApi.test(backendId)
    },
  })
}

export function useBackendUserAuth() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ backendId, userJwt }: { backendId: string; userJwt: string }) => {
      return backendsApi.configureUserAuth(backendId, userJwt)
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['backends'] })
    },
  })
}
