/**
 * React Query hooks for Backend Connections
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { api } from '../services/api'
import type { BackendConnection } from '../types/models'

export function useBackends(orgId: string | null) {
  return useQuery({
    queryKey: ['backends', orgId],
    queryFn: async () => {
      if (!orgId) return []
      const response = await api.get<BackendConnection[]>(`/orgs/${orgId}/backends/`)
      return response.data
    },
    enabled: !!orgId,
  })
}

export function useCreateBackend() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ orgId, data }: { orgId: string; data: any }) => {
      const response = await api.post<BackendConnection>(`/orgs/${orgId}/backends/`, data)
      return response.data
    },
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['backends', variables.orgId] })
    },
  })
}

export function useTestBackend() {
  return useMutation({
    mutationFn: async (backendId: string) => {
      const response = await api.post(`/backends/${backendId}/test/`)
      return response.data
    },
  })
}

export function useBackendUserAuth() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ backendId, userJwt }: { backendId: string; userJwt: string }) => {
      const response = await api.post(`/backends/${backendId}/user-auth/`, { user_jwt: userJwt })
      return response.data
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['backends'] })
    },
  })
}

