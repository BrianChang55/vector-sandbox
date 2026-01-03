/**
 * React Query hooks for Resource Registry
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { api } from '../services/api'
import type { ResourceRegistryEntry } from '../types/models'

export function useResources(backendId: string | null) {
  return useQuery({
    queryKey: ['resources', backendId],
    queryFn: async () => {
      if (!backendId) return []
      const response = await api.get<ResourceRegistryEntry[]>(`/backends/${backendId}/resources/`)
      return response.data
    },
    enabled: !!backendId,
  })
}

export function useUpdateResource() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ resourceId, data }: { resourceId: string; data: Partial<ResourceRegistryEntry> }) => {
      const response = await api.patch<ResourceRegistryEntry>(`/resources/${resourceId}/`, data)
      return response.data
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['resources', data.backend_connection] })
    },
  })
}

export function useDiscoverResources() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async (backendId: string) => {
      const response = await api.post(`/backends/${backendId}/discover/`)
      return response.data
    },
    onSuccess: (_, backendId) => {
      queryClient.invalidateQueries({ queryKey: ['resources', backendId] })
    },
  })
}

