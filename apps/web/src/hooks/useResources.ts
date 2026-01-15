/**
 * React Query hooks for Resource Registry
 * 
 * Uses centralized apiService for all API calls.
 * @see {@link @/services/apiService} for API documentation.
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { resourcesApi } from '@/services/apiService'
import type { ResourceRegistryEntry } from '../types/models'

export function useResources(backendId: string | null) {
  return useQuery({
    queryKey: ['resources', backendId],
    queryFn: async () => {
      if (!backendId) return []
      return resourcesApi.list(backendId)
    },
    enabled: !!backendId,
  })
}

export function useUpdateResource() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ resourceId, data }: { resourceId: string; data: Partial<ResourceRegistryEntry> }) => {
      return resourcesApi.update(resourceId, data)
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
      return resourcesApi.discover(backendId)
    },
    onSuccess: (_, backendId) => {
      queryClient.invalidateQueries({ queryKey: ['resources', backendId] })
    },
  })
}
