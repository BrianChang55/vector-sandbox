/**
 * React Query hooks for Organizations
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { api } from '../services/api'
import type { Organization } from '../types/models'

export function useOrganizations() {
  return useQuery({
    queryKey: ['organizations'],
    queryFn: async () => {
      const response = await api.get<Organization[]>('/orgs/')
      return response.data
    },
  })
}

export function useCreateOrganization() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async (data: { name: string; slug: string }) => {
      const response = await api.post<Organization>('/orgs/', data)
      return response.data
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['organizations'] })
    },
  })
}

export function useSwitchOrganization() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async (orgId: string) => {
      const response = await api.post(`/orgs/${orgId}/switch/`)
      return response.data
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['organizations'] })
    },
  })
}

