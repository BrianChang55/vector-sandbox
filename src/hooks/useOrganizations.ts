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

export function useUpdateOrganization() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ orgId, data }: { orgId: string; data: { name: string; slug: string } }) => {
      const response = await api.patch<Organization>(`/orgs/${orgId}/`, data)
      return response.data
    },
    onMutate: async ({ orgId, data }) => {
      // Cancel outgoing refetches
      await queryClient.cancelQueries({ queryKey: ['organizations'] })
      
      // Snapshot previous value
      const previousOrgs = queryClient.getQueryData<Organization[]>(['organizations'])
      
      // Optimistically update
      queryClient.setQueryData<Organization[]>(['organizations'], (old) =>
        old?.map((org) => (org.id === orgId ? { ...org, ...data } : org))
      )
      
      return { previousOrgs }
    },
    onError: (_err, _variables, context) => {
      // Rollback on error
      if (context?.previousOrgs) {
        queryClient.setQueryData(['organizations'], context.previousOrgs)
      }
    },
    onSettled: () => {
      queryClient.invalidateQueries({ queryKey: ['organizations'] })
    },
  })
}

export function useUploadOrganizationLogo() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ orgId, file }: { orgId: string; file: File }) => {
      const formData = new FormData()
      formData.append('logo', file)
      
      const response = await api.post<{ organization: Organization; message: string }>(
        `/orgs/${orgId}/logo/`,
        formData,
        {
          headers: {
            'Content-Type': 'multipart/form-data',
          },
        }
      )
      return response.data
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['organizations'] })
    },
  })
}

export function useDeleteOrganizationLogo() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async (orgId: string) => {
      const response = await api.delete<{ organization: Organization; message: string }>(
        `/orgs/${orgId}/logo/`
      )
      return response.data
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['organizations'] })
    },
  })
}
