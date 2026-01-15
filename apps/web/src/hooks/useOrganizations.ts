/**
 * React Query hooks for Organizations
 * 
 * Uses centralized apiService for all API calls.
 * @see {@link @/services/apiService} for API documentation.
 */
import { useEffect } from 'react'
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { organizationsApi } from '@/services/apiService'
import { useAppDispatch, useAppSelector } from '../store/hooks'
import { setSelectedOrg } from '../store/slices/uiSlice'
import type { Organization } from '../types/models'

export function useOrganizations() {
  const dispatch = useAppDispatch()
  const selectedOrgId = useAppSelector((state) => state.ui.selectedOrgId)
  
  const query = useQuery({
    queryKey: ['organizations'],
    queryFn: async () => {
      return organizationsApi.list()
    },
  })

  // Auto-select first organization if none is selected or current selection is invalid
  useEffect(() => {
    if (query.data && query.data.length > 0) {
      const selectedOrgExists = query.data.some((org) => org.id === selectedOrgId)
      if (!selectedOrgId || !selectedOrgExists) {
        dispatch(setSelectedOrg(query.data[0].id))
      }
    }
  }, [query.data, selectedOrgId, dispatch])

  return query
}

export function useCreateOrganization() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async (data: { name: string; slug: string }) => {
      return organizationsApi.create(data)
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['organizations'] })
    },
  })
}

export function useSwitchOrganization() {
  const dispatch = useAppDispatch()
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async (orgId: string) => {
      // This is a client-side operation - just update Redux state
      dispatch(setSelectedOrg(orgId))
      return { orgId }
    },
    onSuccess: () => {
      // Invalidate queries that depend on the selected org
      queryClient.invalidateQueries({ queryKey: ['apps'] })
      queryClient.invalidateQueries({ queryKey: ['members'] })
      queryClient.invalidateQueries({ queryKey: ['backends'] })
    },
  })
}

export function useUpdateOrganization() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ orgId, data }: { orgId: string; data: { name: string; slug: string } }) => {
      return organizationsApi.update(orgId, data)
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
      return organizationsApi.uploadLogo(orgId, file)
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
      return organizationsApi.deleteLogo(orgId)
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['organizations'] })
    },
  })
}

export function useDeleteOrganization() {
  const queryClient = useQueryClient()
  const dispatch = useAppDispatch()
  
  return useMutation({
    mutationFn: async ({ orgId, confirmationName }: { orgId: string; confirmationName: string }) => {
      return organizationsApi.delete(orgId, confirmationName)
    },
    onSuccess: (_, { orgId }) => {
      // Invalidate organizations query
      queryClient.invalidateQueries({ queryKey: ['organizations'] })
      
      // Get remaining orgs and switch to first one if available
      const remainingOrgs = queryClient.getQueryData<Organization[]>(['organizations'])?.filter(org => org.id !== orgId)
      if (remainingOrgs && remainingOrgs.length > 0) {
        dispatch(setSelectedOrg(remainingOrgs[0].id))
      } else {
        dispatch(setSelectedOrg(null))
      }
    },
  })
}
