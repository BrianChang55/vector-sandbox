/**
 * React Query hooks for Internal Apps
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { api } from '../services/api'
import type { InternalApp, AppVersion } from '../types/models'

export function useApps(orgId: string | null) {
  return useQuery({
    queryKey: ['apps', orgId],
    queryFn: async () => {
      if (!orgId) return []
      const response = await api.get<InternalApp[]>(`/orgs/${orgId}/apps/`)
      return response.data
    },
    enabled: !!orgId,
  })
}

export function useApp(appId: string | null) {
  return useQuery({
    queryKey: ['app', appId],
    queryFn: async () => {
      if (!appId) return null
      const response = await api.get<InternalApp>(`/apps/${appId}/`)
      return response.data
    },
    enabled: !!appId,
  })
}

export function useCreateApp() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ orgId, data }: { orgId: string; data: any }) => {
      const response = await api.post<InternalApp>(`/orgs/${orgId}/apps/`, data)
      return response.data
    },
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['apps', variables.orgId] })
    },
  })
}

export function useUpdateApp() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ appId, data }: { appId: string; data: any }) => {
      const response = await api.patch<InternalApp>(`/apps/${appId}/`, data)
      return response.data
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['app', data.id] })
      queryClient.invalidateQueries({ queryKey: ['apps'] })
    },
  })
}

export function useDeleteApp() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({ appId }: { appId: string }) => {
      await api.delete(`/apps/${appId}/`)
      return appId
    },
    onSuccess: (appId) => {
      queryClient.invalidateQueries({ queryKey: ['app', appId] })
      queryClient.invalidateQueries({ queryKey: ['apps'] })
    },
  })
}

export function useAppVersions(appId: string | null) {
  return useQuery({
    queryKey: ['versions', appId],
    queryFn: async () => {
      if (!appId) return []
      const response = await api.get<AppVersion[]>(`/apps/${appId}/versions/`)
      return response.data
    },
    enabled: !!appId,
    refetchOnWindowFocus: false,
  })
}

export function useVersionFiles(versionId: string | null) {
  return useQuery({
    queryKey: ['version-files', versionId],
    queryFn: async () => {
      if (!versionId) return []
      const response = await api.get<AppVersion>(`/versions/${versionId}/`)
      return response.data.files || []
    },
    enabled: !!versionId,
  })
}

export function useAppVersion(versionId: string | null) {
  return useQuery({
    queryKey: ['version', versionId],
    queryFn: async () => {
      if (!versionId) return null
      const response = await api.get<AppVersion>(`/versions/${versionId}/`)
      return response.data
    },
    enabled: !!versionId,
  })
}

export function useAiEdit() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ appId, intentMessage, specJson }: { appId: string; intentMessage: string; specJson: any }) => {
      const response = await api.post<AppVersion>(`/apps/${appId}/versions/ai-edit/`, {
        intent_message: intentMessage,
        spec_json: specJson,
        source: 'ai_edit',
      })
      return response.data
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['versions', data.internal_app] })
    },
  })
}

export function useCodeEdit() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ appId, filePath, content }: { appId: string; filePath: string; content: string }) => {
      const response = await api.post<AppVersion>(`/apps/${appId}/versions/code-edit/`, {
        file_path: filePath,
        content,
      })
      return response.data
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['versions', data.internal_app] })
      queryClient.invalidateQueries({ queryKey: ['version', data.id] })
    },
  })
}

export function useRollback() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async (versionId: string) => {
      const response = await api.post<AppVersion>(`/versions/${versionId}/rollback/`)
      return response.data
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['versions', data.internal_app] })
    },
  })
}

export function usePublishApp() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async (appId: string) => {
      const response = await api.post<AppVersion>(`/apps/${appId}/publish/`)
      return response.data
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['app', data.internal_app] })
      queryClient.invalidateQueries({ queryKey: ['versions', data.internal_app] })
    },
  })
}

