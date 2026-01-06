/**
 * React Query hooks for Internal Apps
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { api } from '../services/api'
import type {
  InternalApp,
  AppVersion,
  VersionStateSnapshot,
  VersionAuditLog,
  RollbackPreview,
  VersionDiff,
  PublishedAppResponse,
} from '../types/models'

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

export function useAppVersions(appId: string | null, options?: { includeFiles?: boolean }) {
  const includeFiles = options?.includeFiles ?? false

  return useQuery({
    queryKey: ['versions', appId, includeFiles],
    queryFn: async () => {
      if (!appId) return []
      const response = await api.get<AppVersion[]>(
        `/apps/${appId}/versions/${includeFiles ? '?include_files=true' : ''}`
      )
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
    mutationFn: async ({
      versionId,
      includeSchema = true,
    }: {
      versionId: string
      includeSchema?: boolean
    }) => {
      const response = await api.post<AppVersion>(`/versions/${versionId}/rollback/`, {
        include_schema: includeSchema,
      })
      return response.data
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['versions', data.internal_app] })
      queryClient.invalidateQueries({ queryKey: ['audit-trail', data.internal_app] })
    },
  })
}

/**
 * Fetch rollback preview (dry run) for a version
 */
export function useRollbackPreview(versionId: string | null, includeSchema = true) {
  return useQuery({
    queryKey: ['rollback-preview', versionId, includeSchema],
    queryFn: async () => {
      if (!versionId) return null
      const response = await api.post<RollbackPreview>(`/versions/${versionId}/rollback/`, {
        dry_run: true,
        include_schema: includeSchema,
      })
      return response.data
    },
    enabled: !!versionId,
    staleTime: 30000, // Cache for 30 seconds
  })
}

/**
 * Fetch version snapshot details
 */
export function useVersionSnapshot(versionId: string | null) {
  return useQuery({
    queryKey: ['version-snapshot', versionId],
    queryFn: async () => {
      if (!versionId) return null
      const response = await api.get<VersionStateSnapshot>(`/versions/${versionId}/snapshot/`)
      return response.data
    },
    enabled: !!versionId,
  })
}

/**
 * Fetch diff between two versions
 */
export function useVersionDiff(fromVersionId: string | null, toVersionId: string | null) {
  return useQuery({
    queryKey: ['version-diff', fromVersionId, toVersionId],
    queryFn: async () => {
      if (!fromVersionId || !toVersionId) return null
      const response = await api.get<VersionDiff>(`/versions/${fromVersionId}/diff/${toVersionId}/`)
      return response.data
    },
    enabled: !!fromVersionId && !!toVersionId,
  })
}

/**
 * Fetch audit trail for an app
 */
export function useAuditTrail(appId: string | null, options?: { limit?: number }) {
  const limit = options?.limit ?? 50

  return useQuery({
    queryKey: ['audit-trail', appId, limit],
    queryFn: async () => {
      if (!appId) return []
      const response = await api.get<VersionAuditLog[]>(
        `/apps/${appId}/versions/audit-trail/?limit=${limit}`
      )
      return response.data
    },
    enabled: !!appId,
  })
}

/**
 * Fetch version history with snapshots
 */
export function useVersionHistory(appId: string | null) {
  return useQuery({
    queryKey: ['version-history', appId],
    queryFn: async () => {
      if (!appId) return []
      const response = await api.get<AppVersion[]>(`/apps/${appId}/versions/history/`)
      return response.data
    },
    enabled: !!appId,
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

/**
 * Fetch a published app by org and app slug
 */
export function usePublishedApp(orgSlug: string | null, appSlug: string | null) {
  return useQuery({
    queryKey: ['published-app', orgSlug, appSlug],
    queryFn: async () => {
      if (!orgSlug || !appSlug) return null
      const response = await api.get<PublishedAppResponse>(
        `/published/${orgSlug}/${appSlug}/`
      )
      return response.data
    },
    enabled: !!orgSlug && !!appSlug,
    staleTime: 60000, // Cache for 1 minute
  })
}

// ============================================================================
// App Favorites
// ============================================================================

interface FavoritesResponse {
  favorites: string[]
}

interface ToggleFavoriteResponse {
  favorited: boolean
  app_id: string
}

/**
 * Fetch user's favorite app IDs for an organization
 */
export function useAppFavorites(orgId: string | null) {
  return useQuery({
    queryKey: ['app-favorites', orgId],
    queryFn: async () => {
      if (!orgId) return new Set<string>()
      const response = await api.get<FavoritesResponse>(`/orgs/${orgId}/favorites/`)
      return new Set(response.data.favorites)
    },
    enabled: !!orgId,
    staleTime: 30000, // Cache for 30 seconds
  })
}

/**
 * Toggle favorite status for an app
 */
export function useToggleFavorite() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ orgId, appId }: { orgId: string; appId: string }) => {
      const response = await api.post<ToggleFavoriteResponse>(`/orgs/${orgId}/favorites/`, {
        app_id: appId,
      })
      return response.data
    },
    onMutate: async ({ orgId, appId }) => {
      // Cancel any outgoing refetches
      await queryClient.cancelQueries({ queryKey: ['app-favorites', orgId] })
      
      // Snapshot the previous value
      const previousFavorites = queryClient.getQueryData<Set<string>>(['app-favorites', orgId])
      
      // Optimistically update to the new value
      if (previousFavorites) {
        const newFavorites = new Set(previousFavorites)
        if (newFavorites.has(appId)) {
          newFavorites.delete(appId)
        } else {
          newFavorites.add(appId)
        }
        queryClient.setQueryData(['app-favorites', orgId], newFavorites)
      }
      
      return { previousFavorites }
    },
    onError: (_err, { orgId }, context) => {
      // Rollback on error
      if (context?.previousFavorites) {
        queryClient.setQueryData(['app-favorites', orgId], context.previousFavorites)
      }
    },
    onSettled: (_data, _error, { orgId }) => {
      // Refetch after error or success
      queryClient.invalidateQueries({ queryKey: ['app-favorites', orgId] })
    },
  })
}

