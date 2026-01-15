/**
 * React Query hooks for Internal Apps
 * 
 * Uses centralized apiService for all API calls.
 * @see {@link @/services/apiService} for API documentation.
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { appsApi, versionsApi, favoritesApi } from '@/services/apiService'
import type { InternalApp, AppVersion } from '../types/models'

export function useApps(orgId: string | null) {
  return useQuery({
    queryKey: ['apps', orgId],
    queryFn: async () => {
      if (!orgId) return []
      return appsApi.list(orgId)
    },
    enabled: !!orgId,
  })
}

export function useApp(appId: string | null) {
  return useQuery({
    queryKey: ['app', appId],
    queryFn: async () => {
      if (!appId) return null
      return appsApi.get(appId)
    },
    enabled: !!appId,
  })
}

export function useCreateApp() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ orgId, data }: { orgId: string; data: Partial<InternalApp> }) => {
      return appsApi.create(orgId, data)
    },
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['apps', variables.orgId] })
    },
  })
}

export function useUpdateApp() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ appId, data }: { appId: string; data: Partial<InternalApp> }) => {
      return appsApi.update(appId, data)
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
      await appsApi.delete(appId)
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
      return versionsApi.list(appId, includeFiles)
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
      const version = await versionsApi.get(versionId)
      return version.files || []
    },
    enabled: !!versionId,
  })
}

export function useAppVersion(versionId: string | null) {
  return useQuery({
    queryKey: ['version', versionId],
    queryFn: async () => {
      if (!versionId) return null
      return versionsApi.get(versionId)
    },
    enabled: !!versionId,
  })
}

export function useAiEdit() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ appId, intentMessage, specJson }: { appId: string; intentMessage: string; specJson: unknown }) => {
      return versionsApi.aiEdit(appId, intentMessage, specJson)
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
      return versionsApi.codeEdit(appId, filePath, content)
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
      return versionsApi.rollback(versionId, { includeSchema }) as Promise<AppVersion>
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
      return versionsApi.rollbackPreview(versionId, includeSchema)
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
      return versionsApi.snapshot(versionId)
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
      return versionsApi.diff(fromVersionId, toVersionId)
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
      return versionsApi.auditTrail(appId, limit)
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
      return versionsApi.history(appId)
    },
    enabled: !!appId,
  })
}

export function usePublishApp() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async (appId: string) => {
      return appsApi.publish(appId)
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
      return appsApi.getPublished(orgSlug, appSlug)
    },
    enabled: !!orgSlug && !!appSlug,
    staleTime: 60000, // Cache for 1 minute
  })
}

// ============================================================================
// App Favorites
// ============================================================================

/**
 * Fetch user's favorite app IDs for an organization
 */
export function useAppFavorites(orgId: string | null) {
  return useQuery({
    queryKey: ['app-favorites', orgId],
    queryFn: async () => {
      if (!orgId) return new Set<string>()
      const favorites = await favoritesApi.list(orgId)
      return new Set(favorites)
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
      return favoritesApi.toggle(orgId, appId)
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
