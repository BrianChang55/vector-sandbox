/**
 * React Query hooks for Integrations & Connectors
 * 
 * Organization-level integration model:
 * - The org is registered as a single "user" with Merge
 * - Connectors are connected once per organization
 * - All org members share the connected integrations
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { api } from '../services/api'
import type {
  IntegrationProvider,
  Connector,
  OrgConnectorStatus,
  LinkTokenResponse,
  SyncConnectorsResponse,
  LinkCallbackResponse,
} from '../types/models'

// ============================================================================
// Integration Provider Hooks
// ============================================================================

/**
 * Fetch integration providers for an organization.
 */
export function useIntegrationProviders(orgId: string | null) {
  return useQuery({
    queryKey: ['integrations', orgId],
    queryFn: async () => {
      if (!orgId) return []
      const response = await api.get<IntegrationProvider[]>(`/orgs/${orgId}/integrations/`)
      return response.data
    },
    enabled: !!orgId,
  })
}

/**
 * Fetch a single integration provider.
 */
export function useIntegrationProvider(providerId: string | null) {
  return useQuery({
    queryKey: ['integration', providerId],
    queryFn: async () => {
      if (!providerId) return null
      const response = await api.get<IntegrationProvider>(`/integrations/${providerId}/`)
      return response.data
    },
    enabled: !!providerId,
  })
}

/**
 * Create a new integration provider.
 * 
 * Note: Merge credentials are read from environment variables on the backend.
 * Users only need to specify the display name (optional).
 */
export function useCreateIntegrationProvider() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ 
      orgId, 
      data 
    }: { 
      orgId: string
      data: {
        display_name?: string
      }
    }) => {
      const response = await api.post<IntegrationProvider>(`/orgs/${orgId}/integrations/`, data)
      return response.data
    },
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['integrations', variables.orgId] })
    },
  })
}

/**
 * Update an integration provider.
 * 
 * Note: Merge credentials are read from environment variables and cannot be updated.
 */
export function useUpdateIntegrationProvider() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ 
      providerId, 
      data 
    }: { 
      providerId: string
      data: Partial<{
        display_name: string
        is_active: boolean
      }>
    }) => {
      const response = await api.patch<IntegrationProvider>(`/integrations/${providerId}/`, data)
      return response.data
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['integration', data.id] })
      queryClient.invalidateQueries({ queryKey: ['integrations'] })
    },
  })
}

/**
 * Delete an integration provider.
 */
export function useDeleteIntegrationProvider() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async (providerId: string) => {
      await api.delete(`/integrations/${providerId}/`)
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['integrations'] })
    },
  })
}

// ============================================================================
// Connector Hooks
// ============================================================================

/**
 * Fetch available connectors for an integration provider.
 * Includes org-level connection status for each connector.
 */
export function useConnectors(providerId: string | null) {
  return useQuery({
    queryKey: ['connectors', providerId],
    queryFn: async () => {
      if (!providerId) return []
      const response = await api.get<{ connectors: Connector[] }>(`/integrations/${providerId}/connectors/`)
      return response.data.connectors
    },
    enabled: !!providerId,
  })
}

/**
 * Sync connectors from the integration provider.
 */
export function useSyncConnectors() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async (providerId: string) => {
      const response = await api.post<SyncConnectorsResponse>(`/integrations/${providerId}/sync/`)
      return response.data
    },
    onSuccess: (_, providerId) => {
      queryClient.invalidateQueries({ queryKey: ['connectors', providerId] })
      queryClient.invalidateQueries({ queryKey: ['integration', providerId] })
      queryClient.invalidateQueries({ queryKey: ['org-connections', providerId] })
    },
  })
}

// ============================================================================
// Organization Connection Hooks
// ============================================================================

/**
 * Fetch organization's connection status for all connectors.
 */
export function useOrgConnectorStatus(providerId: string | null) {
  return useQuery({
    queryKey: ['org-connections', providerId],
    queryFn: async () => {
      if (!providerId) return []
      const response = await api.get<{ connections: OrgConnectorStatus[] }>(
        `/integrations/${providerId}/connections/`
      )
      return response.data.connections
    },
    enabled: !!providerId,
  })
}

/**
 * Generate an OAuth link token for connecting the organization to an integration.
 */
export function useGenerateLinkToken() {
  return useMutation({
    mutationFn: async ({ 
      providerId, 
      connectorId 
    }: { 
      providerId: string
      connectorId?: string 
    }) => {
      const response = await api.post<LinkTokenResponse>(
        `/integrations/${providerId}/link-token/`,
        connectorId ? { connector_id: connectorId } : {}
      )
      return response.data
    },
  })
}

/**
 * Handle callback after OAuth flow completion.
 * Records which user connected the integration for the org.
 */
export function useHandleLinkCallback() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: async ({ 
      providerId, 
      connectorId 
    }: { 
      providerId: string
      connectorId: string 
    }) => {
      const response = await api.post<LinkCallbackResponse>(
        `/integrations/${providerId}/link-callback/`,
        { connector_id: connectorId }
      )
      return response.data
    },
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['org-connections', variables.providerId] })
      queryClient.invalidateQueries({ queryKey: ['connectors', variables.providerId] })
      queryClient.invalidateQueries({ queryKey: ['integration', variables.providerId] })
    },
  })
}

// ============================================================================
// Utility Hooks
// ============================================================================

/**
 * Check if the organization has any integration providers configured.
 */
export function useHasIntegrations(orgId: string | null) {
  const { data: providers, isLoading } = useIntegrationProviders(orgId)
  
  return {
    hasIntegrations: (providers?.length ?? 0) > 0,
    isLoading,
    provider: providers?.[0] ?? null, // Currently we only support one provider per org
  }
}

/**
 * Get connected connectors count for an integration provider.
 * Uses the provider's connected_count from the API response.
 */
export function useConnectedConnectorsCount(providerId: string | null) {
  const { data: provider, isLoading } = useIntegrationProvider(providerId)
  
  return {
    connectedCount: provider?.connected_count ?? 0,
    totalCount: provider?.connector_count ?? 0,
    isLoading,
  }
}

// Legacy alias for backwards compatibility
export const useUserConnectorStatus = useOrgConnectorStatus
