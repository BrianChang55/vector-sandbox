/**
 * Centralized API Service
 * 
 * This module consolidates ALL API calls for the Internal Apps application.
 * All HTTP requests should go through this service for:
 * - Consistent error handling
 * - Centralized authentication
 * - Easy API endpoint management
 * - Better debugging and logging
 * 
 * @module apiService
 * 
 * ## Usage
 * 
 * Import specific API functions:
 * ```typescript
 * import { appsApi, versionsApi, organizationsApi } from '@/services/apiService'
 * 
 * // Fetch apps for an organization
 * const apps = await appsApi.list(orgId)
 * 
 * // Create a new app
 * const newApp = await appsApi.create(orgId, { name: 'My App' })
 * ```
 * 
 * ## API Categories
 * 
 * - **appsApi**: App CRUD, publishing, favorites
 * - **versionsApi**: Version management, rollback, diffs, audit trail
 * - **organizationsApi**: Organization CRUD, logo management
 * - **membersApi**: Organization members and invitations
 * - **integrationsApi**: Integration providers and connectors
 * - **backendsApi**: Backend connections and testing
 * - **resourcesApi**: Resource registry management
 * - **dataTablesApi**: App data store tables and rows
 * - **authApi**: Authentication (magic link, OAuth, etc.)
 */

import { api } from './api'
import type {
  InternalApp,
  AppVersion,
  VersionStateSnapshot,
  VersionAuditLog,
  RollbackPreview,
  VersionDiff,
  PublishedAppResponse,
  Organization,
  OrgMember,
  OrgInvite,
  OrgRole,
  MembersListResponse,
  InviteDetails,
  InviteAcceptResponse,
  IntegrationProvider,
  Connector,
  OrgConnectorStatus,
  LinkTokenResponse,
  SyncConnectorsResponse,
  LinkCallbackResponse,
  BackendConnection,
  ResourceRegistryEntry,
} from '../types/models'

import type {
  DataTable,
  DataRow,
  QueryResult,
  QueryResultRow,
  QuerySpec,
  CreateTableRequest,
  UpdateTableRequest,
  InsertRowRequest,
  BulkInsertRequest,
  BulkInsertResponse,
  BulkDeleteRequest,
  BulkDeleteResponse,
} from '../types/dataStore'

// =============================================================================
// APPS API
// =============================================================================

/**
 * Apps API
 * 
 * Manage internal applications - create, update, delete, publish.
 * 
 * @example
 * ```typescript
 * // List all apps for an organization
 * const apps = await appsApi.list('org-123')
 * 
 * // Get a single app
 * const app = await appsApi.get('app-456')
 * 
 * // Create a new app
 * const newApp = await appsApi.create('org-123', { name: 'My App', slug: 'my-app' })
 * 
 * // Update an app
 * const updated = await appsApi.update('app-456', { name: 'Updated Name' })
 * 
 * // Delete an app
 * await appsApi.delete('app-456')
 * 
 * // Publish an app
 * const publishedVersion = await appsApi.publish('app-456')
 * ```
 */
export const appsApi = {
  /**
   * List all apps for an organization
   * @param orgId - Organization ID
   */
  list: async (orgId: string): Promise<InternalApp[]> => {
    const response = await api.get<InternalApp[]>(`/orgs/${orgId}/apps/`)
    return response.data
  },

  /**
   * Get a single app by ID
   * @param appId - App ID
   */
  get: async (appId: string): Promise<InternalApp> => {
    const response = await api.get<InternalApp>(`/apps/${appId}/`)
    return response.data
  },

  /**
   * Create a new app
   * @param orgId - Organization ID
   * @param data - App creation data (name, slug, description, etc.)
   */
  create: async (orgId: string, data: Partial<InternalApp>): Promise<InternalApp> => {
    const response = await api.post<InternalApp>(`/orgs/${orgId}/apps/`, data)
    return response.data
  },

  /**
   * Update an existing app
   * @param appId - App ID
   * @param data - Partial app data to update
   */
  update: async (appId: string, data: Partial<InternalApp>): Promise<InternalApp> => {
    const response = await api.patch<InternalApp>(`/apps/${appId}/`, data)
    return response.data
  },

  /**
   * Delete an app
   * @param appId - App ID
   */
  delete: async (appId: string): Promise<void> => {
    await api.delete(`/apps/${appId}/`)
  },

  /**
   * Publish an app (promote current version to production)
   * @param appId - App ID
   */
  publish: async (appId: string): Promise<AppVersion> => {
    const response = await api.post<AppVersion>(`/apps/${appId}/publish/`)
    return response.data
  },

  /**
   * Get a published app by org slug and app slug (public endpoint)
   * @param orgSlug - Organization slug
   * @param appSlug - App slug
   */
  getPublished: async (orgSlug: string, appSlug: string): Promise<PublishedAppResponse> => {
    const response = await api.get<PublishedAppResponse>(`/published/${orgSlug}/${appSlug}/`)
    return response.data
  },
}

// =============================================================================
// APP FAVORITES API
// =============================================================================

interface FavoritesResponse {
  favorites: string[]
}

interface ToggleFavoriteResponse {
  favorited: boolean
  app_id: string
}

/**
 * App Favorites API
 * 
 * Manage user's favorite apps within an organization.
 * 
 * @example
 * ```typescript
 * // Get user's favorites
 * const favorites = await favoritesApi.list('org-123')
 * 
 * // Toggle favorite status
 * const result = await favoritesApi.toggle('org-123', 'app-456')
 * ```
 */
export const favoritesApi = {
  /**
   * Get user's favorite app IDs for an organization
   * @param orgId - Organization ID
   */
  list: async (orgId: string): Promise<string[]> => {
    const response = await api.get<FavoritesResponse>(`/orgs/${orgId}/favorites/`)
    return response.data.favorites
  },

  /**
   * Toggle favorite status for an app
   * @param orgId - Organization ID
   * @param appId - App ID to toggle
   */
  toggle: async (orgId: string, appId: string): Promise<ToggleFavoriteResponse> => {
    const response = await api.post<ToggleFavoriteResponse>(`/orgs/${orgId}/favorites/`, {
      app_id: appId,
    })
    return response.data
  },
}

// =============================================================================
// VERSIONS API
// =============================================================================

/**
 * Versions API
 * 
 * Manage app versions - view history, rollback, compare diffs.
 * 
 * @example
 * ```typescript
 * // List all versions for an app
 * const versions = await versionsApi.list('app-123')
 * 
 * // Get version with files
 * const version = await versionsApi.get('version-456')
 * 
 * // Rollback to a previous version
 * const newVersion = await versionsApi.rollback('version-456', { includeSchema: true })
 * 
 * // Preview rollback changes (dry run)
 * const preview = await versionsApi.rollbackPreview('version-456')
 * 
 * // Compare two versions
 * const diff = await versionsApi.diff('version-old', 'version-new')
 * ```
 */
export const versionsApi = {
  /**
   * List all versions for an app
   * @param appId - App ID
   * @param includeFiles - Whether to include file contents
   */
  list: async (appId: string, includeFiles = false): Promise<AppVersion[]> => {
    const response = await api.get<AppVersion[]>(
      `/apps/${appId}/versions/${includeFiles ? '?include_files=true' : ''}`
    )
    return response.data
  },

  /**
   * Get a single version by ID
   * @param versionId - Version ID
   */
  get: async (versionId: string): Promise<AppVersion> => {
    const response = await api.get<AppVersion>(`/versions/${versionId}/`)
    return response.data
  },

  /**
   * Get version history with snapshots
   * @param appId - App ID
   */
  history: async (appId: string): Promise<AppVersion[]> => {
    const response = await api.get<AppVersion[]>(`/apps/${appId}/versions/history/`)
    return response.data
  },

  /**
   * Get version snapshot (point-in-time state)
   * @param versionId - Version ID
   */
  snapshot: async (versionId: string): Promise<VersionStateSnapshot> => {
    const response = await api.get<VersionStateSnapshot>(`/versions/${versionId}/snapshot/`)
    return response.data
  },

  /**
   * Get diff between two versions
   * @param fromVersionId - Source version ID
   * @param toVersionId - Target version ID
   */
  diff: async (fromVersionId: string, toVersionId: string): Promise<VersionDiff> => {
    const response = await api.get<VersionDiff>(`/versions/${fromVersionId}/diff/${toVersionId}/`)
    return response.data
  },

  /**
   * Get audit trail for an app
   * @param appId - App ID
   * @param limit - Maximum number of entries to return
   */
  auditTrail: async (appId: string, limit = 50): Promise<VersionAuditLog[]> => {
    const response = await api.get<VersionAuditLog[]>(
      `/apps/${appId}/versions/audit-trail/?limit=${limit}`
    )
    return response.data
  },

  /**
   * Rollback to a previous version
   * @param versionId - Version ID to rollback to
   * @param options - Rollback options
   */
  rollback: async (
    versionId: string,
    options: { includeSchema?: boolean; dryRun?: boolean } = {}
  ): Promise<AppVersion | RollbackPreview> => {
    const response = await api.post<AppVersion | RollbackPreview>(`/versions/${versionId}/rollback/`, {
      include_schema: options.includeSchema ?? true,
      dry_run: options.dryRun ?? false,
    })
    return response.data
  },

  /**
   * Preview rollback changes (dry run)
   * @param versionId - Version ID to preview rollback to
   * @param includeSchema - Whether to include schema changes in preview
   */
  rollbackPreview: async (versionId: string, includeSchema = true): Promise<RollbackPreview> => {
    return versionsApi.rollback(versionId, { includeSchema, dryRun: true }) as Promise<RollbackPreview>
  },

  /**
   * Create a new version via AI edit
   * @param appId - App ID
   * @param intentMessage - User's intent/instruction
   * @param specJson - Current spec JSON
   */
  aiEdit: async (
    appId: string,
    intentMessage: string,
    specJson: unknown
  ): Promise<AppVersion> => {
    const response = await api.post<AppVersion>(`/apps/${appId}/versions/ai-edit/`, {
      intent_message: intentMessage,
      spec_json: specJson,
      source: 'ai_edit',
    })
    return response.data
  },

  /**
   * Create a new version via code edit
   * @param appId - App ID
   * @param filePath - File path being edited
   * @param content - New file content
   */
  codeEdit: async (appId: string, filePath: string, content: string): Promise<AppVersion> => {
    const response = await api.post<AppVersion>(`/apps/${appId}/versions/code-edit/`, {
      file_path: filePath,
      content,
    })
    return response.data
  },

  /**
   * Save files to a version (used by autosave)
   * @param versionId - Version ID
   * @param files - Array of files to save
   */
  saveFiles: async (
    versionId: string,
    files: Array<{ path: string; content: string }>
  ): Promise<void> => {
    await api.post(`/versions/${versionId}/save-files/`, { files })
  },

  /**
   * Cancel a generating version
   * @param versionId - Version ID to cancel
   */
  cancel: async (versionId: string): Promise<void> => {
    await api.post(`/versions/${versionId}/cancel/`)
  },
}

// =============================================================================
// ORGANIZATIONS API
// =============================================================================

/**
 * Organizations API
 * 
 * Manage organizations - create, update, logo management.
 * 
 * @example
 * ```typescript
 * // List user's organizations
 * const orgs = await organizationsApi.list()
 * 
 * // Create organization
 * const newOrg = await organizationsApi.create({ name: 'Acme Inc', slug: 'acme' })
 * 
 * // Update organization
 * const updated = await organizationsApi.update('org-123', { name: 'New Name' })
 * 
 * // Upload logo
 * await organizationsApi.uploadLogo('org-123', logoFile)
 * ```
 */
export const organizationsApi = {
  /**
   * List all organizations the current user belongs to
   */
  list: async (): Promise<Organization[]> => {
    const response = await api.get<Organization[]>('/orgs/')
    return response.data
  },

  /**
   * Create a new organization
   * @param data - Organization data (name, slug)
   */
  create: async (data: { name: string; slug: string }): Promise<Organization> => {
    const response = await api.post<Organization>('/orgs/', data)
    return response.data
  },

  /**
   * Update an organization
   * @param orgId - Organization ID
   * @param data - Updated organization data
   */
  update: async (orgId: string, data: { name: string; slug: string }): Promise<Organization> => {
    const response = await api.patch<Organization>(`/orgs/${orgId}/`, data)
    return response.data
  },

  /**
   * Upload organization logo
   * @param orgId - Organization ID
   * @param file - Logo image file
   */
  uploadLogo: async (orgId: string, file: File): Promise<{ organization: Organization; message: string }> => {
    const formData = new FormData()
    formData.append('logo', file)

    const response = await api.post<{ organization: Organization; message: string }>(
      `/orgs/${orgId}/logo/`,
      formData,
      { headers: { 'Content-Type': 'multipart/form-data' } }
    )
    return response.data
  },

  /**
   * Delete organization logo
   * @param orgId - Organization ID
   */
  deleteLogo: async (orgId: string): Promise<{ organization: Organization; message: string }> => {
    const response = await api.delete<{ organization: Organization; message: string }>(`/orgs/${orgId}/logo/`)
    return response.data
  },

  /**
   * Delete an organization permanently
   * @param orgId - Organization ID
   * @param confirmationName - Must match the organization name exactly to confirm deletion
   */
  delete: async (orgId: string, confirmationName: string): Promise<{ message: string }> => {
    const response = await api.delete<{ message: string }>(`/orgs/${orgId}/`, {
      data: { confirmation_name: confirmationName }
    })
    return response.data
  },
}

// =============================================================================
// MEMBERS API
// =============================================================================

/**
 * Members API
 * 
 * Manage organization members and invitations.
 * 
 * @example
 * ```typescript
 * // Get members and pending invites
 * const { members, pending_invites } = await membersApi.list('org-123')
 * 
 * // Update member role
 * await membersApi.updateRole('org-123', 'member-456', 'admin')
 * 
 * // Remove member
 * await membersApi.remove('org-123', 'member-456')
 * 
 * // Invite new member
 * await membersApi.invite('org-123', 'user@example.com', 'member')
 * 
 * // Cancel invitation
 * await membersApi.cancelInvite('org-123', 'invite-789')
 * ```
 */
export const membersApi = {
  /**
   * Get organization members and pending invites
   * @param orgId - Organization ID
   */
  list: async (orgId: string): Promise<MembersListResponse> => {
    const response = await api.get<MembersListResponse>(`/orgs/${orgId}/members/`)
    return response.data
  },

  /**
   * Update a member's role
   * @param orgId - Organization ID
   * @param memberId - Member ID
   * @param role - New role
   */
  updateRole: async (
    orgId: string,
    memberId: string,
    role: OrgRole
  ): Promise<{ member: OrgMember; message: string }> => {
    const response = await api.patch<{ member: OrgMember; message: string }>(
      `/orgs/${orgId}/members/${memberId}/`,
      { role }
    )
    return response.data
  },

  /**
   * Remove a member from the organization
   * @param orgId - Organization ID
   * @param memberId - Member ID
   */
  remove: async (orgId: string, memberId: string): Promise<{ message: string }> => {
    const response = await api.delete<{ message: string }>(`/orgs/${orgId}/members/${memberId}/`)
    return response.data
  },

  /**
   * Invite a new member to the organization
   * @param orgId - Organization ID
   * @param email - Email address to invite
   * @param role - Role to assign
   */
  invite: async (
    orgId: string,
    email: string,
    role: OrgRole
  ): Promise<{ invite: OrgInvite; message: string }> => {
    const response = await api.post<{ invite: OrgInvite; message: string }>(
      `/orgs/${orgId}/invites/`,
      { email, role }
    )
    return response.data
  },

  /**
   * Cancel a pending invitation
   * @param orgId - Organization ID
   * @param inviteId - Invite ID
   */
  cancelInvite: async (orgId: string, inviteId: string): Promise<{ message: string }> => {
    const response = await api.delete<{ message: string }>(`/orgs/${orgId}/invites/${inviteId}/`)
    return response.data
  },

  /**
   * Resend an invitation email
   * @param orgId - Organization ID
   * @param inviteId - Invite ID
   */
  resendInvite: async (
    orgId: string,
    inviteId: string
  ): Promise<{ invite: OrgInvite; message: string }> => {
    const response = await api.post<{ invite: OrgInvite; message: string }>(
      `/orgs/${orgId}/invites/${inviteId}/resend/`
    )
    return response.data
  },

  /**
   * Verify an invitation token (public endpoint)
   * @param token - Invitation token
   */
  verifyInvite: async (token: string): Promise<InviteDetails> => {
    const response = await api.post<InviteDetails>('/auth/invite/verify', { token })
    return response.data
  },

  /**
   * Accept an invitation (public endpoint)
   * @param token - Invitation token
   */
  acceptInvite: async (token: string): Promise<InviteAcceptResponse> => {
    const response = await api.post<InviteAcceptResponse>('/auth/invite/accept', { token })
    return response.data
  },
}

// =============================================================================
// INTEGRATIONS API
// =============================================================================

/**
 * Integrations API
 * 
 * Manage integration providers and connectors (Merge.dev, etc.).
 * 
 * @example
 * ```typescript
 * // List providers for an org
 * const providers = await integrationsApi.listProviders('org-123')
 * 
 * // Get connectors for a provider
 * const connectors = await integrationsApi.listConnectors('provider-456')
 * 
 * // Sync connectors from remote
 * await integrationsApi.syncConnectors('provider-456')
 * 
 * // Generate OAuth link token
 * const { link_token } = await integrationsApi.generateLinkToken('provider-456', 'connector-789')
 * ```
 */
export const integrationsApi = {
  /**
   * List integration providers for an organization
   * @param orgId - Organization ID
   */
  listProviders: async (orgId: string): Promise<IntegrationProvider[]> => {
    const response = await api.get<IntegrationProvider[]>(`/orgs/${orgId}/integrations/`)
    return response.data
  },

  /**
   * Get a single integration provider
   * @param providerId - Provider ID
   */
  getProvider: async (providerId: string): Promise<IntegrationProvider> => {
    const response = await api.get<IntegrationProvider>(`/integrations/${providerId}/`)
    return response.data
  },

  /**
   * Create a new integration provider
   * @param orgId - Organization ID
   * @param data - Provider data (display_name)
   */
  createProvider: async (
    orgId: string,
    data: { display_name?: string }
  ): Promise<IntegrationProvider> => {
    const response = await api.post<IntegrationProvider>(`/orgs/${orgId}/integrations/`, data)
    return response.data
  },

  /**
   * Update an integration provider
   * @param providerId - Provider ID
   * @param data - Updated provider data
   */
  updateProvider: async (
    providerId: string,
    data: Partial<{ display_name: string; is_active: boolean }>
  ): Promise<IntegrationProvider> => {
    const response = await api.patch<IntegrationProvider>(`/integrations/${providerId}/`, data)
    return response.data
  },

  /**
   * Delete an integration provider
   * @param providerId - Provider ID
   */
  deleteProvider: async (providerId: string): Promise<void> => {
    await api.delete(`/integrations/${providerId}/`)
  },

  /**
   * List connectors for a provider
   * @param providerId - Provider ID
   */
  listConnectors: async (providerId: string): Promise<Connector[]> => {
    const response = await api.get<{ connectors: Connector[] }>(`/integrations/${providerId}/connectors/`)
    return response.data.connectors
  },

  /**
   * Sync connectors from the integration provider
   * @param providerId - Provider ID
   */
  syncConnectors: async (providerId: string): Promise<SyncConnectorsResponse> => {
    const response = await api.post<SyncConnectorsResponse>(`/integrations/${providerId}/sync/`)
    return response.data
  },

  /**
   * Get organization's connection status for all connectors
   * @param providerId - Provider ID
   */
  getConnectionStatus: async (providerId: string): Promise<OrgConnectorStatus[]> => {
    const response = await api.get<{ connections: OrgConnectorStatus[] }>(
      `/integrations/${providerId}/connections/`
    )
    return response.data.connections
  },

  /**
   * Generate an OAuth link token for connecting
   * @param providerId - Provider ID
   * @param connectorId - Optional connector ID to pre-select
   */
  generateLinkToken: async (
    providerId: string,
    connectorId?: string
  ): Promise<LinkTokenResponse> => {
    const response = await api.post<LinkTokenResponse>(
      `/integrations/${providerId}/link-token/`,
      connectorId ? { connector_id: connectorId } : {}
    )
    return response.data
  },

  /**
   * Handle callback after OAuth flow completion
   * @param providerId - Provider ID
   * @param connectorId - Connector ID that was connected
   */
  handleLinkCallback: async (
    providerId: string,
    connectorId: string
  ): Promise<LinkCallbackResponse> => {
    const response = await api.post<LinkCallbackResponse>(
      `/integrations/${providerId}/link-callback/`,
      { connector_id: connectorId }
    )
    return response.data
  },
}

// =============================================================================
// BACKENDS API
// =============================================================================

/**
 * Backends API
 * 
 * Manage backend connections (database, API endpoints, etc.).
 * 
 * @example
 * ```typescript
 * // List backends for an org
 * const backends = await backendsApi.list('org-123')
 * 
 * // Create a new backend
 * const backend = await backendsApi.create('org-123', { name: 'My DB', ... })
 * 
 * // Test backend connection
 * const result = await backendsApi.test('backend-456')
 * ```
 */
export const backendsApi = {
  /**
   * List all backends for an organization
   * @param orgId - Organization ID
   */
  list: async (orgId: string): Promise<BackendConnection[]> => {
    const response = await api.get<BackendConnection[]>(`/orgs/${orgId}/backends/`)
    return response.data
  },

  /**
   * Create a new backend connection
   * @param orgId - Organization ID
   * @param data - Backend configuration
   */
  create: async (orgId: string, data: Partial<BackendConnection>): Promise<BackendConnection> => {
    const response = await api.post<BackendConnection>(`/orgs/${orgId}/backends/`, data)
    return response.data
  },

  /**
   * Test a backend connection
   * @param backendId - Backend ID
   */
  test: async (backendId: string): Promise<{ success: boolean; message?: string }> => {
    const response = await api.post<{ success: boolean; message?: string }>(`/backends/${backendId}/test/`)
    return response.data
  },

  /**
   * Configure user authentication for a backend
   * @param backendId - Backend ID
   * @param userJwt - User's JWT token
   */
  configureUserAuth: async (
    backendId: string,
    userJwt: string
  ): Promise<{ success: boolean }> => {
    const response = await api.post<{ success: boolean }>(`/backends/${backendId}/user-auth/`, {
      user_jwt: userJwt,
    })
    return response.data
  },
}

// =============================================================================
// RESOURCES API
// =============================================================================

/**
 * Resources API
 * 
 * Manage resource registry entries (discovered from backends).
 * 
 * @example
 * ```typescript
 * // List resources for a backend
 * const resources = await resourcesApi.list('backend-123')
 * 
 * // Discover resources from backend
 * await resourcesApi.discover('backend-123')
 * 
 * // Update resource settings
 * await resourcesApi.update('resource-456', { is_enabled: true })
 * ```
 */
export const resourcesApi = {
  /**
   * List resources for a backend
   * @param backendId - Backend ID
   */
  list: async (backendId: string): Promise<ResourceRegistryEntry[]> => {
    const response = await api.get<ResourceRegistryEntry[]>(`/backends/${backendId}/resources/`)
    return response.data
  },

  /**
   * Update a resource
   * @param resourceId - Resource ID
   * @param data - Updated resource data
   */
  update: async (
    resourceId: string,
    data: Partial<ResourceRegistryEntry>
  ): Promise<ResourceRegistryEntry> => {
    const response = await api.patch<ResourceRegistryEntry>(`/resources/${resourceId}/`, data)
    return response.data
  },

  /**
   * Discover resources from a backend
   * @param backendId - Backend ID
   */
  discover: async (backendId: string): Promise<{ discovered_count: number }> => {
    const response = await api.post<{ discovered_count: number }>(`/backends/${backendId}/discover/`)
    return response.data
  },
}

// =============================================================================
// DATA TABLES API
// =============================================================================

/**
 * Data Tables API
 * 
 * Manage app data store tables and rows.
 * 
 * @example
 * ```typescript
 * // List tables
 * const tables = await dataTablesApi.listTables('app-123')
 * 
 * // Create table
 * const table = await dataTablesApi.createTable('app-123', { name: 'Users', ... })
 * 
 * // Query rows
 * const result = await dataTablesApi.queryRows('app-123', 'users', { limit: 50 })
 * 
 * // Insert row
 * const row = await dataTablesApi.insertRow('app-123', 'users', { name: 'John' })
 * ```
 */
export const dataTablesApi = {
  // --------------------------------------------------------------------------
  // Table Operations
  // --------------------------------------------------------------------------

  /**
   * List all tables for an app
   * @param appId - App ID
   */
  listTables: async (appId: string): Promise<DataTable[]> => {
    const response = await api.get<DataTable[]>(`/apps/${appId}/data/tables/`)
    return response.data
  },

  /**
   * Get a single table by slug
   * @param appId - App ID
   * @param tableSlug - Table slug
   */
  getTable: async (appId: string, tableSlug: string): Promise<DataTable> => {
    const response = await api.get<DataTable>(`/apps/${appId}/data/tables/${tableSlug}/`)
    return response.data
  },

  /**
   * Create a new table
   * @param appId - App ID
   * @param data - Table creation data
   */
  createTable: async (appId: string, data: CreateTableRequest): Promise<DataTable> => {
    const response = await api.post<DataTable>(`/apps/${appId}/data/tables/`, data)
    return response.data
  },

  /**
   * Update a table
   * @param appId - App ID
   * @param tableSlug - Table slug
   * @param data - Updated table data
   */
  updateTable: async (
    appId: string,
    tableSlug: string,
    data: UpdateTableRequest
  ): Promise<DataTable> => {
    const response = await api.patch<DataTable>(`/apps/${appId}/data/tables/${tableSlug}/`, data)
    return response.data
  },

  /**
   * Delete a table
   * @param appId - App ID
   * @param tableSlug - Table slug
   */
  deleteTable: async (appId: string, tableSlug: string): Promise<void> => {
    await api.delete(`/apps/${appId}/data/tables/${tableSlug}/`)
  },

  // --------------------------------------------------------------------------
  // Row Operations
  // --------------------------------------------------------------------------

  /**
   * Query rows with optional filtering, sorting, and pagination
   * @param appId - App ID
   * @param tableSlug - Table slug
   * @param querySpec - Query specification (filters, orderBy, limit, offset)
   */
  queryRows: async (
    appId: string,
    tableSlug: string,
    querySpec?: QuerySpec
  ): Promise<QueryResult> => {
    const response = await api.post<QueryResult>(
      `/apps/${appId}/data/tables/${tableSlug}/query/`,
      querySpec || {}
    )
    return response.data
  },

  /**
   * List rows (simple version without filters)
   * @param appId - App ID
   * @param tableSlug - Table slug
   * @param limit - Max rows to return
   * @param offset - Offset for pagination
   */
  listRows: async (
    appId: string,
    tableSlug: string,
    limit = 50,
    offset = 0
  ): Promise<QueryResult> => {
    const response = await api.get<QueryResult>(
      `/apps/${appId}/data/tables/${tableSlug}/rows/`,
      { params: { limit, offset } }
    )
    return response.data
  },

  /**
   * Get a single row by ID
   * @param appId - App ID
   * @param tableSlug - Table slug
   * @param rowId - Row ID
   */
  getRow: async (appId: string, tableSlug: string, rowId: string): Promise<DataRow> => {
    const response = await api.get<DataRow>(
      `/apps/${appId}/data/tables/${tableSlug}/rows/${rowId}/`
    )
    return response.data
  },

  /**
   * Insert a new row
   * @param appId - App ID
   * @param tableSlug - Table slug
   * @param data - Row data
   */
  insertRow: async (
    appId: string,
    tableSlug: string,
    data: InsertRowRequest
  ): Promise<DataRow> => {
    const response = await api.post<DataRow>(
      `/apps/${appId}/data/tables/${tableSlug}/rows/`,
      data
    )
    return response.data
  },

  /**
   * Update a row
   * @param appId - App ID
   * @param tableSlug - Table slug
   * @param rowId - Row ID
   * @param data - Updated row data
   */
  updateRow: async (
    appId: string,
    tableSlug: string,
    rowId: string,
    data: InsertRowRequest
  ): Promise<DataRow> => {
    const response = await api.patch<DataRow>(
      `/apps/${appId}/data/tables/${tableSlug}/rows/${rowId}/`,
      data
    )
    return response.data
  },

  /**
   * Delete a row
   * @param appId - App ID
   * @param tableSlug - Table slug
   * @param rowId - Row ID
   */
  deleteRow: async (appId: string, tableSlug: string, rowId: string): Promise<void> => {
    await api.delete(`/apps/${appId}/data/tables/${tableSlug}/rows/${rowId}/`)
  },

  /**
   * Bulk insert rows
   * @param appId - App ID
   * @param tableSlug - Table slug
   * @param data - Bulk insert data
   */
  bulkInsertRows: async (
    appId: string,
    tableSlug: string,
    data: BulkInsertRequest
  ): Promise<BulkInsertResponse> => {
    const response = await api.post<BulkInsertResponse>(
      `/apps/${appId}/data/tables/${tableSlug}/rows/bulk/`,
      data
    )
    return response.data
  },

  /**
   * Bulk delete rows
   * @param appId - App ID
   * @param tableSlug - Table slug
   * @param data - Bulk delete data (row IDs)
   */
  bulkDeleteRows: async (
    appId: string,
    tableSlug: string,
    data: BulkDeleteRequest
  ): Promise<BulkDeleteResponse> => {
    const response = await api.delete<BulkDeleteResponse>(
      `/apps/${appId}/data/tables/${tableSlug}/rows/bulk/`,
      { data }
    )
    return response.data
  },

  /**
   * Export all rows from a table (handles pagination automatically)
   * @param appId - App ID
   * @param tableSlug - Table slug
   * @returns All rows from the table as QueryResultRow[]
   */
  exportAllRows: async (
    appId: string,
    tableSlug: string
  ): Promise<QueryResultRow[]> => {
    const allRows: QueryResultRow[] = []
    let offset = 0
    const batchSize = 1000
    let hasMore = true

    while (hasMore) {
      const result = await dataTablesApi.queryRows(appId, tableSlug, {
        limit: batchSize,
        offset,
      })

      allRows.push(...result.rows)

      if (result.rows.length < batchSize) {
        hasMore = false
      } else {
        offset += batchSize
      }
    }

    return allRows
  },
}

// =============================================================================
// AI / CODE GENERATION API
// =============================================================================

/**
 * AI Models Response type
 */
export interface AIModel {
  id: string
  name: string
  description: string
  category: 'premium' | 'standard' | 'economy'
  context_length: number
  supports_streaming: boolean
  recommended_for: string[]
  cost: {
    input: number
    output: number
  }
}

export interface ModelsResponse {
  models: AIModel[]
  grouped: {
    premium: AIModel[]
    standard: AIModel[]
    economy: AIModel[]
  }
  default: string
}

export interface ChatSession {
  id: string
  title: string
  model_id: string
  created_at: string
  created_by?: string | null
  message_count: number
  last_message_at?: string | null
}

export interface ChatMessage {
  id: string
  role: 'user' | 'assistant' | 'system'
  content: string
  status: 'pending' | 'streaming' | 'complete' | 'error'
  model_id?: string
  created_at: string
  duration_ms?: number | null
  generated_files?: Record<string, string>
  generated_spec_json?: unknown
  version_created?: string
  error_message?: string
}

/**
 * AI & Code Generation API
 * 
 * Manage AI models, chat sessions, and code generation.
 * Note: Streaming endpoints use SSE and are in aiService.ts/agentService.ts.
 * 
 * @example
 * ```typescript
 * // Get available AI models
 * const models = await aiApi.getModels()
 * 
 * // Get chat sessions for an app
 * const sessions = await aiApi.getChatSessions('app-123')
 * 
 * // Create a new chat session
 * const session = await aiApi.createChatSession('app-123', 'My Session')
 * 
 * // Generate code (non-streaming)
 * const result = await aiApi.generateCode('app-123', 'Create a todo list')
 * ```
 */
export const aiApi = {
  /**
   * Get available AI models
   */
  getModels: async (): Promise<ModelsResponse> => {
    const response = await api.get<ModelsResponse>('/models/')
    return response.data
  },

  /**
   * Get chat sessions for an app
   * @param appId - App ID
   */
  getChatSessions: async (appId: string): Promise<ChatSession[]> => {
    const response = await api.get<{ sessions: ChatSession[] }>(`/apps/${appId}/chat-sessions/`)
    return response.data.sessions
  },

  /**
   * Create a new chat session
   * @param appId - App ID
   * @param title - Optional session title
   * @param modelId - Optional model ID
   */
  createChatSession: async (
    appId: string,
    title?: string,
    modelId?: string
  ): Promise<ChatSession> => {
    const response = await api.post<ChatSession>(`/apps/${appId}/chat-sessions/`, {
      title,
      model_id: modelId,
    })
    return response.data
  },

  /**
   * Get messages for a chat session
   * @param sessionId - Session ID
   */
  getChatMessages: async (sessionId: string): Promise<ChatMessage[]> => {
    const response = await api.get<{ messages: ChatMessage[] }>(
      `/chat-sessions/${sessionId}/messages/`
    )
    return response.data.messages
  },

  /**
   * Generate code without streaming (fallback)
   * For streaming, use generateCodeStreaming in aiService.ts
   * @param appId - App ID
   * @param message - User's prompt
   * @param options - Generation options
   */
  generateCode: async (
    appId: string,
    message: string,
    options: {
      sessionId?: string
      model?: string
      mode?: 'appspec' | 'code'
    } = {}
  ): Promise<{
    session_id: string
    message_id: string
    spec_json: unknown
    version_id: string | null
    version_number: number | null
    duration_ms: number
    validation_errors: string[] | null
  }> => {
    const response = await api.post(`/apps/${appId}/generate/`, {
      message,
      session_id: options.sessionId,
      model: options.model,
      mode: options.mode,
    })
    return response.data
  },

  /**
   * Apply generated code from a message
   * @param messageId - Message ID containing generated code
   */
  applyGeneratedCode: async (messageId: string): Promise<{
    version_id: string
    version_number: number
    files_generated: number
  }> => {
    const response = await api.post(`/messages/${messageId}/apply/`)
    return response.data
  },
}

// =============================================================================
// AUTH API
// =============================================================================

/**
 * Auth API
 * 
 * Authentication endpoints (magic link, OAuth, etc.).
 * Note: Most auth operations are in authService.ts for token handling.
 * 
 * @example
 * ```typescript
 * // Get Google OAuth URL
 * const { oauth_url } = await authApi.getGoogleOAuthUrl()
 * 
 * // Handle OAuth callback
 * const result = await authApi.handleGoogleCallback(code)
 * ```
 */
export const authApi = {
  /**
   * Get Google OAuth URL
   */
  getGoogleOAuthUrl: async (): Promise<{ oauth_url: string }> => {
    const response = await api.get<{ oauth_url: string }>('/auth/google')
    return response.data
  },
}

// =============================================================================
// PUBLIC API (Unauthenticated)
// =============================================================================

/**
 * Public API for unauthenticated endpoints.
 * 
 * These endpoints don't require authentication and are used for public pages
 * like the landing page.
 */

export interface PublicIntegration {
  id: string
  name: string
  logo_url: string | null
  category: string
  categories: string[]
}

export interface PublicIntegrationsResponse {
  integrations: PublicIntegration[]
  count: number
  configured: boolean
  error?: string
}

const API_BASE_URL = import.meta.env.VITE_API_BASE_URL || 'http://localhost:8001/api/v1'

export const publicApi = {
  /**
   * Get available integrations for the landing page.
   * This is an unauthenticated endpoint.
   */
  getIntegrations: async (): Promise<PublicIntegrationsResponse> => {
    try {
      const response = await fetch(`${API_BASE_URL}/public/integrations/`)
      if (!response.ok) {
        throw new Error('Failed to fetch integrations')
      }
      return await response.json()
    } catch (error) {
      console.warn('Failed to fetch public integrations:', error)
      return {
        integrations: [],
        count: 0,
        configured: false,
      }
    }
  },
}

// =============================================================================
// RE-EXPORT API INSTANCE
// =============================================================================

// Re-export the base api instance for advanced use cases
export { api } from './api'
export { getErrorMessage } from './api'

