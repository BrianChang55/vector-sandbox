/**
 * TypeScript types for backend models
 */

// ============================================================================
// Organization & Member Types
// ============================================================================

/**
 * Organization roles with permission levels
 */
export type OrgRole = 'admin' | 'editor' | 'viewer'

/**
 * Permission capabilities based on role
 */
export interface RolePermissions {
  canEditApps: boolean
  canManageIntegrations: boolean
  canManageMembers: boolean
  canUpdateOrgSettings: boolean
}

/**
 * Get permissions for a given role
 */
export function getRolePermissions(role: OrgRole): RolePermissions {
  switch (role) {
    case 'admin':
      return {
        canEditApps: true,
        canManageIntegrations: true,
        canManageMembers: true,
        canUpdateOrgSettings: true,
      }
    case 'editor':
      return {
        canEditApps: true,
        canManageIntegrations: false,
        canManageMembers: false,
        canUpdateOrgSettings: false,
      }
    case 'viewer':
      return {
        canEditApps: false,
        canManageIntegrations: false,
        canManageMembers: false,
        canUpdateOrgSettings: false,
      }
  }
}

export interface Organization {
  id: string
  name: string
  slug: string
  logo_url: string | null
  created_at: string
  updated_at: string
}

/**
 * Organization with user's role
 */
export interface OrganizationWithRole extends Organization {
  role: OrgRole
}

/**
 * Organization member (active user in org)
 */
export interface OrgMember {
  id: string
  user_id: string
  email: string
  first_name: string
  last_name: string
  profile_image_url: string | null
  role: OrgRole
  role_display: string
  joined_at: string
}

/**
 * Pending organization invitation
 */
export interface OrgInvite {
  id: string
  email: string
  role: OrgRole
  role_display: string
  invited_by_email: string | null
  invited_by_name: string | null
  organization_name: string
  created_at: string
  expires_at: string
  is_expired: boolean
  is_valid: boolean
  is_accepted: boolean
}

/**
 * Response from members list endpoint
 */
export interface MembersListResponse {
  members: OrgMember[]
  pending_invites: OrgInvite[]
  current_user_role: OrgRole
  can_manage_members: boolean
}

/**
 * Invitation details for accept page
 */
export interface InviteDetails {
  organization_name: string
  organization_logo_url: string | null
  role: OrgRole
  role_display: string
  invited_by_name: string | null
  email: string
  expires_at: string
}

/**
 * Response from accepting an invitation
 */
export interface InviteAcceptResponse {
  message: string
  organization?: {
    id: string
    name: string
    slug: string
  }
  role?: OrgRole
  is_new_user?: boolean
  requires_signup?: boolean
  email?: string
  organization_name?: string
}

export interface BackendConnection {
  id: string
  organization: string
  adapter_type: 'supabase'
  adapter_type_display: string
  display_name: string
  created_at: string
  updated_at: string
}

export interface ResourceRegistryEntry {
  id: string
  organization: string
  backend_connection: string
  resource_id: string
  resource_name: string
  schema_json: any
  enabled: boolean
  exposed_fields_json: string[]
  ui_constraints_json: Record<string, any>
  allowed_actions_json: any[]
  created_at: string
  updated_at: string
}

export interface InternalApp {
  id: string
  organization: string
  organization_slug: string
  name: string
  slug: string | null
  description: string
  status: 'draft' | 'published'
  status_display: string
  backend_connection: string
  backend_connection_name: string
  published_version_id: string | null
  published_version_number: number | null
  published_at: string | null
  published_url: string | null
  allow_actions_in_preview: boolean
  created_by: string
  created_by_email: string
  created_at: string
  updated_at: string
}

// Response type for published app endpoint
export interface PublishedAppResponse {
  app: InternalApp
  version: AppVersion
  files: Array<{
    id: string
    path: string
    content: string
    content_hash: string
  }>
}

export interface AppVersion {
  id: string
  internal_app: string
  version_number: number
  parent_version: string | null
  source: 'ai_edit' | 'code_edit' | 'rollback' | 'publish' | 'ai' | 'code'
  source_display: string
  intent_message?: string | null
  generation_status?: 'pending' | 'generating' | 'complete' | 'error'
  spec_json: AppSpec
  scope_snapshot_json: any | null
  created_by: string
  created_by_email: string
  created_at: string
  files: VersionFile[]
  // Added by publish endpoint
  published_url?: string
}

// Chat and AI Generation types
export interface ChatSession {
  id: string
  internal_app: string
  title: string
  model_id: string
  is_active: boolean
  created_by: string
  created_at: string
  message_count: number
}

export interface ChatMessage {
  id: string
  session: string
  role: 'user' | 'assistant' | 'system'
  content: string
  status: 'pending' | 'streaming' | 'complete' | 'error'
  model_id?: string
  duration_ms?: number
  generated_spec_json?: AppSpec
  generated_files?: Record<string, string>
  version_created?: string
  error_message?: string
  created_at: string
}

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

export interface VersionFile {
  id: string
  app_version: string
  path: string
  content: string
  content_hash: string
  created_at: string
}

export interface AppSpec {
  appName: string
  pages: PageSpec[]
}

export interface PageSpec {
  id: string
  title: string
  layout: 'table_detail_drawer' | 'tabbed_views'
  primaryResource: string
  view: {
    table?: TableView
    detailDrawer?: DetailDrawer
  }
}

export interface TableView {
  columns: Array<{ field: string; label?: string }>
  filterableFields?: string[]
  searchableFields?: string[]
  sort?: { field: string; dir: 'asc' | 'desc' }
  pagination?: { pageSize: number }
  rowActions?: Array<{ label: string; actionId: string; confirm?: boolean }>
  bulkActions?: Array<{ label: string; actionId: string; confirm?: boolean }>
}

export interface DetailDrawer {
  titleField?: string
  fields: Array<{ field: string; label?: string; readOnly?: boolean }>
  actions?: Array<{ label: string; actionId: string; confirm?: boolean }>
}

// Version State Snapshot types
export interface VersionStateSnapshot {
  id: string
  app_version: string
  tables_json: Array<{
    id: string
    name: string
    slug: string
    description: string
    schema_json: TableSchema
    row_count: number
  }>
  resources_json: Array<{
    id: string
    resource_id: string
    resource_name: string
    schema_json: any
    exposed_fields_json: string[]
    allowed_actions_json: any[]
  }>
  file_count: number
  total_tables: number
  total_resources: number
  created_at: string
}

export interface TableSchema {
  columns: Array<{
    name: string
    type: string
    nullable?: boolean
    default?: any
    primary_key?: boolean
  }>
}

// Version Audit Log types
export type AuditEventType =
  | 'created'
  | 'published'
  | 'rolled_back'
  | 'schema_migrated'
  | 'data_migrated'
  | 'error'

export interface VersionAuditLog {
  id: string
  app_version: string
  event_type: AuditEventType
  event_type_display: string
  details_json: Record<string, any>
  created_by: string | null
  created_by_email: string | null
  created_at: string
}

// Rollback Preview types
export interface RollbackPreview {
  diff: VersionDiff | null
  warnings: RollbackWarning[]
  can_revert: boolean
  target_version: {
    id: string
    version_number: number
    source: string
    created_at: string
    intent_message?: string | null
  }
  schema_compatibility?: SchemaCompatibility
  migration_hints?: MigrationHint[]
}

export interface VersionDiff {
  tables: {
    added: Array<{ name: string; slug: string; schema?: TableSchema }>
    removed: Array<{ name: string; slug: string }>
    modified: Array<{ slug: string; name: string; from: TableSchema; to: TableSchema }>
  }
  resources: {
    added: any[]
    removed: any[]
    modified: any[]
  }
  files: {
    added: string[]
    removed: string[]
    modified: string[]
    from_count: number
    to_count: number
  }
  versions: {
    from: { id: string; version_number: number; source: string; created_at: string }
    to: { id: string; version_number: number; source: string; created_at: string }
  }
}

export interface RollbackWarning {
  type: string
  severity: 'info' | 'warning' | 'error'
  message: string
  details?: any
}

export interface SchemaCompatibility {
  is_compatible: boolean
  risk_level: 'safe' | 'warning' | 'danger'
  tables: Array<{
    slug: string
    name: string
    change: 'added' | 'removed' | 'modified'
    risk: string
    compatibility?: any
  }>
  summary: {
    tables_removed: number
    tables_added: number
    tables_modified: number
    total_warnings: number
    total_errors: number
  }
}

export interface MigrationHint {
  column: string
  type: string
  severity: string
  suggestion: string
  action: string
}

// Extended AppVersion with snapshot and audit logs
export interface AppVersionWithSnapshot extends AppVersion {
  state_snapshot?: VersionStateSnapshot
  audit_logs?: VersionAuditLog[]
}

// ============================================================================
// Integrations & Connectors Types
// ============================================================================

/**
 * Integration Provider - Organization-level configuration for external integrations.
 * The organization is registered as a single "user" with Merge Agent Handler.
 * All org members share the connected integrations.
 */
export interface IntegrationProvider {
  id: string
  organization: string
  organization_name: string
  display_name: string
  is_active: boolean
  is_registered: boolean  // Whether org is registered with Merge
  is_configured: boolean  // Whether Merge API credentials are configured in env
  connector_count: number
  connected_count: number // Number of connected integrations
  created_at: string
  updated_at: string
}

/**
 * Connector - An external service that can be connected (e.g., Jira, Slack, Linear).
 * Connection is at the organization level, not per-user.
 * Data from Tool Pack API: https://docs.ah.merge.dev/api-reference/tool-packs/list-tool-packs
 */
export interface Connector {
  id: string
  connector_id: string  // Slug e.g., "jira", "slack", "linear"
  name: string          // Display name e.g., "Jira", "Slack"
  category: string      // Primary category e.g., "project_management", "communication"
  categories: string[]  // All category tags e.g., ["Project Management", "Ticketing"]
  logo_url: string | null  // Logo image URL
  icon_url: string | null  // Backwards compat - same as logo_url
  source_url: string | null  // Source website URL e.g., "https://linear.app"
  description: string
  is_enabled: boolean
  is_connected: boolean      // Whether the org has connected this
  connected_at: string | null // When the connection was established
  connected_by: string | null // Email of user who connected
  tool_count: number
  tools: ConnectorTool[]
}

/**
 * Tool available on a connector (e.g., create_issue, send_message).
 */
export interface ConnectorTool {
  id: string
  name: string
  description: string
  parameters: Record<string, ConnectorToolParameter>
}

/**
 * Parameter definition for a connector tool.
 */
export interface ConnectorToolParameter {
  type: string
  description: string
  required: boolean
  enum?: string[]
  default?: any
}

/**
 * Organization's connection status to a specific connector.
 */
export interface OrgConnectorStatus {
  connector_id: string
  connector_name: string
  is_connected: boolean
  connected_at: string | null
  connected_by: string | null  // Email of user who connected
}

/**
 * Response from generating a link token for OAuth flow.
 */
export interface LinkTokenResponse {
  link_token: string
  expires_in: number
}

/**
 * Response from syncing connectors.
 */
export interface SyncConnectorsResponse {
  success: boolean
  message: string
  connector_count: number
}

/**
 * Response from handling link callback.
 * Connection is only marked successful if verified with Merge API.
 */
export interface LinkCallbackResponse {
  success: boolean
  connector_id: string
  is_connected: boolean
  connected_at?: string  // Only present if connection succeeded
  connected_by?: string  // Only present if connection succeeded
  message?: string       // Present if connection failed (e.g., "Connection not completed")
  error?: string         // Present if there was a service error
}

