/**
 * TypeScript types for backend models
 */

export interface Organization {
  id: string
  name: string
  slug: string
  logo_url: string | null
  created_at: string
  updated_at: string
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
  name: string
  description: string
  status: 'draft' | 'published'
  status_display: string
  backend_connection: string
  backend_connection_name: string
  allow_actions_in_preview: boolean
  created_by: string
  created_by_email: string
  created_at: string
  updated_at: string
}

export interface AppVersion {
  id: string
  internal_app: string
  version_number: number
  source: 'ai' | 'code' | 'publish'
  source_display: string
  spec_json: AppSpec
  scope_snapshot_json: any | null
  created_by: string
  created_by_email: string
  created_at: string
  files: VersionFile[]
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

