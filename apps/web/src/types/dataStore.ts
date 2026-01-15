/**
 * Types for App Data Store
 * 
 * Represents tables, schemas, rows, and query operations
 * for the per-app data storage system.
 */

// Column types supported by the data store
export type ColumnType = 
  | 'uuid'
  | 'string'
  | 'text'
  | 'integer'
  | 'float'
  | 'boolean'
  | 'datetime'
  | 'date'
  | 'enum'
  | 'json'
  | 'reference'

// Column definition in a table schema
export interface ColumnDef {
  name: string
  type: ColumnType
  nullable?: boolean
  primary_key?: boolean
  auto_generate?: boolean
  unique?: boolean
  default?: unknown
  max_length?: number
  min?: number
  max?: number
  enum_values?: string[]
  reference_table?: string
  reference_column?: string
  auto_now_add?: boolean
  auto_now?: boolean
}

// Index definition
export interface IndexDef {
  columns: string[]
  unique?: boolean
}

// Table schema
export interface TableSchema {
  columns: ColumnDef[]
  indexes?: IndexDef[]
}

// Data table model
export interface DataTable {
  id: string
  internal_app: string
  name: string
  slug: string
  description: string
  schema_json: TableSchema
  row_count: number
  column_count: number
  created_at: string
  updated_at: string
}

// Data row model
export interface DataRow {
  id: string
  table: string
  data: Record<string, unknown>
  row_index: number
  created_at: string
  updated_at: string
}

// Query result row (includes metadata)
export interface QueryResultRow {
  id: string
  row_index: number
  data: Record<string, unknown>
  created_at: string | null
  updated_at: string | null
}

// Query result
export interface QueryResult {
  rows: QueryResultRow[]
  total_count: number
  limit: number
  offset: number
  has_more: boolean
}

// Filter operators
export type FilterOperator = 
  | 'eq'
  | 'neq'
  | 'gt'
  | 'gte'
  | 'lt'
  | 'lte'
  | 'in'
  | 'not_in'
  | 'contains'
  | 'icontains'
  | 'is_null'

// Filter definition
export interface FilterDef {
  field: string
  op: FilterOperator
  value: unknown
}

// Order by definition
export interface OrderByDef {
  field: string
  dir: 'asc' | 'desc'
}

// Query specification
export interface QuerySpec {
  select?: string[]
  filters?: FilterDef[]
  order_by?: OrderByDef[]
  limit?: number
  offset?: number
}

// Create table request
export interface CreateTableRequest {
  name: string
  description?: string
  schema_json: TableSchema
}

// Update table request
export interface UpdateTableRequest {
  name?: string
  description?: string
  schema_json?: TableSchema
}

// Insert row request
export interface InsertRowRequest {
  data: Record<string, unknown>
}

// Bulk insert request
export interface BulkInsertRequest {
  rows: Record<string, unknown>[]
}

// Bulk insert response
export interface BulkInsertResponse {
  created_count: number
  rows: DataRow[]
  errors?: string[]
}

// Bulk delete request
export interface BulkDeleteRequest {
  row_ids: string[]
}

// Bulk delete response
export interface BulkDeleteResponse {
  deleted_count: number
}

