/**
 * Data Store Service
 * 
 * API client for App Data Store operations.
 * Manages tables and rows for internal app data storage.
 */
import { api } from './api'
import type {
  DataTable,
  DataRow,
  QueryResult,
  QuerySpec,
  CreateTableRequest,
  UpdateTableRequest,
  InsertRowRequest,
  BulkInsertRequest,
  BulkInsertResponse,
  BulkDeleteRequest,
  BulkDeleteResponse,
} from '@/types/dataStore'

// ============================================================================
// Table Operations
// ============================================================================

/**
 * List all tables for an app
 */
export async function listTables(appId: string): Promise<DataTable[]> {
  const response = await api.get<DataTable[]>(`/apps/${appId}/data/tables/`)
  return response.data
}

/**
 * Get a single table by slug
 */
export async function getTable(appId: string, tableSlug: string): Promise<DataTable> {
  const response = await api.get<DataTable>(`/apps/${appId}/data/tables/${tableSlug}/`)
  return response.data
}

/**
 * Create a new table
 */
export async function createTable(appId: string, data: CreateTableRequest): Promise<DataTable> {
  const response = await api.post<DataTable>(`/apps/${appId}/data/tables/`, data)
  return response.data
}

/**
 * Update a table
 */
export async function updateTable(
  appId: string,
  tableSlug: string,
  data: UpdateTableRequest
): Promise<DataTable> {
  const response = await api.patch<DataTable>(`/apps/${appId}/data/tables/${tableSlug}/`, data)
  return response.data
}

/**
 * Delete a table
 */
export async function deleteTable(appId: string, tableSlug: string): Promise<void> {
  await api.delete(`/apps/${appId}/data/tables/${tableSlug}/`)
}

// ============================================================================
// Row Operations
// ============================================================================

/**
 * Query rows with optional filtering, sorting, and pagination
 */
export async function queryRows(
  appId: string,
  tableSlug: string,
  querySpec?: QuerySpec
): Promise<QueryResult> {
  const response = await api.post<QueryResult>(
    `/apps/${appId}/data/tables/${tableSlug}/query/`,
    querySpec || {}
  )
  return response.data
}

/**
 * List rows (simple version without filters)
 */
export async function listRows(
  appId: string,
  tableSlug: string,
  limit = 50,
  offset = 0
): Promise<QueryResult> {
  const response = await api.get<QueryResult>(
    `/apps/${appId}/data/tables/${tableSlug}/rows/`,
    { params: { limit, offset } }
  )
  return response.data
}

/**
 * Get a single row by ID
 */
export async function getRow(
  appId: string,
  tableSlug: string,
  rowId: string
): Promise<DataRow> {
  const response = await api.get<DataRow>(
    `/apps/${appId}/data/tables/${tableSlug}/rows/${rowId}/`
  )
  return response.data
}

/**
 * Insert a new row
 */
export async function insertRow(
  appId: string,
  tableSlug: string,
  data: InsertRowRequest
): Promise<DataRow> {
  const response = await api.post<DataRow>(
    `/apps/${appId}/data/tables/${tableSlug}/rows/`,
    data
  )
  return response.data
}

/**
 * Update a row
 */
export async function updateRow(
  appId: string,
  tableSlug: string,
  rowId: string,
  data: InsertRowRequest
): Promise<DataRow> {
  const response = await api.patch<DataRow>(
    `/apps/${appId}/data/tables/${tableSlug}/rows/${rowId}/`,
    data
  )
  return response.data
}

/**
 * Delete a row
 */
export async function deleteRow(
  appId: string,
  tableSlug: string,
  rowId: string
): Promise<void> {
  await api.delete(`/apps/${appId}/data/tables/${tableSlug}/rows/${rowId}/`)
}

/**
 * Bulk insert rows
 */
export async function bulkInsertRows(
  appId: string,
  tableSlug: string,
  data: BulkInsertRequest
): Promise<BulkInsertResponse> {
  const response = await api.post<BulkInsertResponse>(
    `/apps/${appId}/data/tables/${tableSlug}/rows/bulk/`,
    data
  )
  return response.data
}

/**
 * Bulk delete rows
 */
export async function bulkDeleteRows(
  appId: string,
  tableSlug: string,
  data: BulkDeleteRequest
): Promise<BulkDeleteResponse> {
  const response = await api.delete<BulkDeleteResponse>(
    `/apps/${appId}/data/tables/${tableSlug}/rows/bulk/`,
    { data }
  )
  return response.data
}

