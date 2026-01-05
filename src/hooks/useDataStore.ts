/**
 * Custom hooks for App Data Store
 * 
 * React Query hooks for managing tables and rows.
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import * as dataStoreService from '@/services/dataStoreService'
import type {
  QuerySpec,
  CreateTableRequest,
  UpdateTableRequest,
  InsertRowRequest,
  BulkInsertRequest,
  BulkDeleteRequest,
} from '@/types/dataStore'

// Query keys
export const dataStoreKeys = {
  all: ['dataStore'] as const,
  tables: (appId: string) => [...dataStoreKeys.all, 'tables', appId] as const,
  table: (appId: string, tableSlug: string) => [...dataStoreKeys.tables(appId), tableSlug] as const,
  rows: (appId: string, tableSlug: string) => [...dataStoreKeys.all, 'rows', appId, tableSlug] as const,
  row: (appId: string, tableSlug: string, rowId: string) => 
    [...dataStoreKeys.rows(appId, tableSlug), rowId] as const,
}

// ============================================================================
// Table Hooks
// ============================================================================

/**
 * Fetch all tables for an app
 */
export function useTables(appId: string | null) {
  return useQuery({
    queryKey: dataStoreKeys.tables(appId || ''),
    queryFn: () => dataStoreService.listTables(appId!),
    enabled: !!appId,
  })
}

/**
 * Fetch a single table
 */
export function useTable(appId: string | null, tableSlug: string | null) {
  return useQuery({
    queryKey: dataStoreKeys.table(appId || '', tableSlug || ''),
    queryFn: () => dataStoreService.getTable(appId!, tableSlug!),
    enabled: !!appId && !!tableSlug,
  })
}

/**
 * Create a new table
 */
export function useCreateTable() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: ({ appId, data }: { appId: string; data: CreateTableRequest }) =>
      dataStoreService.createTable(appId, data),
    onSuccess: (_, { appId }) => {
      queryClient.invalidateQueries({ queryKey: dataStoreKeys.tables(appId) })
    },
  })
}

/**
 * Update a table
 */
export function useUpdateTable() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: ({
      appId,
      tableSlug,
      data,
    }: {
      appId: string
      tableSlug: string
      data: UpdateTableRequest
    }) => dataStoreService.updateTable(appId, tableSlug, data),
    onSuccess: (updatedTable, { appId, tableSlug }) => {
      queryClient.invalidateQueries({ queryKey: dataStoreKeys.tables(appId) })
      queryClient.setQueryData(
        dataStoreKeys.table(appId, tableSlug),
        updatedTable
      )
    },
  })
}

/**
 * Delete a table
 */
export function useDeleteTable() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: ({ appId, tableSlug }: { appId: string; tableSlug: string }) =>
      dataStoreService.deleteTable(appId, tableSlug),
    onSuccess: (_, { appId, tableSlug }) => {
      queryClient.invalidateQueries({ queryKey: dataStoreKeys.tables(appId) })
      queryClient.removeQueries({ queryKey: dataStoreKeys.table(appId, tableSlug) })
      queryClient.removeQueries({ queryKey: dataStoreKeys.rows(appId, tableSlug) })
    },
  })
}

// ============================================================================
// Row Hooks
// ============================================================================

/**
 * Query rows with filters
 */
export function useQueryRows(
  appId: string | null,
  tableSlug: string | null,
  querySpec?: QuerySpec,
  options?: { enabled?: boolean }
) {
  return useQuery({
    queryKey: [...dataStoreKeys.rows(appId || '', tableSlug || ''), querySpec],
    queryFn: () => dataStoreService.queryRows(appId!, tableSlug!, querySpec),
    enabled: (options?.enabled ?? true) && !!appId && !!tableSlug,
  })
}

/**
 * Insert a new row
 */
export function useInsertRow() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: ({
      appId,
      tableSlug,
      data,
    }: {
      appId: string
      tableSlug: string
      data: InsertRowRequest
    }) => dataStoreService.insertRow(appId, tableSlug, data),
    onSuccess: (_, { appId, tableSlug }) => {
      queryClient.invalidateQueries({ queryKey: dataStoreKeys.rows(appId, tableSlug) })
      queryClient.invalidateQueries({ queryKey: dataStoreKeys.tables(appId) }) // Update row count
    },
  })
}

/**
 * Update a row
 */
export function useUpdateRow() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: ({
      appId,
      tableSlug,
      rowId,
      data,
    }: {
      appId: string
      tableSlug: string
      rowId: string
      data: InsertRowRequest
    }) => dataStoreService.updateRow(appId, tableSlug, rowId, data),
    onSuccess: (_, { appId, tableSlug }) => {
      queryClient.invalidateQueries({ queryKey: dataStoreKeys.rows(appId, tableSlug) })
    },
  })
}

/**
 * Delete a row
 */
export function useDeleteRow() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: ({
      appId,
      tableSlug,
      rowId,
    }: {
      appId: string
      tableSlug: string
      rowId: string
    }) => dataStoreService.deleteRow(appId, tableSlug, rowId),
    onSuccess: (_, { appId, tableSlug }) => {
      queryClient.invalidateQueries({ queryKey: dataStoreKeys.rows(appId, tableSlug) })
      queryClient.invalidateQueries({ queryKey: dataStoreKeys.tables(appId) }) // Update row count
    },
  })
}

/**
 * Bulk insert rows
 */
export function useBulkInsertRows() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: ({
      appId,
      tableSlug,
      data,
    }: {
      appId: string
      tableSlug: string
      data: BulkInsertRequest
    }) => dataStoreService.bulkInsertRows(appId, tableSlug, data),
    onSuccess: (_, { appId, tableSlug }) => {
      queryClient.invalidateQueries({ queryKey: dataStoreKeys.rows(appId, tableSlug) })
      queryClient.invalidateQueries({ queryKey: dataStoreKeys.tables(appId) })
    },
  })
}

/**
 * Bulk delete rows
 */
export function useBulkDeleteRows() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: ({
      appId,
      tableSlug,
      data,
    }: {
      appId: string
      tableSlug: string
      data: BulkDeleteRequest
    }) => dataStoreService.bulkDeleteRows(appId, tableSlug, data),
    onSuccess: (_, { appId, tableSlug }) => {
      queryClient.invalidateQueries({ queryKey: dataStoreKeys.rows(appId, tableSlug) })
      queryClient.invalidateQueries({ queryKey: dataStoreKeys.tables(appId) })
    },
  })
}

