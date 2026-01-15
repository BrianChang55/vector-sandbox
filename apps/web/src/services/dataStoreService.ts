/**
 * Data Store Service
 * 
 * API client for App Data Store operations.
 * Manages tables and rows for internal app data storage.
 * 
 * This module re-exports functions from the centralized apiService.
 * @see {@link @/services/apiService} for full API documentation.
 */
import { dataTablesApi } from './apiService'

// ============================================================================
// Table Operations - Re-exported from centralized apiService
// ============================================================================

/**
 * List all tables for an app
 * @see {@link dataTablesApi.listTables}
 */
export const listTables = dataTablesApi.listTables

/**
 * Get a single table by slug
 * @see {@link dataTablesApi.getTable}
 */
export const getTable = dataTablesApi.getTable

/**
 * Create a new table
 * @see {@link dataTablesApi.createTable}
 */
export const createTable = dataTablesApi.createTable

/**
 * Update a table
 * @see {@link dataTablesApi.updateTable}
 */
export const updateTable = dataTablesApi.updateTable

/**
 * Delete a table
 * @see {@link dataTablesApi.deleteTable}
 */
export const deleteTable = dataTablesApi.deleteTable

// ============================================================================
// Row Operations - Re-exported from centralized apiService
// ============================================================================

/**
 * Query rows with optional filtering, sorting, and pagination
 * @see {@link dataTablesApi.queryRows}
 */
export const queryRows = dataTablesApi.queryRows

/**
 * List rows (simple version without filters)
 * @see {@link dataTablesApi.listRows}
 */
export const listRows = dataTablesApi.listRows

/**
 * Get a single row by ID
 * @see {@link dataTablesApi.getRow}
 */
export const getRow = dataTablesApi.getRow

/**
 * Insert a new row
 * @see {@link dataTablesApi.insertRow}
 */
export const insertRow = dataTablesApi.insertRow

/**
 * Update a row
 * @see {@link dataTablesApi.updateRow}
 */
export const updateRow = dataTablesApi.updateRow

/**
 * Delete a row
 * @see {@link dataTablesApi.deleteRow}
 */
export const deleteRow = dataTablesApi.deleteRow

/**
 * Bulk insert rows
 * @see {@link dataTablesApi.bulkInsertRows}
 */
export const bulkInsertRows = dataTablesApi.bulkInsertRows

/**
 * Bulk delete rows
 * @see {@link dataTablesApi.bulkDeleteRows}
 */
export const bulkDeleteRows = dataTablesApi.bulkDeleteRows

/**
 * Export all rows from a table (handles pagination automatically)
 * @see {@link dataTablesApi.exportAllRows}
 */
export const exportAllRows = dataTablesApi.exportAllRows
