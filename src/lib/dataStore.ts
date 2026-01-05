/**
 * Data Store Runtime Client
 * 
 * Client library for generated apps to interact with the App Data Store.
 * This file is bundled with generated apps and provides a simple API
 * for CRUD operations on data tables.
 */

// Types for the data store API
export interface DataStoreRow {
  id: string;
  data: Record<string, unknown>;
  row_index: number;
  created_at?: string;
  updated_at?: string;
}

export interface QueryFilter {
  field: string;
  op: 'eq' | 'neq' | 'gt' | 'gte' | 'lt' | 'lte' | 'in' | 'not_in' | 'contains' | 'icontains' | 'is_null';
  value: unknown;
}

export interface QueryOrderBy {
  field: string;
  dir: 'asc' | 'desc';
}

export interface QueryOptions {
  filters?: QueryFilter[];
  orderBy?: QueryOrderBy[];
  limit?: number;
  offset?: number;
  select?: string[];
}

export interface QueryResult {
  rows: Array<{
    id: string;
    row_index: number;
    data: Record<string, unknown>;
    created_at: string | null;
    updated_at: string | null;
  }>;
  total_count: number;
  limit: number;
  offset: number;
  has_more: boolean;
}

export interface TableInfo {
  slug: string;
  name: string;
  description: string;
  row_count: number;
  column_count: number;
}

export interface TableSchema {
  slug: string;
  name: string;
  description: string;
  schema: {
    columns: Array<{
      name: string;
      type: string;
      nullable?: boolean;
      primary_key?: boolean;
      unique?: boolean;
      default?: unknown;
      enum_values?: string[];
    }>;
  };
  row_count: number;
}

export interface InsertResult {
  id: string;
  data: Record<string, unknown>;
  row_index: number;
  created_at: string | null;
}

export interface UpdateResult {
  id: string;
  data: Record<string, unknown>;
  row_index: number;
  updated_at: string | null;
}

export interface BulkInsertResult {
  created_count: number;
  rows: Array<{
    id: string;
    data: Record<string, unknown>;
    row_index: number;
  }>;
  errors: string[];
}

export interface BulkDeleteResult {
  deleted_count: number;
}

// Get runtime config from window (injected by preview/runtime)
declare global {
  interface Window {
    __RELAY_CONFIG__?: {
      appId: string;
      versionId?: string;
      apiBaseUrl: string;
    };
  }
}

function getConfig() {
  const config = window.__RELAY_CONFIG__;
  if (!config) {
    throw new Error('Relay runtime config not found. Make sure the app is running in the Relay environment.');
  }
  return config;
}

/**
 * Make a runtime data API call
 */
async function runtimeDataCall<T>(
  operation: string,
  tableSlug: string | null,
  params: Record<string, unknown> = {}
): Promise<T> {
  const config = getConfig();
  
  const body: Record<string, unknown> = {
    appId: config.appId,
    operation,
    params,
  };
  
  if (config.versionId) {
    body.versionId = config.versionId;
  }
  
  if (tableSlug) {
    body.tableSlug = tableSlug;
  }
  
  const response = await fetch(`${config.apiBaseUrl}/runtime/data/`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(body),
  });
  
  const result = await response.json();
  
  if (!response.ok) {
    throw new Error(result.error || `Data store operation failed: ${operation}`);
  }
  
  return result as T;
}

/**
 * Data Store API
 * 
 * Usage:
 * ```typescript
 * import { dataStore } from './lib/dataStore';
 * 
 * // Query rows
 * const result = await dataStore.query('users', {
 *   filters: [{ field: 'status', op: 'eq', value: 'active' }],
 *   orderBy: [{ field: 'name', dir: 'asc' }],
 *   limit: 50
 * });
 * 
 * // Insert a row
 * const newUser = await dataStore.insert('users', { name: 'John', email: 'john@example.com' });
 * 
 * // Update a row
 * await dataStore.update('users', 'row-uuid', { status: 'inactive' });
 * 
 * // Delete a row
 * await dataStore.delete('users', 'row-uuid');
 * ```
 */
export const dataStore = {
  /**
   * List all tables available in the app
   */
  listTables: async (): Promise<TableInfo[]> => {
    const result = await runtimeDataCall<{ tables: TableInfo[] }>('listTables', null);
    return result.tables;
  },
  
  /**
   * Get schema information for a table
   */
  getSchema: async (tableSlug: string): Promise<TableSchema> => {
    return runtimeDataCall<TableSchema>('getSchema', tableSlug);
  },
  
  /**
   * Query rows from a table with optional filtering, sorting, and pagination
   */
  query: async (tableSlug: string, options: QueryOptions = {}): Promise<QueryResult> => {
    return runtimeDataCall<QueryResult>('query', tableSlug, {
      filters: options.filters,
      orderBy: options.orderBy,
      limit: options.limit,
      offset: options.offset,
      select: options.select,
    });
  },
  
  /**
   * Insert a new row into a table
   */
  insert: async (tableSlug: string, data: Record<string, unknown>): Promise<InsertResult> => {
    return runtimeDataCall<InsertResult>('insert', tableSlug, { data });
  },
  
  /**
   * Update an existing row
   */
  update: async (tableSlug: string, rowId: string, data: Record<string, unknown>): Promise<UpdateResult> => {
    return runtimeDataCall<UpdateResult>('update', tableSlug, { rowId, data });
  },
  
  /**
   * Delete a row
   */
  delete: async (tableSlug: string, rowId: string): Promise<{ success: boolean }> => {
    return runtimeDataCall<{ success: boolean }>('delete', tableSlug, { rowId });
  },
  
  /**
   * Bulk insert multiple rows
   */
  bulkInsert: async (tableSlug: string, rows: Record<string, unknown>[]): Promise<BulkInsertResult> => {
    return runtimeDataCall<BulkInsertResult>('bulkInsert', tableSlug, { rows });
  },
  
  /**
   * Bulk delete multiple rows
   */
  bulkDelete: async (tableSlug: string, rowIds: string[]): Promise<BulkDeleteResult> => {
    return runtimeDataCall<BulkDeleteResult>('bulkDelete', tableSlug, { rowIds });
  },
};

// Default export for convenience
export default dataStore;

