/**
 * Sandpack Preview Component
 * 
 * Live React app runtime using CodeSandbox's Sandpack bundler.
 * Renders generated React code in a sandboxed iframe with hot reloading.
 */
import { useMemo, useState, useEffect, useRef, useCallback } from 'react'
import {
  SandpackProvider,
  SandpackPreview as SandpackPreviewPane,
  SandpackCodeEditor,
  SandpackConsole,
  useSandpack,
} from '@codesandbox/sandpack-react'
import { motion, AnimatePresence } from 'framer-motion'
import {
  Monitor,
  Code2,
  Terminal,
  Eye,
  FileText,
  X,
  AlertCircle,
  CheckCircle2,
  Loader2,
  ExternalLink,
  History,
} from 'lucide-react'
import { cn } from '../../lib/utils'
import { versionsApi } from '../../services/apiService'
import type { FileChange } from '../../types/agent'
import type { BundlerError } from '../../hooks/useSandpackValidation'
import { TEMPLATE_UI_COMPONENTS } from './templateComponents'

// Get the API base URL for runtime injection into Sandpack
// This should point to the actual backend, not the Sandpack iframe origin
const RUNTIME_API_BASE_URL = import.meta.env.VITE_API_BASE_URL || 'http://localhost:8001/api/v1'

interface SandpackPreviewProps {
  files: FileChange[]
  appId: string
  versionId?: string
  appName?: string
  className?: string
  hideToolbar?: boolean
  onFilesChange?: (files: FileChange[]) => void
  showVersionsSidebar?: boolean
  onToggleVersionsSidebar?: () => void
  /** Callback when bundler errors are detected (parent handles the fix) */
  onBundlerErrors?: (errors: BundlerError[]) => void
  /** Enable error detection and reporting (only after live generation) */
  enableAutoFix?: boolean
  /** Whether files are being actively streamed (vs loaded from saved version) */
  isStreaming?: boolean
}

type ViewMode = 'preview' | 'code' | 'split'

function hashString(input: string): string {
  // Lightweight deterministic hash (djb2)
  let hash = 5381
  for (let i = 0; i < input.length; i++) {
    hash = ((hash << 5) + hash) ^ input.charCodeAt(i)
  }
  return (hash >>> 0).toString(16)
}

function hashFiles(files: FileChange[]): string {
  // Include path + content so any edit triggers a refresh.
  const parts = files
    .map((f) => `${f.path}\n${f.content || ''}`)
    .sort()
    .join('\n---\n')
  return hashString(parts)
}

function hashSandpackRuntimeFiles(sandpackFiles: Record<string, unknown>): string {
  // Hash the live editor state so we can trigger runs on any code edit.
  const parts = Object.entries(sandpackFiles)
    .map(([path, file]) => `${path}\n${(file as { code?: string }).code || ''}`)
    .sort()
    .join('\n---\n')
  return hashString(parts)
}

// Default files for a React app
const DEFAULT_FILES = {
  '/public/index.html': `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>App Preview</title>
  <script src="https://cdn.tailwindcss.com"></script>
</head>
<body>
  <div id="root"></div>
</body>
</html>`,
  // Sandpack's react-ts template uses root entrypoints (/index.tsx, /App.tsx, /styles.css).
  // We keep backend-stored paths under src/, but normalize them into this structure for preview.
  '/index.tsx': `import React from 'react';
import { createRoot } from 'react-dom/client';
import App from './App';
import './styles.css';

const container = document.getElementById('root');
if (container) {
  const root = createRoot(container);
  root.render(<App />);
}`,
  '/styles.css': `@tailwind base;
@tailwind components;
@tailwind utilities;

body {
  margin: 0;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
}`,
  '/App.tsx': `import React from 'react';
import { Loader2 } from 'lucide-react';

export default function App() {
  return (
    <div className="min-h-screen bg-gray-50 flex items-center justify-center">
      <div className="text-center">
        <div className="w-14 h-14 mx-auto mb-5 bg-white rounded-lg border border-gray-200 shadow-sm flex items-center justify-center">
          <Loader2 className="h-6 w-6 text-gray-400 animate-spin" />
        </div>
        <h2 className="text-lg font-medium text-gray-900 mb-1">Generating</h2>
        <p className="text-sm text-gray-500">Building your application...</p>
      </div>
    </div>
  );
}`,
  '/lib/runtime.ts': `// Runtime API Client
declare global {
  interface Window {
    __RELAY_CONFIG__?: {
      appId: string;
      versionId: string;
      apiBaseUrl: string;
      appName: string;
    };
  }
}

export interface QuerySpec {
  select?: string[];
  filters?: Array<{ field: string; op: string; value: any }>;
  orderBy?: Array<{ field: string; dir: 'asc' | 'desc' }>;
  limit?: number;
  offset?: number;
}

// IMPORTANT: Always use absolute URL fallback to prevent relative URL issues in iframes
const FALLBACK_API_URL = 'http://localhost:8001/api/v1';

function getConfig() {
  const config = window.__RELAY_CONFIG__;
  if (!config?.apiBaseUrl) {
    console.warn('[runtime] RELAY_CONFIG not found - using fallback:', FALLBACK_API_URL);
  }
  return {
    appId: config?.appId || '',
    versionId: config?.versionId || '',
    apiBaseUrl: config?.apiBaseUrl || FALLBACK_API_URL,
    appName: config?.appName || 'App',
  };
}

export async function runtimeQuery<T = any>(params: {
  resourceId: string;
  querySpec?: QuerySpec;
}): Promise<{ data: T[]; count: number }> {
  const config = getConfig();
  try {
    const response = await fetch(\`\${config.apiBaseUrl}/runtime/query/\`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        appId: config.appId,
        versionId: config.versionId,
        ...params,
      }),
    });
    return response.json();
  } catch (error) {
    console.error('Query error:', error);
    return { data: [], count: 0 };
  }
}

export async function runtimeAction(params: {
  actionId: string;
  args?: Record<string, any>;
}): Promise<{ success: boolean; data?: any }> {
  const config = getConfig();
  try {
    const response = await fetch(\`\${config.apiBaseUrl}/runtime/action/\`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        appId: config.appId,
        versionId: config.versionId,
        ...params,
      }),
    });
    return response.json();
  } catch (error) {
    console.error('Action error:', error);
    return { success: false };
  }
}

import { useState, useEffect, useCallback } from 'react';

export function useQuery<T = any>(resourceId: string, querySpec?: QuerySpec, deps: any[] = []) {
  const [data, setData] = useState<T[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  
  const refetch = useCallback(async () => {
    setLoading(true);
    try {
      const result = await runtimeQuery<T>({ resourceId, querySpec });
      setData(result.data);
    } catch (e) {
      setError(String(e));
    } finally {
      setLoading(false);
    }
  }, [resourceId, JSON.stringify(querySpec)]);
  
  useEffect(() => { refetch(); }, [refetch, ...deps]);
  
  return { data, loading, error, refetch };
}`,
  '/lib/dataStore.ts': `// Data Store API Client
// This client provides access to the app's data tables

declare global {
  interface Window {
    __RELAY_CONFIG__?: {
      appId: string;
      versionId?: string;
      apiBaseUrl: string;
      appName?: string;
    };
  }
}

// IMPORTANT: Always use absolute URL fallback to prevent relative URL issues in iframes
const FALLBACK_API_URL = 'http://localhost:8001/api/v1';

function getConfig() {
  const config = window.__RELAY_CONFIG__;
  if (!config?.apiBaseUrl) {
    console.warn('[dataStore] RELAY_CONFIG not found - using fallback:', FALLBACK_API_URL);
  }
  return {
    appId: config?.appId || '',
    versionId: config?.versionId || '',
    apiBaseUrl: config?.apiBaseUrl || FALLBACK_API_URL,
  };
}

interface QueryOptions {
  filters?: Array<{ field: string; op: string; value: any }>;
  orderBy?: Array<{ field: string; dir: 'asc' | 'desc' }>;
  limit?: number;
  offset?: number;
  select?: string[];
}

// IMPORTANT: Row structure - access your fields via row.data.fieldName
// row.id = row UUID (for update/delete)
// row.data = { title: '...', email: '...', ... } (your actual data)
export interface DataRow<T = Record<string, any>> {
  id: string;
  row_index: number;
  data: T;
  created_at: string | null;
  updated_at: string | null;
}

export interface QueryResult<T = Record<string, any>> {
  rows: DataRow<T>[];
  total_count: number;
  limit: number;
  offset: number;
  has_more: boolean;
}

// Helper: extractData(rows) returns [{_id, ...data}]
export function extractData<T extends Record<string, any>>(rows: DataRow<T>[]): (T & { _id: string })[] {
  return rows.map(row => ({ ...row.data, _id: row.id }));
}

interface InsertResult {
  id: string;
  data: Record<string, any>;
  row_index: number;
  created_at: string | null;
}

async function dataApiCall<T>(operation: string, tableSlug: string | null, params: Record<string, any> = {}): Promise<T> {
  const config = getConfig();
  
  // Build the full absolute URL - never use relative URLs in iframes
  const url = \`\${config.apiBaseUrl}/runtime/data/\`;
  console.log('[dataStore] API call:', operation, tableSlug, 'to', url);
  
  const body: Record<string, any> = {
    appId: config.appId,
    versionId: config.versionId,
    operation,
    tableSlug,
    params,
  };
  
  const response = await fetch(url, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  });
  
  if (!response.ok) {
    const error = await response.json().catch(() => ({}));
    throw new Error(error.error || 'Data store operation failed');
  }
  
  return response.json();
}

export const dataStore = {
  // List all tables
  listTables: async (): Promise<Array<{ slug: string; name: string; row_count: number }>> => {
    const result = await dataApiCall<{ tables: any[] }>('listTables', null);
    return result.tables || [];
  },
  
  // Get table schema
  getSchema: async (tableSlug: string): Promise<any> => {
    return dataApiCall('getSchema', tableSlug);
  },
  
  // Query rows with optional filtering, sorting, pagination
  query: async (tableSlug: string, options: QueryOptions = {}): Promise<QueryResult> => {
    return dataApiCall<QueryResult>('query', tableSlug, {
      filters: options.filters,
      orderBy: options.orderBy,
      limit: options.limit,
      offset: options.offset,
      select: options.select,
    });
  },
  
  // Insert a new row
  insert: async (tableSlug: string, data: Record<string, any>): Promise<InsertResult> => {
    return dataApiCall<InsertResult>('insert', tableSlug, { data });
  },
  
  // Update an existing row
  update: async (tableSlug: string, rowId: string, data: Record<string, any>): Promise<InsertResult> => {
    return dataApiCall<InsertResult>('update', tableSlug, { rowId, data });
  },
  
  // Delete a row
  delete: async (tableSlug: string, rowId: string): Promise<{ success: boolean }> => {
    return dataApiCall<{ success: boolean }>('delete', tableSlug, { rowId });
  },
  
  // Bulk insert multiple rows
  bulkInsert: async (tableSlug: string, rows: Record<string, any>[]): Promise<{ created_count: number; rows: any[] }> => {
    return dataApiCall('bulkInsert', tableSlug, { rows });
  },
  
  // Bulk delete multiple rows
  bulkDelete: async (tableSlug: string, rowIds: string[]): Promise<{ deleted_count: number }> => {
    return dataApiCall('bulkDelete', tableSlug, { rowIds });
  },
};

export default dataStore;

// React hook for data queries
import { useState, useEffect, useCallback } from 'react';

export function useDataQuery<T = any>(tableSlug: string, options: QueryOptions = {}, deps: any[] = []) {
  const [rows, setRows] = useState<T[]>([]);
  const [totalCount, setTotalCount] = useState(0);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  
  const refetch = useCallback(async () => {
    setLoading(true);
    setError(null);
    try {
      const result = await dataStore.query(tableSlug, options);
      setRows(result.rows.map(r => ({ id: r.id, ...r.data })) as T[]);
      setTotalCount(result.total_count);
    } catch (e) {
      setError(String(e));
    } finally {
      setLoading(false);
    }
  }, [tableSlug, JSON.stringify(options)]);
  
  useEffect(() => { refetch(); }, [refetch, ...deps]);
  
  return { rows, totalCount, loading, error, refetch };
}`,
}

// Convert FileChange array to Sandpack files format
function convertToSandpackFiles(
  files: FileChange[],
  appId: string,
  versionId: string,
  appName: string
): Record<string, string> {
  const sandpackFiles: Record<string, string> = { ...DEFAULT_FILES, ...TEMPLATE_UI_COMPONENTS }

  // CRITICAL: Use postMessage bridge to bypass CORS/Private Network Access issues
  // The Sandpack iframe cannot directly call localhost due to Chrome's PNA restrictions
  // Instead, we use postMessage to communicate with the parent window (our React app)
  // which then makes the actual API calls and sends results back
  sandpackFiles['/lib/dataStore.ts'] = `// Data Store API Client - Uses postMessage bridge to parent window
// This bypasses CORS/Private Network Access restrictions in Sandpack iframes

// Config is baked in at build time
const CONFIG = {
  appId: '${appId}',
  versionId: '${versionId}',
  apiBaseUrl: '${RUNTIME_API_BASE_URL}',
  appName: '${appName}',
};

console.log('[dataStore] Initialized with postMessage bridge, appId:', CONFIG.appId);

// Pending requests waiting for responses from parent
const pendingRequests: Map<string, { resolve: (data: any) => void; reject: (error: Error) => void }> = new Map();

// Generate unique request ID
function generateRequestId(): string {
  return \`req_\${Date.now()}_\${Math.random().toString(36).substr(2, 9)}\`;
}

// Listen for responses from parent window
window.addEventListener('message', (event) => {
  // Only accept messages that look like our API responses
  if (event.data && event.data.type === 'DATASTORE_RESPONSE') {
    const { requestId, success, data, error } = event.data;
    console.log('[dataStore] Received response for', requestId, success ? 'success' : 'error');
    
    const pending = pendingRequests.get(requestId);
    if (pending) {
      pendingRequests.delete(requestId);
      if (success) {
        pending.resolve(data);
      } else {
        pending.reject(new Error(error || 'API call failed'));
      }
    }
  }
});

interface QueryOptions {
  filters?: Array<{ field: string; op: string; value: any }>;
  orderBy?: Array<{ field: string; dir: 'asc' | 'desc' }>;
  limit?: number;
  offset?: number;
  select?: string[];
}

// ==========================================================================
// IMPORTANT: Row Data Structure
// ==========================================================================
// Each row has this structure:
// {
//   id: "row-uuid",           // Use this for update/delete operations
//   row_index: 1,
//   created_at: "2024-...",
//   updated_at: "2024-...",
//   data: {                   // YOUR FIELDS ARE HERE - access via row.data.fieldName
//     title: "...",
//     email: "...",
//     status: "..."
//   }
// }
// 
// CORRECT: row.data.title, row.data.email
// WRONG:   row.title (undefined!)
// ==========================================================================

export interface DataRow<T = Record<string, any>> {
  id: string;               // Row ID - use for update/delete
  row_index: number;
  data: T;                  // Your table columns live here!
  created_at: string | null;
  updated_at: string | null;
}

export interface QueryResult<T = Record<string, any>> {
  rows: DataRow<T>[];
  total_count: number;
  limit: number;
  offset: number;
  has_more: boolean;
}

// Helper to flatten row data - adds _id and flattens data fields
// Usage: const items = extractData(result.rows);
// Result: [{_id: 'uuid', title: '...', email: '...', ...}]
export function extractData<T extends Record<string, any>>(rows: DataRow<T>[]): (T & { _id: string })[] {
  return rows.map(row => ({ ...row.data, _id: row.id }));
}

interface InsertResult {
  id: string;
  data: Record<string, any>;
  row_index: number;
  created_at: string | null;
}

// Make API call via postMessage bridge to parent window
async function dataApiCall<T>(operation: string, tableSlug: string | null, params: Record<string, any> = {}): Promise<T> {
  const requestId = generateRequestId();
  console.log('[dataStore] Sending request via postMessage:', requestId, operation, tableSlug);
  
  return new Promise((resolve, reject) => {
    // Store the pending request
    pendingRequests.set(requestId, { resolve, reject });
    
    // Set timeout for request
    setTimeout(() => {
      if (pendingRequests.has(requestId)) {
        pendingRequests.delete(requestId);
        reject(new Error('Request timeout - parent window did not respond'));
      }
    }, 30000); // 30 second timeout
    
    // Send request to parent window
    window.parent.postMessage({
      type: 'DATASTORE_REQUEST',
      requestId,
      appId: CONFIG.appId,
      versionId: CONFIG.versionId,
      operation,
      tableSlug,
      params,
    }, '*');
  });
}

export const dataStore = {
  listTables: async (): Promise<Array<{ slug: string; name: string; row_count: number }>> => {
    const result = await dataApiCall<{ tables: any[] }>('listTables', null);
    return result.tables || [];
  },
  
  getSchema: async (tableSlug: string): Promise<any> => {
    return dataApiCall('getSchema', tableSlug);
  },
  
  query: async (tableSlug: string, options: QueryOptions = {}): Promise<QueryResult> => {
    return dataApiCall<QueryResult>('query', tableSlug, {
      filters: options.filters,
      orderBy: options.orderBy,
      limit: options.limit,
      offset: options.offset,
      select: options.select,
    });
  },
  
  insert: async (tableSlug: string, data: Record<string, any>): Promise<InsertResult> => {
    return dataApiCall<InsertResult>('insert', tableSlug, { data });
  },
  
  update: async (tableSlug: string, rowId: string, data: Record<string, any>): Promise<InsertResult> => {
    return dataApiCall<InsertResult>('update', tableSlug, { rowId, data });
  },
  
  delete: async (tableSlug: string, rowId: string): Promise<{ success: boolean }> => {
    return dataApiCall<{ success: boolean }>('delete', tableSlug, { rowId });
  },
  
  bulkInsert: async (tableSlug: string, rows: Record<string, any>[]): Promise<{ created_count: number; rows: any[] }> => {
    return dataApiCall('bulkInsert', tableSlug, { rows });
  },
  
  bulkDelete: async (tableSlug: string, rowIds: string[]): Promise<{ deleted_count: number }> => {
    return dataApiCall('bulkDelete', tableSlug, { rowIds });
  },
};

export default dataStore;
`

  // Inject mcpTools.ts for MCP integration tools - uses postMessage bridge like dataStore
  sandpackFiles['/lib/mcpTools.ts'] = `// MCP Tools Client - Uses postMessage bridge to parent window
// This bypasses CORS/Private Network Access restrictions in Sandpack iframes

// Config is baked in at build time
const CONFIG = {
  appId: '${appId}',
  versionId: '${versionId}',
  apiBaseUrl: '${RUNTIME_API_BASE_URL}',
  appName: '${appName}',
};

console.log('[mcpTools] Initialized with postMessage bridge, appId:', CONFIG.appId);

// Pending requests waiting for responses from parent
const pendingRequests: Map<string, { resolve: (data: any) => void; reject: (error: Error) => void }> = new Map();

// Generate unique request ID
function generateRequestId(): string {
  return \`mcp_\${Date.now()}_\${Math.random().toString(36).substr(2, 9)}\`;
}

// Listen for responses from parent window
window.addEventListener('message', (event) => {
  if (event.data && event.data.type === 'MCP_RESPONSE') {
    const { requestId, success, data, error } = event.data;
    console.log('[mcpTools] Received response for', requestId, success ? 'success' : 'error');
    
    const pending = pendingRequests.get(requestId);
    if (pending) {
      pendingRequests.delete(requestId);
      if (success) {
        pending.resolve(data);
      } else {
        pending.reject(new Error(error || 'MCP call failed'));
      }
    }
  }
});

interface MCPToolResult<T = any> {
  success: boolean;
  data?: T;
  error?: string;
}

export const mcpTools = {
  /**
   * Call an MCP tool by name
   * @param toolName - The full tool name with double underscore (e.g., "stripe__retrieve_balance")
   * @param params - Tool-specific parameters
   */
  async call<T = any>(
    toolName: string,
    params: Record<string, any> = {}
  ): Promise<MCPToolResult<T>> {
    const requestId = generateRequestId();
    console.log('[mcpTools] Sending request via postMessage:', requestId, toolName);
    
    return new Promise((resolve, reject) => {
      pendingRequests.set(requestId, { 
        resolve: (data) => resolve(data as MCPToolResult<T>),
        reject 
      });
      
      // Set timeout for request
      setTimeout(() => {
        if (pendingRequests.has(requestId)) {
          pendingRequests.delete(requestId);
          resolve({ success: false, error: 'Request timeout - parent window did not respond' });
        }
      }, 30000);
      
      // Send request to parent window
      window.parent.postMessage({
        type: 'MCP_REQUEST',
        requestId,
        appId: CONFIG.appId,
        toolName,
        params,
      }, '*');
    });
  },

  /**
   * List all available MCP tools
   */
  async listTools(): Promise<any[]> {
    const requestId = generateRequestId();
    console.log('[mcpTools] Listing tools via postMessage:', requestId);
    
    return new Promise((resolve) => {
      pendingRequests.set(requestId, { 
        resolve: (data) => resolve(data?.tools || []),
        reject: () => resolve([])
      });
      
      setTimeout(() => {
        if (pendingRequests.has(requestId)) {
          pendingRequests.delete(requestId);
          resolve([]);
        }
      }, 30000);
      
      window.parent.postMessage({
        type: 'MCP_LIST_TOOLS',
        requestId,
        appId: CONFIG.appId,
      }, '*');
    });
  },
};

export default mcpTools;
`

  // Also inject config into runtime.ts for legacy compatibility
  sandpackFiles['/lib/runtime.ts'] = `// Runtime API Client - Config injected at build time
// DO NOT MODIFY - this file is auto-generated with app-specific config

const CONFIG = {
  appId: '${appId}',
  versionId: '${versionId}',
  apiBaseUrl: '${RUNTIME_API_BASE_URL}',
  appName: '${appName}',
};

export interface QuerySpec {
  select?: string[];
  filters?: Array<{ field: string; op: string; value: any }>;
  orderBy?: Array<{ field: string; dir: 'asc' | 'desc' }>;
  limit?: number;
  offset?: number;
}

export async function runtimeQuery<T = any>(params: {
  resourceId: string;
  querySpec?: QuerySpec;
}): Promise<{ data: T[]; count: number }> {
  try {
    const response = await fetch(\`\${CONFIG.apiBaseUrl}/runtime/query/\`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        appId: CONFIG.appId,
        versionId: CONFIG.versionId,
        ...params,
      }),
    });
    return response.json();
  } catch (error) {
    console.error('Query error:', error);
    return { data: [], count: 0 };
  }
}

export async function runtimeAction(params: {
  actionId: string;
  args?: Record<string, any>;
}): Promise<{ success: boolean; data?: any }> {
  try {
    const response = await fetch(\`\${CONFIG.apiBaseUrl}/runtime/action/\`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        appId: CONFIG.appId,
        versionId: CONFIG.versionId,
        ...params,
      }),
    });
    return response.json();
  } catch (error) {
    console.error('Action error:', error);
    return { success: false };
  }
}

import { useState, useEffect, useCallback } from 'react';

export function useQuery<T = any>(resourceId: string, querySpec?: QuerySpec, deps: any[] = []) {
  const [data, setData] = useState<T[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  
  const refetch = useCallback(async () => {
    setLoading(true);
    try {
      const result = await runtimeQuery<T>({ resourceId, querySpec });
      setData(result.data);
    } catch (e) {
      setError(String(e));
    } finally {
      setLoading(false);
    }
  }, [resourceId, JSON.stringify(querySpec)]);
  
  useEffect(() => { refetch(); }, [refetch, ...deps]);
  
  return { data, loading, error, refetch };
}
`

  // Inject runtime config into index.html as well (belt and suspenders)
  sandpackFiles['/public/index.html'] = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${appName} - Preview</title>
  <script src="https://cdn.tailwindcss.com"></script>
  <script>
    window.__RELAY_CONFIG__ = {
      appId: '${appId}',
      versionId: '${versionId}',
      apiBaseUrl: '${RUNTIME_API_BASE_URL}',
      appName: '${appName}'
    };
  </script>
</head>
<body>
  <div id="root"></div>
</body>
</html>`

  // Add generated files with path normalization for Sandpack
  // Process in order so that generated files OVERWRITE defaults
  for (const file of files) {
    let path = file.path
    const content = file.content || ''
    
    // Skip empty content
    if (!content.trim()) {
      continue
    }
    
    // Normalize paths - ensure they start with /
    if (!path.startsWith('/')) {
      path = '/' + path
    }
    
    // Backend stores code under src/*; Sandpack's react-ts template expects root files.
    // Map /src/... -> /... so we don't end up with two entrypoints (App.tsx and src/App.tsx).
    if (path.startsWith('/src/')) {
      path = path.replace(/^\/src\//, '/')
    }

    // Map common path patterns to Sandpack's expected structure
    // index.html or src/index.html → skip (we inject our own with runtime config)
    if (path === '/index.html' || path === '/src/index.html') {
      continue
    }
    
    // CRITICAL: Skip lib/dataStore.ts, lib/runtime.ts, and lib/mcpTools.ts - we provide these with proper config
    // AI-generated versions have bad fallbacks that cause CORS/URL issues
    if (path === '/lib/dataStore.ts' || path === '/lib/runtime.ts' || path === '/lib/mcpTools.ts') {
      console.log('[Sandpack] Skipping AI-generated', path, '- using built-in template with proper config')
      continue
    }
    
    // main.tsx → /index.tsx (Sandpack's expected entry point)
    if (path === '/main.tsx') {
      sandpackFiles['/index.tsx'] = content
      continue
    }
    
    // Keep /public as-is; everything else should live at root for this template.
    if (path.startsWith('/public/')) {
      sandpackFiles[path] = content
      continue
    }
    
    // This overwrites any default file at the same path
    sandpackFiles[path] = content
  }

  // Debug: log what files we're sending to Sandpack
  
  // Verify App.tsx was properly set
  if (sandpackFiles['/App.tsx']) {
    console.log('App.tsx content preview:', sandpackFiles['/App.tsx'].substring(0, 100))
  }

  return sandpackFiles
}

// Status indicator component
function PreviewStatus() {
  const { sandpack } = useSandpack()
  const { status } = sandpack
  const [showLoading, setShowLoading] = useState(false)
  const loadingTimeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null)

  const statusConfig: Record<string, { icon: typeof CheckCircle2; label: string; color: string; animate?: boolean }> = {
    idle: { icon: CheckCircle2, label: 'Ready', color: 'text-green-600' },
    running: { icon: Loader2, label: 'Loading', color: 'text-amber-600', animate: true },
    timeout: { icon: AlertCircle, label: 'Timeout', color: 'text-red-500' },
    done: { icon: CheckCircle2, label: 'Ready', color: 'text-green-600' },
  }

  useEffect(() => {
    if (status === 'running') {
      setShowLoading(true)
      if (loadingTimeoutRef.current) {
        clearTimeout(loadingTimeoutRef.current)
      }
      loadingTimeoutRef.current = setTimeout(() => {
        setShowLoading(false)
      }, 2000)
    } else {
      setShowLoading(false)
      if (loadingTimeoutRef.current) {
        clearTimeout(loadingTimeoutRef.current)
        loadingTimeoutRef.current = null
      }
    }

    return () => {
      if (loadingTimeoutRef.current) {
        clearTimeout(loadingTimeoutRef.current)
        loadingTimeoutRef.current = null
      }
    }
  }, [status])

  const displayStatus = showLoading ? 'running' : 'done'
  const config = statusConfig[displayStatus] || statusConfig.idle
  const Icon = config.icon

  return (
    <div className="flex items-center gap-1.5 text-xs">
      <Icon
        className={cn(
          'h-3.5 w-3.5',
          config.color,
          config.animate && 'animate-spin'
        )}
      />
      <span className={config.color}>{config.label}</span>
    </div>
  )
}

/**
 * Loading state types for unified overlay
 */
type LoadingPhase = 'generating' | 'bundling' | 'ready'

/**
 * Unified loading overlay with smooth transitions between phases
 * Prevents jarring flashes between "Generating" and "Loading preview" states
 */
function UnifiedLoadingOverlay({ 
  phase, 
  fileCount,
  onTransitionComplete 
}: { 
  phase: LoadingPhase
  fileCount: number
  onTransitionComplete?: () => void 
}) {
  const [displayPhase, setDisplayPhase] = useState<LoadingPhase>(phase)
  const [isVisible, setIsVisible] = useState(phase !== 'ready')
  const [hasAnimatedIn, setHasAnimatedIn] = useState(false)
  const phaseStartTimeRef = useRef(Date.now())
  const MIN_PHASE_DURATION = 400 // Minimum time to show each phase to prevent flashing
  
  // Mark as animated in after first render
  useEffect(() => {
    if (phase !== 'ready' && !hasAnimatedIn) {
      // Small delay to ensure the overlay is fully rendered before marking as animated
      const t = setTimeout(() => setHasAnimatedIn(true), 50)
      return () => clearTimeout(t)
    }
  }, [phase, hasAnimatedIn])
  
  // Handle phase transitions with minimum duration
  useEffect(() => {
    if (phase === displayPhase) return
    
    const elapsed = Date.now() - phaseStartTimeRef.current
    const remaining = Math.max(0, MIN_PHASE_DURATION - elapsed)
    
    // If transitioning to 'ready', we need to fade out
    if (phase === 'ready') {
      const fadeOutTimer = setTimeout(() => {
        setIsVisible(false)
        // Small delay before fully hiding to allow fade animation
        setTimeout(() => {
          setDisplayPhase('ready')
          setHasAnimatedIn(false)
          onTransitionComplete?.()
        }, 300)
      }, remaining)
      return () => clearTimeout(fadeOutTimer)
    }
    
    // For other transitions (generating → bundling), update after minimum duration
    const transitionTimer = setTimeout(() => {
      setDisplayPhase(phase)
      phaseStartTimeRef.current = Date.now()
    }, remaining)
    
    return () => clearTimeout(transitionTimer)
  }, [phase, displayPhase, onTransitionComplete])
  
  // Reset visibility when phase changes from ready to something else
  useEffect(() => {
    if (phase !== 'ready' && displayPhase === 'ready') {
      setDisplayPhase(phase)
      setIsVisible(true)
      phaseStartTimeRef.current = Date.now()
    }
  }, [phase, displayPhase])
  
  if (!isVisible && displayPhase === 'ready') return null
  
  const isGenerating = displayPhase === 'generating'
  
  return (
    <motion.div 
      className="absolute inset-0 z-30 flex items-center justify-center bg-gray-50"
      // Start fully visible to prevent flash-through of content beneath
      // Only animate opacity when fading OUT (isVisible becomes false)
      initial={false}
      animate={{ opacity: isVisible ? 1 : 0 }}
      transition={{ duration: 0.25, ease: 'easeOut' }}
    >
      <div className="text-center">
        {/* Clean minimal loader */}
        <div className="h-12 w-12 mx-auto mb-5 rounded-lg bg-white border border-gray-200 flex items-center justify-center">
          <Loader2 className="h-5 w-5 text-gray-500 animate-spin" />
        </div>
        
        {/* Text with crossfade */}
        <AnimatePresence mode="wait">
          <motion.div
            key={isGenerating ? 'generating' : 'bundling'}
            initial={hasAnimatedIn ? { opacity: 0 } : false}
            animate={{ opacity: 1 }}
            exit={{ opacity: 0 }}
            transition={{ duration: 0.15 }}
          >
            <h3 className="text-sm font-medium text-gray-900 mb-1">
              {isGenerating ? 'Building' : 'Loading preview'}
            </h3>
            <p className="text-xs text-gray-500">
              {isGenerating 
                ? 'Generating components...' 
                : 'Loading application...'}
            </p>
          </motion.div>
        </AnimatePresence>
        
        {/* File count - only show when generating */}
        {isGenerating && fileCount > 0 && (
          <div className="mt-4 inline-flex items-center gap-1.5 text-xs text-gray-500">
            <CheckCircle2 className="h-3 w-3 text-green-600" />
            <span>{fileCount} file{fileCount !== 1 ? 's' : ''}</span>
          </div>
        )}
      </div>
    </motion.div>
  )
}

/**
 * Reports bundler ready state to parent component
 * Lives inside SandpackProvider, calls onReady when bundling completes
 */
function BundlerReadyReporter({ onReady }: { onReady: (ready: boolean) => void }) {
  const { sandpack, listen } = useSandpack()
  
  useEffect(() => {
    // Listen for Sandpack messages to detect when bundling is done
    const unsubscribe = listen((message) => {
      const msg = message as { type: string }
      
      if (msg.type === 'start') {
        onReady(false)
      }
      
      // 'done' means bundling is complete and preview is ready
      if (msg.type === 'done') {
        onReady(true)
      }
    })
    
    return () => unsubscribe()
  }, [listen, onReady])
  
  // Also check sandpack status as backup
  useEffect(() => {
    if (sandpack.status === 'idle') {
      // Small delay to ensure iframe has rendered
      const t = setTimeout(() => onReady(true), 100)
      return () => clearTimeout(t)
    }
  }, [sandpack.status, onReady])
  
  return null
}


function AutoRunPreview({ filesKey }: { filesKey: string }) {
  const { sandpack } = useSandpack()

  useEffect(() => {
    sandpack.runSandpack()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [filesKey])

  return null
}

function AutoRunOnEdit() {
  const { sandpack } = useSandpack()
  const editKey = useMemo(
    () => hashSandpackRuntimeFiles(sandpack.files as Record<string, unknown>),
    [sandpack.files]
  )

  useEffect(() => {
    sandpack.runSandpack()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [editKey])

  return null
}

function AutoRunOnTab({ viewMode }: { viewMode: ViewMode }) {
  const { sandpack } = useSandpack()

  useEffect(() => {
    if (viewMode === 'code' || viewMode === 'split' || viewMode === 'preview') {
      sandpack.runSandpack()
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [viewMode])

  return null
}

/**
 * SandpackErrorHandler - Detects bundler errors and reports them to parent
 * 
 * This component listens for Sandpack bundler errors and reports them
 * via callback. The actual error fixing is handled by AgenticChatPanel.
 */
function SandpackErrorHandler({
  onBundlerErrors,
  enableAutoFix = false,
}: {
  onBundlerErrors?: (errors: BundlerError[]) => void
  /** Only report errors when enabled (after live generation, not when loading existing apps) */
  enableAutoFix?: boolean
}) {
  const { sandpack, listen } = useSandpack()
  const [errors, setErrors] = useState<BundlerError[]>([])
  const [hasInitialized, setHasInitialized] = useState(false)
  const lastReportedSignatureRef = useRef<string>('')
  
  // Create error signature for deduplication
  const createSignature = useCallback((errs: BundlerError[]) => {
    return errs
      .map(e => `${e.file || ''}:${e.line || 0}:${e.message}`)
      .sort()
      .join('|')
  }, [])
  
  // Listen for Sandpack errors
  useEffect(() => {
    const unsubscribe = listen((message) => {
      const msg = message as { type: string; action?: string; title?: string; message?: string; path?: string; line?: number; column?: number }
      
      // Detect errors
      if (msg.type === 'action' && msg.action === 'show-error') {
        const error: BundlerError = {
          title: msg.title || 'Error',
          message: msg.message || 'Unknown bundler error',
          file: msg.path,
          line: msg.line,
          column: msg.column,
        }
        
        setErrors(prev => {
          const exists = prev.some(e => 
            e.file === error.file && 
            e.line === error.line && 
            e.message === error.message
          )
          if (exists) return prev
          return [...prev, error]
        })
        
        setHasInitialized(true)
      }
      
      // Clear errors on successful compilation
      if (msg.type === 'done' || (msg as { type: string }).type === 'success') {
        setErrors([])
        lastReportedSignatureRef.current = ''
      }
      
      // Track bundler status
      if (msg.type === 'status') {
        const status = (msg as unknown as { status: string }).status
        if (status === 'idle' || status === 'done') {
          setHasInitialized(true)
        }
      }
      
      // Clear on compile start
      if (msg.type === 'start') {
        setErrors([])
      }
    })
    
    return () => unsubscribe()
  }, [listen])
  
  // Also check Sandpack's error state
  useEffect(() => {
    const sandpackError = sandpack.error
    if (sandpackError) {
      const error: BundlerError = {
        title: 'Bundler Error',
        message: sandpackError.message || 'Unknown error',
      }
      
      setErrors(prev => {
        const exists = prev.some(e => e.message === error.message)
        if (exists) return prev
        return [...prev, error]
      })
    }
  }, [sandpack.error])
  
  // Report errors to parent when detected (parent handles the fix)
  useEffect(() => {
    if (
      errors.length === 0 ||
      !hasInitialized ||
      !enableAutoFix
    ) {
      return
    }
    
    // Check if these are new errors (avoid reporting duplicates)
    const signature = createSignature(errors)
    if (signature === lastReportedSignatureRef.current) {
      return
    }
    
    // Debounce to ensure errors are stable before reporting
    const timeout = setTimeout(() => {
      lastReportedSignatureRef.current = signature
      console.log('[SandpackErrorHandler] Reporting bundler errors to parent:', errors)
      onBundlerErrors?.(errors)
    }, 1500)
    
    return () => clearTimeout(timeout)
  }, [errors, hasInitialized, enableAutoFix, createSignature, onBundlerErrors])
  
  return null
}

function FileListPanel() {
  const { sandpack } = useSandpack()
  const files = useMemo(() => Object.keys(sandpack.files || {}).sort(), [sandpack.files])
  const activeFile = sandpack.activeFile
  const openFiles = sandpack.visibleFiles || []

  return (
    <div className="w-56 border-r border-gray-200 bg-white flex-shrink-0 overflow-auto">
      <div className="px-3 py-2 text-[11px] font-semibold text-gray-700 uppercase tracking-wide border-b border-gray-200">
        Files
      </div>
      <div className="py-1">
        {files.map((path) => {
          const label = path.startsWith('/') ? path.slice(1) : path
          const isActive = activeFile === path
          return (
            <button
              key={path}
              onClick={() => {
                if (!openFiles.includes(path) && sandpack.openFile) {
                  sandpack.openFile(path)
                }
                sandpack.setActiveFile(path)
              }}
              className={cn(
                'w-full flex items-center gap-2 px-3 py-2 text-xs text-left transition-colors',
                isActive
                  ? 'bg-gray-100 text-gray-900 border-l-2 border-gray-900'
                  : 'text-gray-700 hover:bg-gray-50'
              )}
              title={label}
            >
              <FileText className="h-3.5 w-3.5 text-gray-400" />
              <span className="truncate">{label}</span>
            </button>
          )
        })}
      </div>
    </div>
  )
}

function EditorTabs() {
  const { sandpack } = useSandpack()
  const openFiles = sandpack.visibleFiles || Object.keys(sandpack.files || {})
  const activeFile = sandpack.activeFile
  const closeFile = sandpack.closeFile as ((path: string) => void) | undefined

  return (
    <div className="flex flex-wrap items-center gap-1 px-3 py-2 border-b border-gray-200 bg-white min-h-[38px]">
      {openFiles.map((path) => {
        const label = path.startsWith('/') ? path.slice(1) : path
        const isActive = activeFile === path
        return (
          <div
            key={path}
            className={cn(
              'inline-flex items-center gap-1.5 px-2.5 h-7 rounded-md text-[11px] border transition-colors max-w-[180px]',
              isActive
                ? 'border-gray-300 bg-white text-gray-900 shadow-sm'
                : 'border-transparent bg-gray-100 text-gray-600 hover:bg-gray-50'
            )}
          >
            <button
              onClick={() => sandpack.setActiveFile(path)}
              className="flex items-center gap-1.5"
              title={label}
            >
              <span className="truncate max-w-[140px]">{label}</span>
            </button>
            {openFiles.length > 1 && closeFile && (
              <button
                onClick={(e) => {
                  e.stopPropagation()
                  closeFile(path)
                }}
                className="text-gray-400 hover:text-gray-700 transition-colors"
                title="Close file"
              >
                <X className="h-3.5 w-3.5" />
              </button>
            )}
          </div>
        )
      })}
    </div>
  )
}

// Normalize Sandpack files into backend payload
function toBackendFiles(
  sandpackFiles: Record<string, { code: string }>
): { path: string; content: string }[] {
  return Object.entries(sandpackFiles)
    .map(([path, file]) => ({
      path: path.startsWith('/') ? path.slice(1) : path,
      content: file.code || '',
    }))
    .filter((f) => {
      if (!f.content.trim()) return false
      if (f.path === 'public/index.html') return false
      if (f.path === 'package.json' || f.path === 'tsconfig.json') return false
      return /\.(tsx|ts|css|json)$/.test(f.path)
    })
    .map((f) => ({
      path: f.path.startsWith('src/') ? f.path : `src/${f.path}`,
      content: f.content,
    }))
}

// Autosave edits to backend with debounce
function AutoSave({
  versionId,
  resetKey,
  viewMode,
  onPersistLocalFiles,
  onFilesChange,
}: {
  versionId: string
  resetKey?: string
  viewMode: ViewMode
  onPersistLocalFiles: (files: Record<string, { code: string }>) => void
  onFilesChange?: (files: FileChange[]) => void
}) {
  const { sandpack } = useSandpack()
  const [status, setStatus] = useState<'idle' | 'pending' | 'saving' | 'saved' | 'error'>('idle')
  const [errorMessage, setErrorMessage] = useState<string | null>(null)
  const saveTimeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null)
  const lastSavedHashRef = useRef<string>('')
  const hasUserEditRef = useRef(false)
  const hydratedRef = useRef(false)
  const allowHashMarkRef = useRef(false)
  const lastModeChangeRef = useRef<number>(Date.now())
  const TAB_SWITCH_COOLDOWN_MS = 800

  const currentHash = useMemo(
    () => hashSandpackRuntimeFiles(sandpack.files as Record<string, unknown>),
    [sandpack.files]
  )

  // Reset tracking when switching versions or when parent signals a new baseline (e.g., new files set)
  useEffect(() => {
    lastSavedHashRef.current = hashSandpackRuntimeFiles(
      sandpack.files as Record<string, unknown>
    )
    hasUserEditRef.current = false
    hydratedRef.current = false
    allowHashMarkRef.current = viewMode === 'code' || viewMode === 'split'
    lastModeChangeRef.current = Date.now()
    setStatus('idle')
    setErrorMessage(null)
    if (saveTimeoutRef.current) {
      clearTimeout(saveTimeoutRef.current)
      saveTimeoutRef.current = null
    }
  }, [versionId, resetKey])

  // Track explicit user edits from Sandpack events so we only save when the user types
  useEffect(() => {
    const sp = sandpack as unknown as { listen?: (fn: (message: { type?: string }) => void) => (() => void) | void }
    const unsubscribe = sp.listen?.((message: { type?: string }) => {
      if (message.type === 'file/change') {
        hasUserEditRef.current = true
        allowHashMarkRef.current = true
      }
    })

    return () => {
      if (typeof unsubscribe === 'function') {
        unsubscribe()
      }
    }
  }, [sandpack])

  // Treat first hash after reset as hydration only; afterwards rely on explicit file/change events.
  useEffect(() => {
    if (!versionId) return
    if (!hydratedRef.current) {
      hydratedRef.current = true
    }
  }, [currentHash, versionId])

  // Enable hash-based detection when entering an editing view (code/split); disable in preview.
  useEffect(() => {
    allowHashMarkRef.current = viewMode === 'code' || viewMode === 'split'
    lastModeChangeRef.current = Date.now()
  }, [viewMode])

  // Fallback: if hash diverges while in an editing view, mark as user edit.
  useEffect(() => {
    if (!versionId) return
    if (!hydratedRef.current) return
    if (viewMode === 'preview') return
    if (!allowHashMarkRef.current) return
    const sinceModeChange = Date.now() - lastModeChangeRef.current
    if (sinceModeChange < TAB_SWITCH_COOLDOWN_MS) return // ignore hash churn right after switching tabs
    if (currentHash !== lastSavedHashRef.current) {
      hasUserEditRef.current = true
    }
  }, [currentHash, viewMode, versionId, lastSavedHashRef.current])

  useEffect(() => {
    if (!versionId) return
    if (currentHash === lastSavedHashRef.current) return
    if (!hasUserEditRef.current) return

    if (saveTimeoutRef.current) {
      clearTimeout(saveTimeoutRef.current)
    }

    setStatus('pending')

    saveTimeoutRef.current = setTimeout(async () => {
      setStatus('saving')
      try {
        // If the hash changed while we were waiting, skip this run and let the next effect handle it
        const liveHash = hashSandpackRuntimeFiles(sandpack.files as Record<string, unknown>)
        if (liveHash !== currentHash) {
          setStatus('idle')
          return
        }

        const backendFiles = toBackendFiles(
          sandpack.files as Record<string, { code: string }>
        )

        if (backendFiles.length === 0) {
          lastSavedHashRef.current = currentHash
          hasUserEditRef.current = false
          setStatus('saved')
          setTimeout(() => setStatus('idle'), 1200)
          return
        }

        await versionsApi.saveFiles(versionId, backendFiles)
        // Persist the current sandpack files locally so rehydration uses the saved snapshot
        onPersistLocalFiles(sandpack.files as Record<string, { code: string }>)
        if (onFilesChange) {
          const updated: FileChange[] = backendFiles.map((f) => ({
            path: f.path,
            content: f.content,
            action: 'modify',
            language: f.path.endsWith('.css')
              ? 'css'
              : f.path.endsWith('.json')
                ? 'json'
                : f.path.endsWith('.ts') && !f.path.endsWith('.tsx')
                  ? 'ts'
                  : 'tsx',
          }))
          onFilesChange(updated)
        }
        lastSavedHashRef.current = currentHash
        hasUserEditRef.current = false
        setStatus('saved')
        setErrorMessage(null)
        setTimeout(() => setStatus('idle'), 1200)
      } catch (error) {
        console.error('Autosave error:', error)
        setStatus('error')
        setErrorMessage((error as Error).message)
      }
    }, 1200)

    return () => {
      if (saveTimeoutRef.current) {
        clearTimeout(saveTimeoutRef.current)
        saveTimeoutRef.current = null
      }
    }
  }, [currentHash, versionId, sandpack.files])

  const labelByStatus: Record<typeof status, { text: string; className: string }> = {
    idle: { text: '', className: 'text-gray-500' },
    pending: { text: 'Pending...', className: 'text-amber-600' },
    saving: { text: 'Saving…', className: 'text-amber-600' },
    saved: { text: 'Saved', className: 'text-green-700' },
    error: { text: 'Save failed', className: 'text-red-600' },
  }

  const { text, className } = labelByStatus[status]

  return (
    <div className={cn('text-xs', className)} title={errorMessage || text}>
      {status === 'saving' ? (
        <span className="inline-flex items-center gap-1">
          <Loader2 className="h-3 w-3 animate-spin" />
          {text}
        </span>
      ) : status === 'error' ? (
        <span className="inline-flex items-center gap-1">
          <AlertCircle className="h-3.5 w-3.5" />
          {text}
        </span>
      ) : status === 'saved' ? (
        <span className="inline-flex items-center gap-1">
          <CheckCircle2 className="h-3.5 w-3.5" />
          {text}
        </span>
      ) : (
        text
      )}
    </div>
  )
}

// Main SandpackPreview component
export function SandpackPreview({
  files,
  appId,
  versionId = '',
  appName = 'App',
  className = '',
  hideToolbar = false,
  onFilesChange,
  showVersionsSidebar = false,
  onToggleVersionsSidebar,
  onBundlerErrors,
  enableAutoFix = false,
  isStreaming = false,
}: SandpackPreviewProps) {
  const [viewMode, setViewMode] = useState<ViewMode>('preview')
  const [showConsole, setShowConsole] = useState(false)
  
  // Track when Sandpack is ready (used for overlay outside SandpackProvider)
  const [isBundlerReady, setIsBundlerReady] = useState(false)

  // OPTIMIZATION: Defer file updates to Sandpack during streaming
  // This prevents constant re-bundling of incomplete code during generation
  // We only update Sandpack files when:
  // 1. Streaming completes (isStreaming goes from true to false)
  // 2. A new version is selected manually (not streaming)
  const [committedFiles, setCommittedFiles] = useState<FileChange[]>(() => 
    isStreaming ? [] : files
  )
  const wasStreamingRef = useRef(isStreaming)
  
  // Commit files to Sandpack only when streaming completes or on initial load
  useEffect(() => {
    const wasStreaming = wasStreamingRef.current
    wasStreamingRef.current = isStreaming
    
    // If streaming just completed, commit the final files
    if (wasStreaming && !isStreaming) {
      setCommittedFiles(files)
      return
    }
    
    // If not streaming (e.g., loading a saved version), update immediately
    if (!isStreaming) {
      setCommittedFiles(files)
    }
    // When streaming, we intentionally do NOT update committedFiles
    // This keeps the previous preview (or generating overlay) stable
  }, [files, isStreaming])
  
  const filesKey = useMemo(() => hashFiles(committedFiles), [committedFiles])
  
  // Use total character count for sandpack key to detect content changes
  // (file count alone missed content-only updates, causing stale previews)
  const getTotalChars = (f: FileChange[]) => f.reduce((sum, file) => sum + (file.content?.length || 0), 0)
  const [sandpackKey, setSandpackKey] = useState(() => `${versionId}-${getTotalChars(committedFiles)}`)
  const [initializedFiles, setInitializedFiles] = useState<Record<string, string>>({})
  // Persist saved files without forcing Sandpack to remount (which clears tabs)
  const persistedFilesRef = useRef<Record<string, string>>({})
  
  // Convert committed files to Sandpack format - only updates when committed files change
  const sandpackFiles = useMemo(() => {
    const converted = convertToSandpackFiles(committedFiles, appId, versionId, appName)
    return converted
  }, [committedFiles, appId, versionId, appName])
  
  // Reset Sandpack when version changes or file content changes
  // This preserves user edits when just switching tabs, but ensures new version loads after generation
  useEffect(() => {
    const newKey = `${versionId}-${getTotalChars(committedFiles)}`
    if (newKey !== sandpackKey) {
      setSandpackKey(newKey)
      setInitializedFiles(sandpackFiles)
      // Reset persisted snapshot when the generation baseline changes
      persistedFilesRef.current = {}
      // Immediately show loading overlay when switching (before Sandpack remounts)
      setIsBundlerReady(false)
    }
  }, [versionId, committedFiles, sandpackFiles, sandpackKey])

  // Keep initializedFiles in sync after a successful autosave so rehydration uses the just-saved snapshot
  const handlePersistLocalFiles = useCallback(
    (filesRecord: Record<string, { code: string }>) => {
      const flattened: Record<string, string> = {}
      Object.entries(filesRecord).forEach(([path, file]) => {
        flattened[path] = file.code || ''
      })
      // Store the snapshot without triggering a Sandpack remount; remounting clears open tabs/active file.
      persistedFilesRef.current = flattened
    },
    []
  )
  
  // Use initializedFiles if set, otherwise use sandpackFiles
  const filesToUse = Object.keys(initializedFiles).length > 0 ? initializedFiles : sandpackFiles
  const defaultEntryFile = useMemo(() => {
    if (filesToUse['/App.tsx']) return '/App.tsx'
    const keys = Object.keys(filesToUse)
    return keys[0] || '/App.tsx'
  }, [filesToUse])

  // PostMessage bridge: Listen for API requests from Sandpack iframe and proxy them
  // This bypasses CORS/Private Network Access restrictions
  useEffect(() => {
    const handleMessage = async (event: MessageEvent) => {
      if (!event.data || !event.data.type) {
        return
      }

      // Handle DATASTORE_REQUEST messages
      if (event.data.type === 'DATASTORE_REQUEST') {
        const { requestId, appId: reqAppId, versionId: reqVersionId, operation, tableSlug, params } = event.data

        try {
          const url = `${RUNTIME_API_BASE_URL}/runtime/data/`

          // Include auth token from parent window's localStorage
          const token = localStorage.getItem('access_token')
          const headers: Record<string, string> = { 'Content-Type': 'application/json' }
          if (token) {
            headers['Authorization'] = `Bearer ${token}`
          }

          const response = await fetch(url, {
            method: 'POST',
            headers,
            body: JSON.stringify({
              appId: reqAppId || appId,
              versionId: reqVersionId || versionId,
              operation,
              tableSlug,
              params,
            }),
          })

          const data = await response.json()

          event.source?.postMessage({
            type: 'DATASTORE_RESPONSE',
            requestId,
            success: response.ok,
            data: response.ok ? data : undefined,
            error: !response.ok ? (data.error || `API error: ${response.status}`) : undefined,
          }, { targetOrigin: '*' })
        } catch (error) {
          event.source?.postMessage({
            type: 'DATASTORE_RESPONSE',
            requestId,
            success: false,
            error: error instanceof Error ? error.message : 'Unknown error',
          }, { targetOrigin: '*' })
        }
        return
      }

      // Handle MCP_REQUEST messages (for connector/integration tools)
      if (event.data.type === 'MCP_REQUEST') {
        const { requestId, appId: reqAppId, toolName, params } = event.data

        try {
          const url = `${RUNTIME_API_BASE_URL}/runtime/connectors/`

          // Include auth token from parent window's localStorage
          const mcpToken = localStorage.getItem('access_token')
          const mcpHeaders: Record<string, string> = { 'Content-Type': 'application/json' }
          if (mcpToken) {
            mcpHeaders['Authorization'] = `Bearer ${mcpToken}`
          }

          const response = await fetch(url, {
            method: 'POST',
            headers: mcpHeaders,
            body: JSON.stringify({
              appId: reqAppId || appId,
              connectorId: '_meta',
              toolId: 'mcp_call',
              params: {
                tool_name: toolName,
                arguments: params || {},
              },
            }),
          })

          const data = await response.json()

          event.source?.postMessage({
            type: 'MCP_RESPONSE',
            requestId,
            success: response.ok && data.success !== false,
            data: response.ok ? data : undefined,
            error: !response.ok || data.success === false ? (data.error || `API error: ${response.status}`) : undefined,
          }, { targetOrigin: '*' })
        } catch (error) {
          event.source?.postMessage({
            type: 'MCP_RESPONSE',
            requestId,
            success: false,
            error: error instanceof Error ? error.message : 'Unknown error',
          }, { targetOrigin: '*' })
        }
        return
      }

      // Handle MCP_LIST_TOOLS messages
      if (event.data.type === 'MCP_LIST_TOOLS') {
        const { requestId, appId: reqAppId } = event.data

        try {
          const url = `${RUNTIME_API_BASE_URL}/runtime/connectors/`
          const response = await fetch(url, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
              appId: reqAppId || appId,
              connectorId: '_meta',
              toolId: 'mcp_tools',
            }),
          })

          const data = await response.json()
          event.source?.postMessage({
            type: 'MCP_RESPONSE',
            requestId,
            success: response.ok,
            data: response.ok ? data : undefined,
            error: !response.ok ? (data.error || `API error: ${response.status}`) : undefined,
          }, { targetOrigin: '*' })
        } catch (error) {
          event.source?.postMessage({
            type: 'MCP_RESPONSE',
            requestId,
            success: false,
            error: error instanceof Error ? error.message : 'Unknown error',
          }, { targetOrigin: '*' })
        }
        return
      }
    }

    window.addEventListener('message', handleMessage)

    return () => {
      window.removeEventListener('message', handleMessage)
    }
  }, [appId, versionId])

  return (
    <div
      className={cn(
        'sandpack-fullheight flex flex-col h-full bg-gray-100 overflow-hidden',
        className
      )}
    >
      <SandpackProvider
        key={sandpackKey}
        template="react-ts"
        files={filesToUse}
        customSetup={{
          dependencies: {
            'lucide-react': 'latest',
            'framer-motion': 'latest',
          },
        }}
        options={{
          externalResources: ['https://cdn.tailwindcss.com'],
          recompileMode: 'immediate',
          recompileDelay: 0,
          visibleFiles: [defaultEntryFile],
          activeFile: defaultEntryFile,
        }}
        theme="light"
      >
        {/* Report bundler ready state to parent (for overlay outside SandpackProvider) */}
        <BundlerReadyReporter onReady={setIsBundlerReady} />
        {/* Auto-run preview - files are already deferred during streaming so this is safe */}
        <AutoRunPreview filesKey={filesKey} />
        <AutoRunOnEdit />
        <AutoRunOnTab viewMode={viewMode} />
        {versionId && (
          <SandpackErrorHandler
            onBundlerErrors={onBundlerErrors}
            enableAutoFix={enableAutoFix}
          />
        )}
        {/* Flex container - uses absolute positioning to ensure footer stays pinned */}
        <div className="relative flex flex-col h-full min-h-0">
        {/* Header - fixed at top (optional) */}
        {!hideToolbar && (
          <div className="flex-shrink-0 flex items-center justify-between px-4 py-3 border-b border-gray-200 bg-white">
            <div className="flex items-center gap-3">
              {/* View mode toggle */}
              <div className="flex items-center gap-1 bg-gray-100 rounded-md p-1">
                {[
                  { key: 'preview' as ViewMode, icon: Eye, label: 'Preview' },
                  { key: 'code' as ViewMode, icon: Code2, label: 'Code' },
                  { key: 'split' as ViewMode, icon: Monitor, label: 'Split' },
                ].map(({ key, icon: Icon, label }) => (
                  <button
                    key={key}
                    onClick={() => setViewMode(key)}
                    className={cn(
                      'flex items-center gap-1.5 px-2.5 py-1.5 rounded text-xs font-medium transition-colors',
                      viewMode === key
                        ? 'bg-white shadow-sm text-gray-900'
                        : 'text-gray-500 hover:text-gray-700'
                    )}
                  >
                    <Icon className="h-3.5 w-3.5" />
                    {label}
                  </button>
                ))}
              </div>

              <PreviewStatus />
            </div>

            <div className="flex items-center gap-3">
              <AutoSave
                versionId={versionId}
                resetKey={filesKey}
                viewMode={viewMode}
                onPersistLocalFiles={handlePersistLocalFiles}
                onFilesChange={onFilesChange}
              />
              <div className="flex items-center gap-1">
              <button
                onClick={() => setShowConsole(!showConsole)}
                className={cn(
                  'p-1.5 rounded-md transition-colors',
                  showConsole
                    ? 'bg-gray-100 text-gray-900'
                    : 'text-gray-400 hover:text-gray-600 hover:bg-gray-100'
                )}
                title="Toggle console"
              >
                <Terminal className="h-4 w-4" />
              </button>

              {onToggleVersionsSidebar && (
                <button
                  onClick={onToggleVersionsSidebar}
                  className={cn(
                    'p-1.5 rounded-md transition-colors',
                    showVersionsSidebar
                      ? 'bg-gray-100 text-gray-900'
                      : 'text-gray-400 hover:text-gray-600 hover:bg-gray-100'
                  )}
                  title={showVersionsSidebar ? 'Hide version history' : 'Show version history'}
                >
                  <History className="h-4 w-4" />
                </button>
              )}
              </div>
            </div>
          </div>
        )}

        {/* Content - fills remaining space (Sandpack editor needs explicit full-height wrappers) */}
        <div className="flex-1 min-h-0 overflow-hidden">
          {viewMode === 'preview' && (
            <div className="relative h-full min-h-0 overflow-auto">
              {/* Unified loading overlay - smooth transitions between generating and bundling states */}
              <UnifiedLoadingOverlay 
                phase={isStreaming ? 'generating' : !isBundlerReady ? 'bundling' : 'ready'} 
                fileCount={files.length} 
              />
              {/* Preview pane - always rendered for faster load, overlay covers it while loading */}
              <SandpackPreviewPane
                className="h-full"
                showNavigator={false}
                showRefreshButton={false}
                showOpenInCodeSandbox={false}
                style={{
                  height: '100%',
                  width: '100%',
                }}
              />
            </div>
          )}

          {viewMode === 'code' && (
            <div className="flex h-full min-h-0">
              <FileListPanel />
              <div className="flex-1 min-h-0 flex flex-col overflow-hidden">
                <EditorTabs />
                <div className="flex-1 min-h-0 overflow-auto">
                  <SandpackCodeEditor
                    className="h-full"
                    showTabs={false}
                    showLineNumbers
                    showInlineErrors
                    wrapContent={false}
                    style={{ height: '100%', minHeight: '100%', width: '100%' }}
                  />
                </div>
              </div>
            </div>
          )}

          {viewMode === 'split' && (
            <div className="flex h-full w-full min-h-0">
              <div className="w-1/2 h-full min-h-0 border-r border-gray-200 flex flex-col overflow-hidden">
                <div className="flex-1 min-h-0 overflow-hidden flex">
                  <FileListPanel />
                  <div className="flex-1 min-h-0 flex flex-col overflow-hidden">
                    <EditorTabs />
                    <div className="flex-1 min-h-0 overflow-auto">
                      <SandpackCodeEditor
                        className="h-full"
                        showTabs={false}
                        showLineNumbers
                        showInlineErrors
                        wrapContent={false}
                        style={{ height: '100%', minHeight: '100%', width: '100%' }}
                      />
                    </div>
                  </div>
                </div>
              </div>
              <div className="w-1/2 h-full min-h-0 flex flex-col overflow-hidden">
                <div className="relative flex-1 min-h-0 overflow-auto">
                  {/* Unified loading overlay - smooth transitions between generating and bundling states */}
                  <UnifiedLoadingOverlay 
                    phase={isStreaming ? 'generating' : !isBundlerReady ? 'bundling' : 'ready'} 
                    fileCount={files.length} 
                  />
                  {/* Preview pane - always rendered for faster load, overlay covers it while loading */}
                  <SandpackPreviewPane
                    className="h-full"
                    showNavigator={false}
                    showRefreshButton={false}
                    showOpenInCodeSandbox={false}
                    style={{
                      height: '100%',
                      width: '100%',
                    }}
                  />
                </div>
              </div>
            </div>
          )}
        </div>

        {/* Bottom section - always pinned to bottom of viewport */}
        <div className="flex-shrink-0 mt-auto">
          {/* Console - pinned above footer */}
          <AnimatePresence>
            {showConsole && (
              <motion.div
                initial={{ height: 0 }}
                animate={{ height: 200 }}
                exit={{ height: 0 }}
                className="border-t border-gray-200 overflow-hidden"
              >
                <SandpackConsole
                  showHeader
                  style={{ height: '100%' }}
                />
              </motion.div>
            )}
          </AnimatePresence>

          {/* Status bar - pinned to bottom */}
          <div className="flex items-center justify-between px-4 py-2 border-t border-gray-200 bg-white
                        text-[10px] text-gray-500">
            <span>{files.length} files {isStreaming ? 'generating...' : 'generated'}</span>
            <span className="flex items-center gap-1">
              {isStreaming ? (
                <>
                  <Loader2 className="h-2.5 w-2.5 animate-spin" />
                  Generating
                </>
              ) : (
                <>
                  <Eye className="h-2.5 w-2.5" />
                  Live Preview
                </>
              )}
            </span>
          </div>
        </div>
        </div>
      </SandpackProvider>
    </div>
  )
}

// Lightweight preview for when Sandpack isn't needed
export function SimplePreview({
  previewUrl,
  className = '',
  showOpenInNewTab = true,
}: {
  previewUrl: string
  className?: string
  showOpenInNewTab?: boolean
}) {
  const [isLoading, setIsLoading] = useState(true)

  return (
    <div className={cn('flex flex-col h-full bg-gray-100', className)}>
      <div className="flex-1 relative">
        <iframe
          src={previewUrl}
          className="w-full h-full border-0"
          onLoad={() => setIsLoading(false)}
          title="App Preview"
        />

        {showOpenInNewTab && (
          <a
            href={previewUrl}
            target="_blank"
            rel="noopener noreferrer"
            className="absolute top-3 right-3 z-10 p-2 rounded-md bg-white/90 text-gray-500 hover:text-gray-700 shadow-sm border border-gray-200 transition-colors"
            title="Open in new tab"
          >
            <ExternalLink className="h-4 w-4" />
          </a>
        )}

        {isLoading && (
          <div className="absolute inset-0 bg-white/80 flex items-center justify-center">
            <Loader2 className="h-8 w-8 text-gray-700 animate-spin" />
          </div>
        )}
      </div>
    </div>
  )
}

