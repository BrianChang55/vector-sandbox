"""
Enhanced Code Generation Service

Production-ready code generation with:
- Multi-file generation with proper dependencies
- Component library generation
- TypeScript type safety
- Tailwind styling with dark/light themes
- Error boundaries and loading states
"""
import json
import hashlib
from typing import Dict, Any, List, Optional
from dataclasses import dataclass

from ..models import AppVersion, VersionFile


@dataclass
class GeneratedFile:
    """A generated code file."""
    path: str
    content: str
    description: str


class EnhancedCodegenService:
    """
    Enhanced code generation service for production-ready internal apps.
    """
    
    # Expanded allowlisted file paths for V2
    ALLOWLISTED_PATHS = [
        # Core app files
        'src/app/page.tsx',
        'src/app/layout.tsx',
        'src/app/globals.css',
        
        # Components
        'src/components/TableView.tsx',
        'src/components/DetailDrawer.tsx',
        'src/components/DataGrid.tsx',
        'src/components/FormBuilder.tsx',
        'src/components/Dashboard.tsx',
        'src/components/Kanban.tsx',
        'src/components/Charts.tsx',
        'src/components/Filters.tsx',
        'src/components/Pagination.tsx',
        'src/components/SearchBar.tsx',
        'src/components/ActionButtons.tsx',
        'src/components/StatusBadge.tsx',
        'src/components/UserAvatar.tsx',
        'src/components/EmptyState.tsx',
        'src/components/LoadingState.tsx',
        'src/components/ErrorBoundary.tsx',
        
        # UI primitives
        'src/components/ui/button.tsx',
        'src/components/ui/input.tsx',
        'src/components/ui/select.tsx',
        'src/components/ui/dialog.tsx',
        'src/components/ui/dropdown.tsx',
        'src/components/ui/table.tsx',
        'src/components/ui/card.tsx',
        'src/components/ui/badge.tsx',
        'src/components/ui/avatar.tsx',
        'src/components/ui/toast.tsx',
        
        # Lib/utilities
        'src/lib/runtimeClient.ts',
        'src/lib/types.ts',
        'src/lib/utils.ts',
        'src/lib/hooks.ts',
        'src/lib/constants.ts',
        
        # Hooks
        'src/hooks/useData.ts',
        'src/hooks/useActions.ts',
        'src/hooks/useFilters.ts',
        'src/hooks/usePagination.ts',
    ]
    
    @staticmethod
    def generate_complete_app(app_version: AppVersion) -> List[VersionFile]:
        """
        Generate a complete, production-ready internal app.
        
        Args:
            app_version: AppVersion instance with spec_json
            
        Returns:
            List of VersionFile instances
        """
        spec_json = app_version.spec_json
        files = []
        
        # 1. Runtime client (API layer)
        files.append(EnhancedCodegenService._generate_runtime_client())
        
        # 2. TypeScript types
        files.append(EnhancedCodegenService._generate_types(spec_json))
        
        # 3. Utility functions
        files.append(EnhancedCodegenService._generate_utils())
        
        # 4. Custom hooks
        files.append(EnhancedCodegenService._generate_hooks())
        
        # 5. UI Components
        files.extend(EnhancedCodegenService._generate_ui_components())
        
        # 6. Main components based on spec
        files.extend(EnhancedCodegenService._generate_app_components(spec_json))
        
        # 7. Main page
        files.append(EnhancedCodegenService._generate_main_page(spec_json))
        
        # 8. Layout
        files.append(EnhancedCodegenService._generate_layout(spec_json))
        
        # 9. Global styles
        files.append(EnhancedCodegenService._generate_global_styles())
        
        # Save all files
        version_files = []
        for gen_file in files:
            content_hash = hashlib.md5(gen_file.content.encode()).hexdigest()
            vf = VersionFile(
                app_version=app_version,
                path=gen_file.path,
                content=gen_file.content,
            )
            vf.save()
            version_files.append(vf)
        
        return version_files
    
    @staticmethod
    def _generate_runtime_client() -> GeneratedFile:
        """Generate the runtime API client."""
        content = '''/**
 * Runtime Client for Internal Apps
 * 
 * All data operations go through this client, which proxies to the Vector backend.
 * This ensures RLS is enforced and service keys are never exposed.
 */

const API_BASE_URL = typeof window !== 'undefined' 
  ? window.location.origin + '/api/v1' 
  : '/api/v1';

// Types
export interface QuerySpec {
  select?: string[];
  filters?: Array<{ field: string; op: string; value: any }>;
  orderBy?: Array<{ field: string; dir: 'asc' | 'desc' }>;
  limit?: number;
  offset?: number;
  search?: string;
}

export interface QueryResult<T = any> {
  data: T[];
  count: number;
  hasMore: boolean;
}

export interface ActionResult<T = any> {
  success: boolean;
  data?: T;
  error?: string;
}

export interface RuntimeConfig {
  appId: string;
  versionId: string;
}

// Get runtime config from window
function getConfig(): RuntimeConfig {
  if (typeof window !== 'undefined' && (window as any).__VECTOR_CONFIG__) {
    return (window as any).__VECTOR_CONFIG__;
  }
  return { appId: '', versionId: '' };
}

// API functions
export async function runtimeQuery<T = any>(params: {
  resourceId: string;
  querySpec: QuerySpec;
}): Promise<QueryResult<T>> {
  const config = getConfig();
  
  try {
    const response = await fetch(`${API_BASE_URL}/runtime/query/`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      credentials: 'include',
      body: JSON.stringify({
        appId: config.appId,
        versionId: config.versionId,
        resourceId: params.resourceId,
        querySpec: params.querySpec,
      }),
    });
    
    if (!response.ok) {
      throw new Error(`Query failed: ${response.statusText}`);
    }
    
    const result = await response.json();
    return {
      data: result.data || [],
      count: result.count || 0,
      hasMore: (result.count || 0) > (params.querySpec.offset || 0) + (result.data?.length || 0),
    };
  } catch (error) {
    console.error('Runtime query error:', error);
    return { data: [], count: 0, hasMore: false };
  }
}

export async function runtimeAction<T = any>(params: {
  actionId: string;
  args: Record<string, any>;
}): Promise<ActionResult<T>> {
  const config = getConfig();
  
  try {
    const response = await fetch(`${API_BASE_URL}/runtime/action/`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      credentials: 'include',
      body: JSON.stringify({
        appId: config.appId,
        versionId: config.versionId,
        actionId: params.actionId,
        args: params.args,
      }),
    });
    
    if (!response.ok) {
      const error = await response.json().catch(() => ({}));
      return { success: false, error: error.message || response.statusText };
    }
    
    const result = await response.json();
    return { success: true, data: result.data };
  } catch (error) {
    console.error('Runtime action error:', error);
    return { success: false, error: String(error) };
  }
}

// Convenience hooks-friendly function
export function createResourceFetcher(resourceId: string) {
  return async (querySpec: QuerySpec) => runtimeQuery({ resourceId, querySpec });
}
'''
        return GeneratedFile(
            path='src/lib/runtimeClient.ts',
            content=content,
            description='Runtime API client for data operations'
        )
    
    @staticmethod
    def _generate_types(spec_json: Dict[str, Any]) -> GeneratedFile:
        """Generate TypeScript types from spec."""
        app_name = spec_json.get('appName', 'App')
        pages = spec_json.get('pages', [])
        
        # Generate types for each resource
        type_defs = []
        for page in pages:
            resource_id = page.get('primaryResource', '')
            if resource_id:
                type_name = resource_id.split('.')[-1].title().replace('_', '')
                columns = page.get('view', {}).get('table', {}).get('columns', [])
                
                fields = []
                for col in columns:
                    field = col.get('field', '')
                    field_type = 'string'  # Default to string
                    if col.get('type') == 'number':
                        field_type = 'number'
                    elif col.get('type') == 'boolean':
                        field_type = 'boolean'
                    elif col.get('type') == 'date':
                        field_type = 'string'  # ISO date string
                    
                    fields.append(f'  {field}: {field_type};')
                
                type_defs.append(f'''export interface {type_name} {{
{chr(10).join(fields) if fields else '  id: string;'}
}}''')
        
        content = f'''/**
 * Type definitions for {app_name}
 * 
 * Auto-generated from AppSpec. Do not edit directly.
 */

// App Spec Types
export interface AppSpec {{
  appName: string;
  pages: PageSpec[];
}}

export interface PageSpec {{
  id: string;
  title: string;
  layout: 'table_detail_drawer' | 'tabbed_views' | 'dashboard' | 'form' | 'kanban';
  primaryResource: string;
  view: ViewSpec;
}}

export interface ViewSpec {{
  table?: TableSpec;
  detailDrawer?: DetailDrawerSpec;
  stats?: StatSpec[];
}}

export interface TableSpec {{
  columns: ColumnSpec[];
  filterableFields?: string[];
  searchableFields?: string[];
  sort?: {{ field: string; dir: 'asc' | 'desc' }};
  pagination?: {{ pageSize: number }};
  rowActions?: ActionSpec[];
  bulkActions?: ActionSpec[];
}}

export interface ColumnSpec {{
  field: string;
  label?: string;
  type?: 'text' | 'number' | 'date' | 'badge' | 'avatar' | 'boolean';
  sortable?: boolean;
  width?: string;
}}

export interface DetailDrawerSpec {{
  titleField?: string;
  sections?: SectionSpec[];
  fields?: FieldSpec[];
  actions?: ActionSpec[];
}}

export interface SectionSpec {{
  title: string;
  fields: FieldSpec[];
}}

export interface FieldSpec {{
  field: string;
  label?: string;
  type?: 'text' | 'textarea' | 'select' | 'date' | 'number' | 'boolean';
  readOnly?: boolean;
  options?: {{ value: string; label: string }}[];
}}

export interface ActionSpec {{
  label: string;
  actionId: string;
  confirm?: boolean;
  variant?: 'default' | 'destructive' | 'outline';
}}

export interface StatSpec {{
  label: string;
  value: string;
  change?: string;
  trend?: 'up' | 'down' | 'neutral';
}}

// Resource Types
{chr(10).join(type_defs) if type_defs else '// No resources defined yet'}

// Utility Types
export type FilterOperator = 'eq' | 'neq' | 'gt' | 'gte' | 'lt' | 'lte' | 'like' | 'ilike' | 'in' | 'is';

export interface Filter {{
  field: string;
  op: FilterOperator;
  value: any;
}}

export interface SortOrder {{
  field: string;
  dir: 'asc' | 'desc';
}}
'''
        return GeneratedFile(
            path='src/lib/types.ts',
            content=content,
            description='TypeScript type definitions'
        )
    
    @staticmethod
    def _generate_utils() -> GeneratedFile:
        """Generate utility functions."""
        content = '''/**
 * Utility functions for Internal Apps
 */

import { clsx, type ClassValue } from 'clsx';

// Tailwind class merger
export function cn(...inputs: ClassValue[]): string {
  return clsx(inputs);
}

// Format date for display
export function formatDate(date: string | Date, options?: Intl.DateTimeFormatOptions): string {
  const d = typeof date === 'string' ? new Date(date) : date;
  return d.toLocaleDateString('en-US', options || {
    year: 'numeric',
    month: 'short',
    day: 'numeric',
  });
}

// Format relative time
export function formatRelativeTime(date: string | Date): string {
  const d = typeof date === 'string' ? new Date(date) : date;
  const now = new Date();
  const diff = now.getTime() - d.getTime();
  
  const minutes = Math.floor(diff / 60000);
  const hours = Math.floor(diff / 3600000);
  const days = Math.floor(diff / 86400000);
  
  if (minutes < 1) return 'Just now';
  if (minutes < 60) return `${minutes}m ago`;
  if (hours < 24) return `${hours}h ago`;
  if (days < 7) return `${days}d ago`;
  
  return formatDate(d);
}

// Format number with commas
export function formatNumber(num: number): string {
  return num.toLocaleString('en-US');
}

// Format currency
export function formatCurrency(amount: number, currency = 'USD'): string {
  return new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency,
  }).format(amount);
}

// Truncate string
export function truncate(str: string, length: number): string {
  if (str.length <= length) return str;
  return str.slice(0, length) + '...';
}

// Get initials from name
export function getInitials(name: string): string {
  return name
    .split(' ')
    .map((n) => n[0])
    .join('')
    .toUpperCase()
    .slice(0, 2);
}

// Generate unique ID
export function generateId(): string {
  return Math.random().toString(36).substring(2, 9);
}

// Debounce function
export function debounce<T extends (...args: any[]) => any>(
  fn: T,
  delay: number
): (...args: Parameters<T>) => void {
  let timeoutId: ReturnType<typeof setTimeout>;
  return (...args: Parameters<T>) => {
    clearTimeout(timeoutId);
    timeoutId = setTimeout(() => fn(...args), delay);
  };
}

// Status color mapping
export function getStatusColor(status: string): string {
  const statusLower = status?.toLowerCase() || '';
  
  const colors: Record<string, string> = {
    active: 'bg-emerald-100 text-emerald-800 dark:bg-emerald-900/30 dark:text-emerald-400',
    completed: 'bg-emerald-100 text-emerald-800 dark:bg-emerald-900/30 dark:text-emerald-400',
    success: 'bg-emerald-100 text-emerald-800 dark:bg-emerald-900/30 dark:text-emerald-400',
    pending: 'bg-amber-100 text-amber-800 dark:bg-amber-900/30 dark:text-amber-400',
    processing: 'bg-blue-100 text-blue-800 dark:bg-blue-900/30 dark:text-blue-400',
    inactive: 'bg-zinc-100 text-zinc-800 dark:bg-zinc-800 dark:text-zinc-400',
    cancelled: 'bg-zinc-100 text-zinc-800 dark:bg-zinc-800 dark:text-zinc-400',
    error: 'bg-red-100 text-red-800 dark:bg-red-900/30 dark:text-red-400',
    failed: 'bg-red-100 text-red-800 dark:bg-red-900/30 dark:text-red-400',
  };
  
  return colors[statusLower] || 'bg-zinc-100 text-zinc-800 dark:bg-zinc-800 dark:text-zinc-400';
}
'''
        return GeneratedFile(
            path='src/lib/utils.ts',
            content=content,
            description='Utility functions'
        )
    
    @staticmethod
    def _generate_hooks() -> GeneratedFile:
        """Generate React hooks for data fetching."""
        content = '''/**
 * Custom React hooks for data fetching
 */

import { useState, useEffect, useCallback, useMemo } from 'react';
import { runtimeQuery, runtimeAction, type QuerySpec, type QueryResult, type ActionResult } from './runtimeClient';
import { debounce } from './utils';

// Hook for fetching data from a resource
export function useResourceData<T = any>(
  resourceId: string,
  initialQuerySpec: QuerySpec = {}
) {
  const [data, setData] = useState<T[]>([]);
  const [count, setCount] = useState(0);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [querySpec, setQuerySpec] = useState(initialQuerySpec);

  const fetchData = useCallback(async () => {
    if (!resourceId) return;
    
    setLoading(true);
    setError(null);
    
    try {
      const result = await runtimeQuery<T>({ resourceId, querySpec });
      setData(result.data);
      setCount(result.count);
    } catch (err) {
      setError(String(err));
    } finally {
      setLoading(false);
    }
  }, [resourceId, querySpec]);

  useEffect(() => {
    fetchData();
  }, [fetchData]);

  const refresh = useCallback(() => {
    fetchData();
  }, [fetchData]);

  const updateFilters = useCallback((filters: QuerySpec['filters']) => {
    setQuerySpec((prev) => ({ ...prev, filters, offset: 0 }));
  }, []);

  const updateSort = useCallback((orderBy: QuerySpec['orderBy']) => {
    setQuerySpec((prev) => ({ ...prev, orderBy }));
  }, []);

  const updatePagination = useCallback((limit: number, offset: number) => {
    setQuerySpec((prev) => ({ ...prev, limit, offset }));
  }, []);

  const updateSearch = useCallback((search: string) => {
    setQuerySpec((prev) => ({ ...prev, search, offset: 0 }));
  }, []);

  return {
    data,
    count,
    loading,
    error,
    querySpec,
    refresh,
    updateFilters,
    updateSort,
    updatePagination,
    updateSearch,
  };
}

// Hook for executing actions
export function useAction<T = any>(actionId: string) {
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [result, setResult] = useState<T | null>(null);

  const execute = useCallback(async (args: Record<string, any>): Promise<ActionResult<T>> => {
    setLoading(true);
    setError(null);
    
    try {
      const res = await runtimeAction<T>({ actionId, args });
      if (res.success) {
        setResult(res.data || null);
      } else {
        setError(res.error || 'Action failed');
      }
      return res;
    } catch (err) {
      const errorMsg = String(err);
      setError(errorMsg);
      return { success: false, error: errorMsg };
    } finally {
      setLoading(false);
    }
  }, [actionId]);

  return { execute, loading, error, result };
}

// Hook for search with debounce
export function useSearch(onSearch: (query: string) => void, delay = 300) {
  const [searchTerm, setSearchTerm] = useState('');

  const debouncedSearch = useMemo(
    () => debounce(onSearch, delay),
    [onSearch, delay]
  );

  useEffect(() => {
    debouncedSearch(searchTerm);
  }, [searchTerm, debouncedSearch]);

  return { searchTerm, setSearchTerm };
}

// Hook for pagination
export function usePagination(totalCount: number, pageSize = 20) {
  const [page, setPage] = useState(1);
  
  const totalPages = Math.ceil(totalCount / pageSize);
  const offset = (page - 1) * pageSize;
  
  const goToPage = useCallback((newPage: number) => {
    setPage(Math.max(1, Math.min(newPage, totalPages)));
  }, [totalPages]);

  const nextPage = useCallback(() => {
    goToPage(page + 1);
  }, [page, goToPage]);

  const prevPage = useCallback(() => {
    goToPage(page - 1);
  }, [page, goToPage]);

  return {
    page,
    pageSize,
    offset,
    totalPages,
    goToPage,
    nextPage,
    prevPage,
    hasNext: page < totalPages,
    hasPrev: page > 1,
  };
}
'''
        return GeneratedFile(
            path='src/lib/hooks.ts',
            content=content,
            description='Custom React hooks'
        )
    
    @staticmethod
    def _generate_ui_components() -> List[GeneratedFile]:
        """Generate UI component library."""
        files = []
        
        # StatusBadge component
        files.append(GeneratedFile(
            path='src/components/StatusBadge.tsx',
            content='''/**
 * Status Badge Component
 */
import { getStatusColor } from '../lib/utils';

interface StatusBadgeProps {
  status: string;
  className?: string;
}

export function StatusBadge({ status, className = '' }: StatusBadgeProps) {
  return (
    <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${getStatusColor(status)} ${className}`}>
      {status}
    </span>
  );
}
''',
            description='Status badge component'
        ))
        
        # Loading state
        files.append(GeneratedFile(
            path='src/components/LoadingState.tsx',
            content='''/**
 * Loading State Component
 */
interface LoadingStateProps {
  message?: string;
}

export function LoadingState({ message = 'Loading...' }: LoadingStateProps) {
  return (
    <div className="flex flex-col items-center justify-center py-12">
      <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-violet-600 mb-4"></div>
      <p className="text-sm text-zinc-500">{message}</p>
    </div>
  );
}
''',
            description='Loading state component'
        ))
        
        # Empty state
        files.append(GeneratedFile(
            path='src/components/EmptyState.tsx',
            content='''/**
 * Empty State Component
 */
interface EmptyStateProps {
  title: string;
  description?: string;
  icon?: React.ReactNode;
  action?: React.ReactNode;
}

export function EmptyState({ title, description, icon, action }: EmptyStateProps) {
  return (
    <div className="flex flex-col items-center justify-center py-12 text-center">
      {icon && (
        <div className="w-16 h-16 rounded-2xl bg-zinc-100 dark:bg-zinc-800 flex items-center justify-center mb-4">
          {icon}
        </div>
      )}
      <h3 className="text-lg font-semibold text-zinc-900 dark:text-zinc-100 mb-2">{title}</h3>
      {description && (
        <p className="text-sm text-zinc-500 max-w-sm mb-6">{description}</p>
      )}
      {action}
    </div>
  );
}
''',
            description='Empty state component'
        ))
        
        # Error boundary
        files.append(GeneratedFile(
            path='src/components/ErrorBoundary.tsx',
            content='''/**
 * Error Boundary Component
 */
import React from 'react';

interface ErrorBoundaryProps {
  children: React.ReactNode;
  fallback?: React.ReactNode;
}

interface ErrorBoundaryState {
  hasError: boolean;
  error: Error | null;
}

export class ErrorBoundary extends React.Component<ErrorBoundaryProps, ErrorBoundaryState> {
  constructor(props: ErrorBoundaryProps) {
    super(props);
    this.state = { hasError: false, error: null };
  }

  static getDerivedStateFromError(error: Error): ErrorBoundaryState {
    return { hasError: true, error };
  }

  componentDidCatch(error: Error, errorInfo: React.ErrorInfo) {
    console.error('Error caught by boundary:', error, errorInfo);
  }

  render() {
    if (this.state.hasError) {
      if (this.props.fallback) {
        return this.props.fallback;
      }
      
      return (
        <div className="flex flex-col items-center justify-center py-12 px-4">
          <div className="w-16 h-16 rounded-2xl bg-red-100 dark:bg-red-900/30 flex items-center justify-center mb-4">
            <svg className="h-8 w-8 text-red-600 dark:text-red-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
            </svg>
          </div>
          <h3 className="text-lg font-semibold text-zinc-900 dark:text-zinc-100 mb-2">
            Something went wrong
          </h3>
          <p className="text-sm text-zinc-500 max-w-sm text-center mb-4">
            {this.state.error?.message || 'An unexpected error occurred'}
          </p>
          <button
            onClick={() => window.location.reload()}
            className="px-4 py-2 text-sm font-medium text-white bg-violet-600 rounded-lg hover:bg-violet-500"
          >
            Reload Page
          </button>
        </div>
      );
    }

    return this.props.children;
  }
}
''',
            description='Error boundary component'
        ))
        
        return files
    
    @staticmethod
    def _generate_app_components(spec_json: Dict[str, Any]) -> List[GeneratedFile]:
        """Generate main app components based on spec."""
        files = []
        pages = spec_json.get('pages', [])
        
        for page in pages:
            layout = page.get('layout', 'table_detail_drawer')
            
            if layout == 'table_detail_drawer':
                files.append(EnhancedCodegenService._generate_table_view(page))
                files.append(EnhancedCodegenService._generate_detail_drawer(page))
        
        return files
    
    @staticmethod
    def _generate_table_view(page: Dict[str, Any]) -> GeneratedFile:
        """Generate enhanced TableView component."""
        table_spec = page.get('view', {}).get('table', {})
        columns = table_spec.get('columns', [])
        resource_id = page.get('primaryResource', '')
        
        column_headers = '\n'.join([
            f'            <th className="px-6 py-3 text-left text-xs font-semibold text-zinc-500 uppercase tracking-wider">{col.get("label", col.get("field", ""))}</th>'
            for col in columns
        ])
        
        column_cells = '\n'.join([
            f'              <td className="px-6 py-4 whitespace-nowrap text-sm text-zinc-900 dark:text-zinc-100">'
            f'{{row["{col.get("field", "")}"]}}'
            f'</td>'
            for col in columns
        ])
        
        content = f'''/**
 * Table View Component
 * 
 * Auto-generated for resource: {resource_id}
 */
import {{ useState }} from 'react';
import {{ useResourceData, usePagination, useSearch }} from '../lib/hooks';
import {{ LoadingState }} from './LoadingState';
import {{ EmptyState }} from './EmptyState';
import {{ StatusBadge }} from './StatusBadge';

interface TableViewProps {{
  resourceId: string;
  onRowClick?: (row: any) => void;
  pageSize?: number;
}}

export function TableView({{ resourceId, onRowClick, pageSize = 20 }}: TableViewProps) {{
  const {{ data, count, loading, error, refresh, updateSearch, updatePagination }} = useResourceData(
    resourceId,
    {{ limit: pageSize, offset: 0 }}
  );
  
  const {{ page, totalPages, nextPage, prevPage, hasNext, hasPrev, offset }} = usePagination(count, pageSize);
  const {{ searchTerm, setSearchTerm }} = useSearch(updateSearch);

  // Update pagination when page changes
  const handlePageChange = (newPage: number) => {{
    updatePagination(pageSize, (newPage - 1) * pageSize);
  }};

  if (loading && data.length === 0) {{
    return <LoadingState message="Loading data..." />;
  }}

  if (error) {{
    return (
      <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg p-4">
        <p className="text-sm text-red-700 dark:text-red-400">{{error}}</p>
      </div>
    );
  }}

  return (
    <div className="space-y-4">
      {{/* Search */}}
      <div className="flex items-center gap-4">
        <div className="relative flex-1 max-w-sm">
          <input
            type="text"
            placeholder="Search..."
            value={{searchTerm}}
            onChange={{(e) => setSearchTerm(e.target.value)}}
            className="w-full pl-10 pr-4 py-2 bg-white dark:bg-zinc-800 border border-zinc-200 dark:border-zinc-700 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-violet-500"
          />
          <svg className="absolute left-3 top-2.5 h-4 w-4 text-zinc-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
          </svg>
        </div>
        <button
          onClick={{refresh}}
          className="p-2 text-zinc-500 hover:text-zinc-700 dark:hover:text-zinc-300 hover:bg-zinc-100 dark:hover:bg-zinc-800 rounded-lg"
        >
          <svg className="h-5 w-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15" />
          </svg>
        </button>
      </div>

      {{/* Table */}}
      <div className="overflow-hidden bg-white dark:bg-zinc-900 rounded-xl border border-zinc-200 dark:border-zinc-800 shadow-sm">
        <table className="min-w-full divide-y divide-zinc-200 dark:divide-zinc-800">
          <thead className="bg-zinc-50 dark:bg-zinc-800/50">
            <tr>
{column_headers}
            </tr>
          </thead>
          <tbody className="divide-y divide-zinc-100 dark:divide-zinc-800">
            {{data.length === 0 ? (
              <tr>
                <td colSpan={{100}} className="px-6 py-12 text-center text-zinc-500">
                  No data found
                </td>
              </tr>
            ) : (
              data.map((row, idx) => (
                <tr 
                  key={{idx}}
                  onClick={{() => onRowClick?.(row)}}
                  className="hover:bg-zinc-50 dark:hover:bg-zinc-800/50 cursor-pointer transition-colors"
                >
{column_cells}
                </tr>
              ))
            )}}
          </tbody>
        </table>
      </div>

      {{/* Pagination */}}
      <div className="flex items-center justify-between text-sm text-zinc-500">
        <span>Showing {{offset + 1}} to {{Math.min(offset + pageSize, count)}} of {{count}} results</span>
        <div className="flex items-center gap-2">
          <button
            onClick={{prevPage}}
            disabled={{!hasPrev}}
            className="px-3 py-1.5 rounded-lg border border-zinc-200 dark:border-zinc-700 disabled:opacity-50 disabled:cursor-not-allowed hover:bg-zinc-50 dark:hover:bg-zinc-800"
          >
            Previous
          </button>
          <span>Page {{page}} of {{totalPages || 1}}</span>
          <button
            onClick={{nextPage}}
            disabled={{!hasNext}}
            className="px-3 py-1.5 rounded-lg border border-zinc-200 dark:border-zinc-700 disabled:opacity-50 disabled:cursor-not-allowed hover:bg-zinc-50 dark:hover:bg-zinc-800"
          >
            Next
          </button>
        </div>
      </div>
    </div>
  );
}}
'''
        return GeneratedFile(
            path='src/components/TableView.tsx',
            content=content,
            description='Table view component'
        )
    
    @staticmethod
    def _generate_detail_drawer(page: Dict[str, Any]) -> GeneratedFile:
        """Generate DetailDrawer component."""
        drawer_spec = page.get('view', {}).get('detailDrawer', {})
        fields = drawer_spec.get('fields', [])
        
        field_renders = '\n'.join([
            f'''            <div>
              <label className="block text-sm font-medium text-zinc-500 dark:text-zinc-400 mb-1">{field.get("label", field.get("field", ""))}</label>
              <div className="text-sm text-zinc-900 dark:text-zinc-100">{{data?.["{field.get("field", "")}"] ?? '-'}}</div>
            </div>'''
            for field in fields
        ])
        
        content = f'''/**
 * Detail Drawer Component
 */
interface DetailDrawerProps {{
  isOpen: boolean;
  onClose: () => void;
  data: any | null;
  title?: string;
}}

export function DetailDrawer({{ isOpen, onClose, data, title = 'Details' }}: DetailDrawerProps) {{
  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 z-50 flex">
      {{/* Backdrop */}}
      <div 
        className="absolute inset-0 bg-black/50 backdrop-blur-sm"
        onClick={{onClose}}
      />
      
      {{/* Drawer */}}
      <div className="absolute right-0 top-0 bottom-0 w-full max-w-md bg-white dark:bg-zinc-900 shadow-2xl overflow-hidden flex flex-col">
        {{/* Header */}}
        <div className="flex items-center justify-between px-6 py-4 border-b border-zinc-200 dark:border-zinc-800">
          <h2 className="text-lg font-semibold text-zinc-900 dark:text-zinc-100">{{title}}</h2>
          <button
            onClick={{onClose}}
            className="p-2 hover:bg-zinc-100 dark:hover:bg-zinc-800 rounded-lg transition-colors"
          >
            <svg className="h-5 w-5 text-zinc-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M6 18L18 6M6 6l12 12" />
            </svg>
          </button>
        </div>
        
        {{/* Content */}}
        <div className="flex-1 overflow-y-auto p-6">
          <div className="space-y-6">
{field_renders if field_renders else '            <p className="text-zinc-500">No fields configured</p>'}
          </div>
        </div>
        
        {{/* Footer */}}
        <div className="px-6 py-4 border-t border-zinc-200 dark:border-zinc-800">
          <button
            onClick={{onClose}}
            className="w-full px-4 py-2 bg-zinc-100 dark:bg-zinc-800 hover:bg-zinc-200 dark:hover:bg-zinc-700 text-zinc-700 dark:text-zinc-300 rounded-lg transition-colors"
          >
            Close
          </button>
        </div>
      </div>
    </div>
  );
}}
'''
        return GeneratedFile(
            path='src/components/DetailDrawer.tsx',
            content=content,
            description='Detail drawer component'
        )
    
    @staticmethod
    def _generate_main_page(spec_json: Dict[str, Any]) -> GeneratedFile:
        """Generate main page component."""
        app_name = spec_json.get('appName', 'App')
        pages = spec_json.get('pages', [])
        
        # Generate page content based on first page
        page_content = ''
        if pages:
            page = pages[0]
            resource_id = page.get('primaryResource', '')
            title = page.get('title', 'Data')
            
            page_content = f'''
        <main className="flex-1 p-6">
          <div className="max-w-7xl mx-auto">
            <div className="mb-6">
              <h2 className="text-xl font-semibold text-zinc-900 dark:text-zinc-100">{title}</h2>
            </div>
            <TableView 
              resourceId="{resource_id}"
              onRowClick={{handleRowClick}}
            />
          </div>
        </main>
        
        <DetailDrawer
          isOpen={{drawerOpen}}
          onClose={{() => setDrawerOpen(false)}}
          data={{selectedRow}}
          title="Details"
        />'''
        
        content = f'''/**
 * Main Page Component
 * 
 * Auto-generated from AppSpec: {app_name}
 */
import {{ useState }} from 'react';
import {{ TableView }} from '../components/TableView';
import {{ DetailDrawer }} from '../components/DetailDrawer';
import {{ ErrorBoundary }} from '../components/ErrorBoundary';

export default function Page() {{
  const [selectedRow, setSelectedRow] = useState<any>(null);
  const [drawerOpen, setDrawerOpen] = useState(false);

  const handleRowClick = (row: any) => {{
    setSelectedRow(row);
    setDrawerOpen(true);
  }};

  return (
    <ErrorBoundary>
      <div className="min-h-screen bg-zinc-50 dark:bg-zinc-950">
        {{/* Header */}}
        <header className="bg-white dark:bg-zinc-900 border-b border-zinc-200 dark:border-zinc-800">
          <div className="max-w-7xl mx-auto px-6 py-4">
            <h1 className="text-xl font-bold text-zinc-900 dark:text-zinc-100">{app_name}</h1>
          </div>
        </header>
{page_content}
      </div>
    </ErrorBoundary>
  );
}}
'''
        return GeneratedFile(
            path='src/app/page.tsx',
            content=content,
            description='Main page component'
        )
    
    @staticmethod
    def _generate_layout(spec_json: Dict[str, Any]) -> GeneratedFile:
        """Generate app layout."""
        app_name = spec_json.get('appName', 'App')
        
        content = f'''/**
 * App Layout
 */
import './globals.css';
import {{ ErrorBoundary }} from '../components/ErrorBoundary';

export const metadata = {{
  title: '{app_name}',
  description: 'Internal application built with Vector',
}};

export default function RootLayout({{
  children,
}}: {{
  children: React.ReactNode;
}}) {{
  return (
    <html lang="en" className="dark">
      <body className="bg-zinc-50 dark:bg-zinc-950 text-zinc-900 dark:text-zinc-100 antialiased">
        <ErrorBoundary>
          {{children}}
        </ErrorBoundary>
      </body>
    </html>
  );
}}
'''
        return GeneratedFile(
            path='src/app/layout.tsx',
            content=content,
            description='App layout'
        )
    
    @staticmethod
    def _generate_global_styles() -> GeneratedFile:
        """Generate global CSS."""
        content = '''/**
 * Global Styles
 */
@tailwind base;
@tailwind components;
@tailwind utilities;

@layer base {
  :root {
    --background: 250 250 250;
    --foreground: 24 24 27;
  }

  .dark {
    --background: 9 9 11;
    --foreground: 250 250 250;
  }
  
  body {
    @apply bg-zinc-50 dark:bg-zinc-950;
  }
  
  /* Scrollbar */
  ::-webkit-scrollbar {
    width: 8px;
    height: 8px;
  }
  ::-webkit-scrollbar-track {
    @apply bg-transparent;
  }
  ::-webkit-scrollbar-thumb {
    @apply bg-zinc-300 dark:bg-zinc-700 rounded;
  }
  ::-webkit-scrollbar-thumb:hover {
    @apply bg-zinc-400 dark:bg-zinc-600;
  }
}

@layer utilities {
  .animate-in {
    animation: animateIn 0.2s ease-out;
  }
  
  @keyframes animateIn {
    from {
      opacity: 0;
      transform: translateY(4px);
    }
    to {
      opacity: 1;
      transform: translateY(0);
    }
  }
}
'''
        return GeneratedFile(
            path='src/app/globals.css',
            content=content,
            description='Global CSS styles'
        )

