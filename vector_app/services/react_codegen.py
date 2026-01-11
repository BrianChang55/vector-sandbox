"""
React Code Generation Service

Generates production-quality React/TypeScript code from AppSpec or direct instructions.
Produces complete, runnable applications that work with the Sandpack runtime.
"""
import logging
import json
from typing import Dict, Any, List, Optional
from dataclasses import dataclass

logger = logging.getLogger(__name__)


@dataclass
class GeneratedFile:
    """A generated file for the React app."""
    path: str
    content: str
    language: str


class ReactCodegenService:
    """
    Generates complete React applications.
    
    Produces:
    - App.tsx - Main application component
    - Components - Reusable UI components
    - lib/runtime.ts - API client for backend data
    - lib/types.ts - TypeScript type definitions
    - styles.css - Tailwind-based styling
    """
    
    @staticmethod
    def generate_app(
        spec_json: Optional[Dict[str, Any]] = None,
        generated_components: Optional[Dict[str, str]] = None,
        app_name: str = "App",
        registry_surface: Optional[Dict[str, Any]] = None,
    ) -> List[GeneratedFile]:
        """
        Generate a complete React application.
        
        Args:
            spec_json: AppSpec JSON (optional)
            generated_components: Custom component code from AI
            app_name: Name of the application
            registry_surface: Available backend resources
            
        Returns:
            List of GeneratedFile objects
        """
        files = []
        
        # 1. Generate runtime client
        files.append(GeneratedFile(
            path="/src/lib/runtime.ts",
            content=ReactCodegenService._generate_runtime_client(),
            language="typescript",
        ))
        
        # 2. Generate types
        files.append(GeneratedFile(
            path="/src/lib/types.ts",
            content=ReactCodegenService._generate_types(registry_surface),
            language="typescript",
        ))
        
        # 3. Generate main App component
        if generated_components and "App.tsx" in generated_components:
            app_content = generated_components["App.tsx"]
        elif spec_json:
            app_content = ReactCodegenService._generate_app_from_spec(spec_json, app_name)
        else:
            app_content = ReactCodegenService._generate_default_app(app_name)
        
        files.append(GeneratedFile(
            path="/src/App.tsx",
            content=app_content,
            language="typescript",
        ))
        
        # 4. Generate additional components from AI if provided
        if generated_components:
            for path, content in generated_components.items():
                if path != "App.tsx" and content.strip():
                    # Normalize path
                    file_path = path if path.startswith("/") else f"/src/{path}"
                    files.append(GeneratedFile(
                        path=file_path,
                        content=content,
                        language="typescript" if path.endswith(('.ts', '.tsx')) else "css",
                    ))
        
        # 5. Generate index file
        files.append(GeneratedFile(
            path="/src/index.tsx",
            content=ReactCodegenService._generate_index(),
            language="typescript",
        ))
        
        # 6. Generate styles
        files.append(GeneratedFile(
            path="/src/styles.css",
            content=ReactCodegenService._generate_styles(),
            language="css",
        ))
        
        return files
    
    @staticmethod
    def _generate_runtime_client() -> str:
        """Generate the runtime API client."""
        return '''/**
 * Runtime API Client
 * 
 * Handles all data operations through the Vector proxy.
 * NEVER call backend databases directly.
 */

// Config is injected by the runtime
declare global {
  interface Window {
    __VECTOR_CONFIG__?: {
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

export interface QueryResult<T = any> {
  data: T[];
  count: number;
}

export interface ActionResult {
  success: boolean;
  data?: any;
  error?: string;
}

function getConfig() {
  return window.__VECTOR_CONFIG__ || {
    appId: '',
    versionId: '',
    apiBaseUrl: '/api/v1',
    appName: 'App',
  };
}

export async function runtimeQuery<T = any>(params: {
  resourceId: string;
  querySpec?: QuerySpec;
}): Promise<QueryResult<T>> {
  const config = getConfig();
  
  try {
    const response = await fetch(`${config.apiBaseUrl}/runtime/query/`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        appId: config.appId,
        versionId: config.versionId,
        resourceId: params.resourceId,
        querySpec: params.querySpec || {},
      }),
    });
    
    if (!response.ok) {
      throw new Error(`Query failed: ${response.statusText}`);
    }
    
    return response.json();
  } catch (error) {
    console.error('Runtime query error:', error);
    return { data: [], count: 0 };
  }
}

export async function runtimeAction(params: {
  actionId: string;
  args?: Record<string, any>;
}): Promise<ActionResult> {
  const config = getConfig();
  
  try {
    const response = await fetch(`${config.apiBaseUrl}/runtime/action/`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        appId: config.appId,
        versionId: config.versionId,
        actionId: params.actionId,
        args: params.args || {},
      }),
    });
    
    if (!response.ok) {
      throw new Error(`Action failed: ${response.statusText}`);
    }
    
    return response.json();
  } catch (error) {
    console.error('Runtime action error:', error);
    return { success: false, error: String(error) };
  }
}

// Utility hooks for React
import { useState, useEffect, useCallback } from 'react';

export function useQuery<T = any>(
  resourceId: string,
  querySpec?: QuerySpec,
  deps: any[] = []
) {
  const [data, setData] = useState<T[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  
  const refetch = useCallback(async () => {
    setLoading(true);
    setError(null);
    try {
      const result = await runtimeQuery<T>({ resourceId, querySpec });
      setData(result.data);
    } catch (e) {
      setError(String(e));
    } finally {
      setLoading(false);
    }
  }, [resourceId, JSON.stringify(querySpec)]);
  
  useEffect(() => {
    refetch();
  }, [refetch, ...deps]);
  
  return { data, loading, error, refetch };
}

export function useAction(actionId: string) {
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  
  const execute = useCallback(async (args?: Record<string, any>) => {
    setLoading(true);
    setError(null);
    try {
      const result = await runtimeAction({ actionId, args });
      if (!result.success) {
        throw new Error(result.error || 'Action failed');
      }
      return result;
    } catch (e) {
      setError(String(e));
      throw e;
    } finally {
      setLoading(false);
    }
  }, [actionId]);
  
  return { execute, loading, error };
}
'''
    
    @staticmethod
    def _generate_types(registry_surface: Optional[Dict[str, Any]]) -> str:
        """Generate TypeScript type definitions."""
        types = '''/**
 * TypeScript Type Definitions
 */

export interface Resource {
  id: string;
  [key: string]: any;
}

'''
        
        # Generate types for each resource if available
        if registry_surface and registry_surface.get("resources"):
            for resource in registry_surface["resources"]:
                resource_id = resource.get("resource_id", "Resource")
                # Convert to PascalCase
                type_name = "".join(word.capitalize() for word in resource_id.replace("_", " ").split())
                fields = resource.get("exposed_fields", [])
                
                types += f"export interface {type_name} {{\n"
                types += "  id: string;\n"
                for field in fields:
                    types += f"  {field}?: any;\n"
                types += "}\n\n"
        
        return types
    
    @staticmethod
    def _generate_app_from_spec(spec_json: Dict[str, Any], app_name: str) -> str:
        """Generate App.tsx from AppSpec JSON."""
        pages = spec_json.get("pages", [])
        
        if not pages:
            return ReactCodegenService._generate_default_app(app_name)
        
        page = pages[0]  # Take first page for now
        resource_id = page.get("primaryResource", "data")
        table_spec = page.get("view", {}).get("table", {})
        columns = table_spec.get("columns", [])
        
        columns_jsx = ",\n      ".join([
            f'{{ field: "{c.get("field", "id")}", label: "{c.get("label", c.get("field", "Field"))}" }}'
            for c in columns[:6]  # Limit to 6 columns
        ])
        
        return f'''import React, {{ useState }} from 'react';
import {{ useQuery }} from './lib/runtime';
import {{ Search, Filter, RefreshCw, ChevronLeft, ChevronRight }} from 'lucide-react';

const columns = [
  {columns_jsx or '{ field: "id", label: "ID" }'}
];

export default function App() {{
  const [searchTerm, setSearchTerm] = useState('');
  const [page, setPage] = useState(1);
  const pageSize = 20;
  
  const {{ data, loading, error, refetch }} = useQuery(
    '{resource_id}',
    {{
      limit: pageSize,
      offset: (page - 1) * pageSize,
    }}
  );
  
  const filteredData = data.filter((row: any) =>
    searchTerm === '' ||
    Object.values(row).some((val) =>
      String(val).toLowerCase().includes(searchTerm.toLowerCase())
    )
  );

  return (
    <div className="min-h-screen bg-gray-50">
      {{/* Header */}}
      <header className="bg-white border-b border-gray-200 px-6 py-4">
        <h1 className="text-xl font-bold text-gray-900">{app_name}</h1>
        <p className="text-sm text-gray-500 mt-1">{page.get("title", "Data View")}</p>
      </header>

      {{/* Toolbar */}}
      <div className="bg-white border-b border-gray-200 px-6 py-3 flex items-center gap-4">
        <div className="relative flex-1 max-w-md">
          <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-gray-400" />
          <input
            type="text"
            placeholder="Search..."
            value={{searchTerm}}
            onChange={{(e) => setSearchTerm(e.target.value)}}
            className="w-full pl-10 pr-4 py-2 border border-gray-200 rounded-lg text-sm
                     focus:outline-none focus:ring-2 focus:ring-violet-500 focus:border-transparent"
          />
        </div>
        <button
          onClick={{refetch}}
          disabled={{loading}}
          className="px-3 py-2 text-gray-600 hover:bg-gray-100 rounded-lg transition-colors
                   disabled:opacity-50"
        >
          <RefreshCw className={{`h-4 w-4 ${{loading ? 'animate-spin' : ''}}`}} />
        </button>
      </div>

      {{/* Content */}}
      <main className="p-6">
        {{error && (
          <div className="bg-red-50 border border-red-200 rounded-lg p-4 mb-4 text-red-700 text-sm">
            {{error}}
          </div>
        )}}

        <div className="bg-white rounded-xl shadow-sm border border-gray-200 overflow-hidden">
          <table className="min-w-full divide-y divide-gray-200">
            <thead className="bg-gray-50">
              <tr>
                {{columns.map((col) => (
                  <th
                    key={{col.field}}
                    className="px-6 py-3 text-left text-xs font-semibold text-gray-600 uppercase tracking-wider"
                  >
                    {{col.label}}
                  </th>
                ))}}
              </tr>
            </thead>
            <tbody className="bg-white divide-y divide-gray-100">
              {{loading ? (
                <tr>
                  <td colSpan={{columns.length}} className="px-6 py-12 text-center">
                    <div className="flex items-center justify-center gap-2 text-gray-500">
                      <RefreshCw className="h-5 w-5 animate-spin" />
                      Loading...
                    </div>
                  </td>
                </tr>
              ) : filteredData.length === 0 ? (
                <tr>
                  <td colSpan={{columns.length}} className="px-6 py-12 text-center text-gray-500">
                    No data found
                  </td>
                </tr>
              ) : (
                filteredData.map((row: any, idx: number) => (
                  <tr key={{row.id || idx}} className="hover:bg-gray-50 transition-colors cursor-pointer">
                    {{columns.map((col) => (
                      <td key={{col.field}} className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                        {{formatCellValue(row[col.field])}}
                      </td>
                    ))}}
                  </tr>
                ))
              )}}
            </tbody>
          </table>
        </div>

        {{/* Pagination */}}
        <div className="flex items-center justify-between mt-4">
          <span className="text-sm text-gray-500">
            Showing {{Math.min((page - 1) * pageSize + 1, filteredData.length)}} - 
            {{Math.min(page * pageSize, filteredData.length)}} of {{filteredData.length}}
          </span>
          <div className="flex items-center gap-2">
            <button
              onClick={{() => setPage((p) => Math.max(1, p - 1))}}
              disabled={{page === 1}}
              className="p-2 border border-gray-200 rounded-lg hover:bg-gray-50 disabled:opacity-50"
            >
              <ChevronLeft className="h-4 w-4" />
            </button>
            <span className="text-sm text-gray-600">Page {{page}}</span>
            <button
              onClick={{() => setPage((p) => p + 1)}}
              disabled={{filteredData.length < pageSize}}
              className="p-2 border border-gray-200 rounded-lg hover:bg-gray-50 disabled:opacity-50"
            >
              <ChevronRight className="h-4 w-4" />
            </button>
          </div>
        </div>
      </main>
    </div>
  );
}}

function formatCellValue(value: any): React.ReactNode {{
  if (value === null || value === undefined) return '-';
  if (typeof value === 'boolean') return value ? '✓' : '✗';
  if (value instanceof Date) return value.toLocaleDateString();
  if (typeof value === 'object') return JSON.stringify(value);
  return String(value);
}}
'''
    
    @staticmethod
    def _generate_default_app(app_name: str) -> str:
        """Generate a default App component."""
        return f'''import React from 'react';
import {{ Sparkles, Database, ArrowRight }} from 'lucide-react';

export default function App() {{
  return (
    <div className="min-h-screen bg-gradient-to-br from-violet-50 to-fuchsia-50 flex items-center justify-center p-6">
      <div className="max-w-md text-center">
        <div className="w-20 h-20 mx-auto mb-6 bg-gradient-to-br from-violet-500 to-fuchsia-500 
                      rounded-2xl flex items-center justify-center shadow-lg">
          <Sparkles className="h-10 w-10 text-white" />
        </div>
        
        <h1 className="text-3xl font-bold text-gray-900 mb-4">
          {app_name}
        </h1>
        
        <p className="text-gray-600 mb-8">
          Your app is ready! Connect data sources and describe the UI you want to build.
        </p>
        
        <div className="space-y-3">
          <div className="flex items-center gap-3 p-4 bg-white rounded-xl border border-gray-200 text-left">
            <Database className="h-5 w-5 text-violet-500 flex-shrink-0" />
            <div className="flex-1">
              <p className="text-sm font-medium text-gray-900">Connect Data</p>
              <p className="text-xs text-gray-500">Link your database or API</p>
            </div>
            <ArrowRight className="h-4 w-4 text-gray-400" />
          </div>
          
          <div className="flex items-center gap-3 p-4 bg-white rounded-xl border border-gray-200 text-left">
            <Sparkles className="h-5 w-5 text-fuchsia-500 flex-shrink-0" />
            <div className="flex-1">
              <p className="text-sm font-medium text-gray-900">Build with AI</p>
              <p className="text-xs text-gray-500">Describe your app in plain English</p>
            </div>
            <ArrowRight className="h-4 w-4 text-gray-400" />
          </div>
        </div>
      </div>
    </div>
  );
}}
'''
    
    @staticmethod
    def _generate_index() -> str:
        """Generate the index.tsx entry point."""
        return '''import React from 'react';
import { createRoot } from 'react-dom/client';
import App from './App';
import './styles.css';

const container = document.getElementById('root');
if (container) {
  const root = createRoot(container);
  root.render(
    <React.StrictMode>
      <App />
    </React.StrictMode>
  );
}
'''
    
    @staticmethod
    def _generate_styles() -> str:
        """Generate base styles."""
        return '''@tailwind base;
@tailwind components;
@tailwind utilities;

/* Custom styles */
body {
  margin: 0;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

/* Animations */
@keyframes fadeIn {
  from { opacity: 0; }
  to { opacity: 1; }
}

.animate-fadeIn {
  animation: fadeIn 0.3s ease-out;
}

/* Custom scrollbar */
::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}

::-webkit-scrollbar-track {
  background: transparent;
}

::-webkit-scrollbar-thumb {
  background: #d1d5db;
  border-radius: 3px;
}

::-webkit-scrollbar-thumb:hover {
  background: #9ca3af;
}
'''


# Singleton
_react_codegen_service: Optional['ReactCodegenService'] = None


def get_react_codegen_service() -> ReactCodegenService:
    """Get singleton instance."""
    global _react_codegen_service
    if _react_codegen_service is None:
        _react_codegen_service = ReactCodegenService()
    return _react_codegen_service

