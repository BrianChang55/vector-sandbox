"""
Code generation service - AppSpec → TSX
"""

import logging
from typing import Dict, Any, List
from ..models import AppVersion, VersionFile

logger = logging.getLogger(__name__)


class CodegenService:
    """Service for generating TSX code from AppSpec."""

    # Allowlisted file paths (V1)
    ALLOWLISTED_PATHS = [
        "src/app/page.tsx",
        "src/components/TableView.tsx",
        "src/components/DetailDrawer.tsx",
        "src/lib/runtimeClient.ts",
        "src/lib/types.ts",
    ]

    @staticmethod
    def generate_files_from_spec(app_version: AppVersion) -> List[VersionFile]:
        """
        Generate TSX files from AppSpec.

        Args:
            app_version: AppVersion instance with spec_json

        Returns:
            List of VersionFile instances
        """
        spec_json = app_version.spec_json

        files = []

        # Generate runtime client (shared across all apps)
        runtime_client = CodegenService._generate_runtime_client()
        files.append(
            VersionFile(
                app_version=app_version,
                path="src/lib/runtimeClient.ts",
                content=runtime_client,
            )
        )

        # Generate types
        types_content = CodegenService._generate_types()
        files.append(
            VersionFile(
                app_version=app_version,
                path="src/lib/types.ts",
                content=types_content,
            )
        )

        # Generate main page
        page_content = CodegenService._generate_page(spec_json)
        files.append(
            VersionFile(
                app_version=app_version,
                path="src/app/page.tsx",
                content=page_content,
            )
        )

        # Generate table view component
        table_view_content = CodegenService._generate_table_view()
        files.append(
            VersionFile(
                app_version=app_version,
                path="src/components/TableView.tsx",
                content=table_view_content,
            )
        )

        # Generate detail drawer component
        drawer_content = CodegenService._generate_detail_drawer()
        files.append(
            VersionFile(
                app_version=app_version,
                path="src/components/DetailDrawer.tsx",
                content=drawer_content,
            )
        )

        # Calculate hashes and save
        for file in files:
            file.save()

        return files

    @staticmethod
    def _generate_runtime_client() -> str:
        """Generate runtime client code."""
        return """import axios from 'axios';

const API_BASE_URL = window.location.origin + '/api/v1';

export interface QuerySpec {
  select?: string[];
  filters?: Array<{ field: string; op: string; value: any }>;
  orderBy?: Array<{ field: string; dir: 'asc' | 'desc' }>;
  limit?: number;
  offset?: number;
}

export interface QueryResult {
  data: any[];
  count: number;
}

export interface ActionResult {
  success: boolean;
  data: any;
}

export async function runtimeQuery(params: {
  appId: string;
  versionId: string;
  resourceId: string;
  querySpec: QuerySpec;
}): Promise<QueryResult> {
  const response = await axios.post(`${API_BASE_URL}/runtime/query`, params);
  return response.data;
}

export async function runtimeAction(params: {
  appId: string;
  versionId: string;
  actionId: string;
  args: Record<string, any>;
}): Promise<ActionResult> {
  const response = await axios.post(`${API_BASE_URL}/runtime/action`, params);
  return response.data;
}
"""

    @staticmethod
    def _generate_types() -> str:
        """Generate TypeScript types."""
        return """export interface AppSpec {
  appName: string;
  pages: PageSpec[];
}

export interface PageSpec {
  id: string;
  title: string;
  layout: 'table_detail_drawer' | 'tabbed_views';
  primaryResource: string;
  view: {
    table?: TableView;
    detailDrawer?: DetailDrawer;
  };
}

export interface TableView {
  columns: Array<{ field: string; label?: string }>;
  filterableFields?: string[];
  searchableFields?: string[];
  sort?: { field: string; dir: 'asc' | 'desc' };
  pagination?: { pageSize: number };
  rowActions?: Array<{ label: string; actionId: string; confirm?: boolean }>;
  bulkActions?: Array<{ label: string; actionId: string; confirm?: boolean }>;
}

export interface DetailDrawer {
  titleField?: string;
  fields: Array<{ field: string; label?: string; readOnly?: boolean }>;
  actions?: Array<{ label: string; actionId: string; confirm?: boolean }>;
}
"""

    @staticmethod
    def _generate_page(spec_json: Dict[str, Any]) -> str:
        """Generate main page component from AppSpec."""
        import json

        app_name = spec_json.get("appName", "App")
        pages = spec_json.get("pages", [])

        page_components = []
        for page in pages:
            page_id = page.get("id", "page1")
            page_title = page.get("title", "Page")
            primary_resource = page.get("primaryResource", "")

            view_table = page.get("view", {}).get("table", {})
            view_table_json = json.dumps(view_table)
            component_code = f"""          <div key="{page_id}">
            <h1>{page_title}</h1>
            <TableView
              resourceId="{primary_resource}"
              spec={{{view_table_json}}}
            />
          </div>"""
            page_components.append(component_code)

        return f"""import React from 'react';
import {{ TableView }} from '../components/TableView';

export default function Page() {{
  return (
    <div className="p-6">
      <h1 className="text-2xl font-bold mb-4">{app_name}</h1>
{''.join(page_components)}
    </div>
  );
}}
"""

    @staticmethod
    def _generate_table_view() -> str:
        """Generate TableView component."""
        return """import React, { useEffect, useState } from 'react';
import { runtimeQuery } from '../lib/runtimeClient';

interface TableViewProps {
  resourceId: string;
  spec: any;
  appId?: string;
  versionId?: string;
}

export function TableView({ resourceId, spec, appId = '', versionId = '' }: TableViewProps) {
  const [data, setData] = useState<any[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    async function fetchData() {
      try {
        const result = await runtimeQuery({
          appId,
          versionId,
          resourceId,
          querySpec: {
            select: spec.columns?.map((c: any) => c.field) || ['*'],
            limit: spec.pagination?.pageSize || 50,
          },
        });
        setData(result.data);
      } catch (error) {
        console.error('Error fetching data:', error);
      } finally {
        setLoading(false);
      }
    }
    fetchData();
  }, [resourceId, appId, versionId]);

  if (loading) return <div>Loading...</div>;

  return (
    <div className="overflow-x-auto">
      <table className="min-w-full divide-y divide-gray-200">
        <thead className="bg-gray-50">
          <tr>
            {spec.columns?.map((col: any) => (
              <th key={col.field} className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">
                {col.label || col.field}
              </th>
            ))}
          </tr>
        </thead>
        <tbody className="bg-white divide-y divide-gray-200">
          {data.map((row: any, idx: number) => (
            <tr key={idx}>
              {spec.columns?.map((col: any) => (
                <td key={col.field} className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  {row[col.field]}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
"""

    @staticmethod
    def _generate_detail_drawer() -> str:
        """Generate DetailDrawer component."""
        return """import React from 'react';

interface DetailDrawerProps {
  resourceId: string;
  rowId: string;
  spec: any;
  appId?: string;
  versionId?: string;
  onClose: () => void;
}

export function DetailDrawer({ resourceId, rowId, spec, appId = '', versionId = '', onClose }: DetailDrawerProps) {
  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 z-50 flex items-center justify-end">
      <div className="bg-white w-96 h-full shadow-xl overflow-y-auto">
        <div className="p-6">
          <div className="flex justify-between items-center mb-4">
            <h2 className="text-xl font-bold">Details</h2>
            <button onClick={onClose} className="text-gray-500 hover:text-gray-700">×</button>
          </div>
          <div className="space-y-4">
            {spec.fields?.map((field: any) => (
              <div key={field.field}>
                <label className="block text-sm font-medium text-gray-700">{field.label || field.field}</label>
                <div className="mt-1 text-sm text-gray-900">
                  {/* Field value will be loaded from data */}
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
}
"""
