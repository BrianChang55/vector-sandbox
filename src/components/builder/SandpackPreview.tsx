/**
 * Sandpack Preview Component
 * 
 * Live React app runtime using CodeSandbox's Sandpack bundler.
 * Renders generated React code in a sandboxed iframe with hot reloading.
 */
import { useMemo, useState, useEffect } from 'react'
import {
  SandpackProvider,
  SandpackPreview as SandpackPreviewPane,
  SandpackCodeEditor,
  SandpackConsole,
  useSandpack,
} from '@codesandbox/sandpack-react'
import { motion, AnimatePresence } from 'framer-motion'
import {
  Play,
  RefreshCw,
  Monitor,
  Code2,
  Terminal,
  Eye,
  AlertCircle,
  CheckCircle2,
  Loader2,
  ExternalLink,
} from 'lucide-react'
import { cn } from '../../lib/utils'
import { api } from '../../services/api'
import type { FileChange } from '../../types/agent'

interface SandpackPreviewProps {
  files: FileChange[]
  appId: string
  versionId?: string
  appName?: string
  className?: string
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
import { Sparkles } from 'lucide-react';

export default function App() {
  return (
    <div className="min-h-screen bg-gray-50 flex items-center justify-center">
      <div className="text-center">
        <div className="w-16 h-16 mx-auto mb-4 bg-gray-900 rounded-xl flex items-center justify-center">
          <Sparkles className="h-8 w-8 text-white" />
        </div>
        <h1 className="text-2xl font-bold text-gray-900">App Preview</h1>
        <p className="text-gray-600 mt-2">Generating your app...</p>
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

function getConfig() {
  return window.__RELAY_CONFIG__ || {
    appId: '',
    versionId: '',
    apiBaseUrl: '/api/v1',
    appName: 'App',
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
}

// Convert FileChange array to Sandpack files format
function convertToSandpackFiles(
  files: FileChange[],
  appId: string,
  versionId: string,
  appName: string
): Record<string, string> {
  const sandpackFiles: Record<string, string> = { ...DEFAULT_FILES }

  // Inject runtime config
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
      apiBaseUrl: window.location.origin + '/api/v1',
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

  const statusConfig: Record<string, { icon: typeof CheckCircle2; label: string; color: string; animate?: boolean }> = {
    idle: { icon: CheckCircle2, label: 'Ready', color: 'text-green-600' },
    running: { icon: Loader2, label: 'Loading', color: 'text-amber-600', animate: true },
    timeout: { icon: AlertCircle, label: 'Timeout', color: 'text-red-500' },
    done: { icon: CheckCircle2, label: 'Ready', color: 'text-green-600' },
  }

  const config = statusConfig[status] || statusConfig.idle
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

// Refresh button that uses Sandpack context
function RefreshButton() {
  const { sandpack } = useSandpack()

  return (
    <button
      onClick={() => sandpack.runSandpack()}
      className="p-1.5 text-gray-400 hover:text-gray-600 hover:bg-gray-100 
               rounded-md transition-colors"
      title="Refresh preview"
    >
      <RefreshCw className="h-4 w-4" />
    </button>
  )
}

// Save button that saves files to the backend
function SaveButton({ versionId, onSave }: { versionId: string; onSave?: () => void }) {
  const { sandpack } = useSandpack()
  const [saving, setSaving] = useState(false)
  const [saved, setSaved] = useState(false)

  const handleSave = async () => {
    if (!versionId || saving) return
    
    setSaving(true)
    try {
      // Get all files from Sandpack
      const files = Object.entries(sandpack.files).map(([path, file]) => ({
        path: path.startsWith('/') ? path.slice(1) : path,
        content: (file as { code: string }).code || '',
      }))
      
      // Persist only code files and normalize back to backend's src/* convention
      const backendFiles = files
        .filter(f => {
          if (!f.content.trim()) return false
          if (f.path === 'public/index.html') return false
          if (f.path === 'package.json' || f.path === 'tsconfig.json') return false
          // Save typical source files
          return /\.(tsx|ts|css|json)$/.test(f.path)
        })
        .map(f => ({
          // Sandpack is root-based; backend expects src/*
          path: f.path.startsWith('src/') ? f.path : `src/${f.path}`,
          content: f.content,
        }))
      
      await api.post(`/versions/${versionId}/save-files/`, { files: backendFiles })
      setSaved(true)
      setTimeout(() => setSaved(false), 2000)
      onSave?.()
    } catch (error) {
      console.error('Save error:', error)
    } finally {
      setSaving(false)
    }
  }

  return (
    <button
      onClick={handleSave}
      disabled={saving || !versionId}
      className={cn(
        'flex items-center gap-1.5 px-3 py-1.5 rounded-md text-xs font-medium transition-colors',
        saved
          ? 'bg-green-100 text-green-700'
          : 'bg-gray-900 text-white hover:bg-gray-800 disabled:opacity-50'
      )}
      title="Save changes"
    >
      {saving ? (
        <Loader2 className="h-3.5 w-3.5 animate-spin" />
      ) : saved ? (
        <CheckCircle2 className="h-3.5 w-3.5" />
      ) : (
        <Play className="h-3.5 w-3.5" />
      )}
      {saving ? 'Saving...' : saved ? 'Saved!' : 'Save'}
    </button>
  )
}

// Main SandpackPreview component
export function SandpackPreview({
  files,
  appId,
  versionId = '',
  appName = 'App',
  className = '',
}: SandpackPreviewProps) {
  const [viewMode, setViewMode] = useState<ViewMode>('preview')
  const [showConsole, setShowConsole] = useState(false)
  const filesKey = useMemo(() => hashFiles(files), [files])
  
  // Track file count to know when to reset Sandpack
  // Only reset when file count changes (new generation) or versionId changes
  const [sandpackKey, setSandpackKey] = useState(() => `${versionId}-${files.length}`)
  const [initializedFiles, setInitializedFiles] = useState<Record<string, string>>({})
  
  // Convert files to Sandpack format - but only update when we want to reset
  const sandpackFiles = useMemo(() => {
    const converted = convertToSandpackFiles(files, appId, versionId, appName)
    return converted
  }, [files, appId, versionId, appName])
  
  // Only reset Sandpack when version changes or file count increases (new generation)
  // This preserves user edits when just switching tabs
  useEffect(() => {
    const newKey = `${versionId}-${files.length}`
    if (newKey !== sandpackKey) {
      setSandpackKey(newKey)
      setInitializedFiles(sandpackFiles)
    }
  }, [versionId, files.length, sandpackFiles, sandpackKey])
  
  // Use initializedFiles if set, otherwise use sandpackFiles
  const filesToUse = Object.keys(initializedFiles).length > 0 ? initializedFiles : sandpackFiles

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
        }}
        theme="light"
      >
        <AutoRunPreview filesKey={filesKey} />
        <AutoRunOnEdit />
        <AutoRunOnTab viewMode={viewMode} />
        {/* Flex container - uses absolute positioning to ensure footer stays pinned */}
        <div className="relative flex flex-col h-full min-h-0">
        {/* Header - fixed at top */}
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

          <div className="flex items-center gap-2">
            <SaveButton versionId={versionId} />
            
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

            <RefreshButton />

            <button
              className="p-1.5 text-gray-400 hover:text-gray-600 hover:bg-gray-100 
                       rounded-md transition-colors"
              title="Open in new tab"
            >
              <ExternalLink className="h-4 w-4" />
            </button>
            </div>
          </div>
        </div>

        {/* Content - fills remaining space (Sandpack editor needs explicit full-height wrappers) */}
        <div className="flex-1 min-h-0 overflow-hidden">
          {viewMode === 'preview' && (
            <div className="h-full min-h-0 overflow-auto">
              <SandpackPreviewPane
                className="h-full"
                showNavigator={false}
                showRefreshButton={false}
                showOpenInCodeSandbox={false}
                style={{ height: '100%', width: '100%' }}
              />
            </div>
          )}

          {viewMode === 'code' && (
            <div className="h-full min-h-0 overflow-auto">
              <SandpackCodeEditor
                className="h-full"
                showTabs
                showLineNumbers
                showInlineErrors
                wrapContent={false}
                style={{ height: '100%', minHeight: '100%', width: '100%' }}
              />
            </div>
          )}

          {viewMode === 'split' && (
            <div className="flex h-full w-full min-h-0">
              <div className="w-1/2 h-full min-h-0 border-r border-gray-200 flex flex-col overflow-hidden">
                <div className="flex-1 min-h-0 overflow-auto">
                  <SandpackCodeEditor
                    className="h-full"
                    showTabs
                    showLineNumbers
                    showInlineErrors
                    wrapContent={false}
                    style={{ height: '100%', minHeight: '100%', width: '100%' }}
                  />
                </div>
              </div>
              <div className="w-1/2 h-full min-h-0 flex flex-col overflow-hidden">
                <div className="flex-1 min-h-0 overflow-auto">
                  <SandpackPreviewPane
                    className="h-full"
                    showNavigator={false}
                    showRefreshButton={false}
                    showOpenInCodeSandbox={false}
                    style={{ height: '100%', width: '100%' }}
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
            <span>{files.length} files generated</span>
            <span className="flex items-center gap-1">
              <Play className="h-2.5 w-2.5" />
              Live Preview
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
}: {
  previewUrl: string
  className?: string
}) {
  const [isLoading, setIsLoading] = useState(true)

  return (
    <div className={cn('flex flex-col h-full bg-gray-100', className)}>
      <div className="flex items-center justify-between px-4 py-3 border-b border-gray-200 bg-white">
        <div className="flex items-center gap-2">
          <Eye className="h-4 w-4 text-gray-500" />
          <span className="text-sm font-medium text-gray-900">Preview</span>
        </div>
        <a
          href={previewUrl}
          target="_blank"
          rel="noopener noreferrer"
          className="p-1.5 text-gray-400 hover:text-gray-600 hover:bg-gray-100 
                   rounded-md transition-colors"
        >
          <ExternalLink className="h-4 w-4" />
        </a>
      </div>

      <div className="flex-1 relative">
        <iframe
          src={previewUrl}
          className="w-full h-full border-0"
          onLoad={() => setIsLoading(false)}
          title="App Preview"
        />

        {isLoading && (
          <div className="absolute inset-0 bg-white/80 flex items-center justify-center">
            <Loader2 className="h-8 w-8 text-gray-700 animate-spin" />
          </div>
        )}
      </div>
    </div>
  )
}

