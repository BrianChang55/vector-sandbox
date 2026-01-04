/**
 * Preview Panel Component
 * 
 * Live preview of the generated internal app with code view toggle.
 * Light enterprise theme.
 */
import { useState, useRef, useEffect } from 'react'
import { 
  RefreshCw, 
  Monitor, 
  ExternalLink,
  Maximize2,
  Play,
  Pause,
  Eye,
  Code2
} from 'lucide-react'
import { CodeEditor } from './CodeEditor'
import { cn } from '../../lib/utils'
import { buildPreviewUrl } from '../../lib/preview'

interface VersionFile {
  id: string
  path: string
  content: string
}

interface PreviewPanelProps {
  appId: string
  versionId: string | null
  versionFiles?: VersionFile[]
  previousVersionFiles?: VersionFile[]
  className?: string
  previewOnly?: boolean
}

type ViewMode = 'preview' | 'code'

export function PreviewPanel({ 
  appId, 
  versionId, 
  versionFiles = [],
  previousVersionFiles,
  className = '',
  previewOnly = false,
}: PreviewPanelProps) {
  const [viewMode, setViewMode] = useState<ViewMode>('preview')
  const [isLoading, setIsLoading] = useState(false)
  const [autoRefresh, setAutoRefresh] = useState(true)
  const iframeRef = useRef<HTMLIFrameElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  // If no versionId, backend will serve latest version
  const previewUrl = buildPreviewUrl(appId, versionId || undefined)

  const handleRefresh = () => {
    if (iframeRef.current && previewUrl) {
      setIsLoading(true)
      iframeRef.current.src = previewUrl
    }
  }

  const handleFullscreen = () => {
    if (containerRef.current) {
      if (!document.fullscreenElement) {
        containerRef.current.requestFullscreen()
      } else {
        document.exitFullscreen()
      }
    }
  }

  const handleOpenExternal = () => {
    if (previewUrl) {
      window.open(previewUrl, '_blank')
    }
  }

  // Auto-refresh when version changes
  useEffect(() => {
    if (autoRefresh && previewUrl) {
      handleRefresh()
    }
  }, [versionId, autoRefresh])

  // Force preview mode when previewOnly is enabled
  useEffect(() => {
    if (previewOnly && viewMode !== 'preview') {
      setViewMode('preview')
    }
  }, [previewOnly, viewMode])

  return (
    <div ref={containerRef} className={`flex flex-col h-full bg-gray-100 ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between px-4 py-3 border-b border-gray-200 bg-white">
        <div className="flex items-center gap-3">
          {/* Preview/Code Toggle (hidden in preview-only mode) */}
          {!previewOnly && (
            <div className="flex items-center gap-1 bg-gray-100 rounded-md p-1">
              {[
                { key: 'preview' as ViewMode, icon: Eye, label: 'Preview' },
                { key: 'code' as ViewMode, icon: Code2, label: 'Code' },
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
          )}
        </div>

        <div className="flex items-center gap-1">
          {viewMode === 'preview' && (
            <>
              {/* Auto-refresh toggle */}
              <button
                onClick={() => setAutoRefresh(!autoRefresh)}
                className={cn(
                  'p-1.5 rounded-md transition-colors',
                  autoRefresh
                    ? 'bg-green-50 text-green-600'
                    : 'text-gray-400 hover:text-gray-600 hover:bg-gray-100'
                )}
                title={autoRefresh ? 'Auto-refresh on' : 'Auto-refresh off'}
              >
                {autoRefresh ? (
                  <Play className="h-4 w-4" />
                ) : (
                  <Pause className="h-4 w-4" />
                )}
              </button>
              
              <button
                onClick={handleRefresh}
                disabled={!previewUrl}
                className="p-1.5 text-gray-400 hover:text-gray-600 hover:bg-gray-100 
                         disabled:opacity-50 disabled:cursor-not-allowed rounded-md transition-colors"
                title="Refresh preview"
              >
                <RefreshCw className={cn('h-4 w-4', isLoading && 'animate-spin')} />
              </button>
              
              <button
                onClick={handleFullscreen}
                className="p-1.5 text-gray-400 hover:text-gray-600 hover:bg-gray-100 rounded-md transition-colors"
                title="Fullscreen"
              >
                <Maximize2 className="h-4 w-4" />
              </button>
              
              <button
                onClick={handleOpenExternal}
                disabled={!previewUrl}
                className="p-1.5 text-gray-400 hover:text-gray-600 hover:bg-gray-100 
                         disabled:opacity-50 disabled:cursor-not-allowed rounded-md transition-colors"
                title="Open in new tab"
              >
                <ExternalLink className="h-4 w-4" />
              </button>
            </>
          )}
        </div>
      </div>

      {/* Content Area */}
      <div className="flex-1 overflow-hidden">
        {viewMode === 'preview' ? (
          <div className="flex items-center justify-center h-full p-4">
            {previewUrl ? (
              <div
                className="relative bg-white rounded-lg shadow-lg overflow-hidden border border-gray-200 w-full h-full"
              >
                <iframe
                  ref={iframeRef}
                  src={previewUrl}
                  className="w-full h-full border-0"
                  onLoad={() => setIsLoading(false)}
                  title="App Preview"
                />
                
                {/* Loading overlay */}
                {isLoading && (
                  <div className="absolute inset-0 bg-white/80 flex items-center justify-center">
                    <RefreshCw className="h-8 w-8 text-gray-400 animate-spin" />
                  </div>
                )}
              </div>
            ) : (
              <div className="flex flex-col items-center justify-center text-center">
                <div className="h-20 w-20 rounded-xl bg-gray-200 flex items-center justify-center mb-4">
                  <Monitor className="h-10 w-10 text-gray-500" />
                </div>
                <h3 className="text-lg font-medium text-gray-900 mb-2">
                  No Preview Available
                </h3>
                <p className="text-sm text-gray-700 max-w-xs">
                  Generate your first version using the AI builder to see a live preview.
                </p>
              </div>
            )}
          </div>
        ) : (
          <CodeEditor
            files={versionFiles}
            previousFiles={previousVersionFiles}
            readOnly
            className="h-full"
          />
        )}
      </div>

      {/* Status Bar */}
      {versionId && viewMode === 'preview' && (
        <div className="flex items-center justify-between px-4 py-2 border-t border-gray-200 bg-white
                      text-[10px] text-gray-700">
          <span>Version: {versionId.slice(0, 8)}...</span>
          <span className="flex items-center gap-1">
            <span className={cn(
              'w-1.5 h-1.5 rounded-full',
              isLoading ? 'bg-yellow-500' : 'bg-green-500'
            )} />
            {isLoading ? 'Loading...' : 'Ready'}
          </span>
        </div>
      )}
    </div>
  )
}
