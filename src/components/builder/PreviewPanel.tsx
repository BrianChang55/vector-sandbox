/**
 * Preview Panel Component
 * 
 * Live preview of the generated internal app.
 * Supports responsive viewport and refresh controls.
 * Light enterprise theme.
 */
import { useState, useRef, useEffect } from 'react'
import { 
  RefreshCw, 
  Smartphone, 
  Tablet, 
  Monitor, 
  ExternalLink,
  Maximize2,
  Play,
  Pause
} from 'lucide-react'
import { cn } from '../../lib/utils'

interface PreviewPanelProps {
  appId: string
  versionId: string | null
  className?: string
}

type Viewport = 'mobile' | 'tablet' | 'desktop'

const viewportSizes: Record<Viewport, { width: number; height: number; label: string }> = {
  mobile: { width: 375, height: 667, label: 'Mobile' },
  tablet: { width: 768, height: 1024, label: 'Tablet' },
  desktop: { width: 1280, height: 800, label: 'Desktop' },
}

export function PreviewPanel({ appId, versionId, className = '' }: PreviewPanelProps) {
  const [viewport, setViewport] = useState<Viewport>('desktop')
  const [isLoading, setIsLoading] = useState(false)
  const [autoRefresh, setAutoRefresh] = useState(true)
  const iframeRef = useRef<HTMLIFrameElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  const previewUrl = versionId 
    ? `/preview/apps/${appId}?version=${versionId}`
    : null

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

  const viewportConfig = viewportSizes[viewport]
  const scale = viewport === 'desktop' ? 1 : 0.75

  return (
    <div ref={containerRef} className={`flex flex-col h-full bg-gray-100 ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between px-4 py-3 border-b border-gray-200 bg-white">
        <div className="flex items-center gap-3">
          <span className="text-sm font-semibold text-gray-900">Preview</span>
          
          {/* Viewport Selector */}
          <div className="flex items-center gap-1 bg-gray-100 rounded-md p-1">
            {[
              { key: 'mobile' as Viewport, icon: Smartphone },
              { key: 'tablet' as Viewport, icon: Tablet },
              { key: 'desktop' as Viewport, icon: Monitor },
            ].map(({ key, icon: Icon }) => (
              <button
                key={key}
                onClick={() => setViewport(key)}
                className={cn(
                  'p-1.5 rounded transition-colors',
                  viewport === key
                    ? 'bg-white shadow-sm text-gray-900'
                    : 'text-gray-400 hover:text-gray-600'
                )}
                title={viewportSizes[key].label}
              >
                <Icon className="h-4 w-4" />
              </button>
            ))}
          </div>
          
          <span className="text-[10px] text-gray-600">
            {viewportConfig.width} × {viewportConfig.height}
          </span>
        </div>

        <div className="flex items-center gap-1">
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
        </div>
      </div>

      {/* Preview Area */}
      <div className="flex-1 flex items-center justify-center overflow-auto p-4">
        {previewUrl ? (
          <div
            className="relative bg-white rounded-lg shadow-lg overflow-hidden border border-gray-200"
            style={{
              width: viewport === 'desktop' ? '100%' : viewportConfig.width,
              height: viewport === 'desktop' ? '100%' : viewportConfig.height,
              maxWidth: '100%',
              maxHeight: '100%',
              transform: viewport !== 'desktop' ? `scale(${scale})` : undefined,
              transformOrigin: 'center center',
            }}
          >
            {/* Device Frame for mobile/tablet */}
            {viewport !== 'desktop' && (
              <div className="absolute inset-0 pointer-events-none z-10">
                <div className="absolute top-0 left-1/2 -translate-x-1/2 w-24 h-6 bg-gray-900 rounded-b-xl" />
              </div>
            )}
            
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

      {/* Status Bar */}
      {versionId && (
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
