/**
 * Preview Panel Component
 * 
 * Live preview of the generated internal app.
 * Supports responsive viewport and refresh controls.
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
import { motion } from 'framer-motion'

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
    <div ref={containerRef} className={`flex flex-col h-full bg-zinc-950 ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between px-4 py-3 border-b border-zinc-800/50">
        <div className="flex items-center gap-3">
          <span className="text-sm font-semibold text-zinc-200">Preview</span>
          
          {/* Viewport Selector */}
          <div className="flex items-center gap-1 bg-zinc-800/50 rounded-lg p-1">
            {[
              { key: 'mobile' as Viewport, icon: Smartphone },
              { key: 'tablet' as Viewport, icon: Tablet },
              { key: 'desktop' as Viewport, icon: Monitor },
            ].map(({ key, icon: Icon }) => (
              <button
                key={key}
                onClick={() => setViewport(key)}
                className={`p-1.5 rounded-md transition-colors ${
                  viewport === key
                    ? 'bg-zinc-700 text-zinc-200'
                    : 'text-zinc-500 hover:text-zinc-300'
                }`}
                title={viewportSizes[key].label}
              >
                <Icon className="h-4 w-4" />
              </button>
            ))}
          </div>
          
          <span className="text-[10px] text-zinc-600">
            {viewportConfig.width} × {viewportConfig.height}
          </span>
        </div>

        <div className="flex items-center gap-1">
          {/* Auto-refresh toggle */}
          <button
            onClick={() => setAutoRefresh(!autoRefresh)}
            className={`p-1.5 rounded-md transition-colors ${
              autoRefresh
                ? 'bg-emerald-500/20 text-emerald-400'
                : 'text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800'
            }`}
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
            className="p-1.5 text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800 
                     disabled:opacity-50 disabled:cursor-not-allowed rounded-md transition-colors"
            title="Refresh preview"
          >
            <RefreshCw className={`h-4 w-4 ${isLoading ? 'animate-spin' : ''}`} />
          </button>
          
          <button
            onClick={handleFullscreen}
            className="p-1.5 text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800 rounded-md transition-colors"
            title="Fullscreen"
          >
            <Maximize2 className="h-4 w-4" />
          </button>
          
          <button
            onClick={handleOpenExternal}
            disabled={!previewUrl}
            className="p-1.5 text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800 
                     disabled:opacity-50 disabled:cursor-not-allowed rounded-md transition-colors"
            title="Open in new tab"
          >
            <ExternalLink className="h-4 w-4" />
          </button>
        </div>
      </div>

      {/* Preview Area */}
      <div className="flex-1 flex items-center justify-center overflow-auto p-4 bg-[#1a1a1a]">
        {previewUrl ? (
          <motion.div
            layout
            className="relative bg-white rounded-lg shadow-2xl overflow-hidden"
            style={{
              width: viewport === 'desktop' ? '100%' : viewportConfig.width,
              height: viewport === 'desktop' ? '100%' : viewportConfig.height,
              maxWidth: '100%',
              maxHeight: '100%',
              transform: viewport !== 'desktop' ? `scale(${scale})` : undefined,
              transformOrigin: 'center center',
            }}
          >
            {/* Device Frame */}
            {viewport !== 'desktop' && (
              <div className="absolute inset-0 pointer-events-none z-10">
                <div className="absolute top-0 left-1/2 -translate-x-1/2 w-24 h-6 bg-black rounded-b-xl" />
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
              <div className="absolute inset-0 bg-zinc-900/80 flex items-center justify-center">
                <RefreshCw className="h-8 w-8 text-violet-400 animate-spin" />
              </div>
            )}
          </motion.div>
        ) : (
          <div className="flex flex-col items-center justify-center text-center">
            <div className="w-20 h-20 rounded-2xl bg-zinc-800/50 flex items-center justify-center mb-4">
              <Monitor className="h-10 w-10 text-zinc-600" />
            </div>
            <h3 className="text-lg font-semibold text-zinc-400 mb-2">
              No Preview Available
            </h3>
            <p className="text-sm text-zinc-600 max-w-xs">
              Generate your first version using the AI builder to see a live preview.
            </p>
          </div>
        )}
      </div>

      {/* Status Bar */}
      {versionId && (
        <div className="flex items-center justify-between px-4 py-2 border-t border-zinc-800/50 
                      text-[10px] text-zinc-600">
          <span>Version: {versionId.slice(0, 8)}...</span>
          <span className="flex items-center gap-1">
            <span className={`w-1.5 h-1.5 rounded-full ${isLoading ? 'bg-amber-500' : 'bg-emerald-500'}`} />
            {isLoading ? 'Loading...' : 'Ready'}
          </span>
        </div>
      )}
    </div>
  )
}

