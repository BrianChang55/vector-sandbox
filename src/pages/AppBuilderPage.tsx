/**
 * App Builder Page
 * 
 * Production-ready vibe coding interface for building internal apps.
 * Three-panel layout: Chat + Preview + Code/Versions
 * 
 * Light enterprise theme matching the rest of the application.
 */
import { useState, useEffect, useCallback } from 'react'
import { useParams, useNavigate } from 'react-router-dom'
import { 
  ArrowLeft, 
  Settings, 
  Share2, 
  MoreVertical,
  Layers,
  Code2,
  Clock,
  Play,
  Rocket,
  Loader2,
  CheckCircle
} from 'lucide-react'

import { useApp, useAppVersions, useRollback, usePublishApp } from '../hooks/useApps'
import { useAppSelector, useAppDispatch } from '../store/hooks'
import { setSelectedVersion } from '../store/slices/uiSlice'

import { ChatPanel } from '../components/builder/ChatPanel'
import { PreviewPanel } from '../components/builder/PreviewPanel'
import { CodeEditor } from '../components/builder/CodeEditor'
import { VersionsPanel } from '../components/builder/VersionsPanel'
import { Button } from '../components/ui/button'
import { cn } from '../lib/utils'

type RightPanelView = 'code' | 'versions'

export function AppBuilderPage() {
  const { appId } = useParams<{ appId: string }>()
  const navigate = useNavigate()
  const dispatch = useAppDispatch()
  
  const { data: app, isLoading: appLoading } = useApp(appId || null)
  const { data: versions, refetch: refetchVersions } = useAppVersions(appId || null)
  const rollback = useRollback()
  const publishApp = usePublishApp()
  
  const selectedVersionId = useAppSelector((state) => state.ui.selectedVersionId)
  
  const [sessionId, setSessionId] = useState<string | null>(null)
  const [rightPanelView, setRightPanelView] = useState<RightPanelView>('code')
  const [isPublishing, setIsPublishing] = useState(false)

  // Select latest version by default
  useEffect(() => {
    if (versions && versions.length > 0 && !selectedVersionId) {
      dispatch(setSelectedVersion(versions[0].id))
    }
  }, [versions, selectedVersionId, dispatch])

  const selectedVersion = versions?.find((v) => v.id === selectedVersionId) || versions?.[0]

  const handleVersionCreated = useCallback((versionId: string, _versionNumber: number) => {
    dispatch(setSelectedVersion(versionId))
    refetchVersions()
  }, [dispatch, refetchVersions])

  const handleVersionSelect = useCallback((versionId: string) => {
    dispatch(setSelectedVersion(versionId))
  }, [dispatch])

  const handleRollback = useCallback(async (versionId: string) => {
    try {
      const result = await rollback.mutateAsync(versionId)
      dispatch(setSelectedVersion(result.id))
      refetchVersions()
    } catch (error) {
      console.error('Rollback failed:', error)
    }
  }, [rollback, dispatch, refetchVersions])

  const handlePublish = async () => {
    if (!appId) return
    setIsPublishing(true)
    try {
      await publishApp.mutateAsync(appId)
      refetchVersions()
    } catch (error) {
      console.error('Publish failed:', error)
    } finally {
      setIsPublishing(false)
    }
  }

  if (appLoading) {
    return (
      <div className="flex items-center justify-center h-screen bg-gray-50">
        <Loader2 className="h-8 w-8 text-gray-400 animate-spin" />
      </div>
    )
  }

  if (!app) {
    return (
      <div className="flex flex-col items-center justify-center h-screen bg-gray-50">
        <p className="text-gray-700 mb-4">App not found</p>
        <Button onClick={() => navigate('/apps')} variant="outline">
          Back to Apps
        </Button>
      </div>
    )
  }

  return (
    <div className="flex flex-col h-screen bg-gray-50 overflow-hidden">
      {/* Top Bar */}
      <header className="flex items-center justify-between px-4 py-2 border-b border-gray-200 bg-white">
        <div className="flex items-center gap-3">
          <button
            onClick={() => navigate('/apps')}
            className="p-2 text-gray-400 hover:text-gray-600 hover:bg-gray-100 rounded-md transition-colors"
          >
            <ArrowLeft className="h-4 w-4" />
          </button>
          
          <div className="flex items-center gap-2">
            <div className="w-8 h-8 rounded-md bg-gray-100 flex items-center justify-center">
              <Layers className="h-4 w-4 text-gray-500" />
            </div>
            <div>
              <h1 className="text-sm font-semibold text-gray-900">{app.name}</h1>
              <div className="flex items-center gap-2">
                <span className={cn(
                  'text-[10px] px-1.5 py-0.5 rounded-full font-medium',
                  app.status === 'published' 
                    ? 'bg-green-50 text-green-700 border border-green-200'
                    : 'bg-gray-100 text-gray-600'
                )}>
                  {app.status}
                </span>
                {selectedVersion && (
                  <span className="text-[10px] text-gray-600">
                    v{selectedVersion.version_number}
                  </span>
                )}
              </div>
            </div>
          </div>
        </div>

        <div className="flex items-center gap-2">
          {/* Run/Preview button */}
          <Button
            variant="outline"
            size="sm"
            className="gap-1.5 text-xs"
            onClick={() => {
              if (selectedVersion) {
                window.open(`/preview/apps/${appId}?version=${selectedVersion.id}`, '_blank')
              }
            }}
            disabled={!selectedVersion}
          >
            <Play className="h-3.5 w-3.5" />
            Preview
          </Button>

          {/* Publish button */}
          <Button
            size="sm"
            className="gap-1.5 text-xs"
            onClick={handlePublish}
            disabled={isPublishing || !selectedVersion}
          >
            {isPublishing ? (
              <Loader2 className="h-3.5 w-3.5 animate-spin" />
            ) : app.status === 'published' ? (
              <CheckCircle className="h-3.5 w-3.5" />
            ) : (
              <Rocket className="h-3.5 w-3.5" />
            )}
            {isPublishing ? 'Publishing...' : app.status === 'published' ? 'Published' : 'Publish'}
          </Button>

          <div className="w-px h-6 bg-gray-200" />

          <button className="p-2 text-gray-400 hover:text-gray-600 hover:bg-gray-100 rounded-md transition-colors">
            <Share2 className="h-4 w-4" />
          </button>
          <button className="p-2 text-gray-400 hover:text-gray-600 hover:bg-gray-100 rounded-md transition-colors">
            <Settings className="h-4 w-4" />
          </button>
          <button className="p-2 text-gray-400 hover:text-gray-600 hover:bg-gray-100 rounded-md transition-colors">
            <MoreVertical className="h-4 w-4" />
          </button>
        </div>
      </header>

      {/* Main Content - Three Panel Layout */}
      <div className="flex flex-1 overflow-hidden">
        {/* Left Panel - Chat */}
        <div className="w-96 flex-shrink-0 border-r border-gray-200 bg-white">
          <ChatPanel
            appId={appId!}
            sessionId={sessionId}
            onSessionChange={setSessionId}
            onVersionCreated={handleVersionCreated}
            className="h-full"
          />
        </div>

        {/* Center Panel - Preview */}
        <div className="flex-1 min-w-0">
          <PreviewPanel
            appId={appId!}
            versionId={selectedVersionId}
            className="h-full"
          />
        </div>

        {/* Right Panel - Code/Versions */}
        <div className="w-96 flex-shrink-0 border-l border-gray-200 bg-white flex flex-col">
          {/* Panel Tabs */}
          <div className="flex border-b border-gray-200">
            {[
              { key: 'code' as RightPanelView, icon: Code2, label: 'Code' },
              { key: 'versions' as RightPanelView, icon: Clock, label: 'Versions' },
            ].map(({ key, icon: Icon, label }) => (
              <button
                key={key}
                onClick={() => setRightPanelView(key)}
                className={cn(
                  'flex-1 flex items-center justify-center gap-2 px-4 py-3 text-sm transition-colors relative',
                  rightPanelView === key
                    ? 'text-gray-900'
                    : 'text-gray-500 hover:text-gray-700'
                )}
              >
                <Icon className="h-4 w-4" />
                {label}
                {rightPanelView === key && (
                  <div className="absolute bottom-0 left-0 right-0 h-0.5 bg-gray-900" />
                )}
              </button>
            ))}
          </div>

          {/* Panel Content */}
          <div className="flex-1 overflow-hidden">
            {rightPanelView === 'code' ? (
              <CodeEditor
                files={selectedVersion?.files || []}
                previousFiles={
                  versions && versions.length > 1 && selectedVersion
                    ? versions.find(v => v.version_number === selectedVersion.version_number - 1)?.files
                    : undefined
                }
                readOnly
                className="h-full"
              />
            ) : (
              <VersionsPanel
                versions={versions || []}
                selectedVersionId={selectedVersionId}
                onVersionSelect={handleVersionSelect}
                onRollback={handleRollback}
                className="h-full"
              />
            )}
          </div>
        </div>
      </div>
    </div>
  )
}
