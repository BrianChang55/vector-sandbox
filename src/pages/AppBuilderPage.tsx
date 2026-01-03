/**
 * App Builder Page
 * 
 * Production-ready vibe coding interface for building internal apps.
 * Three-panel layout: Chat + Preview + Code/Versions
 * 
 * Inspired by Cursor, Lovable, Replit, and v0.
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
import { motion, AnimatePresence } from 'framer-motion'

import { useApp, useAppVersions, useRollback, usePublishApp } from '../hooks/useApps'
import { useAppSelector, useAppDispatch } from '../store/hooks'
import { setSelectedVersion } from '../store/slices/uiSlice'

import { ChatPanel } from '../components/builder/ChatPanel'
import { PreviewPanel } from '../components/builder/PreviewPanel'
import { CodeEditor } from '../components/builder/CodeEditor'
import { VersionsPanel } from '../components/builder/VersionsPanel'
import { Button } from '../components/ui/button'

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
      <div className="flex items-center justify-center h-screen bg-zinc-950">
        <Loader2 className="h-8 w-8 text-violet-500 animate-spin" />
      </div>
    )
  }

  if (!app) {
    return (
      <div className="flex flex-col items-center justify-center h-screen bg-zinc-950">
        <p className="text-zinc-400 mb-4">App not found</p>
        <Button onClick={() => navigate('/apps')} variant="outline">
          Back to Apps
        </Button>
      </div>
    )
  }

  return (
    <div className="flex flex-col h-screen bg-zinc-950 overflow-hidden">
      {/* Top Bar */}
      <header className="flex items-center justify-between px-4 py-2 border-b border-zinc-800/50 bg-zinc-900/50 backdrop-blur-xl">
        <div className="flex items-center gap-3">
          <button
            onClick={() => navigate('/apps')}
            className="p-2 text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800 rounded-lg transition-colors"
          >
            <ArrowLeft className="h-4 w-4" />
          </button>
          
          <div className="flex items-center gap-2">
            <div className="w-8 h-8 rounded-lg bg-gradient-to-br from-violet-500 to-fuchsia-500 
                          flex items-center justify-center">
              <Layers className="h-4 w-4 text-white" />
            </div>
            <div>
              <h1 className="text-sm font-semibold text-zinc-200">{app.name}</h1>
              <div className="flex items-center gap-2">
                <span className={`text-[10px] px-1.5 py-0.5 rounded-full font-medium ${
                  app.status === 'published' 
                    ? 'bg-emerald-500/20 text-emerald-400'
                    : 'bg-zinc-700/50 text-zinc-500'
                }`}>
                  {app.status}
                </span>
                {selectedVersion && (
                  <span className="text-[10px] text-zinc-600">
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
            className="gap-1.5 text-xs border-zinc-700 bg-zinc-800/50"
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
            className="gap-1.5 text-xs bg-gradient-to-r from-violet-600 to-fuchsia-600 
                     hover:from-violet-500 hover:to-fuchsia-500 border-0"
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

          <div className="w-px h-6 bg-zinc-800" />

          <button className="p-2 text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800 rounded-lg transition-colors">
            <Share2 className="h-4 w-4" />
          </button>
          <button className="p-2 text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800 rounded-lg transition-colors">
            <Settings className="h-4 w-4" />
          </button>
          <button className="p-2 text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800 rounded-lg transition-colors">
            <MoreVertical className="h-4 w-4" />
          </button>
        </div>
      </header>

      {/* Main Content - Three Panel Layout */}
      <div className="flex flex-1 overflow-hidden">
        {/* Left Panel - Chat */}
        <motion.div 
          className="w-96 flex-shrink-0 border-r border-zinc-800/50"
          initial={{ x: -20, opacity: 0 }}
          animate={{ x: 0, opacity: 1 }}
          transition={{ duration: 0.2 }}
        >
          <ChatPanel
            appId={appId!}
            sessionId={sessionId}
            onSessionChange={setSessionId}
            onVersionCreated={handleVersionCreated}
            className="h-full"
          />
        </motion.div>

        {/* Center Panel - Preview */}
        <motion.div 
          className="flex-1 min-w-0"
          initial={{ y: 20, opacity: 0 }}
          animate={{ y: 0, opacity: 1 }}
          transition={{ duration: 0.2, delay: 0.1 }}
        >
          <PreviewPanel
            appId={appId!}
            versionId={selectedVersionId}
            className="h-full"
          />
        </motion.div>

        {/* Right Panel - Code/Versions */}
        <motion.div 
          className="w-96 flex-shrink-0 border-l border-zinc-800/50 flex flex-col"
          initial={{ x: 20, opacity: 0 }}
          animate={{ x: 0, opacity: 1 }}
          transition={{ duration: 0.2, delay: 0.2 }}
        >
          {/* Panel Tabs */}
          <div className="flex border-b border-zinc-800/50">
            {[
              { key: 'code' as RightPanelView, icon: Code2, label: 'Code' },
              { key: 'versions' as RightPanelView, icon: Clock, label: 'Versions' },
            ].map(({ key, icon: Icon, label }) => (
              <button
                key={key}
                onClick={() => setRightPanelView(key)}
                className={`flex-1 flex items-center justify-center gap-2 px-4 py-3 text-sm
                          transition-colors relative ${
                            rightPanelView === key
                              ? 'text-zinc-200'
                              : 'text-zinc-500 hover:text-zinc-300'
                          }`}
              >
                <Icon className="h-4 w-4" />
                {label}
                {rightPanelView === key && (
                  <motion.div
                    layoutId="rightPanelTab"
                    className="absolute bottom-0 left-0 right-0 h-0.5 bg-violet-500"
                  />
                )}
              </button>
            ))}
          </div>

          {/* Panel Content */}
          <div className="flex-1 overflow-hidden">
            <AnimatePresence mode="wait">
              {rightPanelView === 'code' ? (
                <motion.div
                  key="code"
                  initial={{ opacity: 0 }}
                  animate={{ opacity: 1 }}
                  exit={{ opacity: 0 }}
                  className="h-full"
                >
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
                </motion.div>
              ) : (
                <motion.div
                  key="versions"
                  initial={{ opacity: 0 }}
                  animate={{ opacity: 1 }}
                  exit={{ opacity: 0 }}
                  className="h-full"
                >
                  <VersionsPanel
                    versions={versions || []}
                    selectedVersionId={selectedVersionId}
                    onVersionSelect={handleVersionSelect}
                    onRollback={handleRollback}
                    className="h-full"
                  />
                </motion.div>
              )}
            </AnimatePresence>
          </div>
        </motion.div>
      </div>
    </div>
  )
}
