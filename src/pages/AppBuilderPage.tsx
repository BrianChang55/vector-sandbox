/**
 * App Builder Page
 * 
 * Production-ready agentic coding interface for building internal apps.
 * Two-panel layout: Agentic Chat + Live Preview
 * 
 * Features:
 * - Research → Plan → Execute → Validate workflow
 * - Real-time thinking and progress visibility
 * - Sandpack-based live React app preview
 * - App Data Store management (tables, rows, queries)
 * 
 * Light enterprise theme matching the rest of the application.
 */
import { useState, useEffect, useCallback, useMemo, useRef, useLayoutEffect } from 'react'
import { useParams, useNavigate, useSearchParams } from 'react-router-dom'
import { 
  ArrowLeft, 
  Layers,
  Play,
  Loader2,
  Code2,
  Database,
  Plug,
} from 'lucide-react'

import { useApp, useAppVersions, usePublishApp, useRollback } from '../hooks/useApps'
import { useOrgMembers } from '../hooks/useMembers'
import { useAppSelector, useAppDispatch } from '../store/hooks'
import { setSelectedVersion } from '../store/slices/uiSlice'

import { AgenticChatPanel } from '../components/builder/AgenticChatPanel'
import { PreviewPanel } from '../components/builder/PreviewPanel'
import { SandpackPreview } from '../components/builder/SandpackPreview'
import { VersionsPanel } from '../components/builder/VersionsPanel'
import { PublishPopover } from '../components/builder/PublishPopover'
import { DataPanel } from '../components/data'
import { AppConnectorsPanel } from '../components/connectors'
import { Button } from '../components/ui/button'
import { useToast, toast } from '../components/ui/toast'
import { cn } from '../lib/utils'
import type { FileChange } from '../types/agent'
import type { BundlerError } from '../hooks/useSandpackValidation'
import { upsertFileChange } from '../services/agentService'

type AppTab = 'builder' | 'data' | 'integrations'

export function AppBuilderPage() {
  const { appId } = useParams<{ appId: string }>()
  const [searchParams, setSearchParams] = useSearchParams()
  const navigate = useNavigate()
  const dispatch = useAppDispatch()
  
  const { data: app, isLoading: appLoading, refetch: refetchApp } = useApp(appId || null)
  const { data: versions, refetch: refetchVersions } = useAppVersions(appId || null, { includeFiles: true })
  
  // Get current user's role for permission checks
  const selectedOrgId = useAppSelector((state) => state.ui.selectedOrgId)
  const { data: membersData } = useOrgMembers(selectedOrgId)
  const currentUserRole = membersData?.current_user_role
  const isViewer = currentUserRole === 'viewer'
  
  const publishApp = usePublishApp()
  const rollbackMutation = useRollback()
  const { addToast } = useToast()
  const toastHelpers = toast(addToast)
  
  const selectedVersionId = useAppSelector((state) => state.ui.selectedVersionId)
  
  // Redirect viewers to the published app or preview page
  useEffect(() => {
    if (isViewer && app) {
      if (app.published_url) {
        navigate(app.published_url, { replace: true })
      } else {
        // If not published, redirect to preview
        navigate(`/preview/apps/${app.id}`, { replace: true })
      }
    }
  }, [isViewer, app, navigate])
  
  // Tab state from URL
  const activeTab = (searchParams.get('tab') as AppTab) || 'builder'
  const setActiveTab = (tab: AppTab) => {
    setSearchParams({ tab })
  }
  
  const [sessionId, setSessionId] = useState<string | null>(null)
  const [isPublishing, setIsPublishing] = useState(false)
  
  // Pending prompt from landing page (visible - goes in input box)
  const [pendingPrompt, setPendingPrompt] = useState<string | null>(null)
  
  // Hidden prompt from templates (invisible - auto-submitted, never displayed)
  const [pendingHiddenPrompt, setPendingHiddenPrompt] = useState<string | null>(null)
  
  // Check for pending prompts on mount (use layout effect to run before render)
  useLayoutEffect(() => {
    // Check for hidden prompt from template (takes priority)
    const storedHiddenPromptRaw = localStorage.getItem('pending_hidden_prompt')
    if (storedHiddenPromptRaw) {
      let validHiddenPrompt: string | null = null
      try {
        const parsed = JSON.parse(storedHiddenPromptRaw)
        const age = Date.now() - parsed.timestamp
        // Hidden prompts have 5-minute expiration
        if (parsed.hiddenPrompt && parsed.isHidden && parsed.timestamp && age <= 5 * 60 * 1000) {
          validHiddenPrompt = parsed.hiddenPrompt
        }
      } catch {
        // Invalid JSON format
      }
      if (validHiddenPrompt) {
        setPendingHiddenPrompt(validHiddenPrompt)
        // Don't set pending prompt - hidden prompts are auto-submitted
        return
      } else {
        localStorage.removeItem('pending_hidden_prompt')
      }
    }
    
    // Check for regular pending prompt (visible in input box)
    const storedPromptRaw = localStorage.getItem('pending_prompt')
    if (storedPromptRaw) {
      let validPrompt: string | null = null
      try {
        const parsed = JSON.parse(storedPromptRaw)
        const age = Date.now() - parsed.timestamp
        if (parsed.prompt && parsed.timestamp && age <= 5 * 60 * 1000) {
          validPrompt = parsed.prompt
        }
      } catch {
        // Invalid JSON format
      }
      if (validPrompt) {
        setPendingPrompt(validPrompt)
      } else {
        localStorage.removeItem('pending_prompt')
      }
    }
  }, [])
  
  // Callback when initial prompt is consumed by chat panel
  const handleInitialPromptConsumed = useCallback(() => {
    localStorage.removeItem('pending_prompt')
    setPendingPrompt(null)
  }, [])
  
  // Callback when hidden prompt is consumed by chat panel
  const handleHiddenPromptConsumed = useCallback(() => {
    localStorage.removeItem('pending_hidden_prompt')
    setPendingHiddenPrompt(null)
  }, [])
  const [streamingFiles, setStreamingFiles] = useState<FileChange[]>([]) // Files from active generation
  const [isActivelyGenerating, setIsActivelyGenerating] = useState(false) // Track if generation is in progress
  const [publishError, setPublishError] = useState<string | null>(null)
  const [showVersionsSidebar, setShowVersionsSidebar] = useState(false)
  const [activeGeneratingVersionId, setActiveGeneratingVersionId] = useState<string | null>(null)
  // Track whether auto-fix should be enabled (stays true for a window after generation completes)
  const [autoFixEnabled, setAutoFixEnabled] = useState(false)
  const autoFixTimeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null)
  // Track bundler errors for auto-fix (passed to chat panel)
  const [bundlerErrors, setBundlerErrors] = useState<BundlerError[] | undefined>(undefined)
  
  // Resizable chat panel
  const [chatPanelWidth, setChatPanelWidth] = useState(420)
  const [isResizing, setIsResizing] = useState(false)
  const MIN_CHAT_WIDTH = 340
  const MAX_CHAT_WIDTH = 640
  
  // Handle chat panel resize
  const handleResizeMouseDown = useCallback((e: React.MouseEvent) => {
    e.preventDefault()
    setIsResizing(true)
    document.body.style.cursor = 'col-resize'
    document.body.style.userSelect = 'none'
    
    const handleMouseMove = (e: MouseEvent) => {
      const newWidth = Math.min(MAX_CHAT_WIDTH, Math.max(MIN_CHAT_WIDTH, e.clientX))
      setChatPanelWidth(newWidth)
    }
    
    const handleMouseUp = () => {
      setIsResizing(false)
      document.body.style.cursor = ''
      document.body.style.userSelect = ''
      document.removeEventListener('mousemove', handleMouseMove)
      document.removeEventListener('mouseup', handleMouseUp)
    }
    
    document.addEventListener('mousemove', handleMouseMove)
    document.addEventListener('mouseup', handleMouseUp)
  }, [])

  // Select latest version by default
  useEffect(() => {
    if (versions && versions.length > 0 && !selectedVersionId) {
      dispatch(setSelectedVersion(versions[0].id))
    }
  }, [versions, selectedVersionId, dispatch])

  const selectedVersion = versions?.find((v) => v.id === selectedVersionId) || versions?.[0]
  const latestVersion = versions?.[0]
  const latestVersionGenerating = latestVersion?.generation_status && latestVersion.generation_status !== 'complete'
  const canPublish = !!selectedVersion && !isPublishing && !latestVersionGenerating

  // Compute files for preview from selected version (synchronous, no lag)
  const versionFiles = useMemo(() => {
    const versionToLoad = versions?.find((v) => v.id === selectedVersionId) || versions?.[0]
    
    if (!versionToLoad?.files || versionToLoad.files.length === 0) {
      return []
    }
    
    // Convert version files to FileChange format for SandpackPreview
    return versionToLoad.files.reduce(
      (acc: FileChange[], f: { path: string; content: string }) => {
        const ext = f.path.split('.').pop()?.toLowerCase()
        const language: FileChange['language'] =
          ext === 'css'
            ? 'css'
            : ext === 'json'
              ? 'json'
              : ext === 'html'
                ? 'html'
                : ext === 'ts'
                  ? 'ts'
                  : 'tsx'

        return upsertFileChange(acc, {
          path: f.path,
          content: f.content,
          action: 'create',
          language,
        })
      },
      []
    )
  }, [selectedVersionId, versions])
  
  // Use streaming files during active generation, otherwise use version files
  const generatedFiles = isActivelyGenerating ? streamingFiles : versionFiles

  // Reset active generation flag when generation completes
  useEffect(() => {
    if (activeGeneratingVersionId === null && isActivelyGenerating) {
      // Generation finished - wait for versions to refetch before switching to version files
      // This prevents showing stale version files before the new version is fetched
      const handleGenerationComplete = async () => {
        await refetchVersions()
        // Only switch to version files AFTER we've fetched the updated versions
        setIsActivelyGenerating(false)
        setStreamingFiles([])
        
        // Keep auto-fix enabled for 15 seconds after generation completes
        // This gives time for Sandpack to bundle and detect errors
        if (autoFixTimeoutRef.current) {
          clearTimeout(autoFixTimeoutRef.current)
        }
        autoFixTimeoutRef.current = setTimeout(() => {
          setAutoFixEnabled(false)
          autoFixTimeoutRef.current = null
        }, 15000)
      }
      handleGenerationComplete()
    }
  }, [activeGeneratingVersionId, isActivelyGenerating, refetchVersions])
  
  // Clean up timeout on unmount
  useEffect(() => {
    return () => {
      if (autoFixTimeoutRef.current) {
        clearTimeout(autoFixTimeoutRef.current)
      }
    }
  }, [])

  const handleVersionCreated = useCallback((versionId: string, _: number) => {
    dispatch(setSelectedVersion(versionId))
    refetchVersions()
  }, [dispatch, refetchVersions])

  const handleFilesGenerated = useCallback((files: FileChange[]) => {
    setIsActivelyGenerating(true)
    setStreamingFiles(files)
    // Enable auto-fix during generation
    setAutoFixEnabled(true)
    // Clear any existing timeout
    if (autoFixTimeoutRef.current) {
      clearTimeout(autoFixTimeoutRef.current)
      autoFixTimeoutRef.current = null
    }
  }, [])

  const handleRollback = useCallback(async (versionId: string, options?: { include_schema?: boolean }) => {
    try {
      const newVersion = await rollbackMutation.mutateAsync({
        versionId,
        includeSchema: options?.include_schema ?? true,
      })
      // Must await refetch so the new version is in the array before selecting it
      // Otherwise versionFiles memo falls back to the old first version
      await refetchVersions()
      dispatch(setSelectedVersion(newVersion.id))
      toastHelpers.success(
        `Rolled back to version ${newVersion.version_number}`,
        'A new version has been created based on the selected historical version.'
      )
    } catch (error: any) {
      console.error('Rollback failed:', error)
      toastHelpers.error(
        'Rollback failed',
        error?.response?.data?.error || 'An unexpected error occurred.'
      )
    }
  }, [rollbackMutation, dispatch, refetchVersions, toastHelpers])

  const handleVersionSelect = useCallback((versionId: string) => {
    dispatch(setSelectedVersion(versionId))
    // Reset to show version files when manually selecting a version
    setIsActivelyGenerating(false)
    setStreamingFiles([])
    // Disable auto-fix when loading a previous version (not a fresh generation)
    setAutoFixEnabled(false)
    if (autoFixTimeoutRef.current) {
      clearTimeout(autoFixTimeoutRef.current)
      autoFixTimeoutRef.current = null
    }
  }, [dispatch])

  // Callback when AgenticChatPanel successfully fixes errors
  const handleErrorsCleared = useCallback(() => {
    setBundlerErrors(undefined)
  }, [])

  const handlePublish = async () => {
    if (!appId) return
    setPublishError(null)
    setIsPublishing(true)
    try {
      await publishApp.mutateAsync(appId)
      // Refetch both app (for updated published_url) and versions
      await Promise.all([refetchApp(), refetchVersions()])
      toastHelpers.success('App published!', 'Your app is now live.')
    } catch (error: any) {
      const apiMessage = error?.response?.data?.error || error?.message || 'Publish failed'
      setPublishError(apiMessage)
      toastHelpers.error('Publish failed', apiMessage)
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
    <>
    <div className="flex flex-col h-screen bg-gray-50 overflow-hidden">
      {/* Top Bar */}
      <header className="grid grid-cols-3 items-center px-4 py-2 border-b border-gray-200 bg-white">
        {/* Left - Back button and app info */}
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

        {/* Center - Tab Navigation */}
        <div className="flex justify-center">
          <div className="flex items-center h-8 bg-gray-100 rounded-lg p-0.5">
            <button
              onClick={() => setActiveTab('builder')}
              className={cn(
                'flex items-center gap-1.5 px-3 h-7 rounded-md text-xs font-medium transition-colors',
                activeTab === 'builder'
                  ? 'bg-white text-gray-900 shadow-sm'
                  : 'text-gray-600 hover:text-gray-900'
              )}
            >
              <Code2 className="h-3.5 w-3.5" />
              App
            </button>
            <button
              onClick={() => setActiveTab('data')}
              className={cn(
                'flex items-center gap-1.5 px-3 h-7 rounded-md text-xs font-medium transition-colors',
                activeTab === 'data'
                  ? 'bg-white text-gray-900 shadow-sm'
                  : 'text-gray-600 hover:text-gray-900'
              )}
            >
              <Database className="h-3.5 w-3.5" />
              Data
            </button>
            <button
              onClick={() => setActiveTab('integrations')}
              className={cn(
                'flex items-center gap-1.5 px-3 h-7 rounded-md text-xs font-medium transition-colors',
                activeTab === 'integrations'
                  ? 'bg-white text-gray-900 shadow-sm'
                  : 'text-gray-600 hover:text-gray-900'
              )}
            >
              <Plug className="h-3.5 w-3.5" />
              Integrations
            </button>
          </div>
        </div>

        {/* Right - Action buttons */}
        <div className="flex items-center gap-2 justify-end">
          {/* Run/Preview button */}
          <Button
            variant="outline"
            size="sm"
            className="gap-1.5 text-xs"
            onClick={() => {
              if (selectedVersion) {
                const previewPageUrl = `/preview/apps/${appId}?version=${selectedVersion.id}`
                window.open(previewPageUrl, '_blank')
              }
            }}
            disabled={!selectedVersion}
          >
            <Play className="h-3.5 w-3.5" />
            Preview
          </Button>

          {/* Publish Popover */}
          <PublishPopover
            app={app}
            isPublishing={isPublishing}
            canPublish={canPublish}
            onPublish={handlePublish}
          />
        </div>
      </header>

      {/* Main Content */}
      <div className="flex flex-1 overflow-hidden relative">
        {/* Overlay to prevent iframe from capturing mouse events during resize */}
        {isResizing && (
          <div className="absolute inset-0 z-20 cursor-col-resize" />
        )}
        {activeTab === 'builder' ? (
          <>
            {/* Left Panel - Chat (Agentic or Simple) */}
            <div 
              className="flex-shrink-0 border-r border-gray-200 bg-white h-full relative"
              style={{ width: chatPanelWidth }}
            >
              <AgenticChatPanel
                appId={appId!}
                sessionId={sessionId}
                onSessionChange={setSessionId}
                onVersionCreated={handleVersionCreated}
                onFilesGenerated={handleFilesGenerated}
                onGeneratingVersionChange={setActiveGeneratingVersionId}
                bundlerErrors={bundlerErrors}
                currentVersionId={selectedVersionId || undefined}
                onErrorsCleared={handleErrorsCleared}
                initialPrompt={pendingPrompt || undefined}
                onInitialPromptConsumed={handleInitialPromptConsumed}
                hiddenPrompt={pendingHiddenPrompt || undefined}
                onHiddenPromptConsumed={handleHiddenPromptConsumed}
                className="h-full"
              />
              {/* Resize handle */}
              <div
                onMouseDown={handleResizeMouseDown}
                className="absolute top-0 right-0 w-[1px] h-full cursor-col-resize hover:bg-gray-900/50 active:bg-gray-900/70 transition-colors z-10"
                title="Drag to resize"
              />
            </div>

            {/* Center Panel - Preview (Sandpack for generated files, iframe for versions) */}
            <div className="flex-1 min-w-0 h-full">
              {generatedFiles.length > 0 ? (
                <SandpackPreview
                  files={generatedFiles}
                  appId={appId!}
                  versionId={selectedVersionId || undefined}
                  appName={app.name}
                  className="h-full"
                  onFilesChange={(files) => {
                    setStreamingFiles(files)
                    setIsActivelyGenerating(true) // Show edited files instead of version files
                    // Enable auto-fix when user edits code (same as during generation)
                    setAutoFixEnabled(true)
                    // Reset timeout to disable after 15 seconds of no edits
                    if (autoFixTimeoutRef.current) {
                      clearTimeout(autoFixTimeoutRef.current)
                    }
                    autoFixTimeoutRef.current = setTimeout(() => {
                      setAutoFixEnabled(false)
                      autoFixTimeoutRef.current = null
                    }, 15000)
                  }}
                  showVersionsSidebar={showVersionsSidebar}
                  onToggleVersionsSidebar={() => setShowVersionsSidebar(!showVersionsSidebar)}
                  enableAutoFix={autoFixEnabled}
                  onBundlerErrors={setBundlerErrors}
                  isStreaming={isActivelyGenerating}
                />
              ) : (
                <PreviewPanel
                  appId={appId!}
                  versionId={selectedVersionId}
                  versionFiles={selectedVersion?.files || []}
                  previousVersionFiles={
                    versions && versions.length > 1 && selectedVersion
                      ? versions.find(v => v.version_number === selectedVersion.version_number - 1)?.files
                      : undefined
                  }
                  className="h-full"
                />
              )}
            </div>

            {/* Right Panel - Versions Sidebar (Collapsible) */}
            {showVersionsSidebar && (
              <div className="w-[400px] flex-shrink-0 border-l border-gray-200 bg-white h-full">
                <VersionsPanel
                  versions={versions || []}
                  selectedVersionId={selectedVersionId}
                  onVersionSelect={handleVersionSelect}
                  onRollback={handleRollback}
                  activeGeneratingVersionId={activeGeneratingVersionId}
                  className="h-full"
                />
              </div>
            )}
          </>
        ) : activeTab === 'data' ? (
          /* Data Tab - Full width data management */
          <DataPanel appId={appId!} className="flex-1" />
        ) : (
          /* Integrations Tab - External connectors */
          <AppConnectorsPanel appId={appId!} className="flex-1" />
        )}
      </div>
    </div>

    {/* Publish error toast */}
    {publishError && (
      <div className="fixed bottom-4 right-4 bg-white border border-gray-200 shadow-lg rounded-md px-4 py-3 text-sm text-red-600">
        {publishError}
      </div>
    )}
    </>
  )
}
