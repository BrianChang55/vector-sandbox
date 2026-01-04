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
  Play,
  Rocket,
  Loader2,
  CheckCircle,
} from 'lucide-react'

import { useApp, useAppVersions, usePublishApp } from '../hooks/useApps'
import { useAppSelector, useAppDispatch } from '../store/hooks'
import { setSelectedVersion } from '../store/slices/uiSlice'

import { AgenticChatPanel } from '../components/builder/AgenticChatPanel'
import { PreviewPanel } from '../components/builder/PreviewPanel'
import { SandpackPreview } from '../components/builder/SandpackPreview'
import { Button } from '../components/ui/button'
import { cn } from '../lib/utils'
import type { FileChange } from '../types/agent'

export function AppBuilderPage() {
  const { appId } = useParams<{ appId: string }>()
  const navigate = useNavigate()
  const dispatch = useAppDispatch()
  
  const { data: app, isLoading: appLoading } = useApp(appId || null)
  const { data: versions, refetch: refetchVersions } = useAppVersions(appId || null)
  const publishApp = usePublishApp()
  
  const selectedVersionId = useAppSelector((state) => state.ui.selectedVersionId)
  
  const [sessionId, setSessionId] = useState<string | null>(null)
  const [isPublishing, setIsPublishing] = useState(false)
  const [generatedFiles, setGeneratedFiles] = useState<FileChange[]>([])

  // Select latest version by default
  useEffect(() => {
    if (versions && versions.length > 0 && !selectedVersionId) {
      dispatch(setSelectedVersion(versions[0].id))
    }
  }, [versions, selectedVersionId, dispatch])

  const selectedVersion = versions?.find((v) => v.id === selectedVersionId) || versions?.[0]

  // Load existing version files into generatedFiles for Sandpack preview
  // This runs when a version is selected (either on initial load or when switching versions)
  useEffect(() => {
    if (selectedVersion?.files && selectedVersion.files.length > 0) {
      // Convert version files to FileChange format for SandpackPreview
      const filesForPreview: FileChange[] = selectedVersion.files.map(
        (f: { path: string; content: string }) => {
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

          return {
            path: f.path,
            content: f.content,
            action: 'create',
            language,
          }
        }
      )
      setGeneratedFiles(filesForPreview)
    }
  }, [selectedVersion]) // Re-run when selected version object changes

  const handleVersionCreated = useCallback((versionId: string, _: number) => {
    dispatch(setSelectedVersion(versionId))
    refetchVersions()
  }, [dispatch, refetchVersions])

  const handleFilesGenerated = useCallback((files: FileChange[]) => {
    setGeneratedFiles(files)
  }, [])

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

      {/* Main Content - Two Panel Layout */}
      <div className="flex flex-1 overflow-hidden">
        {/* Left Panel - Chat (Agentic or Simple) */}
        <div className="w-[420px] flex-shrink-0 border-r border-gray-200 bg-white h-full">
          <AgenticChatPanel
            appId={appId!}
            sessionId={sessionId}
            onSessionChange={setSessionId}
            onVersionCreated={handleVersionCreated}
            onFilesGenerated={handleFilesGenerated}
            className="h-full"
          />
        </div>

        {/* Right Panel - Preview (Sandpack for generated files, iframe for versions) */}
        <div className="flex-1 min-w-0 h-full">
          {generatedFiles.length > 0 ? (
            <SandpackPreview
              files={generatedFiles}
              appId={appId!}
              versionId={selectedVersionId || undefined}
              appName={app.name}
              className="h-full"
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
      </div>
    </div>
  )
}
