/**
 * Published App Page
 * 
 * Full-screen runtime for published internal apps.
 * Accessed via /:orgSlug/:appSlug URL.
 * Renders the app in an immersive Sandpack runtime environment.
 */
import { useMemo } from 'react'
import { useParams, useNavigate } from 'react-router-dom'
import { Loader2, AlertCircle, ArrowLeft } from 'lucide-react'
import { SandpackPreview } from '../components/builder/SandpackPreview'
import { usePublishedApp } from '../hooks/useApps'
import { Button } from '../components/ui/button'
import type { FileChange } from '../types/agent'
import { upsertFileChange } from '../services/agentService'

export function PublishedAppPage() {
  const { orgSlug, appSlug } = useParams<{ orgSlug: string; appSlug: string }>()
  const navigate = useNavigate()

  const { data, isLoading, error } = usePublishedApp(orgSlug || null, appSlug || null)

  // Convert files to FileChange format for SandpackPreview
  const filesForPreview = useMemo<FileChange[]>(() => {
    if (!data?.files) return []
    return data.files.reduce((acc: FileChange[], f) => {
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
    }, [])
  }, [data?.files])

  // Loading state
  if (isLoading) {
    return (
      <div className="flex items-center justify-center h-screen bg-gray-50">
        <div className="flex flex-col items-center gap-4">
          <Loader2 className="h-10 w-10 text-gray-400 animate-spin" />
          <p className="text-sm text-gray-500">Loading app...</p>
        </div>
      </div>
    )
  }

  // Error state - not found or not published
  if (error || !data) {
    const errorMessage = (error as any)?.response?.data?.error || 'App not found'
    const is404 = (error as any)?.response?.status === 404
    const isForbidden = (error as any)?.response?.status === 403

    return (
      <div className="flex items-center justify-center h-screen bg-gray-50">
        <div className="flex flex-col items-center gap-4 max-w-md text-center px-4">
          <div className="w-16 h-16 rounded-full bg-red-50 flex items-center justify-center">
            <AlertCircle className="h-8 w-8 text-red-500" />
          </div>
          <h1 className="text-xl font-semibold text-gray-900">
            {is404 ? 'App Not Found' : isForbidden ? 'Access Denied' : 'Error'}
          </h1>
          <p className="text-gray-600">{errorMessage}</p>
          <Button
            variant="outline"
            onClick={() => navigate('/apps')}
            className="gap-2 mt-2"
          >
            <ArrowLeft className="h-4 w-4" />
            Go to Apps
          </Button>
        </div>
      </div>
    )
  }

  // No files state
  if (filesForPreview.length === 0) {
    return (
      <div className="flex items-center justify-center h-screen bg-gray-50">
        <div className="flex flex-col items-center gap-4 max-w-md text-center px-4">
          <AlertCircle className="h-12 w-12 text-amber-500" />
          <h1 className="text-xl font-semibold text-gray-900">No Files Found</h1>
          <p className="text-gray-600">
            This published version doesn't have any files to display.
          </p>
          <Button
            variant="outline"
            onClick={() => navigate('/apps')}
            className="gap-2 mt-2"
          >
            <ArrowLeft className="h-4 w-4" />
            Go to Apps
          </Button>
        </div>
      </div>
    )
  }

  // Full-screen Sandpack preview (immersive mode - no toolbar)
  return (
    <div className="h-screen w-screen overflow-hidden">
      <SandpackPreview
        files={filesForPreview}
        appId={data.app.id}
        versionId={data.version.id}
        appName={data.app.name}
        hideToolbar
        className="h-full w-full"
      />
    </div>
  )
}

