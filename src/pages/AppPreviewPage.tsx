import { useEffect, useMemo, useState } from 'react'
import { useParams, useSearchParams } from 'react-router-dom'
import { Check, Link2, Loader2, ExternalLink } from 'lucide-react'
import { SandpackPreview, SimplePreview } from '../components/builder/SandpackPreview'
import { useApp, useAppVersions } from '../hooks/useApps'
import { buildPreviewUrl } from '../lib/preview'
import { Button } from '../components/ui/button'
import type { FileChange } from '../types/agent'
import { upsertFileChange } from '../services/agentService'

export function AppPreviewPage() {
  const { appId } = useParams<{ appId: string }>()
  const [searchParams, setSearchParams] = useSearchParams()
  const queryVersionId = searchParams.get('version')

  const { data: app, isLoading: appLoading } = useApp(appId || null)
  // Include files so we can render Sandpack with the exact version like the builder
  const { data: versions, isLoading: versionsLoading } = useAppVersions(appId || null, { includeFiles: true })
  const [selectedVersionId, setSelectedVersionId] = useState<string | null>(queryVersionId)

  const sortedVersions = useMemo(
    () =>
      (versions || []).slice().sort((a, b) => (b.version_number || 0) - (a.version_number || 0)),
    [versions]
  )

  // Keep URL and local state aligned to a real version ID so sharing works and we never land on "no versions"
  useEffect(() => {
    if (!sortedVersions || sortedVersions.length === 0) return

    const hasQueryVersion = queryVersionId && sortedVersions.some((v) => v.id === queryVersionId)
    const nextVersionId = hasQueryVersion ? queryVersionId! : sortedVersions[0].id

    setSelectedVersionId(nextVersionId)

    const currentQueryVersion = searchParams.get('version')
    if (currentQueryVersion !== nextVersionId) {
      const next = new URLSearchParams(searchParams)
      next.set('version', nextVersionId)
      setSearchParams(next, { replace: true })
    }
  }, [sortedVersions, queryVersionId, searchParams, setSearchParams])

  const selectedVersion = useMemo(() => {
    if (!sortedVersions || sortedVersions.length === 0) return null
    if (selectedVersionId) {
      const match = sortedVersions.find((v) => v.id === selectedVersionId)
      if (match) return match
    }
    return sortedVersions[0]
  }, [sortedVersions, selectedVersionId])

  const effectiveVersionId = selectedVersion?.id || null

  const filesForPreview = useMemo<FileChange[]>(() => {
    if (!selectedVersion?.files) return []
    return selectedVersion.files.reduce((acc: FileChange[], f: { path: string; content: string }) => {
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
  }, [selectedVersion])

  const lastUpdatedText = useMemo(() => {
    if (!selectedVersion?.created_at) return null
    const date = new Date(selectedVersion.created_at)
    if (Number.isNaN(date.getTime())) return null
    return date.toLocaleDateString()
  }, [selectedVersion])

  const [copied, setCopied] = useState(false)
  const deepLink = useMemo(() => {
    if (!effectiveVersionId) return ''
    if (typeof window === 'undefined') return ''
    const url = new URL(window.location.href)
    url.searchParams.set('version', effectiveVersionId)
    return url.toString()
  }, [effectiveVersionId])

  const handleCopyLink = async () => {
    if (!deepLink) return
    try {
      await navigator.clipboard?.writeText(deepLink)
      setCopied(true)
      setTimeout(() => setCopied(false), 1200)
    } catch (error) {
      console.error('Failed to copy link', error)
    }
  }

  const previewUrl = useMemo(() => {
    if (!appId || !effectiveVersionId) return ''
    return buildPreviewUrl(appId, effectiveVersionId)
  }, [appId, effectiveVersionId])

  if (!appId) {
    return (
      <div className="flex items-center justify-center h-screen bg-gray-50 text-gray-700">
        Unable to load preview (missing app id).
      </div>
    )
  }

  if (appLoading || versionsLoading) {
    return (
      <div className="flex items-center justify-center h-screen bg-gray-50">
        <Loader2 className="h-8 w-8 text-gray-400 animate-spin" />
      </div>
    )
  }

  if (!versions || versions.length === 0) {
    return (
      <div className="flex flex-col items-center justify-center h-screen bg-gray-50 text-gray-700">
        <p className="mb-2 font-medium">No versions yet</p>
        <p className="text-sm text-gray-500">This app does not have any versions yet.</p>
      </div>
    )
  }

  return (
    <div className="flex flex-col h-screen bg-gray-50 overflow-hidden">
      <header className="flex items-center justify-between px-4 py-3 border-b border-gray-200 bg-white">
        <div className="flex items-center gap-3">
          <div>
            <div className="text-sm font-semibold text-gray-900">
              {app?.name || 'App Preview'}
            </div>
            <div className="text-[11px] text-gray-600">
              {lastUpdatedText ? `Last updated ${lastUpdatedText}` : 'Last updated'}
            </div>
          </div>
        </div>

        <div className="flex items-center gap-2">
          <Button
            variant="outline"
            size="sm"
            className="p-2"
            onClick={handleCopyLink}
            disabled={!deepLink}
            title={copied ? 'Link copied' : 'Copy link'}
          >
            {copied ? <Check className="h-3.5 w-3.5" /> : <Link2 className="h-3.5 w-3.5" />}
          </Button>
        </div>
      </header>

      <div className="flex-1 min-h-0">
        {filesForPreview.length > 0 ? (
          <SandpackPreview
            files={filesForPreview}
            appId={appId}
            versionId={effectiveVersionId || undefined}
            appName={app?.name || 'App'}
            hideToolbar
            className="h-full"
          />
        ) : previewUrl ? (
          <SimplePreview previewUrl={previewUrl} className="h-full" showOpenInNewTab={false} />
        ) : (
          <div className="flex items-center justify-center h-full text-gray-600">
            Unable to build preview URL.
          </div>
        )}
      </div>
    </div>
  )
}

