import { useEffect, useMemo, useState } from 'react'
import { useNavigate, useParams, useSearchParams } from 'react-router-dom'
import { Check, Link2, Loader2 } from 'lucide-react'
import { SandpackPreview } from '../components/builder/SandpackPreview'
import { useApp, useAppVersion, useAppVersions } from '../hooks/useApps'
import type { FileChange } from '../types/agent'
import { Button } from '../components/ui/button'

export function AppPreviewPage() {
  const { appId } = useParams<{ appId: string }>()
  const navigate = useNavigate()
  const [searchParams, setSearchParams] = useSearchParams()
  const queryVersionId = searchParams.get('version')

  const { data: app, isLoading: appLoading } = useApp(appId || null)
  const { data: versions, isLoading: versionsLoading } = useAppVersions(appId || null)

  const latestVersionId = versions && versions.length > 0 ? versions[0].id : null

  // Keep URL in sync with a real version ID so sharing works and we never land on "no versions"
  useEffect(() => {
    if (!versions || versions.length === 0) return

    const hasQueryVersion = queryVersionId && versions.some((v) => v.id === queryVersionId)
    const fallbackId = queryVersionId && !hasQueryVersion ? latestVersionId : null

    // If no version provided or the provided one is invalid, force the latest
    if (!queryVersionId || fallbackId) {
      const targetVersionId = hasQueryVersion ? queryVersionId! : latestVersionId!
      const next = new URLSearchParams(searchParams)
      next.set('version', targetVersionId)
      setSearchParams(next, { replace: true })
    }
  }, [versions, queryVersionId, latestVersionId, searchParams, setSearchParams])

  const effectiveVersionId = useMemo(() => {
    if (!versions || versions.length === 0) return null
    const hasQuery = queryVersionId && versions.some((v) => v.id === queryVersionId)
    return hasQuery ? queryVersionId : latestVersionId
  }, [versions, queryVersionId, latestVersionId])

  // Fetch detail as a fallback, but we prefer the versions list (it already includes files like the builder uses)
  const { data: versionData, isLoading: versionLoading } = useAppVersion(effectiveVersionId || null)

  const selectedVersion = useMemo(() => {
    const fromList =
      versions?.find((v) => v.id === effectiveVersionId) ||
      versions?.[0] ||
      null
    return fromList || versionData || null
  }, [versions, effectiveVersionId, versionData])

  const filesForPreview = useMemo<FileChange[]>(() => {
    if (!selectedVersion?.files) return []
    return selectedVersion.files.map((f) => {
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
    })
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

  if (!appId) {
    return (
      <div className="flex items-center justify-center h-screen bg-gray-50 text-gray-700">
        Unable to load preview (missing app id).
      </div>
    )
  }

  if (appLoading || versionsLoading || versionLoading) {
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
            className="gap-1 text-xs"
            onClick={handleCopyLink}
            disabled={!deepLink}
          >
            {copied ? <Check className="h-3.5 w-3.5" /> : <Link2 className="h-3.5 w-3.5" />}
            {copied ? 'Copied' : 'Copy link'}
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
        ) : (
          <div className="flex items-center justify-center h-full text-gray-600">
            No files found for this version.
          </div>
        )}
      </div>
    </div>
  )
}

