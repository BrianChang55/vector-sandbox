import { useMemo, useState } from 'react'
import { useNavigate, useParams, useSearchParams } from 'react-router-dom'
import { ArrowLeft, RefreshCw, ExternalLink } from 'lucide-react'
import { buildPreviewUrl } from '../lib/preview'
import { Button } from '../components/ui/button'

export function AppPreviewPage() {
  const { appId } = useParams<{ appId: string }>()
  const [searchParams] = useSearchParams()
  const navigate = useNavigate()
  const [reloadToken, setReloadToken] = useState(0)

  const versionId = searchParams.get('version')

  const previewUrl = useMemo(() => {
    if (!appId) return null
    return buildPreviewUrl(appId, versionId)
  }, [appId, versionId])

  return (
    <div className="h-screen w-screen bg-gray-50 flex flex-col">
      <header className="flex items-center justify-between px-4 py-3 border-b border-gray-200 bg-white">
        <div className="flex items-center gap-2">
          <Button variant="ghost" size="sm" className="gap-2" onClick={() => navigate(-1)}>
            <ArrowLeft className="h-4 w-4" />
            Back
          </Button>
          <div className="text-sm font-semibold text-gray-900">
            App Preview {versionId ? `(version ${versionId.slice(0, 8)})` : ''}
          </div>
        </div>

        <div className="flex items-center gap-2">
          <Button
            variant="outline"
            size="sm"
            className="gap-1.5"
            onClick={() => setReloadToken((t) => t + 1)}
            disabled={!previewUrl}
          >
            <RefreshCw className="h-4 w-4" />
            Reload
          </Button>
          <Button
            variant="secondary"
            size="sm"
            className="gap-1.5"
            onClick={() => previewUrl && window.open(previewUrl, '_blank')}
            disabled={!previewUrl}
          >
            <ExternalLink className="h-4 w-4" />
            Open in new tab
          </Button>
        </div>
      </header>

      <main className="flex-1 bg-gray-100">
        {previewUrl ? (
          <iframe
            key={`${previewUrl}-${reloadToken}`}
            src={previewUrl}
            className="w-full h-full border-0 bg-white"
            title="App Preview"
          />
        ) : (
          <div className="flex items-center justify-center h-full text-gray-600">
            Unable to load preview (missing app id).
          </div>
        )}
      </main>
    </div>
  )
}

