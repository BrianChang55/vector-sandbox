import { useParams, useSearchParams } from 'react-router-dom'
import { PreviewPanel } from '../components/builder/PreviewPanel'

export function AppPreviewPage() {
  const { appId } = useParams<{ appId: string }>()
  const [searchParams] = useSearchParams()
  const versionId = searchParams.get('version')

  return (
    <div className="h-screen w-screen bg-gray-100 flex">
      {appId ? (
        <PreviewPanel
          appId={appId}
          versionId={versionId}
          className="flex-1"
          previewOnly
        />
      ) : (
        <div className="m-auto text-gray-600">Unable to load preview (missing app id).</div>
      )}
    </div>
  )
}

