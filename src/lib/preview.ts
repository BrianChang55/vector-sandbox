const API_BASE_URL = import.meta.env.VITE_API_BASE_URL || 'http://localhost:8001/api/v1'

// Remove the API prefix so we can hit non-API routes like /preview/apps/:id
const PREVIEW_BASE_URL = API_BASE_URL.replace(/\/api\/v1\/?$/, '')

export function buildPreviewUrl(appId: string, versionId?: string | null) {
  const versionQuery = versionId ? `?version=${versionId}` : ''
  return `${PREVIEW_BASE_URL}/preview/apps/${appId}${versionQuery}`
}

