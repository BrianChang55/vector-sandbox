const API_BASE_URL = import.meta.env.VITE_API_BASE_URL || '/api/v1'

// Remove the API prefix so we can hit non-API routes like /preview/apps/:id
const PREVIEW_BASE_URL = API_BASE_URL.replace(/\/api\/v1\/?$/, '')

export function buildPreviewUrl(appId: string, versionId?: string | null) {
  const params = new URLSearchParams()
  
  if (versionId) {
    params.set('version', versionId)
  }
  
  // Include auth token for cross-origin iframe embedding
  const accessToken = localStorage.getItem('access_token')
  if (accessToken) {
    params.set('token', accessToken)
  }
  
  const queryString = params.toString()
  return `${PREVIEW_BASE_URL}/preview/apps/${appId}/${queryString ? `?${queryString}` : ''}`
}

