/**
 * Google OAuth utility - uses localStorage events for cross-window communication
 * This avoids COOP issues that break window.opener
 */

const API_BASE_URL = import.meta.env.VITE_API_BASE_URL || 'http://localhost:8001/api/v1'

export interface OAuthResult {
  success: boolean
  error?: string
}

export async function initiateGoogleOAuth(): Promise<OAuthResult> {
  // Get OAuth URL from backend
  const response = await fetch(`${API_BASE_URL}/auth/google`, {
    method: 'GET',
    credentials: 'include',
  })

  if (!response.ok) {
    const error = await response.json()
    throw new Error(error.error || 'Failed to initiate Google OAuth')
  }

  const data = await response.json()
  const oauthUrl = data.oauth_url

  // Clear any previous OAuth state
  localStorage.removeItem('oauth_complete')

  // Open popup window
  const width = 500
  const height = 600
  const left = window.screen.width / 2 - width / 2
  const top = window.screen.height / 2 - height / 2

  const popup = window.open(
    oauthUrl,
    'Google OAuth',
    `width=${width},height=${height},left=${left},top=${top},toolbar=no,menubar=no,scrollbars=yes,resizable=yes`
  )

  if (!popup) {
    throw new Error('Popup blocked. Please allow popups for this site.')
  }

  // Wait for OAuth to complete via localStorage event
  return new Promise((resolve, reject) => {
    const timeoutId = setTimeout(() => {
      window.removeEventListener('storage', storageHandler)
      clearInterval(pollInterval)
      reject(new Error('OAuth timeout - please try again'))
    }, 5 * 60 * 1000) // 5 minute timeout

    // Listen for storage events (works when popup sets localStorage)
    const storageHandler = (event: StorageEvent) => {
      if (event.key === 'oauth_complete') {
        cleanup()
        const value = event.newValue
        if (value === 'success') {
          resolve({ success: true })
        } else {
          reject(new Error(value || 'OAuth failed'))
        }
      }
    }

    // Also poll localStorage in case storage event doesn't fire (same-origin edge case)
    const pollInterval = setInterval(() => {
      const value = localStorage.getItem('oauth_complete')
      if (value) {
        cleanup()
        localStorage.removeItem('oauth_complete')
        if (value === 'success') {
          resolve({ success: true })
        } else {
          reject(new Error(value))
        }
      }
    }, 500)

    const cleanup = () => {
      clearTimeout(timeoutId)
      clearInterval(pollInterval)
      window.removeEventListener('storage', storageHandler)
      localStorage.removeItem('oauth_complete')
    }

    window.addEventListener('storage', storageHandler)
  })
}

