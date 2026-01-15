/**
 * Google OAuth utility - uses localStorage events for cross-window communication
 * This avoids COOP issues that break window.opener
 * 
 * Uses centralized apiService for all API calls.
 * @see {@link @/services/apiService} for API documentation.
 */

import { authApi } from '@/services/apiService'

export interface OAuthResult {
  success: boolean
  error?: string
}

export async function initiateGoogleOAuth(): Promise<OAuthResult> {
  // Get OAuth URL from backend using centralized API
  const { oauth_url: oauthUrl } = await authApi.getGoogleOAuthUrl()

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
