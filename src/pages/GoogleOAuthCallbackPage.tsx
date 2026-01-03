import { useEffect, useRef } from 'react'

/**
 * Google OAuth callback page.
 * Backend redirects here with tokens in URL hash after successful OAuth.
 * 
 * If opened as popup: stores tokens, signals completion via localStorage, closes popup
 * If opened directly: stores tokens and redirects to /apps
 */
export function GoogleOAuthCallbackPage() {
  const handledRef = useRef(false)

  useEffect(() => {
    // Prevent multiple executions
    if (handledRef.current) return
    handledRef.current = true

    // Extract tokens from URL hash
    const hash = window.location.hash.substring(1)
    const params = new URLSearchParams(hash)
    
    const accessToken = params.get('access_token')
    const refreshToken = params.get('refresh_token')
    const error = params.get('error') || new URLSearchParams(window.location.search).get('error')

    // Check if we're in a popup (window.opener may be null due to COOP, so also check window size)
    const isPopup = window.opener !== null || (window.innerWidth <= 600 && window.innerHeight <= 700)

    if (isPopup) {
      // POPUP FLOW: Store tokens, signal completion, close popup
      if (error) {
        localStorage.setItem('oauth_complete', error)
        setTimeout(() => window.close(), 100)
        return
      }

      if (!accessToken || !refreshToken) {
        localStorage.setItem('oauth_complete', 'Missing tokens')
        setTimeout(() => window.close(), 100)
        return
      }

      // Store tokens
      localStorage.setItem('access_token', accessToken)
      localStorage.setItem('refresh_token', refreshToken)
      // Signal success to opener window
      localStorage.setItem('oauth_complete', 'success')
      // Close popup
      setTimeout(() => window.close(), 100)
      return
    }

    // DIRECT FLOW: Store tokens and redirect
    if (error) {
      window.location.replace('/login?error=' + encodeURIComponent(error))
      return
    }

    if (!accessToken || !refreshToken) {
      window.location.replace('/login?error=' + encodeURIComponent('Authentication failed'))
      return
    }

    localStorage.setItem('access_token', accessToken)
    localStorage.setItem('refresh_token', refreshToken)
    // Redirect to apps page
    window.location.replace('/apps')
  }, [])

  return (
    <div className="flex min-h-screen items-center justify-center bg-gray-50">
      <div className="text-center">
        <div className="mb-4">
          <div className="mx-auto h-8 w-8 animate-spin rounded-full border-4 border-blue-300 border-t-blue-600"></div>
        </div>
        <p className="text-gray-700">Completing sign in...</p>
      </div>
    </div>
  )
}

