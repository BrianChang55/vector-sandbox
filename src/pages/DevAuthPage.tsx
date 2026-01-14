/**
 * Dev-only page that accepts JWT tokens via URL and stores them in localStorage.
 * Used by dev.sh to auto-login during development.
 * 
 * SECURITY NOTES:
 * 1. This route ONLY works in development mode (import.meta.env.DEV)
 * 2. The route is lazy-loaded and excluded from production bundles via tree-shaking
 * 3. A build-time guard in vite.config.ts will fail builds if this code appears in production
 * 
 * KNOWN RISK - Tokens in URL:
 * Passing tokens via URL query parameters is inherently less secure because:
 * - Tokens may appear in browser history
 * - Tokens may be logged by local dev servers
 * - Tokens could leak via referrer headers (mitigated: we redirect immediately)
 * 
 * This is acceptable for LOCAL DEVELOPMENT ONLY because:
 * - Only used on localhost
 * - Tokens are short-lived dev tokens
 * - The alternative (manual login every time) significantly impacts developer productivity
 * 
 * URL format: /dev-auth?access=XXX&refresh=YYY
 */
import { useEffect } from 'react'
import { useSearchParams } from 'react-router-dom'

export function DevAuthPage() {
  const [searchParams] = useSearchParams()
  
  useEffect(() => {
    // SECURITY: Only allow in development mode
    if (!import.meta.env.DEV) {
      console.error('DevAuthPage is only available in development mode')
      window.location.replace('/login')
      return
    }
    
    const accessToken = searchParams.get('access')
    const refreshToken = searchParams.get('refresh')
    
    if (accessToken && refreshToken) {
      // Store tokens
      localStorage.setItem('access_token', accessToken)
      localStorage.setItem('refresh_token', refreshToken)
      
      // Clear tokens from URL to prevent them from appearing in browser history
      // Using replaceState before navigation for defense-in-depth
      window.history.replaceState({}, '', '/dev-auth')
      
      // Redirect to apps page
      window.location.replace('/apps')
    } else {
      // No tokens, redirect to login
      window.location.replace('/login')
    }
  }, [searchParams])

  // Don't render anything in production
  if (!import.meta.env.DEV) {
    return null
  }

  return (
    <div className="flex min-h-screen items-center justify-center bg-gray-50">
      <div className="text-center">
        <div className="mx-auto mb-4 h-8 w-8 animate-spin rounded-full border-4 border-blue-300 border-t-blue-600"></div>
        <p className="text-sm text-gray-600">Setting up dev session...</p>
      </div>
    </div>
  )
}
