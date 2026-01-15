/**
 * Login page - Clean, minimal design
 * Uses AuthModal component for authentication
 */
import { useEffect } from 'react'
import { useNavigate, useSearchParams } from 'react-router-dom'
import { useAppSelector } from '@/store/hooks'
import { AuthModal } from '@/components/auth/AuthModal'
import { Logo } from '@/components/Logo'

export function LoginPage() {
  const navigate = useNavigate()
  const [searchParams] = useSearchParams()
  const { isAuthenticated } = useAppSelector((state) => state.auth)

  // Check for OAuth errors in URL
  const oauthError = searchParams.get('error')

  useEffect(() => {
    if (isAuthenticated) {
      navigate('/apps', { replace: true })
    }
  }, [isAuthenticated, navigate])

  // Show redirecting state if authenticated
  if (isAuthenticated) {
    return (
      <div className="flex min-h-screen items-center justify-center bg-gray-50">
        <div className="text-center">
          <div className="mx-auto h-8 w-8 animate-spin rounded-full border-4 border-gray-300 border-t-gray-900"></div>
          <p className="mt-4 text-gray-600">Redirecting...</p>
        </div>
      </div>
    )
  }

  return (
    <div className="flex min-h-screen items-center justify-center bg-gray-50 px-4">
      <div className="w-full max-w-md">
        {/* Logo */}
        <div className="mb-8 flex justify-center">
          <a href="/">
            <Logo size="lg" showText />
          </a>
        </div>

        {/* Auth Card */}
        <div className="rounded-lg border border-gray-200 bg-white p-8 shadow-sm">
          {/* OAuth Error Display */}
          {oauthError && (
            <div className="mb-6 rounded-md bg-red-50 border border-red-200 p-3">
              <p className="text-sm text-red-800">{decodeURIComponent(oauthError)}</p>
            </div>
          )}

          <AuthModal initialMode="signin" />
        </div>
      </div>
    </div>
  )
}
