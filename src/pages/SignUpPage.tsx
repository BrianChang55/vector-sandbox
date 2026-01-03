/**
 * Sign up page - Clean, minimal design
 * Uses AuthModal component for authentication
 */
import { useEffect } from 'react'
import { useNavigate, useSearchParams } from 'react-router-dom'
import { useAppSelector } from '@/store/hooks'
import { AuthModal } from '@/components/auth/AuthModal'

export function SignUpPage() {
  const navigate = useNavigate()
  const [searchParams] = useSearchParams()
  const { isAuthenticated } = useAppSelector((state) => state.auth)

  // Get referral code and prefilled email from URL
  const referralCode = searchParams.get('ref') || undefined
  const prefillEmail = searchParams.get('email') || ''

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
    <div className="flex min-h-screen items-center justify-center bg-gray-50 px-4 py-12">
      <div className="w-full max-w-md">
        {/* Logo */}
        <div className="mb-8 flex justify-center">
          <a href="/" className="flex items-center gap-2">
            <div className="h-8 w-8 rounded bg-gray-900"></div>
            <span className="text-lg font-semibold text-gray-900">Internal Apps</span>
          </a>
        </div>

        {/* Auth Card */}
        <div className="rounded-lg border border-gray-200 bg-white p-8 shadow-sm">
          <AuthModal 
            initialMode="signup" 
            referralCode={referralCode}
            prefillEmail={prefillEmail}
          />
        </div>
      </div>
    </div>
  )
}
