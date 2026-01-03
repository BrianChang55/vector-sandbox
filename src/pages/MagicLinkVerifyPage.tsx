import { useEffect, useState, useRef } from 'react'
import { useSearchParams } from 'react-router-dom'
import { useAppDispatch } from '@/store/hooks'
import { verifyMagicLink, clearError } from '@/store/slices/authSlice'

type VerifyStatus = 'verifying' | 'success' | 'error'

export function MagicLinkVerifyPage() {
  const dispatch = useAppDispatch()
  const [searchParams] = useSearchParams()
  const handledRef = useRef(false)
  
  const [status, setStatus] = useState<VerifyStatus>('verifying')
  const [errorMessage, setErrorMessage] = useState<string>('')
  const [isNewUser, setIsNewUser] = useState<boolean | null>(null)
  
  const token = searchParams.get('token')

  useEffect(() => {
    // Prevent multiple executions (same pattern as GoogleOAuthCallbackPage)
    if (handledRef.current) return
    handledRef.current = true

    const verifyToken = async () => {
      if (!token) {
        setStatus('error')
        setErrorMessage('Invalid magic link. Please request a new one.')
        return
      }

      dispatch(clearError())
      
      const result = await dispatch(verifyMagicLink({ token }))
      
      if (verifyMagicLink.fulfilled.match(result)) {
        // Set isNewUser from the response payload directly (not Redux)
        const userIsNew = result.payload.is_new_user
        setIsNewUser(userIsNew)
        
        setStatus('success')
        
        // Redirect to apps page
        setTimeout(() => {
          window.location.replace('/apps')
        }, 1500)
      } else {
        setStatus('error')
        setErrorMessage(
          typeof result.payload === 'string' 
            ? result.payload 
            : 'Invalid or expired magic link. Please request a new one.'
        )
      }
    }

    verifyToken()
  }, [token, dispatch])

  return (
    <div className="relative min-h-screen w-full overflow-hidden bg-gray-50">
      {/* Header / Logo */}
      <header className="relative z-10 px-8 py-6">
        <a href="/" className="flex items-center gap-2">
          {/* Placeholder for logo */}
          <div className="h-8 w-8 bg-blue-600 rounded-md flex items-center justify-center text-white font-bold">IA</div>
          <span className="font-semibold text-xl text-gray-900">
            Internal Apps
          </span>
        </a>
      </header>

      {/* Content */}
      <div className="relative z-10 flex min-h-[calc(100vh-88px)] items-center justify-center px-4">
        <div className="w-full max-w-md rounded-2xl bg-white p-8 shadow-xl">
          {/* Verifying State */}
          {status === 'verifying' && (
            <div className="text-center">
              <div className="mx-auto mb-6 flex h-16 w-16 items-center justify-center rounded-full bg-blue-100">
                <div className="h-8 w-8 animate-spin rounded-full border-4 border-blue-300 border-t-blue-600"></div>
              </div>
              <h2 className="font-semibold text-2xl text-gray-900">
                Verifying your link...
              </h2>
              <p className="mt-2 text-sm text-gray-600">
                Please wait while we sign you in.
              </p>
            </div>
          )}

          {/* Success State */}
          {status === 'success' && (
            <div className="text-center">
              <div className="mx-auto mb-6 flex h-14 w-14 items-center justify-center rounded-full bg-green-100">
                <svg 
                  className="h-7 w-7 text-green-600" 
                  fill="none" 
                  viewBox="0 0 24 24" 
                  stroke="currentColor"
                >
                  <path 
                    strokeLinecap="round" 
                    strokeLinejoin="round" 
                    strokeWidth={2} 
                    d="M5 13l4 4L19 7" 
                  />
                </svg>
              </div>
              <h2 className="font-semibold text-2xl text-gray-900">
                {isNewUser === true ? 'Welcome to Internal Apps!' : 'Welcome back!'}
              </h2>
              <p className="mt-2 text-sm text-gray-600">
                  Redirecting you to the app...
              </p>
            </div>
          )}

          {/* Error State */}
          {status === 'error' && (
            <div className="text-center">
              <div className="mx-auto mb-6 flex h-14 w-14 items-center justify-center rounded-full bg-red-100">
                <svg 
                  className="h-7 w-7 text-red-600" 
                  fill="none" 
                  viewBox="0 0 24 24" 
                  stroke="currentColor"
                >
                  <path 
                    strokeLinecap="round" 
                    strokeLinejoin="round" 
                    strokeWidth={2} 
                    d="M6 18L18 6M6 6l12 12" 
                  />
                </svg>
              </div>
              <h2 className="font-semibold text-2xl text-gray-900">
                Link expired or invalid
              </h2>
              <p className="mt-2 text-sm text-gray-600">
                {errorMessage}
              </p>
              
              <div className="mt-8 space-y-3">
                <a
                  href="/login"
                  className="block w-full rounded-lg bg-blue-600 py-3 text-center text-sm font-medium text-white transition-colors hover:bg-blue-700"
                >
                  Request a New Link
                </a>
                <a
                  href="/"
                  className="block w-full rounded-lg border border-gray-200 py-3 text-center text-sm font-medium text-gray-700 transition-colors hover:bg-gray-50"
                >
                  Go to Homepage
                </a>
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
  )
}

