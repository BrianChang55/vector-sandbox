/**
 * Authentication Modal - Production-ready unified auth component
 * Supports magic link and Google OAuth with signin/signup mode toggle
 */
import { useState, useEffect } from 'react'
// import { useNavigate } from 'react-router-dom'
import { useAppDispatch, useAppSelector } from '@/store/hooks'
import { 
  requestMagicLink, 
  googleOAuth, 
  clearError, 
  resetMagicLinkState 
} from '@/store/slices/authSlice'
import { Input } from '@/components/ui/input'

interface AuthModalProps {
  initialMode?: 'signin' | 'signup'
  referralCode?: string
  prefillEmail?: string
}

type AuthStep = 'form' | 'check_inbox'

export function AuthModal({ 
  initialMode = 'signin', 
  referralCode,
  prefillEmail = ''
}: AuthModalProps) {
  // const navigate = useNavigate() // Reserved for future navigation needs
  const dispatch = useAppDispatch()
  const { loading, error, magicLinkSent, magicLinkEmail } = useAppSelector((state) => state.auth)

  const [mode, setMode] = useState<'signin' | 'signup'>(initialMode)
  const [step, setStep] = useState<AuthStep>('form')
  
  // Form fields
  const [email, setEmail] = useState(prefillEmail)
  const [firstName, setFirstName] = useState('')
  const [lastName, setLastName] = useState('')
  
  // Validation state
  const [emailError, setEmailError] = useState('')
  const [nameError, setNameError] = useState('')
  
  // Show message when user needs to sign up
  const [showAccountNotFound, setShowAccountNotFound] = useState(false)
  
  // Track which action is loading
  const [isSendingMagicLink, setIsSendingMagicLink] = useState(false)
  const [isGoogleLoading, setIsGoogleLoading] = useState(false)

  // Update email when prefill changes
  useEffect(() => {
    if (prefillEmail) {
      setEmail(prefillEmail)
    }
  }, [prefillEmail])

  // Handle magic link sent
  useEffect(() => {
    if (magicLinkSent && magicLinkEmail) {
      setStep('check_inbox')
    }
  }, [magicLinkSent, magicLinkEmail])

  // Reset magic link state when component unmounts
  useEffect(() => {
    return () => {
      dispatch(resetMagicLinkState())
    }
  }, [dispatch])

  // Email validation
  const validateEmail = (emailValue: string): boolean => {
    if (!emailValue.trim()) {
      setEmailError('Email is required')
      return false
    }
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/
    if (!emailRegex.test(emailValue.trim())) {
      setEmailError('Please enter a valid email address')
      return false
    }
    setEmailError('')
    return true
  }

  // Name validation (for signup)
  const validateName = (): boolean => {
    if (mode === 'signup' && (!firstName.trim() || !lastName.trim())) {
      setNameError('First and last name are required')
      return false
    }
    setNameError('')
    return true
  }

  // Form validation
  const isFormValid = mode === 'signin' 
    ? email.trim() !== '' && !emailError
    : firstName.trim() !== '' && lastName.trim() !== '' && email.trim() !== '' && !emailError

  const handleEmailChange = (value: string) => {
    setEmail(value)
    if (emailError) setEmailError('')
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    dispatch(clearError())
    setShowAccountNotFound(false)
    
    // Validate email
    if (!validateEmail(email) || !validateName()) {
      return
    }
    
    if (!isFormValid) {
      return
    }
    
    // Request magic link
    setIsSendingMagicLink(true)
    const result = await dispatch(requestMagicLink({
      email: email.trim().toLowerCase(),
      first_name: mode === 'signup' ? firstName.trim() : undefined,
      last_name: mode === 'signup' ? lastName.trim() : undefined,
      referral_code: referralCode,
    }))
    setIsSendingMagicLink(false)
    
    if (requestMagicLink.rejected.match(result)) {
      // Check if the user needs to sign up (account not found)
      const payload = result.payload as { message?: string; is_new_user?: boolean } | string
      if (typeof payload === 'object' && payload.is_new_user === true) {
        // Switch to signup mode and show a friendly message
        setMode('signup')
        setShowAccountNotFound(true)
      }
    }
  }

  const handleGoogleOAuth = async () => {
    setIsGoogleLoading(true)
    dispatch(clearError())
    
    try {
      await dispatch(googleOAuth())
    } catch {
      // Error is handled by Redux
    } finally {
      setIsGoogleLoading(false)
    }
  }

  const handleResendMagicLink = async () => {
    dispatch(clearError())
    
    setIsSendingMagicLink(true)
    await dispatch(requestMagicLink({
      email: email.trim().toLowerCase(),
      first_name: mode === 'signup' ? firstName.trim() : undefined,
      last_name: mode === 'signup' ? lastName.trim() : undefined,
      referral_code: referralCode,
    }))
    setIsSendingMagicLink(false)
  }

  const handleBackToForm = () => {
    setStep('form')
    dispatch(resetMagicLinkState())
  }

  // Render Check Inbox step
  if (step === 'check_inbox') {
    return (
      <div className="w-full text-center">
        {/* Email Icon */}
        <div className="mx-auto mb-6 flex h-16 w-16 items-center justify-center rounded-full bg-blue-50">
          <svg 
            className="h-8 w-8 text-blue-600" 
            fill="none" 
            viewBox="0 0 24 24" 
            stroke="currentColor"
          >
            <path 
              strokeLinecap="round" 
              strokeLinejoin="round" 
              strokeWidth={2} 
              d="M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z" 
            />
          </svg>
        </div>

        {/* Header */}
        <h2 className="text-2xl font-semibold text-gray-900">
          Check your inbox
        </h2>
        <div className="mt-3 text-sm text-gray-600">
          <p className="mb-1">
            We sent a magic link to
          </p>
          <p className="font-medium text-gray-900">{magicLinkEmail}</p>
          <p className="mt-2">
            Click the link in the email to {mode === 'signin' ? 'sign in' : 'complete your account'}.
          </p>
        </div>

        {/* Actions */}
        <div className="mt-8 space-y-3">
          <button
            type="button"
            onClick={handleBackToForm}
            className="w-full rounded-md border border-gray-200 bg-white py-2.5 text-sm font-medium text-gray-700 transition-colors hover:bg-gray-50"
          >
            Use a different email
          </button>
          <p className="text-xs text-gray-500">
            Didn't receive the email?{' '}
            <button
              type="button"
              onClick={handleResendMagicLink}
              disabled={isSendingMagicLink}
              className="font-medium text-gray-900 hover:underline disabled:opacity-50"
            >
              {isSendingMagicLink ? 'Sending...' : 'Resend'}
            </button>
          </p>
        </div>
      </div>
    )
  }

  // Render Form step
  return (
    <div className="w-full">
      {/* Header */}
      <h2 className="text-2xl font-semibold text-gray-900">
        {mode === 'signin' ? 'Welcome back' : 'Create your account'}
      </h2>
      <p className="mt-1 text-sm text-gray-600">
        {mode === 'signin' 
          ? 'Enter your email to receive a sign-in link.'
          : 'Start building internal tools with AI.'
        }
      </p>

      {/* Error Display */}
      {error && (
        <div className="mt-4 rounded-md bg-red-50 border border-red-200 p-3">
          <p className="text-sm text-red-800">{error}</p>
        </div>
      )}

      {/* Google OAuth Button */}
      <button
        type="button"
        onClick={handleGoogleOAuth}
        disabled={loading || isGoogleLoading}
        className="mt-6 flex w-full items-center justify-center gap-3 rounded-md border border-gray-300 bg-white py-2.5 text-sm font-medium text-gray-700 transition-all hover:bg-gray-50 hover:border-gray-400 disabled:opacity-50 disabled:cursor-not-allowed"
      >
        {isGoogleLoading ? (
          <div className="h-5 w-5 animate-spin rounded-full border-2 border-gray-300 border-t-gray-600"></div>
        ) : (
          <svg className="h-5 w-5" viewBox="0 0 24 24">
            <path
              fill="#4285F4"
              d="M22.56 12.25c0-.78-.07-1.53-.2-2.25H12v4.26h5.92c-.26 1.37-1.04 2.53-2.21 3.31v2.77h3.57c2.08-1.92 3.28-4.74 3.28-8.09z"
            />
            <path
              fill="#34A853"
              d="M12 23c2.97 0 5.46-.98 7.28-2.66l-3.57-2.77c-.98.66-2.23 1.06-3.71 1.06-2.86 0-5.29-1.93-6.16-4.53H2.18v2.84C3.99 20.53 7.7 23 12 23z"
            />
            <path
              fill="#FBBC05"
              d="M5.84 14.09c-.22-.66-.35-1.36-.35-2.09s.13-1.43.35-2.09V7.07H2.18C1.43 8.55 1 10.22 1 12s.43 3.45 1.18 4.93l2.85-2.22.81-.62z"
            />
            <path
              fill="#EA4335"
              d="M12 5.38c1.62 0 3.06.56 4.21 1.64l3.15-3.15C17.45 2.09 14.97 1 12 1 7.7 1 3.99 3.47 2.18 7.07l3.66 2.84c.87-2.6 3.3-4.53 6.16-4.53z"
            />
          </svg>
        )}
        <span>Continue with Google</span>
      </button>

      {/* Divider */}
      <div className="relative my-6">
        <div className="absolute inset-0 flex items-center">
          <div className="w-full border-t border-gray-200"></div>
        </div>
        <div className="relative flex justify-center">
          <span className="bg-white px-4 text-xs font-medium uppercase tracking-wider text-gray-400">
            Or continue with email
          </span>
        </div>
      </div>

      {/* Mode Toggle Tabs */}
      <div className="mb-6 flex rounded-lg border border-gray-200 p-1">
        <button
          type="button"
          onClick={() => {
            setMode('signin')
            setShowAccountNotFound(false)
            setNameError('')
          }}
          disabled={isSendingMagicLink || isGoogleLoading}
          className={`flex-1 rounded-md py-2 text-sm font-medium transition-colors disabled:opacity-50 disabled:cursor-not-allowed ${
            mode === 'signin'
              ? 'bg-gray-100 text-gray-900'
              : 'text-gray-500 hover:text-gray-700'
          }`}
        >
          Sign In
        </button>
        <button
          type="button"
          onClick={() => {
            setMode('signup')
            setShowAccountNotFound(false)
          }}
          disabled={isSendingMagicLink || isGoogleLoading}
          className={`flex-1 rounded-md py-2 text-sm font-medium transition-colors disabled:opacity-50 disabled:cursor-not-allowed ${
            mode === 'signup'
              ? 'bg-gray-100 text-gray-900'
              : 'text-gray-500 hover:text-gray-700'
          }`}
        >
          Sign Up
        </button>
      </div>

      {/* Form */}
      <form onSubmit={handleSubmit} className="space-y-4">
        {/* Account not found message */}
        {showAccountNotFound && mode === 'signup' && (
          <div className="rounded-lg bg-amber-50 border border-amber-200 p-3">
            <div className="flex items-start gap-2">
              <svg className="h-5 w-5 text-amber-600 mt-0.5 flex-shrink-0" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
              </svg>
              <div>
                <p className="text-sm font-medium text-amber-800">No account found</p>
                <p className="text-xs text-amber-700 mt-0.5">
                  We couldn't find an account with this email. Create an account below.
                </p>
              </div>
            </div>
          </div>
        )}
        
        {/* Name fields - only for signup */}
        {mode === 'signup' && (
          <>
            <div className="grid grid-cols-2 gap-4">
              <div>
                <label htmlFor="firstName" className="block text-sm font-medium text-gray-700 mb-1.5">
                  First Name
                </label>
                <Input
                  id="firstName"
                  type="text"
                  placeholder="John"
                  value={firstName}
                  onChange={(e) => {
                    setFirstName(e.target.value)
                    if (nameError) setNameError('')
                  }}
                  autoComplete="given-name"
                  disabled={isSendingMagicLink || isGoogleLoading}
                  className={nameError && !firstName.trim() ? 'border-red-300 focus-visible:ring-red-500' : ''}
                />
              </div>
              <div>
                <label htmlFor="lastName" className="block text-sm font-medium text-gray-700 mb-1.5">
                  Last Name
                </label>
                <Input
                  id="lastName"
                  type="text"
                  placeholder="Doe"
                  value={lastName}
                  onChange={(e) => {
                    setLastName(e.target.value)
                    if (nameError) setNameError('')
                  }}
                  autoComplete="family-name"
                  disabled={isSendingMagicLink || isGoogleLoading}
                  className={nameError && !lastName.trim() ? 'border-red-300 focus-visible:ring-red-500' : ''}
                />
              </div>
            </div>
            {nameError && (
              <p className="text-sm text-red-600">{nameError}</p>
            )}
          </>
        )}

        {/* Email */}
        <div>
          <label htmlFor="email" className="block text-sm font-medium text-gray-700 mb-1.5">
            Email
          </label>
          <Input
            id="email"
            type="email"
            placeholder="you@example.com"
            value={email}
            onChange={(e) => handleEmailChange(e.target.value)}
            onBlur={() => {
              if (email.trim()) validateEmail(email)
            }}
            required
            autoComplete="email"
            disabled={isSendingMagicLink || isGoogleLoading}
            className={emailError ? 'border-red-300 focus-visible:ring-red-500' : ''}
          />
          {emailError && (
            <p className="mt-1 text-sm text-red-600">{emailError}</p>
          )}
        </div>

        {/* Submit Button */}
        <button
          type="submit"
          disabled={isSendingMagicLink || !isFormValid}
          className="w-full rounded-md bg-gray-900 py-2.5 text-sm font-medium text-white transition-colors hover:bg-gray-800 disabled:opacity-50 disabled:cursor-not-allowed"
        >
          {isSendingMagicLink
            ? 'Sending link...'
            : mode === 'signin'
            ? 'Send magic link'
            : 'Create account'}
        </button>
      </form>

      {/* Footer */}
      <p className="mt-6 text-center text-xs text-gray-500">
        By continuing, you agree to our Terms of Service and Privacy Policy.
      </p>
    </div>
  )
}

