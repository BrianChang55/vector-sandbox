/**
 * InviteAcceptPage - Handle invitation acceptance from email link
 *
 * Flow:
 * 1. User clicks link in email: /invite/accept?token=xxx
 * 2. Verify token and show invitation details
 * 3. User clicks accept:
 *    - If logged in as invited email: join org, redirect to apps
 *    - If logged in as different email: show error
 *    - If not logged in and user exists: redirect to login with return URL
 *    - If not logged in and new user: redirect to signup with return URL
 */
import { useEffect, useState } from 'react'
import { useSearchParams, useNavigate, Link } from 'react-router-dom'
import { useAppSelector } from '../store/hooks'
import { useVerifyInvite, useAcceptInvite } from '../hooks/useMembers'
import { Button } from '../components/ui/button'
import {
  Building2,
  Check,
  Loader2,
  AlertCircle,
  Shield,
  PencilLine,
  Eye,
  ArrowRight,
} from 'lucide-react'
import type { OrgRole } from '../types/models'

const ROLE_INFO: Record<OrgRole, { icon: React.ElementType; description: string }> = {
  admin: {
    icon: Shield,
    description: 'Full access - manage members, integrations, and apps',
  },
  editor: {
    icon: PencilLine,
    description: 'Can edit and build apps',
  },
  viewer: {
    icon: Eye,
    description: 'View and run published apps only',
  },
}

export function InviteAcceptPage() {
  const [searchParams] = useSearchParams()
  const navigate = useNavigate()
  const token = searchParams.get('token')

  const { user, isAuthenticated } = useAppSelector((state) => state.auth)
  const { data: inviteDetails, isLoading, error } = useVerifyInvite(token)
  const acceptInvite = useAcceptInvite()

  const [acceptError, setAcceptError] = useState<string | null>(null)

  const handleAccept = async () => {
    if (!token) return

    setAcceptError(null)

    try {
      const result = await acceptInvite.mutateAsync(token)

      if (result.requires_signup) {
        // New user - redirect to signup with token
        navigate(`/signup?invite_token=${token}&email=${encodeURIComponent(result.email || '')}`)
        return
      }

      // Success - redirect to apps
      navigate('/apps')
    } catch (err: any) {
      setAcceptError(
        err?.response?.data?.error ||
          err?.response?.data?.token?.[0] ||
          'Failed to accept invitation. Please try again.'
      )
    }
  }

  // If not authenticated and we have invite details, check if user needs to login
  useEffect(() => {
    if (!isLoading && inviteDetails && !isAuthenticated) {
      // User needs to authenticate first
      // We'll show the invite details but require login to accept
    }
  }, [isLoading, inviteDetails, isAuthenticated])

  // No token provided
  if (!token) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center px-4">
        <div className="w-full max-w-md">
          <div className="bg-white rounded-lg border border-gray-200 p-8 text-center">
            <div className="h-12 w-12 rounded-full bg-red-100 flex items-center justify-center mx-auto mb-4">
              <AlertCircle className="h-6 w-6 text-red-600" />
            </div>
            <h1 className="text-lg font-semibold text-gray-900 mb-2">Invalid Invitation Link</h1>
            <p className="text-sm text-gray-500 mb-6">
              The invitation link is missing or invalid. Please check the link in your email.
            </p>
            <Link to="/login">
              <Button variant="outline">Go to Login</Button>
            </Link>
          </div>
        </div>
      </div>
    )
  }

  // Loading
  if (isLoading) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center px-4">
        <div className="text-center">
          <Loader2 className="h-8 w-8 animate-spin text-gray-400 mx-auto" />
          <p className="mt-3 text-sm text-gray-500">Verifying invitation...</p>
        </div>
      </div>
    )
  }

  // Error or invalid token
  if (error || !inviteDetails) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center px-4">
        <div className="w-full max-w-md">
          <div className="bg-white rounded-lg border border-gray-200 p-8 text-center">
            <div className="h-12 w-12 rounded-full bg-red-100 flex items-center justify-center mx-auto mb-4">
              <AlertCircle className="h-6 w-6 text-red-600" />
            </div>
            <h1 className="text-lg font-semibold text-gray-900 mb-2">
              Invitation Expired or Invalid
            </h1>
            <p className="text-sm text-gray-500 mb-6">
              This invitation has expired or is no longer valid. Please ask the organization admin
              to send you a new invitation.
            </p>
            <Link to="/login">
              <Button variant="outline">Go to Login</Button>
            </Link>
          </div>
        </div>
      </div>
    )
  }

  const RoleIcon = ROLE_INFO[inviteDetails.role]?.icon ?? Eye
  const roleDescription = ROLE_INFO[inviteDetails.role]?.description ?? ''

  // Check if logged in as wrong user
  const isWrongUser = isAuthenticated && user?.email?.toLowerCase() !== inviteDetails.email.toLowerCase()

  return (
    <div className="min-h-screen bg-gray-50 flex items-center justify-center px-4">
      <div className="w-full max-w-md">
        {/* Logo */}
        <div className="mb-8 flex justify-center">
          <div className="flex items-center gap-2">
            <img 
              src="/logo.png" 
              alt="Vector" 
              className="h-8 w-8 rounded"
            />
            <span className="text-lg font-semibold text-gray-900">Vector</span>
          </div>
        </div>

        {/* Invitation Card */}
        <div className="bg-white rounded-lg border border-gray-200 p-8 shadow-sm">
          {/* Organization Info */}
          <div className="flex items-center gap-4 mb-8">
            {inviteDetails.organization_logo_url ? (
              <img
                src={inviteDetails.organization_logo_url}
                alt=""
                className="h-12 w-12 rounded-lg object-cover"
              />
            ) : (
              <div className="h-12 w-12 rounded-lg bg-gray-100 flex items-center justify-center">
                <Building2 className="h-6 w-6 text-gray-400" />
              </div>
            )}
            <div>
              <h1 className="text-lg font-medium text-gray-900">
                {inviteDetails.organization_name}
              </h1>
              {inviteDetails.invited_by_name && (
                <p className="text-sm text-gray-500">
                  Invited by {inviteDetails.invited_by_name}
                </p>
              )}
            </div>
          </div>

          {/* Role Info */}
          <div className="mb-8">
            <div className="flex items-center gap-3">
              <div className="h-10 w-10 rounded-lg bg-gray-100 flex items-center justify-center">
                <RoleIcon className="h-5 w-5 text-gray-500" />
              </div>
              <div>
                <div className="text-sm font-medium text-gray-900">
                  {inviteDetails.role_display}
                </div>
                <div className="text-xs text-gray-500">{roleDescription}</div>
              </div>
            </div>
          </div>

          {/* Invited email */}
          <div className="text-sm text-gray-500 mb-8">
            Invitation for{' '}
            <span className="font-medium text-gray-700">{inviteDetails.email}</span>
          </div>

          {/* Error */}
          {acceptError && (
            <div className="flex items-center gap-2 p-3 bg-red-50 border border-red-200 rounded-lg text-sm text-red-700 mb-6">
              <AlertCircle className="h-4 w-4 flex-shrink-0" />
              {acceptError}
            </div>
          )}

          {/* Spacer to push button down */}
          <div className="pt-4">
            {/* Actions */}
            {isWrongUser ? (
              <div>
                <Button
                  variant="outline"
                  className="w-full"
                  onClick={() => {
                    // Logout and redirect back
                    localStorage.removeItem('access_token')
                    localStorage.removeItem('refresh_token')
                    window.location.href = `/invite/accept?token=${token}`
                  }}
                >
                  Sign out and continue
                </Button>
              </div>
            ) : isAuthenticated ? (
              <Button
                className="w-full"
                onClick={handleAccept}
                disabled={acceptInvite.isPending}
              >
                {acceptInvite.isPending ? (
                  <>
                    <Loader2 className="h-4 w-4 mr-2 animate-spin" />
                    Joining...
                  </>
                ) : (
                  <>
                    <Check className="h-4 w-4 mr-2" />
                    Accept Invitation
                  </>
                )}
              </Button>
            ) : (
              <div className="space-y-3">
                <Button className="w-full" onClick={handleAccept} disabled={acceptInvite.isPending}>
                  {acceptInvite.isPending ? (
                    <>
                      <Loader2 className="h-4 w-4 mr-2 animate-spin" />
                      Checking...
                    </>
                  ) : (
                    <>
                      <ArrowRight className="h-4 w-4 mr-2" />
                      Continue
                    </>
                  )}
                </Button>
                <p className="text-xs text-center text-gray-400">
                  You'll be asked to sign in or create an account
                </p>
              </div>
            )}
          </div>
        </div>
      </div>
    </div>
  )
}

export default InviteAcceptPage
