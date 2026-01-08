/**
 * AdminGuard component - protects admin routes with email whitelist
 * 
 * Requires:
 * 1. User to be authenticated
 * 2. User's email to be in the admin whitelist
 */
import { Navigate, useLocation } from 'react-router-dom'
import { useAppSelector, useAppDispatch } from '@/store/hooks'
import { fetchCurrentUser } from '@/store/slices/authSlice'
import { useEffect } from 'react'
import type { ReactNode } from 'react'

// Admin email whitelist - only these emails can access admin pages
// Configure via VITE_ADMIN_EMAILS env var (comma-separated) or add directly here
const ENV_ADMIN_EMAILS = import.meta.env.VITE_ADMIN_EMAILS || ''

const ADMIN_EMAIL_WHITELIST: string[] = [
  // Emails from environment variable
  ...ENV_ADMIN_EMAILS.split(',').map((e: string) => e.trim()).filter(Boolean),
  // Add additional whitelisted admin emails here if needed
  // e.g. 'admin@example.com',
]

// Check if email is whitelisted (case-insensitive)
function isEmailWhitelisted(email: string | undefined): boolean {
  if (!email) return false
  const normalizedEmail = email.toLowerCase().trim()
  return ADMIN_EMAIL_WHITELIST.some(
    (whitelisted) => whitelisted.toLowerCase().trim() === normalizedEmail
  )
}

interface AdminGuardProps {
  children: ReactNode
}

export function AdminGuard({ children }: AdminGuardProps) {
  const { isAuthenticated, loading, user } = useAppSelector((state) => state.auth)
  const dispatch = useAppDispatch()
  const location = useLocation()

  // Fetch user data on mount if token exists
  useEffect(() => {
    const token = localStorage.getItem('access_token')
    if (token && !user && !loading) {
      dispatch(fetchCurrentUser())
    }
  }, [dispatch, user, loading])

  // Show loading spinner while checking auth
  if (loading) {
    return (
      <div className="flex h-screen items-center justify-center bg-white">
        <div className="h-8 w-8 animate-spin rounded-full border-2 border-gray-300 border-t-gray-900" />
      </div>
    )
  }

  // Redirect to login if not authenticated
  if (!isAuthenticated) {
    return <Navigate to="/login" state={{ from: location }} replace />
  }

  // Check if user email is whitelisted
  if (!isEmailWhitelisted(user?.email)) {
    return (
      <div className="flex h-screen items-center justify-center bg-gray-50">
        <div className="max-w-md text-center p-8">
          <div className="h-16 w-16 mx-auto mb-4 rounded-full bg-red-100 flex items-center justify-center">
            <svg 
              className="h-8 w-8 text-red-600" 
              fill="none" 
              viewBox="0 0 24 24" 
              stroke="currentColor"
            >
              <path 
                strokeLinecap="round" 
                strokeLinejoin="round" 
                strokeWidth={2} 
                d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" 
              />
            </svg>
          </div>
          <h1 className="text-xl font-semibold text-gray-900 mb-2">
            Access Denied
          </h1>
          <p className="text-sm text-gray-600 mb-4">
            You don't have permission to access admin pages.
          </p>
          <p className="text-xs text-gray-500 mb-6">
            Logged in as: <span className="font-medium">{user?.email}</span>
          </p>
          <a 
            href="/apps" 
            className="inline-flex items-center justify-center px-4 py-2 text-sm font-medium text-white bg-gray-900 rounded-md hover:bg-gray-800"
          >
            Go to Apps
          </a>
        </div>
      </div>
    )
  }

  return <>{children}</>
}

export { ADMIN_EMAIL_WHITELIST, isEmailWhitelisted }

