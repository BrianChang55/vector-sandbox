/**
 * Authentication service
 */
import { api } from './api'
import type { 
  AuthResponse, 
  LoginCredentials, 
  SignUpData, 
  AuthMeResponse,
  MagicLinkRequestData,
  MagicLinkRequestResponse,
  MagicLinkVerifyData,
  MagicLinkVerifyResponse,
} from '@/types/auth'

export const authService = {
  // ============================================================================
  // Magic Link Authentication (Primary method)
  // ============================================================================
  
  /**
   * Request a magic link for email authentication.
   * Works for both new users (signup) and existing users (signin).
   */
  requestMagicLink: async (data: MagicLinkRequestData): Promise<MagicLinkRequestResponse> => {
    const response = await api.post<MagicLinkRequestResponse>('/auth/magic-link/request', data)
    return response.data
  },

  /**
   * Verify a magic link token and complete authentication.
   * Returns JWT tokens on success.
   */
  verifyMagicLink: async (data: MagicLinkVerifyData): Promise<MagicLinkVerifyResponse> => {
    const response = await api.post<MagicLinkVerifyResponse>('/auth/magic-link/verify', data)
    const responseData = response.data
    
    // Store tokens on successful verification
    if (responseData.tokens?.access) {
      localStorage.setItem('access_token', responseData.tokens.access)
      localStorage.setItem('refresh_token', responseData.tokens.refresh)
    }
    
    return responseData
  },

  // ============================================================================
  // Core Authentication
  // ============================================================================

  getCurrentUser: async (): Promise<AuthMeResponse> => {
    const response = await api.get<AuthMeResponse>('/auth/me')
    return response.data
  },

  refreshToken: async (refreshToken: string): Promise<{ access: string }> => {
    const response = await api.post<{ access: string }>('/auth/refresh', { refresh: refreshToken })
    return response.data
  },

  logout: (): void => {
    localStorage.removeItem('access_token')
    localStorage.removeItem('refresh_token')
  },

  // ============================================================================
  // Google OAuth
  // ============================================================================

  googleOAuth: async (): Promise<AuthResponse> => {
    // Popup-based OAuth using localStorage for cross-window communication
    const { initiateGoogleOAuth } = await import('@/utils/googleOAuth')
    
    // This will open popup and wait for it to complete
    const result = await initiateGoogleOAuth()
    
    if (!result.success) {
      throw new Error(result.error || 'OAuth failed')
    }
    
    // Tokens are already stored by the callback page
    // Redirect to apps page
    window.location.href = '/apps'
    
    // This won't execute due to redirect
    return { user: null as any, tokens: { access: '', refresh: '' } }
  },

  // ============================================================================
  // Legacy Authentication (kept for backwards compatibility)
  // ============================================================================

  login: async (credentials: LoginCredentials): Promise<AuthResponse> => {
    const response = await api.post<AuthResponse>('/auth/login', credentials)
    const data = response.data
    if (data.tokens?.access) {
      localStorage.setItem('access_token', data.tokens.access)
      localStorage.setItem('refresh_token', data.tokens.refresh)
    }
    return data
  },

  signUp: async (signUpData: SignUpData): Promise<AuthResponse> => {
    const payload = {
      email: signUpData.email,
      password: signUpData.password,
      password_confirm: signUpData.password,
      first_name: signUpData.first_name || '',
      last_name: signUpData.last_name || '',
      referral_code: signUpData.referral_code,
    }
    const response = await api.post<AuthResponse>('/auth/signup', payload)
    const data = response.data
    if (data.tokens?.access) {
      localStorage.setItem('access_token', data.tokens.access)
      localStorage.setItem('refresh_token', data.tokens.refresh)
    }
    return data
  },
}

