/**
 * Authentication types
 */

export interface User {
  id: string
  email: string
  first_name: string
  last_name: string
  profile_image_url?: string
  date_joined: string
  created_at: string
  updated_at: string
  google_id?: string
}

export interface LoginCredentials {
  email: string
  password: string
}

export interface SignUpData {
  email: string
  password: string
  first_name: string
  last_name: string
  referral_code?: string
}

export interface AuthResponse {
  user: User
  tokens: {
    access: string
    refresh: string
  }
  message?: string
}

// Magic Link Authentication
export interface MagicLinkRequestData {
  email: string
  first_name?: string
  last_name?: string
  referral_code?: string
}

export interface MagicLinkRequestResponse {
  message: string
  email: string
  is_new_user: boolean
}

export interface MagicLinkVerifyData {
  token: string
}

export interface MagicLinkVerifyResponse {
  user: User
  tokens: {
    access: string
    refresh: string
  }
  is_new_user: boolean
  message: string
}

export interface AuthMeResponse {
  user: User
  organizations: Array<{
    id: string
    name: string
    slug: string
    role: string
  }>
}

