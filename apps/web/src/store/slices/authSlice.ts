/**
 * Authentication Redux slice
 */
import { createSlice, createAsyncThunk } from '@reduxjs/toolkit'
import { authService } from '@/services/authService'
import { getErrorMessage } from '@/services/api'
import type { 
  User, 
  LoginCredentials, 
  SignUpData,
  MagicLinkRequestData,
  MagicLinkVerifyData,
  MagicLinkRequestResponse,
  MagicLinkVerifyResponse,
} from '@/types/auth'
import type { AppDispatch } from '../index'
import { setSelectedOrg } from './uiSlice'

interface AuthState {
  user: User | null
  token: string | null
  isAuthenticated: boolean
  loading: boolean
  error: string | null
  // Magic link state
  magicLinkSent: boolean
  magicLinkEmail: string | null
  isNewUser: boolean | null
}

const initialState: AuthState = {
  user: null,
  token: localStorage.getItem('access_token'),
  isAuthenticated: !!localStorage.getItem('access_token'),
  loading: false,
  error: null,
  // Magic link state
  magicLinkSent: false,
  magicLinkEmail: null,
  isNewUser: null,
}

// ============================================================================
// Magic Link Authentication (Primary method)
// ============================================================================

export const requestMagicLink = createAsyncThunk<
  MagicLinkRequestResponse,
  MagicLinkRequestData
>(
  'auth/requestMagicLink',
  async (data, { rejectWithValue }) => {
    try {
      const response = await authService.requestMagicLink(data)
      return response
    } catch (error: any) {
      // Check if this is a "new user needs to sign up" error
      const responseData = error?.response?.data
      if (responseData?.is_new_user === true) {
        return rejectWithValue({
          message: responseData.error || 'Account not found',
          is_new_user: true,
        })
      }
      // Extract error message with proper network error handling
      const errorMessage = getErrorMessage(error)
      return rejectWithValue(errorMessage)
    }
  }
)

export const verifyMagicLink = createAsyncThunk<
  MagicLinkVerifyResponse,
  MagicLinkVerifyData
>(
  'auth/verifyMagicLink',
  async (data, { rejectWithValue }) => {
    try {
      const response = await authService.verifyMagicLink(data)
      return response
    } catch (error: any) {
      // Extract error message with proper network error handling
      const responseData = error?.response?.data
      const errorMessage = responseData?.token?.[0]
        || responseData?.error 
        || getErrorMessage(error)
        || 'Invalid or expired magic link'
      return rejectWithValue(errorMessage)
    }
  }
)

// ============================================================================
// Google OAuth
// ============================================================================

export const googleOAuth = createAsyncThunk(
  'auth/googleOAuth',
  async (_, { rejectWithValue }) => {
    try {
      const response = await authService.googleOAuth()
      return response
    } catch (error) {
      // Extract error message with proper network error handling
      const errorMessage = getErrorMessage(error)
      return rejectWithValue(errorMessage)
    }
  }
)

export const login = createAsyncThunk(
  'auth/login',
  async (credentials: LoginCredentials, { rejectWithValue }) => {
    try {
      const response = await authService.login(credentials)
      return response
    } catch (error) {
      const errorMessage = getErrorMessage(error)
      return rejectWithValue(errorMessage)
    }
  }
)

export const signUp = createAsyncThunk(
  'auth/signUp',
  async (data: SignUpData, { rejectWithValue }) => {
    try {
      const response = await authService.signUp(data)
      return response
    } catch (error) {
      const errorMessage = getErrorMessage(error)
      return rejectWithValue(errorMessage)
    }
  }
)

export const fetchCurrentUser = createAsyncThunk<
  User,
  void,
  { dispatch: AppDispatch }
>(
  'auth/fetchCurrentUser',
  async (_, { rejectWithValue, dispatch }) => {
    try {
      const response = await authService.getCurrentUser()
      // Set first organization as selected if available
      if (response.organizations.length > 0) {
        dispatch(setSelectedOrg(response.organizations[0].id))
      }
      // Return user data
      return response.user
    } catch (error) {
      return rejectWithValue(error)
    }
  }
)

const authSlice = createSlice({
  name: 'auth',
  initialState,
  reducers: {
    logout: (state) => {
      state.user = null
      state.token = null
      state.isAuthenticated = false
      state.magicLinkSent = false
      state.magicLinkEmail = null
      state.isNewUser = null
      authService.logout()
    },
    clearError: (state) => {
      state.error = null
    },
    resetMagicLinkState: (state) => {
      state.magicLinkSent = false
      state.magicLinkEmail = null
      state.isNewUser = null
      state.error = null
    },
  },
  extraReducers: (builder) => {
    builder
      // ============================================================================
      // Magic Link Authentication
      // ============================================================================
      
      // Request Magic Link
      .addCase(requestMagicLink.pending, (state) => {
        state.loading = true
        state.error = null
      })
      .addCase(requestMagicLink.fulfilled, (state, action) => {
        state.loading = false
        state.magicLinkSent = true
        state.magicLinkEmail = action.payload.email
        state.isNewUser = action.payload.is_new_user
      })
      .addCase(requestMagicLink.rejected, (state, action) => {
        state.loading = false
        // Don't set error if this is a "new user needs to sign up" case - handled in UI
        const payload = action.payload as { is_new_user?: boolean; message?: string } | string
        if (typeof payload === 'object' && payload.is_new_user === true) {
          // Don't set error - the component will handle this inline
          return
        }
        state.error = typeof payload === 'string' ? payload : 'Failed to send magic link'
      })
      
      // Verify Magic Link
      .addCase(verifyMagicLink.pending, (state) => {
        state.loading = true
        state.error = null
      })
      .addCase(verifyMagicLink.fulfilled, (state, action) => {
        state.loading = false
        state.user = action.payload.user
        state.token = action.payload.tokens.access
        state.isAuthenticated = true
        state.isNewUser = action.payload.is_new_user
        // Reset magic link state
        state.magicLinkSent = false
        state.magicLinkEmail = null
      })
      .addCase(verifyMagicLink.rejected, (state, action) => {
        state.loading = false
        state.error = typeof action.payload === 'string' ? action.payload : 'Invalid or expired magic link'
      })
      
      // ============================================================================
      // Legacy Authentication
      // ============================================================================
      
      // Login
      .addCase(login.pending, (state) => {
        state.loading = true
        state.error = null
      })
      .addCase(login.fulfilled, (state, action) => {
        state.loading = false
        state.user = action.payload.user
        state.token = action.payload.tokens.access
        state.isAuthenticated = true
      })
      .addCase(login.rejected, (state, action) => {
        state.loading = false
        state.error = typeof action.payload === 'string' ? action.payload : 'Login failed'
      })
      // Sign up
      .addCase(signUp.pending, (state) => {
        state.loading = true
        state.error = null
      })
      .addCase(signUp.fulfilled, (state, action) => {
        state.loading = false
        state.user = action.payload.user
        state.token = action.payload.tokens.access
        state.isAuthenticated = true
      })
      .addCase(signUp.rejected, (state, action) => {
        state.loading = false
        state.error = typeof action.payload === 'string' ? action.payload : 'Sign up failed'
      })
      // Fetch current user
      .addCase(fetchCurrentUser.pending, (state) => {
        state.loading = true
      })
      .addCase(fetchCurrentUser.fulfilled, (state, action) => {
        state.loading = false
        state.user = action.payload
        state.isAuthenticated = true
      })
      .addCase(fetchCurrentUser.rejected, (state) => {
        state.loading = false
        state.isAuthenticated = false
        state.token = null
        authService.logout()
      })
      // Google OAuth
      .addCase(googleOAuth.pending, (state) => {
        state.loading = true
        state.error = null
      })
      .addCase(googleOAuth.fulfilled, (state, action) => {
        state.loading = false
        state.user = action.payload.user
        state.token = action.payload.tokens.access
        state.isAuthenticated = true
      })
      .addCase(googleOAuth.rejected, (state, action) => {
        state.loading = false
        // action.payload is already a string (error message) due to rejectWithValue
        state.error = typeof action.payload === 'string' ? action.payload : 'Google OAuth failed'
      })
  },
})

export const { logout, clearError, resetMagicLinkState } = authSlice.actions
export default authSlice.reducer

