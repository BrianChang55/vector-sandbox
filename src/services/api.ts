import axios, { AxiosError } from 'axios'
import type { AxiosInstance, InternalAxiosRequestConfig } from 'axios'
import type { ApiError } from '@/types/api'
import { logger } from '@/services/loggingService'

const API_BASE_URL = import.meta.env.VITE_API_BASE_URL || 'http://localhost:8000/api/v1'

// Create axios instance
export const api: AxiosInstance = axios.create({
  baseURL: API_BASE_URL,
  headers: {
    'Content-Type': 'application/json',
  },
})

// Request interceptor - Add auth token
api.interceptors.request.use(
  (config: InternalAxiosRequestConfig) => {
    const token = localStorage.getItem('access_token')
    if (token && config.headers) {
      config.headers.Authorization = `Bearer ${token}`
    }
    return config
  },
  (error) => {
    return Promise.reject(error)
  }
)

// Response interceptor - Handle errors
api.interceptors.response.use(
  (response) => response,
  async (error: AxiosError<ApiError>) => {
    const originalRequest = error.config as InternalAxiosRequestConfig & { _retry?: boolean }

    // Log API errors to Sentry
    const endpoint = originalRequest?.url || 'unknown'
    const statusCode = error.response?.status || 0
    const errorMessage = error.response?.data?.error || error.response?.data?.message || error.message

    // Don't log 401s as errors (expected for auth flow)
    if (statusCode !== 401) {
      logger.apiError(endpoint, statusCode, errorMessage, {
        method: originalRequest?.method?.toUpperCase(),
      })
    }

    // Handle 401 Unauthorized - could implement token refresh here
    if (error.response?.status === 401 && !originalRequest._retry) {
      originalRequest._retry = true
      // Remove token and redirect to login
      localStorage.removeItem('access_token')
      localStorage.removeItem('refresh_token')
      window.location.href = '/login'
      return Promise.reject(error)
    }

    return Promise.reject(error)
  }
)

// Helper function to extract error message
export function getErrorMessage(error: unknown): string {
  if (axios.isAxiosError<ApiError>(error)) {
    // Check for network errors (API unreachable)
    if (!error.response) {
      // Network error - API is unreachable
      if (error.code === 'ERR_NETWORK' || error.message.includes('Network Error') || error.message.includes('fetch')) {
        return 'Unable to connect to server. Please check your internet connection and try again.'
      }
      // Request timeout
      if (error.code === 'ECONNABORTED' || error.message.includes('timeout')) {
        return 'Request timed out. Please try again.'
      }
      // Generic network error
      return 'Unable to connect to server. Please try again later.'
    }
    // Server responded with error
    return error.response?.data?.error || error.response?.data?.message || error.message
  }
  if (error instanceof Error) {
    // Check for fetch errors
    if (error.message.includes('fetch') || error.message.includes('Failed to fetch')) {
      return 'Unable to connect to server. Please check your internet connection and try again.'
    }
    return error.message
  }
  return 'An unexpected error occurred'
}

