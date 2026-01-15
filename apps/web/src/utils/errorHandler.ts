import { getErrorMessage } from '@/services/api'

export function handleApiError(error: unknown): string {
  return getErrorMessage(error)
}

export function showErrorToast(message: string) {
  // Deprecated: Use useToast hook instead
  console.error('Error toast requested (deprecated):', message)
}

