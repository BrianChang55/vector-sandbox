export interface ApiResponse<T> {
  data?: T
  error?: string
  message?: string
}

export interface ApiError {
  error: string
  message?: string
  detail?: string
}

