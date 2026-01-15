/**
 * Sandpack Validation Hook
 * 
 * Detects bundler errors from Sandpack's CodeSandbox bundler and provides
 * structured error information for automatic fixing via the backend.
 * 
 * This is a fallback for errors that TypeScript validation didn't catch.
 */
import { useState, useEffect, useCallback, useRef } from 'react'
import { useSandpack } from '@codesandbox/sandpack-react'

// Bundler error structure
export interface BundlerError {
  title: string
  message: string
  file?: string
  line?: number
  column?: number
}

// Sandpack message types we care about
interface SandpackMessage {
  type: string
  action?: string
  title?: string
  message?: string
  path?: string
  line?: number
  column?: number
}

export interface SandpackValidationResult {
  /** Whether there are any bundler errors */
  hasErrors: boolean
  /** List of bundler errors detected */
  errors: BundlerError[]
  /** Whether the bundler is still running */
  isLoading: boolean
  /** Whether the bundler has finished at least once */
  hasInitialized: boolean
  /** Clear all errors (e.g., before retrying) */
  clearErrors: () => void
}

/**
 * Hook to detect and collect Sandpack bundler errors.
 * 
 * Usage:
 * ```tsx
 * const { hasErrors, errors, isLoading } = useSandpackValidation()
 * 
 * if (hasErrors && !isLoading) {
 *   // Trigger fix flow with errors
 * }
 * ```
 */
export function useSandpackValidation(): SandpackValidationResult {
  const { sandpack, listen } = useSandpack()
  const [errors, setErrors] = useState<BundlerError[]>([])
  const [isLoading, setIsLoading] = useState(true)
  const [hasInitialized, setHasInitialized] = useState(false)
  
  // Track if we've already triggered a fix for current errors
  const lastErrorSignatureRef = useRef<string>('')
  
  // Clear errors
  const clearErrors = useCallback(() => {
    setErrors([])
    lastErrorSignatureRef.current = ''
  }, [])
  
  // Listen for Sandpack messages
  useEffect(() => {
    const unsubscribe = listen((message: SandpackMessage) => {
      // Track bundler status
      if (message.type === 'status') {
        const status = (message as unknown as { status: string }).status
        if (status === 'running') {
          setIsLoading(true)
        } else if (status === 'idle' || status === 'done') {
          setIsLoading(false)
          setHasInitialized(true)
        }
      }
      
      // Detect errors
      if (message.type === 'action' && message.action === 'show-error') {
        const error: BundlerError = {
          title: message.title || 'Error',
          message: message.message || 'Unknown bundler error',
          file: message.path,
          line: message.line,
          column: message.column,
        }
        
        setErrors(prev => {
          // Avoid duplicates
          const exists = prev.some(e => 
            e.file === error.file && 
            e.line === error.line && 
            e.message === error.message
          )
          if (exists) return prev
          return [...prev, error]
        })
        
        setHasInitialized(true)
        setIsLoading(false)
      }
      
      // Clear errors on successful compilation
      if (message.type === 'done' || message.type === 'success') {
        setErrors([])
        setIsLoading(false)
        setHasInitialized(true)
      }
      
      // Handle compile start - clear previous errors
      if (message.type === 'start') {
        setErrors([])
        setIsLoading(true)
      }
    })
    
    return () => unsubscribe()
  }, [listen])
  
  // Also check Sandpack's internal error state
  useEffect(() => {
    // Check Sandpack's bundler status
    const status = sandpack.status
    if (status === 'idle') {
      setIsLoading(false)
      setHasInitialized(true)
    } else if (status === 'running') {
      setIsLoading(true)
    }
    
    // Check for any error in the sandpack state
    const sandpackError = sandpack.error
    if (sandpackError) {
      const error: BundlerError = {
        title: 'Bundler Error',
        message: sandpackError.message || 'Unknown error',
      }
      
      setErrors(prev => {
        const exists = prev.some(e => e.message === error.message)
        if (exists) return prev
        return [...prev, error]
      })
    }
  }, [sandpack.status, sandpack.error])
  
  return {
    hasErrors: errors.length > 0,
    errors,
    isLoading,
    hasInitialized,
    clearErrors,
  }
}

/**
 * Create a signature string from errors for deduplication
 */
export function createErrorSignature(errors: BundlerError[]): string {
  return errors
    .map(e => `${e.file || ''}:${e.line || 0}:${e.message}`)
    .sort()
    .join('|')
}

