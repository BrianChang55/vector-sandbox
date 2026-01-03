/**
 * Dialog Provider & useDialog hook
 * 
 * Provides programmatic access to dialogs throughout the application.
 * Replaces browser's native alert(), confirm(), and prompt() with
 * beautiful, accessible modal dialogs.
 * 
 * Usage:
 * 
 * ```tsx
 * // Wrap your app with DialogProvider
 * <DialogProvider>
 *   <App />
 * </DialogProvider>
 * 
 * // Use the hook in any component
 * const { alert, confirm, prompt } = useDialog()
 * 
 * // Show an alert
 * await alert({ title: 'Success!', description: 'Your changes have been saved.' })
 * 
 * // Show a confirmation dialog
 * const confirmed = await confirm({ 
 *   title: 'Delete item?', 
 *   description: 'This action cannot be undone.',
 *   variant: 'destructive'
 * })
 * if (confirmed) { ... }
 * 
 * // Show a prompt dialog
 * const name = await prompt({ 
 *   title: 'Rename file', 
 *   placeholder: 'Enter new name' 
 * })
 * if (name) { ... }
 * ```
 */

import * as React from 'react'
import { 
  AlertDialog, 
  ConfirmDialog, 
  PromptDialog,
  type DialogVariant,
} from './dialog'

// ============================================================================
// Types
// ============================================================================

interface AlertOptions {
  title: string
  description?: string
  variant?: DialogVariant
  confirmText?: string
}

interface ConfirmOptions {
  title: string
  description?: string
  variant?: DialogVariant
  confirmText?: string
  cancelText?: string
}

interface PromptOptions {
  title: string
  description?: string
  placeholder?: string
  defaultValue?: string
  confirmText?: string
  cancelText?: string
  inputType?: 'text' | 'email' | 'password' | 'url' | 'number'
  required?: boolean
}

interface DialogContextValue {
  /**
   * Show an alert dialog (replaces window.alert)
   * Returns a promise that resolves when the dialog is closed
   */
  alert: (options: AlertOptions) => Promise<void>
  
  /**
   * Show a confirmation dialog (replaces window.confirm)
   * Returns a promise that resolves to true if confirmed, false if cancelled
   */
  confirm: (options: ConfirmOptions) => Promise<boolean>
  
  /**
   * Show a prompt dialog (replaces window.prompt)
   * Returns a promise that resolves to the entered value, or null if cancelled
   */
  prompt: (options: PromptOptions) => Promise<string | null>
}

// ============================================================================
// Context
// ============================================================================

const DialogContext = React.createContext<DialogContextValue | undefined>(undefined)

// ============================================================================
// Provider
// ============================================================================

interface DialogState {
  type: 'alert' | 'confirm' | 'prompt' | null
  options: AlertOptions | ConfirmOptions | PromptOptions | null
  resolve: ((value: unknown) => void) | null
}

export function DialogProvider({ children }: { children: React.ReactNode }) {
  const [state, setState] = React.useState<DialogState>({
    type: null,
    options: null,
    resolve: null,
  })

  const alert = React.useCallback((options: AlertOptions): Promise<void> => {
    return new Promise((resolve) => {
      setState({
        type: 'alert',
        options,
        resolve: () => resolve(),
      })
    })
  }, [])

  const confirm = React.useCallback((options: ConfirmOptions): Promise<boolean> => {
    return new Promise((resolve) => {
      setState({
        type: 'confirm',
        options,
        resolve: (value) => resolve(value as boolean),
      })
    })
  }, [])

  const prompt = React.useCallback((options: PromptOptions): Promise<string | null> => {
    return new Promise((resolve) => {
      setState({
        type: 'prompt',
        options,
        resolve: (value) => resolve(value as string | null),
      })
    })
  }, [])

  const handleClose = React.useCallback(() => {
    setState((prev) => {
      // Resolve with appropriate default for type
      if (prev.resolve) {
        if (prev.type === 'confirm') {
          prev.resolve(false)
        } else if (prev.type === 'prompt') {
          prev.resolve(null)
        } else {
          prev.resolve(undefined)
        }
      }
      return { type: null, options: null, resolve: null }
    })
  }, [])

  const handleConfirmAlert = React.useCallback(() => {
    setState((prev) => {
      prev.resolve?.(undefined)
      return { type: null, options: null, resolve: null }
    })
  }, [])

  const handleConfirmConfirm = React.useCallback(() => {
    setState((prev) => {
      prev.resolve?.(true)
      return { type: null, options: null, resolve: null }
    })
  }, [])

  const handleConfirmPrompt = React.useCallback((value: string) => {
    setState((prev) => {
      prev.resolve?.(value)
      return { type: null, options: null, resolve: null }
    })
  }, [])

  const handleCancelPrompt = React.useCallback(() => {
    setState((prev) => {
      prev.resolve?.(null)
      return { type: null, options: null, resolve: null }
    })
  }, [])

  const contextValue = React.useMemo(
    () => ({ alert, confirm, prompt }),
    [alert, confirm, prompt]
  )

  return (
    <DialogContext.Provider value={contextValue}>
      {children}

      {/* Alert Dialog */}
      {state.type === 'alert' && state.options && (
        <AlertDialog
          open={true}
          onOpenChange={(open) => !open && handleClose()}
          title={(state.options as AlertOptions).title}
          description={(state.options as AlertOptions).description}
          variant={(state.options as AlertOptions).variant}
          confirmText={(state.options as AlertOptions).confirmText}
          onConfirm={handleConfirmAlert}
        />
      )}

      {/* Confirm Dialog */}
      {state.type === 'confirm' && state.options && (
        <ConfirmDialog
          open={true}
          onOpenChange={(open) => !open && handleClose()}
          title={(state.options as ConfirmOptions).title}
          description={(state.options as ConfirmOptions).description}
          variant={(state.options as ConfirmOptions).variant}
          confirmText={(state.options as ConfirmOptions).confirmText}
          cancelText={(state.options as ConfirmOptions).cancelText}
          onConfirm={handleConfirmConfirm}
          onCancel={handleClose}
        />
      )}

      {/* Prompt Dialog */}
      {state.type === 'prompt' && state.options && (
        <PromptDialog
          open={true}
          onOpenChange={(open) => !open && handleClose()}
          title={(state.options as PromptOptions).title}
          description={(state.options as PromptOptions).description}
          placeholder={(state.options as PromptOptions).placeholder}
          defaultValue={(state.options as PromptOptions).defaultValue}
          confirmText={(state.options as PromptOptions).confirmText}
          cancelText={(state.options as PromptOptions).cancelText}
          inputType={(state.options as PromptOptions).inputType}
          required={(state.options as PromptOptions).required}
          onConfirm={handleConfirmPrompt}
          onCancel={handleCancelPrompt}
        />
      )}
    </DialogContext.Provider>
  )
}

// ============================================================================
// Hook
// ============================================================================

export function useDialog(): DialogContextValue {
  const context = React.useContext(DialogContext)
  if (!context) {
    throw new Error('useDialog must be used within a DialogProvider')
  }
  return context
}

// Export types for external use
export type { AlertOptions, ConfirmOptions, PromptOptions }

