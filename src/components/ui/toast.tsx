/**
 * Toast Component
 * 
 * Simple toast notification system using Radix UI.
 * Light enterprise theme following STYLE_GUIDE.md
 */
import * as React from 'react'
import * as ToastPrimitive from '@radix-ui/react-toast'
import {
  CheckCircle,
  AlertCircle,
  Info,
  X,
  AlertTriangle,
} from 'lucide-react'
import { cn } from '../../lib/utils'

// Toast types
export type ToastType = 'success' | 'error' | 'warning' | 'info'

interface Toast {
  id: string
  type: ToastType
  title: string
  description?: string
  duration?: number
}

// Toast Context
interface ToastContextValue {
  toasts: Toast[]
  addToast: (toast: Omit<Toast, 'id'>) => void
  removeToast: (id: string) => void
}

const ToastContext = React.createContext<ToastContextValue | null>(null)

// Hook to use toast
export function useToast() {
  const context = React.useContext(ToastContext)
  if (!context) {
    throw new Error('useToast must be used within a ToastProvider')
  }
  return context
}

// Toast Provider
interface ToastProviderProps {
  children: React.ReactNode
}

export function ToastProvider({ children }: ToastProviderProps) {
  const [toasts, setToasts] = React.useState<Toast[]>([])

  const addToast = React.useCallback((toast: Omit<Toast, 'id'>) => {
    const id = Math.random().toString(36).substr(2, 9)
    setToasts((prev) => [...prev, { ...toast, id }])
  }, [])

  const removeToast = React.useCallback((id: string) => {
    setToasts((prev) => prev.filter((t) => t.id !== id))
  }, [])

  return (
    <ToastContext.Provider value={{ toasts, addToast, removeToast }}>
      <ToastPrimitive.Provider swipeDirection="right">
        {children}
        {toasts.map((toast) => (
          <ToastItem
            key={toast.id}
            toast={toast}
            onClose={() => removeToast(toast.id)}
          />
        ))}
        <ToastPrimitive.Viewport className="fixed bottom-4 right-4 flex flex-col gap-2 w-96 max-w-[100vw] z-[100] outline-none" />
      </ToastPrimitive.Provider>
    </ToastContext.Provider>
  )
}

// Toast Item Component
interface ToastItemProps {
  toast: Toast
  onClose: () => void
}

const toastIcons: Record<ToastType, typeof CheckCircle> = {
  success: CheckCircle,
  error: AlertCircle,
  warning: AlertTriangle,
  info: Info,
}

const toastStyles: Record<ToastType, { bg: string; border: string; icon: string }> = {
  success: {
    bg: 'bg-green-50',
    border: 'border-green-200',
    icon: 'text-green-600',
  },
  error: {
    bg: 'bg-red-50',
    border: 'border-red-200',
    icon: 'text-red-600',
  },
  warning: {
    bg: 'bg-yellow-50',
    border: 'border-yellow-200',
    icon: 'text-yellow-600',
  },
  info: {
    bg: 'bg-blue-50',
    border: 'border-blue-200',
    icon: 'text-blue-600',
  },
}

function ToastItem({ toast, onClose }: ToastItemProps) {
  const Icon = toastIcons[toast.type]
  const styles = toastStyles[toast.type]

  return (
    <ToastPrimitive.Root
      duration={toast.duration || 5000}
      onOpenChange={(open) => {
        if (!open) onClose()
      }}
      className={cn(
        'flex items-start gap-3 p-4 rounded-lg border shadow-lg',
        'data-[state=open]:animate-in data-[state=closed]:animate-out',
        'data-[state=closed]:fade-out-80 data-[state=closed]:slide-out-to-right-full',
        'data-[state=open]:slide-in-from-right-full',
        styles.bg,
        styles.border
      )}
    >
      <Icon className={cn('h-5 w-5 flex-shrink-0 mt-0.5', styles.icon)} />
      <div className="flex-1 min-w-0">
        <ToastPrimitive.Title className="text-sm font-medium text-gray-900">
          {toast.title}
        </ToastPrimitive.Title>
        {toast.description && (
          <ToastPrimitive.Description className="text-sm text-gray-600 mt-1">
            {toast.description}
          </ToastPrimitive.Description>
        )}
      </div>
      <ToastPrimitive.Close
        className="p-1 text-gray-400 hover:text-gray-600 rounded transition-colors"
        aria-label="Close"
      >
        <X className="h-4 w-4" />
      </ToastPrimitive.Close>
    </ToastPrimitive.Root>
  )
}

// Convenience functions for common toast types
export function toast(toastFn: ToastContextValue['addToast']) {
  return {
    success: (title: string, description?: string) =>
      toastFn({ type: 'success', title, description }),
    error: (title: string, description?: string) =>
      toastFn({ type: 'error', title, description }),
    warning: (title: string, description?: string) =>
      toastFn({ type: 'warning', title, description }),
    info: (title: string, description?: string) =>
      toastFn({ type: 'info', title, description }),
  }
}

