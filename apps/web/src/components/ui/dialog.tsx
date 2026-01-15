/**
 * Enterprise-grade Dialog component system
 * 
 * Clean, minimal, properly centered modals with smooth animations.
 */

import * as React from 'react'
import * as DialogPrimitive from '@radix-ui/react-dialog'
import { X, AlertCircle, CheckCircle, AlertTriangle, Info } from 'lucide-react'
import { cn } from '@/lib/utils'
import { Button } from './button'

// ============================================================================
// Base Dialog Components
// ============================================================================

const Dialog = DialogPrimitive.Root
const DialogTrigger = DialogPrimitive.Trigger
const DialogPortal = DialogPrimitive.Portal
const DialogClose = DialogPrimitive.Close

const DialogOverlay = React.forwardRef<
  React.ComponentRef<typeof DialogPrimitive.Overlay>,
  React.ComponentPropsWithoutRef<typeof DialogPrimitive.Overlay>
>(({ className, ...props }, ref) => (
  <DialogPrimitive.Overlay
    ref={ref}
    className={cn(
      'fixed inset-0 z-50 bg-black/50 backdrop-blur-[1px]',
      'data-[state=open]:animate-in data-[state=closed]:animate-out',
      'data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0',
      className
    )}
    {...props}
  />
))
DialogOverlay.displayName = DialogPrimitive.Overlay.displayName

const DialogContent = React.forwardRef<
  React.ComponentRef<typeof DialogPrimitive.Content>,
  React.ComponentPropsWithoutRef<typeof DialogPrimitive.Content> & {
    showCloseButton?: boolean
  }
>(({ className, children, showCloseButton = true, ...props }, ref) => (
  <DialogPortal>
    <DialogOverlay />
    <DialogPrimitive.Content
      ref={ref}
      className={cn(
        // Centering
        'fixed left-1/2 top-1/2 z-50 -translate-x-1/2 -translate-y-1/2',
        // Sizing
        'w-full max-w-md',
        // Appearance
        'bg-white rounded-lg shadow-xl',
        'border border-gray-100',
        // Animation
        'data-[state=open]:animate-in data-[state=closed]:animate-out',
        'data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0',
        'data-[state=closed]:zoom-out-95 data-[state=open]:zoom-in-95',
        'data-[state=closed]:slide-out-to-left-1/2 data-[state=closed]:slide-out-to-top-[48%]',
        'data-[state=open]:slide-in-from-left-1/2 data-[state=open]:slide-in-from-top-[48%]',
        'duration-200',
        // Focus
        'focus:outline-none',
        className
      )}
      {...props}
    >
      {children}
      {showCloseButton && (
        <DialogPrimitive.Close 
          className={cn(
            'absolute right-3 top-3',
            'rounded-sm p-1.5',
            'text-gray-400 hover:text-gray-600',
            'hover:bg-gray-100',
            'transition-colors',
            'focus:outline-none focus:ring-2 focus:ring-gray-300'
          )}
        >
          <X className="h-4 w-4" />
          <span className="sr-only">Close</span>
        </DialogPrimitive.Close>
      )}
    </DialogPrimitive.Content>
  </DialogPortal>
))
DialogContent.displayName = DialogPrimitive.Content.displayName

const DialogHeader = ({
  className,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) => (
  <div
    className={cn('px-5 pt-5 pb-1', className)}
    {...props}
  />
)
DialogHeader.displayName = 'DialogHeader'

const DialogBody = ({
  className,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) => (
  <div
    className={cn('px-5 py-3', className)}
    {...props}
  />
)
DialogBody.displayName = 'DialogBody'

const DialogFooter = ({
  className,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) => (
  <div
    className={cn(
      'flex justify-end gap-2 px-5 pb-5 pt-3',
      className
    )}
    {...props}
  />
)
DialogFooter.displayName = 'DialogFooter'

const DialogTitle = React.forwardRef<
  React.ComponentRef<typeof DialogPrimitive.Title>,
  React.ComponentPropsWithoutRef<typeof DialogPrimitive.Title>
>(({ className, ...props }, ref) => (
  <DialogPrimitive.Title
    ref={ref}
    className={cn(
      'text-base font-semibold text-gray-900',
      className
    )}
    {...props}
  />
))
DialogTitle.displayName = DialogPrimitive.Title.displayName

const DialogDescription = React.forwardRef<
  React.ComponentRef<typeof DialogPrimitive.Description>,
  React.ComponentPropsWithoutRef<typeof DialogPrimitive.Description>
>(({ className, ...props }, ref) => (
  <DialogPrimitive.Description
    ref={ref}
    className={cn('text-sm text-gray-500 mt-1', className)}
    {...props}
  />
))
DialogDescription.displayName = DialogPrimitive.Description.displayName

// ============================================================================
// Dialog Variants & Icons
// ============================================================================

type DialogVariant = 'default' | 'destructive' | 'warning' | 'success' | 'info'

const variantConfig: Record<DialogVariant, { 
  icon: React.ElementType
  iconBg: string
  iconColor: string
  buttonVariant: 'default' | 'destructive'
}> = {
  default: {
    icon: Info,
    iconBg: 'bg-gray-100',
    iconColor: 'text-gray-600',
    buttonVariant: 'default',
  },
  destructive: {
    icon: AlertCircle,
    iconBg: 'bg-red-50',
    iconColor: 'text-red-500',
    buttonVariant: 'destructive',
  },
  warning: {
    icon: AlertTriangle,
    iconBg: 'bg-amber-50',
    iconColor: 'text-amber-500',
    buttonVariant: 'default',
  },
  success: {
    icon: CheckCircle,
    iconBg: 'bg-green-50',
    iconColor: 'text-green-500',
    buttonVariant: 'default',
  },
  info: {
    icon: Info,
    iconBg: 'bg-blue-50',
    iconColor: 'text-blue-500',
    buttonVariant: 'default',
  },
}

// ============================================================================
// Alert Dialog
// ============================================================================

interface AlertDialogProps {
  open: boolean
  onOpenChange: (open: boolean) => void
  title: string
  description?: string
  variant?: DialogVariant
  confirmText?: string
  onConfirm?: () => void
}

function AlertDialog({
  open,
  onOpenChange,
  title,
  description,
  variant = 'default',
  confirmText = 'OK',
  onConfirm,
}: AlertDialogProps) {
  const config = variantConfig[variant]
  const Icon = config.icon

  const handleConfirm = () => {
    onConfirm?.()
    onOpenChange(false)
  }

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent showCloseButton={false} className="max-w-sm">
        <div className="p-5 text-center">
          <div className={cn(
            'mx-auto flex h-11 w-11 items-center justify-center rounded-full',
            config.iconBg
          )}>
            <Icon className={cn('h-5 w-5', config.iconColor)} />
          </div>
          
          <DialogTitle className="mt-4 text-base font-semibold text-gray-900">
            {title}
          </DialogTitle>
          
          {description && (
            <DialogDescription className="mt-1.5 text-sm text-gray-500">
              {description}
            </DialogDescription>
          )}
          
          <div className="mt-5">
            <Button
              onClick={handleConfirm}
              className="w-full"
              variant={config.buttonVariant}
            >
              {confirmText}
            </Button>
          </div>
        </div>
      </DialogContent>
    </Dialog>
  )
}

// ============================================================================
// Confirm Dialog
// ============================================================================

interface ConfirmDialogProps {
  open: boolean
  onOpenChange: (open: boolean) => void
  title: string
  description?: string
  variant?: DialogVariant
  confirmText?: string
  cancelText?: string
  onConfirm: () => void | Promise<void>
  onCancel?: () => void
  loading?: boolean
}

function ConfirmDialog({
  open,
  onOpenChange,
  title,
  description,
  variant = 'default',
  confirmText = 'Confirm',
  cancelText = 'Cancel',
  onConfirm,
  onCancel,
  loading = false,
}: ConfirmDialogProps) {
  const config = variantConfig[variant]
  const Icon = config.icon
  const [isLoading, setIsLoading] = React.useState(false)

  const handleConfirm = async () => {
    setIsLoading(true)
    try {
      await onConfirm()
      onOpenChange(false)
    } finally {
      setIsLoading(false)
    }
  }

  const handleCancel = () => {
    onCancel?.()
    onOpenChange(false)
  }

  const showLoading = loading || isLoading

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent showCloseButton={false} className="max-w-sm">
        <div className="p-5 text-center">
          <div className={cn(
            'mx-auto flex h-11 w-11 items-center justify-center rounded-full',
            config.iconBg
          )}>
            <Icon className={cn('h-5 w-5', config.iconColor)} />
          </div>
          
          <DialogTitle className="mt-4 text-base font-semibold text-gray-900">
            {title}
          </DialogTitle>
          
          {description && (
            <DialogDescription className="mt-1.5 text-sm text-gray-500">
              {description}
            </DialogDescription>
          )}
          
          <div className="mt-5 flex gap-2">
            <Button
              onClick={handleCancel}
              variant="outline"
              className="flex-1"
              disabled={showLoading}
              disableFocusRing
            >
              {cancelText}
            </Button>
            <Button
              onClick={handleConfirm}
              variant={config.buttonVariant}
              className="flex-1"
              disabled={showLoading}
            >
              {showLoading ? (
                <span className="flex items-center justify-center gap-2">
                  <svg className="h-4 w-4 animate-spin" viewBox="0 0 24 24" fill="none">
                    <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" />
                    <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z" />
                  </svg>
                </span>
              ) : (
                confirmText
              )}
            </Button>
          </div>
        </div>
      </DialogContent>
    </Dialog>
  )
}

// ============================================================================
// Prompt Dialog
// ============================================================================

interface PromptDialogProps {
  open: boolean
  onOpenChange: (open: boolean) => void
  title: string
  description?: string
  placeholder?: string
  defaultValue?: string
  confirmText?: string
  cancelText?: string
  onConfirm: (value: string) => void | Promise<void>
  onCancel?: () => void
  inputType?: 'text' | 'email' | 'password' | 'url' | 'number'
  required?: boolean
}

function PromptDialog({
  open,
  onOpenChange,
  title,
  description,
  placeholder = '',
  defaultValue = '',
  confirmText = 'Continue',
  cancelText = 'Cancel',
  onConfirm,
  onCancel,
  inputType = 'text',
  required = false,
}: PromptDialogProps) {
  const [value, setValue] = React.useState(defaultValue)
  const [isLoading, setIsLoading] = React.useState(false)
  const inputRef = React.useRef<HTMLInputElement>(null)

  React.useEffect(() => {
    if (open) {
      setValue(defaultValue)
      setTimeout(() => inputRef.current?.focus(), 50)
    }
  }, [open, defaultValue])

  const handleConfirm = async () => {
    if (required && !value.trim()) return

    setIsLoading(true)
    try {
      await onConfirm(value)
      onOpenChange(false)
    } finally {
      setIsLoading(false)
    }
  }

  const handleCancel = () => {
    onCancel?.()
    onOpenChange(false)
  }

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault()
      handleConfirm()
    }
  }

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent showCloseButton className="max-w-sm">
        <div className="p-5">
          {/* Header */}
          <DialogTitle className="text-base font-semibold text-gray-900">
            {title}
          </DialogTitle>
          {description && (
            <DialogDescription className="mt-1 text-sm text-gray-500">
              {description}
            </DialogDescription>
          )}
          
          {/* Input */}
          <div className="mt-4">
            <input
              ref={inputRef}
              type={inputType}
              value={value}
              onChange={(e) => setValue(e.target.value)}
              onKeyDown={handleKeyDown}
              placeholder={placeholder}
              disabled={isLoading}
              className={cn(
                'w-full h-10 px-3 rounded-md text-sm',
                'border border-gray-200',
                'bg-white text-gray-900',
                'placeholder:text-gray-400',
                'focus:outline-none focus:ring-2 focus:ring-gray-900 focus:border-transparent',
                'disabled:opacity-50 disabled:cursor-not-allowed',
                'transition-shadow'
              )}
            />
          </div>
          
          {/* Actions */}
          <div className="mt-5 flex justify-end gap-2">
            <Button
              onClick={handleCancel}
              variant="outline"
              size="sm"
              disabled={isLoading}
            >
              {cancelText}
            </Button>
            <Button
              onClick={handleConfirm}
              size="sm"
              disabled={isLoading || (required && !value.trim())}
            >
              {isLoading ? (
                <span className="flex items-center gap-2">
                  <svg className="h-3.5 w-3.5 animate-spin" viewBox="0 0 24 24" fill="none">
                    <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" />
                    <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z" />
                  </svg>
                </span>
              ) : (
                confirmText
              )}
            </Button>
          </div>
        </div>
      </DialogContent>
    </Dialog>
  )
}

// ============================================================================
// Custom Content Dialog
// ============================================================================

interface CustomDialogProps {
  open: boolean
  onOpenChange: (open: boolean) => void
  title: string
  description?: string
  children: React.ReactNode
  className?: string
  showCloseButton?: boolean
}

function CustomDialog({
  open,
  onOpenChange,
  title,
  description,
  children,
  className,
  showCloseButton = true,
}: CustomDialogProps) {
  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent showCloseButton={showCloseButton} className={className}>
        <DialogHeader>
          <DialogTitle>{title}</DialogTitle>
          {description && <DialogDescription>{description}</DialogDescription>}
        </DialogHeader>
        <DialogBody>{children}</DialogBody>
      </DialogContent>
    </Dialog>
  )
}

// ============================================================================
// Exports
// ============================================================================

export {
  Dialog,
  DialogPortal,
  DialogOverlay,
  DialogTrigger,
  DialogClose,
  DialogContent,
  DialogHeader,
  DialogBody,
  DialogFooter,
  DialogTitle,
  DialogDescription,
  AlertDialog,
  ConfirmDialog,
  PromptDialog,
  CustomDialog,
}

export type { DialogVariant, AlertDialogProps, ConfirmDialogProps, PromptDialogProps, CustomDialogProps }
