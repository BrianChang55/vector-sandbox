/**
 * Drawer Component
 * 
 * Slide-in panel from left or right edge.
 * Enterprise-grade, minimal, clean design.
 */

import * as React from 'react'
import * as DialogPrimitive from '@radix-ui/react-dialog'
import { X } from 'lucide-react'
import { cn } from '@/lib/utils'

type DrawerSide = 'left' | 'right'

// ============================================================================
// Base Drawer Components
// ============================================================================

const Drawer = DialogPrimitive.Root
const DrawerTrigger = DialogPrimitive.Trigger
const DrawerClose = DialogPrimitive.Close
const DrawerPortal = DialogPrimitive.Portal

const DrawerOverlay = React.forwardRef<
  React.ComponentRef<typeof DialogPrimitive.Overlay>,
  React.ComponentPropsWithoutRef<typeof DialogPrimitive.Overlay>
>(({ className, ...props }, ref) => (
  <DialogPrimitive.Overlay
    ref={ref}
    className={cn(
      'fixed inset-0 z-50',
      'bg-black/30 backdrop-blur-[2px]',
      'data-[state=open]:animate-fade-in data-[state=closed]:animate-fade-out',
      className
    )}
    {...props}
  />
))
DrawerOverlay.displayName = 'DrawerOverlay'

interface DrawerContentProps
  extends React.ComponentPropsWithoutRef<typeof DialogPrimitive.Content> {
  side?: DrawerSide
  showCloseButton?: boolean
}

const DrawerContent = React.forwardRef<
  React.ComponentRef<typeof DialogPrimitive.Content>,
  DrawerContentProps
>(({ className, children, side = 'right', showCloseButton = true, ...props }, ref) => (
  <DrawerPortal>
    <DrawerOverlay />
    <DialogPrimitive.Content
      ref={ref}
      className={cn(
        // Position
        'fixed z-50 top-0 bottom-0',
        side === 'left' ? 'left-0' : 'right-0',
        // Sizing
        'w-full max-w-md h-full',
        // Appearance - enhanced shadow for depth
        'bg-white border-gray-200',
        side === 'left' ? 'border-r shadow-[4px_0_24px_-4px_rgba(0,0,0,0.12)]' : 'border-l shadow-[-4px_0_24px_-4px_rgba(0,0,0,0.12)]',
        // Animation - slide from appropriate side
        side === 'left'
          ? 'data-[state=open]:animate-slide-in-from-left data-[state=closed]:animate-slide-out-to-left'
          : 'data-[state=open]:animate-slide-in-from-right data-[state=closed]:animate-slide-out-to-right',
        // Focus
        'focus:outline-none',
        className
      )}
      {...props}
    >
      <div className="flex flex-col h-full">
        {children}
      </div>
      {showCloseButton && (
        <DialogPrimitive.Close
          className={cn(
            'absolute top-4',
            side === 'left' ? 'right-4' : 'right-4',
            'p-1.5 rounded-md',
            'text-gray-400 hover:text-gray-600',
            'hover:bg-gray-100',
            'transition-colors duration-150',
            'focus:outline-none'
          )}
        >
          <X className="h-4 w-4" />
          <span className="sr-only">Close</span>
        </DialogPrimitive.Close>
      )}
    </DialogPrimitive.Content>
  </DrawerPortal>
))
DrawerContent.displayName = 'DrawerContent'

const DrawerHeader = ({
  className,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) => (
  <div
    className={cn(
      'flex-shrink-0 px-6 py-5 border-b border-gray-200',
      className
    )}
    {...props}
  />
)
DrawerHeader.displayName = 'DrawerHeader'

const DrawerTitle = React.forwardRef<
  React.ComponentRef<typeof DialogPrimitive.Title>,
  React.ComponentPropsWithoutRef<typeof DialogPrimitive.Title>
>(({ className, ...props }, ref) => (
  <DialogPrimitive.Title
    ref={ref}
    className={cn('text-base font-semibold text-gray-900', className)}
    {...props}
  />
))
DrawerTitle.displayName = 'DrawerTitle'

const DrawerDescription = React.forwardRef<
  React.ComponentRef<typeof DialogPrimitive.Description>,
  React.ComponentPropsWithoutRef<typeof DialogPrimitive.Description>
>(({ className, ...props }, ref) => (
  <DialogPrimitive.Description
    ref={ref}
    className={cn('text-sm text-gray-500 mt-1', className)}
    {...props}
  />
))
DrawerDescription.displayName = 'DrawerDescription'

const DrawerBody = ({
  className,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) => (
  <div
    className={cn('flex-1 overflow-y-auto px-6 py-5', className)}
    {...props}
  />
)
DrawerBody.displayName = 'DrawerBody'

const DrawerFooter = ({
  className,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) => (
  <div
    className={cn(
      'flex-shrink-0 flex items-center justify-end gap-3 px-6 py-4 border-t border-gray-200 bg-gray-50',
      className
    )}
    {...props}
  />
)
DrawerFooter.displayName = 'DrawerFooter'

// ============================================================================
// Exports
// ============================================================================

export {
  Drawer,
  DrawerPortal,
  DrawerOverlay,
  DrawerTrigger,
  DrawerClose,
  DrawerContent,
  DrawerHeader,
  DrawerBody,
  DrawerFooter,
  DrawerTitle,
  DrawerDescription,
}

export type { DrawerSide }

