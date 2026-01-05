/**
 * UI Components - Central exports
 */

// Button
export { Button, buttonVariants, type ButtonProps } from './button'

// Input
export { Input, type InputProps } from './input'

// Dialog System
export {
  // Base components
  Dialog,
  DialogPortal,
  DialogOverlay,
  DialogTrigger,
  DialogClose,
  DialogContent,
  DialogHeader,
  DialogFooter,
  DialogTitle,
  DialogDescription,
  // Pre-built dialogs
  AlertDialog,
  ConfirmDialog,
  PromptDialog,
  CustomDialog,
  // Types
  type DialogVariant,
  type AlertDialogProps,
  type ConfirmDialogProps,
  type PromptDialogProps,
  type CustomDialogProps,
} from './dialog'

// Dialog Provider & Hook
export { 
  DialogProvider, 
  useDialog,
  type AlertOptions,
  type ConfirmOptions,
  type PromptOptions,
} from './dialog-provider'

// Dropdown Menu
export * from './dropdown-menu'

// Drawer
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
  type DrawerSide,
} from './drawer'

// Toast
export {
  ToastProvider,
  useToast,
  toast,
  type ToastType,
} from './toast'

