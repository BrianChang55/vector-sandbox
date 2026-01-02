# Frontend Style Guide

This document defines the coding standards, best practices, and patterns for the Internal Apps web application. **All developers and AI agents must refer to this guide when building features.**

## Table of Contents

1. [Technology Stack](#technology-stack)
2. [Project Structure](#project-structure)
3. [React Best Practices](#react-best-practices)
4. [TypeScript Guidelines](#typescript-guidelines)
5. [Tailwind CSS Guidelines](#tailwind-css-guidelines)
6. [Redux State Management](#redux-state-management)
7. [React Query Patterns](#react-query-patterns)
8. [Component Patterns](#component-patterns)
9. [Accessibility](#accessibility)
10. [Performance](#performance)
11. [Code Quality](#code-quality)

---

## Technology Stack

### Core Technologies
- **React 19+** - UI library with functional components and hooks
- **TypeScript** - Type safety and better developer experience
- **Vite** - Build tool and dev server
- **Tailwind CSS** - Utility-first CSS framework
- **Redux Toolkit** - Global state management
- **React Query (TanStack Query)** - Server state management and data fetching
- **Radix UI** - Accessible component primitives

### Key Libraries
- `clsx` + `tailwind-merge` - Class name utilities (via `cn()` helper)
- `class-variance-authority` - Component variant management
- `@radix-ui/*` - Accessible UI primitives
- `axios` - HTTP client for API calls

---

## Project Structure

```
src/
├── components/        # Reusable UI components
│   ├── ui/           # Base UI components (buttons, inputs, etc.)
│   └── features/     # Feature-specific components
├── store/            # Redux store configuration
│   ├── index.ts      # Store setup
│   ├── hooks.ts      # Typed Redux hooks
│   └── slices/       # Redux slices
├── services/         # API services and external integrations
├── hooks/            # Custom React hooks
├── lib/              # Utility functions
│   └── utils.ts      # General utilities (cn, etc.)
├── types/            # TypeScript type definitions
├── utils/            # Helper functions
├── App.tsx           # Root component
└── main.tsx          # Application entry point
```

---

## React Best Practices

### Component Structure

```typescript
// 1. Imports (grouped and ordered)
import { useState, useEffect } from 'react'
import { useSelector } from 'react-redux'
import * as Dialog from '@radix-ui/react-dialog'

import { Button } from '@/components/ui/Button'
import { useAuth } from '@/hooks/useAuth'
import { cn } from '@/lib/utils'
import type { User } from '@/types'

// 2. Types/Interfaces
interface ComponentProps {
  user: User
  onEdit?: (user: User) => void
  className?: string
}

// 3. Component (functional component only)
export function Component({ user, onEdit, className }: ComponentProps) {
  // 4. Hooks (in order: useState, useEffect, custom hooks, etc.)
  const [isOpen, setIsOpen] = useState(false)
  const authUser = useSelector((state: RootState) => state.auth.user)

  useEffect(() => {
    // Side effects
  }, [])

  // 5. Event handlers
  const handleEdit = () => {
    onEdit?.(user)
  }

  // 6. Render
  return (
    <div className={cn('base-classes', className)}>
      {/* Component JSX */}
    </div>
  )
}
```

### Rules

- **Functional components only** - No class components
- **Named exports** for components and utilities
- **Default exports** only for page/route components
- **Custom hooks** must start with `use` prefix
- **Hooks at the top** - All hooks must be called at the top level, not conditionally

### File Naming

- Components: `PascalCase.tsx` (e.g., `Button.tsx`, `UserProfile.tsx`)
- Utilities: `camelCase.ts` (e.g., `utils.ts`, `apiClient.ts`)
- Types: `camelCase.ts` or inline (e.g., `types.ts` or in component files)
- Hooks: `useCamelCase.ts` (e.g., `useAuth.ts`, `useUserData.ts`)
- Services: `camelCase.ts` (e.g., `authService.ts`, `api.ts`)

### Import Order

1. React and React-related imports
2. Third-party library imports
3. Internal imports (components, hooks, utils, types)
4. Relative imports (same directory)
5. CSS/asset imports

```typescript
import { useState, useEffect } from 'react'
import { useSelector } from 'react-redux'
import { useQuery } from '@tanstack/react-query'
import * as Dialog from '@radix-ui/react-dialog'

import { Button } from '@/components/ui/Button'
import { useAuth } from '@/hooks/useAuth'
import { cn } from '@/lib/utils'
import type { User } from '@/types'

import './Component.css'
```

---

## TypeScript Guidelines

### Type Definitions

- **Define interfaces** for component props and data structures
- **Use type aliases** for unions and intersections
- **Export types** from `types/` directory for shared types
- **Avoid `any`** - Use `unknown` if type is truly unknown, then narrow

```typescript
// types/user.ts
export interface User {
  id: string
  email: string
  name: string
}

// Component
import type { User } from '@/types/user'

interface UserCardProps {
  user: User
  onEdit?: (user: User) => void
  className?: string
}
```

### Type Safety

```typescript
// ✅ Good - Type narrowing
function processData(data: unknown): string {
  if (typeof data === 'string') {
    return data.toUpperCase()
  }
  throw new Error('Invalid data')
}

// ❌ Bad - Using any
function processData(data: any): string {
  return data.toUpperCase()
}
```

### Generic Types

Use generics for reusable type-safe utilities:

```typescript
interface ApiResponse<T> {
  data: T
  error?: string
}

function fetchData<T>(url: string): Promise<ApiResponse<T>> {
  // Implementation
}
```

---

## Tailwind CSS Guidelines

### Usage Principles

- **Always use Tailwind utility classes** - Avoid custom CSS when possible
- **Use `cn()` utility** for conditional classes and merging
- **Use theme tokens** defined in `tailwind.config.js` for colors, spacing, etc.
- **Prefer composition** - Build complex components from simple utilities
- **Mobile-first approach** - Base styles for mobile, then add breakpoints

### Class Naming Pattern

```typescript
// ✅ Good - Use cn() for merging and conditional classes
<button className={cn(
  'base-classes px-4 py-2 rounded-md',
  isActive && 'bg-primary text-white',
  variant === 'primary' && 'bg-blue-600',
  className // Allow external overrides
)}>

// ❌ Bad - String concatenation
<button className={`base ${isActive ? 'active' : ''} ${className}`}>
```

### Responsive Design

```typescript
// Mobile-first: base styles apply to mobile, then scale up
<div className="
  w-full 
  md:w-1/2 
  lg:w-1/3 
  xl:w-1/4
  p-4 
  md:p-6 
  lg:p-8
">
```

### Color Usage

```typescript
// ✅ Good - Use theme colors
<div className="bg-primary text-primary-foreground">
<div className="text-muted-foreground">
<div className="bg-background border-border">

// ❌ Bad - Hardcoded colors
<div className="bg-[#0EA5E9]">
```

### Spacing and Sizing

```typescript
// ✅ Good - Use Tailwind spacing scale
<div className="p-4 m-2 gap-4">

// ✅ Good - Use consistent spacing
<div className="space-y-4"> {/* 1rem gap between children */}
<div className="flex gap-4"> {/* 1rem gap in flex container */}
```

---

## Redux State Management

### When to Use Redux

- **Global state** - Shared across multiple components
- **Complex state logic** - State that involves multiple reducers
- **Server cache** - For client-side caching (complement React Query)
- **UI state** - Modal states, theme, sidebar state
- **NOT for**: Server state (use React Query), form state (use local state), component-local state

### Redux Toolkit Patterns

```typescript
// store/slices/exampleSlice.ts
import { createSlice, createAsyncThunk } from '@reduxjs/toolkit'
import type { PayloadAction } from '@reduxjs/toolkit'
import { exampleService } from '@/services/exampleService'

interface ExampleState {
  items: Item[]
  loading: boolean
  error: string | null
}

const initialState: ExampleState = {
  items: [],
  loading: false,
  error: null,
}

// Async thunk for API calls
export const fetchItems = createAsyncThunk(
  'example/fetchItems',
  async (_, { rejectWithValue }) => {
    try {
      const data = await exampleService.getItems()
      return data
    } catch (error) {
      return rejectWithValue(getErrorMessage(error))
    }
  }
)

const exampleSlice = createSlice({
  name: 'example',
  initialState,
  reducers: {
    // Synchronous reducers
    clearError: (state) => {
      state.error = null
    },
    addItem: (state, action: PayloadAction<Item>) => {
      state.items.push(action.payload)
    },
  },
  extraReducers: (builder) => {
    builder
      .addCase(fetchItems.pending, (state) => {
        state.loading = true
        state.error = null
      })
      .addCase(fetchItems.fulfilled, (state, action) => {
        state.loading = false
        state.items = action.payload
      })
      .addCase(fetchItems.rejected, (state, action) => {
        state.loading = false
        state.error = action.payload as string
      })
  },
})

export const { clearError, addItem } = exampleSlice.actions
export default exampleSlice.reducer
```

### Using Redux in Components

```typescript
// ✅ Good - Use typed hooks
import { useAppDispatch, useAppSelector } from '@/store/hooks'
import { fetchItems, clearError } from '@/store/slices/exampleSlice'

function Component() {
  const dispatch = useAppDispatch()
  const { items, loading, error } = useAppSelector((state) => state.example)

  useEffect(() => {
    dispatch(fetchItems())
  }, [dispatch])

  return (
    // Component JSX
  )
}

// ❌ Bad - Direct useSelector/useDispatch
import { useSelector, useDispatch } from 'react-redux'
```

---

## React Query Patterns

### When to Use React Query

- **Server state** - Data fetched from API
- **Caching** - Automatic caching and background refetching
- **Synchronization** - Keep server and client data in sync
- **Optimistic updates** - Update UI before server confirms
- **Pagination** - Built-in support for paginated data
- **NOT for**: Client-only state (use useState/Redux), form state

### Query Pattern

```typescript
// hooks/useItems.ts
import { useQuery } from '@tanstack/react-query'
import { exampleService } from '@/services/exampleService'

export function useItems() {
  return useQuery({
    queryKey: ['items'],
    queryFn: () => exampleService.getItems(),
    staleTime: 5 * 60 * 1000, // 5 minutes
    gcTime: 10 * 60 * 1000, // 10 minutes (formerly cacheTime)
  })
}

// Component usage
function ItemsList() {
  const { data: items, isLoading, error } = useItems()

  if (isLoading) return <LoadingSpinner />
  if (error) return <ErrorMessage error={error} />
  
  return (
    <ul>
      {items?.map(item => <Item key={item.id} item={item} />)}
    </ul>
  )
}
```

### Mutation Pattern

```typescript
// hooks/useCreateItem.ts
import { useMutation, useQueryClient } from '@tanstack/react-query'
import { exampleService } from '@/services/exampleService'

export function useCreateItem() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: (data: CreateItemData) => exampleService.createItem(data),
    onSuccess: () => {
      // Invalidate and refetch items query
      queryClient.invalidateQueries({ queryKey: ['items'] })
    },
    onError: (error) => {
      // Handle error
      console.error('Failed to create item:', error)
    },
  })
}

// Component usage
function CreateItemForm() {
  const createItem = useCreateItem()

  const handleSubmit = async (data: CreateItemData) => {
    try {
      await createItem.mutateAsync(data)
      // Success handled by onSuccess
    } catch (error) {
      // Error handled by onError
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      {/* Form fields */}
    </form>
  )
}
```

### Query Key Patterns

```typescript
// ✅ Good - Hierarchical query keys
const queryKeys = {
  items: ['items'] as const,
  item: (id: string) => ['items', id] as const,
  itemComments: (id: string) => ['items', id, 'comments'] as const,
}

// Usage
useQuery({ queryKey: queryKeys.items })
useQuery({ queryKey: queryKeys.item(id) })
```

### Optimistic Updates

```typescript
export function useUpdateItem() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: ({ id, data }: { id: string; data: UpdateItemData }) =>
      exampleService.updateItem(id, data),
    onMutate: async ({ id, data }) => {
      // Cancel outgoing refetches
      await queryClient.cancelQueries({ queryKey: ['items', id] })

      // Snapshot previous value
      const previousItem = queryClient.getQueryData(['items', id])

      // Optimistically update
      queryClient.setQueryData(['items', id], (old: Item) => ({
        ...old,
        ...data,
      }))

      return { previousItem }
    },
    onError: (err, variables, context) => {
      // Rollback on error
      if (context?.previousItem) {
        queryClient.setQueryData(['items', variables.id], context.previousItem)
      }
    },
    onSettled: (data, error, variables) => {
      // Refetch after error or success
      queryClient.invalidateQueries({ queryKey: ['items', variables.id] })
    },
  })
}
```

### Combining Redux and React Query

```typescript
// Use Redux for: UI state, client-side state, global app state
// Use React Query for: Server state, API data, caching

// Example: Auth state in Redux, user data from API in React Query
function UserProfile() {
  // Redux: Auth state (isAuthenticated, token)
  const isAuthenticated = useAppSelector((state) => state.auth.isAuthenticated)
  
  // React Query: Server data (user profile from API)
  const { data: user } = useQuery({
    queryKey: ['user', 'profile'],
    queryFn: () => userService.getProfile(),
    enabled: isAuthenticated, // Only fetch when authenticated
  })

  // Component logic
}
```

---

## Component Patterns

### Component with Variants (CVA)

```typescript
import { cva, type VariantProps } from 'class-variance-authority'
import { cn } from '@/lib/utils'

const buttonVariants = cva(
  'inline-flex items-center justify-center rounded-md font-medium transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-offset-2 disabled:opacity-50 disabled:pointer-events-none',
  {
    variants: {
      variant: {
        default: 'bg-primary text-primary-foreground hover:bg-primary/90',
        destructive: 'bg-destructive text-destructive-foreground hover:bg-destructive/90',
        outline: 'border border-input bg-background hover:bg-accent',
        ghost: 'hover:bg-accent hover:text-accent-foreground',
      },
      size: {
        default: 'h-10 px-4 py-2',
        sm: 'h-9 rounded-md px-3',
        lg: 'h-11 rounded-md px-8',
      },
    },
    defaultVariants: {
      variant: 'default',
      size: 'default',
    },
  }
)

interface ButtonProps
  extends React.ButtonHTMLAttributes<HTMLButtonElement>,
    VariantProps<typeof buttonVariants> {
  asChild?: boolean
}

export function Button({ className, variant, size, ...props }: ButtonProps) {
  return (
    <button
      className={cn(buttonVariants({ variant, size }), className)}
      {...props}
    />
  )
}
```

### Radix UI Component Pattern

```typescript
import * as Dialog from '@radix-ui/react-dialog'
import { cn } from '@/lib/utils'

interface ModalProps {
  open: boolean
  onOpenChange: (open: boolean) => void
  children: React.ReactNode
}

export function Modal({ open, onOpenChange, children }: ModalProps) {
  return (
    <Dialog.Root open={open} onOpenChange={onOpenChange}>
      <Dialog.Portal>
        <Dialog.Overlay className="fixed inset-0 bg-black/50" />
        <Dialog.Content
          className={cn(
            'fixed top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2',
            'bg-background rounded-lg p-6 shadow-lg',
            'max-w-md w-full'
          )}
        >
          {children}
        </Dialog.Content>
      </Dialog.Portal>
    </Dialog.Root>
  )
}
```

---

## Accessibility

### Semantic HTML

- Use semantic HTML elements (`<nav>`, `<main>`, `<header>`, `<button>`, etc.)
- Provide proper ARIA labels when semantic HTML isn't sufficient
- Use Radix UI components (they're accessible by default)

### Keyboard Navigation

- Ensure all interactive elements are keyboard accessible
- Use proper focus management
- Test with keyboard-only navigation (Tab, Enter, Escape, Arrow keys)

### Screen Readers

- Provide alt text for images
- Use ARIA labels for complex interactions
- Test with screen readers

```typescript
// ✅ Good
<button aria-label="Close dialog">
  <XIcon />
</button>

// ✅ Good
<img src="..." alt="User profile picture" />

// ❌ Bad
<div onClick={handleClick}>Click me</div>
```

---

## Performance

### React Optimization

```typescript
// Use React.memo() sparingly for expensive components
export const ExpensiveComponent = React.memo(function ExpensiveComponent({ data }: Props) {
  // Component implementation
})

// Use useMemo() for expensive calculations
const expensiveValue = useMemo(() => {
  return computeExpensiveValue(data)
}, [data])

// Use useCallback() for stable function references
const handleClick = useCallback(() => {
  doSomething(id)
}, [id])
```

### Code Splitting

```typescript
// Lazy load routes
const AdminPage = React.lazy(() => import('./pages/AdminPage'))

function App() {
  return (
    <Suspense fallback={<LoadingSpinner />}>
      <Routes>
        <Route path="/admin" element={<AdminPage />} />
      </Routes>
    </Suspense>
  )
}
```

### Bundle Optimization

```typescript
// ✅ Good - Tree-shakeable imports
import { Button } from '@/components/ui/Button'

// ❌ Bad - Imports entire library
import * as Components from '@/components'
```

---

## Code Quality

### Linting and Formatting

- ESLint is configured - fix linting errors before committing
- Follow ESLint warnings and errors
- Use consistent formatting (Prettier if configured)

### Error Handling

```typescript
// ✅ Good - Proper error handling
try {
  const data = await api.getData()
  // Use data
} catch (error) {
  const message = getErrorMessage(error)
  logger.error('Failed to fetch data', { error: message })
  // Show user-friendly error
}

// ❌ Bad - Silent failures
const data = await api.getData() // What if this fails?
```

### Testing (when implemented)

- Write unit tests for utilities
- Write integration tests for components
- Test user interactions, not implementation details
- Use React Testing Library for component tests

---

## References

- [React Documentation](https://react.dev)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/)
- [Tailwind CSS Documentation](https://tailwindcss.com/docs)
- [Redux Toolkit Documentation](https://redux-toolkit.js.org/)
- [React Query Documentation](https://tanstack.com/query/latest)
- [Radix UI Documentation](https://www.radix-ui.com/)

---

**Remember**: Consistency is key. When in doubt, refer to existing code patterns and this guide.

