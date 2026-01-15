/**
 * Create Organization Dialog
 * 
 * Clean, minimal dialog for creating a new organization.
 * Follows the enterprise style guide.
 */
import * as React from 'react'
import { Loader2, Plus } from 'lucide-react'
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
  DialogDescription,
  DialogBody,
  DialogFooter,
} from '../ui/dialog'
import { Button } from '../ui/button'
import { useCreateOrganization, useSwitchOrganization, useOrganizations } from '../../hooks/useOrganizations'

interface CreateOrganizationDialogProps {
  open: boolean
  onOpenChange: (open: boolean) => void
}

/**
 * Generate a URL-safe slug from a name
 */
function generateSlug(name: string): string {
  return name
    .toLowerCase()
    .trim()
    .replace(/[^a-z0-9\s-]/g, '')
    .replace(/\s+/g, '-')
    .replace(/-+/g, '-')
    .slice(0, 50)
}

export function CreateOrganizationDialog({
  open,
  onOpenChange,
}: CreateOrganizationDialogProps) {
  const [name, setName] = React.useState('')
  const [error, setError] = React.useState<string | null>(null)
  const inputRef = React.useRef<HTMLInputElement>(null)

  const createOrg = useCreateOrganization()
  const switchOrg = useSwitchOrganization()
  const { data: organizations } = useOrganizations()

  // Check if organization name or slug already exists
  const trimmedName = name.trim()
  const slug = generateSlug(trimmedName)
  const nameExists = React.useMemo(() => {
    if (!trimmedName || !organizations) return false
    return organizations.some(
      (org) => 
        org.name.toLowerCase() === trimmedName.toLowerCase() ||
        org.slug.toLowerCase() === slug.toLowerCase()
    )
  }, [trimmedName, slug, organizations])

  // Focus input when dialog opens
  React.useEffect(() => {
    if (open) {
      setName('')
      setError(null)
      setTimeout(() => inputRef.current?.focus(), 50)
    }
  }, [open])

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    
    if (!trimmedName) {
      setError('Organization name is required')
      return
    }

    if (trimmedName.length < 2) {
      setError('Organization name must be at least 2 characters')
      return
    }

    if (!slug) {
      setError('Please enter a valid organization name')
      return
    }

    if (nameExists) {
      setError('An organization with this name already exists')
      return
    }

    setError(null)

    try {
      const newOrg = await createOrg.mutateAsync({ name: trimmedName, slug })
      // Switch to the newly created organization and wait for it to complete
      await switchOrg.mutateAsync(newOrg.id)
      onOpenChange(false)
    } catch (err: any) {
      const message = err?.response?.data?.detail 
        || err?.response?.data?.slug?.[0]
        || err?.response?.data?.name?.[0]
        || 'Failed to create organization'
      setError(message)
    }
  }

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault()
      handleSubmit(e)
    }
  }

  const isLoading = createOrg.isPending

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent showCloseButton className="max-w-md">
        <form onSubmit={handleSubmit}>
          <DialogHeader>
            <DialogTitle>Create Organization</DialogTitle>
            <DialogDescription>
              Create a new organization to manage apps and team members.
            </DialogDescription>
          </DialogHeader>
          <DialogBody>
            <div className="space-y-4">
              <div>
                <label 
                  htmlFor="org-name" 
                  className="block text-sm font-medium text-gray-700 mb-2"
                >
                  Organization name
                </label>
                <input
                  ref={inputRef}
                  id="org-name"
                  type="text"
                  value={name}
                  onChange={(e) => {
                    setName(e.target.value)
                    setError(null)
                  }}
                  onKeyDown={handleKeyDown}
                  placeholder="Acme Inc."
                  disabled={isLoading}
                  autoComplete="off"
                  className="w-full px-3 py-2 bg-white border border-gray-200 rounded text-sm text-gray-900 placeholder:text-gray-400 focus:outline-none focus:border-gray-400"
                  autoFocus
                />
                {nameExists && (
                  <p className="mt-2 text-xs text-red-600">
                    An organization with this name already exists
                  </p>
                )}
                {error && !nameExists && (
                  <p className="mt-2 text-xs text-red-600">
                    {error}
                  </p>
                )}
              </div>
            </div>
          </DialogBody>
          <DialogFooter>
            <Button
              type="button"
              variant="outline"
              onClick={() => onOpenChange(false)}
              disabled={isLoading}
            >
              Cancel
            </Button>
            <Button
              type="submit"
              disabled={isLoading || !name.trim() || nameExists}
            >
              {isLoading ? (
                <>
                  <Loader2 className="h-4 w-4 mr-2 animate-spin" />
                  Creating...
                </>
              ) : (
                <>
                  <Plus className="h-4 w-4 mr-2" />
                  Create Organization
                </>
              )}
            </Button>
          </DialogFooter>
        </form>
      </DialogContent>
    </Dialog>
  )
}

