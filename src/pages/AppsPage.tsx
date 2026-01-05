/**
 * Apps list page
 *
 * Clean, light-themed enterprise dashboard showing all internal apps.
 * Follows the established design system: white backgrounds, gray borders,
 * subtle shadows, and minimal design.
 */
import { useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { useAppSelector } from '../store/hooks'
import { useApps, useCreateApp, useUpdateApp, useDeleteApp } from '../hooks/useApps'
import { Button } from '../components/ui/button'
import { useDialog } from '../components/ui/dialog-provider'
import { DropdownMenu, DropdownMenuContent, DropdownMenuItem, DropdownMenuSeparator, DropdownMenuTrigger } from '../components/ui/dropdown-menu'
import { Dialog, DialogBody, DialogContent, DialogDescription, DialogFooter, DialogHeader, DialogTitle } from '../components/ui/dialog'
import { 
  Plus, 
  Layers, 
  Clock,
  CheckCircle,
  Code2,
  FolderOpen,
  MoreVertical,
  PencilLine,
  Trash2,
  Loader2,
} from 'lucide-react'
import { formatDistanceToNow } from 'date-fns'

export function AppsPage() {
  const selectedOrgId = useAppSelector((state) => state.ui.selectedOrgId)
  const { data: apps, isLoading } = useApps(selectedOrgId || null)
  const createApp = useCreateApp()
  const updateApp = useUpdateApp()
  const deleteApp = useDeleteApp()
  const { confirm } = useDialog()
  const navigate = useNavigate()

  const [savingAppId, setSavingAppId] = useState<string | null>(null)
  const [deletingAppId, setDeletingAppId] = useState<string | null>(null)
  const [formDialogOpen, setFormDialogOpen] = useState(false)
  const [formMode, setFormMode] = useState<'create' | 'edit'>('create')
  const [formAppId, setFormAppId] = useState<string | null>(null)
  const [formName, setFormName] = useState('')
  const [formDescription, setFormDescription] = useState('')
  const [formSaving, setFormSaving] = useState(false)

  const resetFormDialog = () => {
    setFormAppId(null)
    setFormName('')
    setFormDescription('')
    setFormMode('create')
  }

  const openCreateDialog = () => {
    resetFormDialog()
    setFormMode('create')
    setFormDialogOpen(true)
  }

  const openEditDialog = (app: { id: string; name: string; description?: string | null }) => {
    setFormMode('edit')
    setFormAppId(app.id)
    setFormName(app.name || '')
    setFormDescription((app.description || '').slice(0, 60))
    setFormDialogOpen(true)
  }

  const handleFormSubmit = async () => {
    const cleanedName = formName.trim()
    const cleanedDescription = formDescription.trim().slice(0, 60)

    if (!cleanedName) return

    if (formMode === 'create' && !selectedOrgId) return

    setFormSaving(true)

    try {
      if (formMode === 'create') {
        await createApp.mutateAsync({
          orgId: selectedOrgId!,
          data: { name: cleanedName, description: cleanedDescription, backend_connection: null },
        })
      } else if (formMode === 'edit' && formAppId) {
        setSavingAppId(formAppId)
        await updateApp.mutateAsync({
          appId: formAppId,
          data: { name: cleanedName, description: cleanedDescription },
        })
      }
      setFormDialogOpen(false)
      resetFormDialog()
    } catch (error) {
      console.error('Failed to save app', error)
    } finally {
      setFormSaving(false)
      setSavingAppId(null)
    }
  }

  const handleFormOpenChange = (open: boolean) => {
    if (!open && !formSaving) {
      setFormDialogOpen(false)
      resetFormDialog()
    } else {
      setFormDialogOpen(open)
    }
  }

  const handleDeleteApp = async (appId: string, appName: string) => {
    const confirmed = await confirm({
      title: 'Delete app?',
      description: `This will permanently delete "${appName || 'this app'}".`,
      variant: 'destructive',
      confirmText: 'Delete',
    })

    if (!confirmed) return

    setDeletingAppId(appId)
    try {
      await deleteApp.mutateAsync({ appId })
    } catch (error) {
      console.error('Failed to delete app', error)
    } finally {
      setDeletingAppId(null)
    }
  }

  if (!selectedOrgId) {
    return (
      <div className="flex items-center justify-center h-full bg-gray-50">
        <div className="text-center">
          <Layers className="h-12 w-12 text-gray-300 mx-auto mb-4" />
          <p className="text-gray-500">Please select an organization</p>
        </div>
      </div>
    )
  }

  if (isLoading) {
    return (
      <div className="flex items-center justify-center h-full bg-gray-50">
        <div className="text-gray-500">Loading apps...</div>
      </div>
    )
  }

  const dialogTitle = formMode === 'create' ? 'Create New App' : 'Edit App'
  const dialogSubtitle =
    formMode === 'create'
      ? 'Add a title and short description for your new internal app.'
      : 'Update the title and description.'
  const dialogPrimaryText = formMode === 'create' ? 'Create App' : 'Save Changes'

  return (
    <>
      <div className="min-h-full bg-gray-50">
      {/* Header */}
      <div className="bg-white border-b border-gray-200">
        <div className="max-w-6xl mx-auto px-6 py-3 min-h-[66px] flex items-center justify-between">
          <div>
            <h1 className="text-xl font-semibold text-gray-900">Apps</h1>
            <p className="text-sm text-gray-500 mt-0.5">
              Build and manage internal applications
            </p>
          </div>
          <Button onClick={openCreateDialog}>
            <Plus className="h-4 w-4 mr-1.5" />
            New App
          </Button>
        </div>
      </div>

      {/* Content */}
      <div className="max-w-6xl mx-auto px-6 py-6">
        {apps && apps.length > 0 ? (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
            {apps.map((app) => (
              <div
                key={app.id}
                className="group block bg-white rounded-lg border border-gray-200 p-5 
                         hover:border-gray-300 hover:shadow-sm transition-all"
                role="button"
                tabIndex={0}
                onClick={() => navigate(`/apps/${app.id}`)}
                onKeyDown={(event) => {
                  if (event.key === 'Enter' || event.key === ' ') {
                    event.preventDefault()
                    navigate(`/apps/${app.id}`)
                  }
                }}
              >
                <div className="flex items-start justify-between mb-3">
                  <div className="h-10 w-10 rounded-lg bg-gray-100 flex items-center justify-center">
                    <Layers className="h-5 w-5 text-gray-500" />
                  </div>
                  <div className="flex items-center gap-1" onClick={(event) => event.stopPropagation()}>
                    {savingAppId === app.id && (
                      <Loader2 className="h-4 w-4 text-gray-400 animate-spin" />
                    )}
                    <DropdownMenu>
                      <DropdownMenuTrigger asChild>
                        <button
                          className="p-1 rounded-md text-gray-400 hover:text-gray-600 hover:bg-gray-100 transition-colors"
                          aria-label="App actions"
                        >
                          <MoreVertical className="h-4 w-4" />
                        </button>
                      </DropdownMenuTrigger>
                      <DropdownMenuContent align="end" sideOffset={6}>
                        <DropdownMenuItem
                          onSelect={(event) => {
                            event.preventDefault()
                            openEditDialog(app)
                          }}
                          className="gap-2"
                        >
                          <PencilLine className="h-4 w-4 text-gray-500" />
                          Edit
                        </DropdownMenuItem>
                        <DropdownMenuSeparator />
                        <DropdownMenuItem
                          onSelect={(event) => {
                            event.preventDefault()
                            handleDeleteApp(app.id, app.name)
                          }}
                          className="gap-2 text-red-600 focus:bg-red-50"
                        >
                          <Trash2 className="h-4 w-4" />
                          Delete app
                        </DropdownMenuItem>
                      </DropdownMenuContent>
                    </DropdownMenu>
                  </div>
                </div>

                <div className="mb-1">
                  <div
                    className="inline-block -mx-2 px-2 py-1 rounded-[4px] cursor-text transition-colors hover:bg-gray-50"
                    onClick={(event) => {
                      event.preventDefault()
                      event.stopPropagation()
                      openEditDialog(app)
                    }}
                    onMouseDown={(event) => event.stopPropagation()}
                  >
                    <span className="font-medium text-gray-900 hover:text-gray-700 truncate">
                      {app.name || 'Untitled app'}
                    </span>
                  </div>
                </div>
                <div className="mb-4">
                  <div
                    className="relative -mx-2 px-2 py-1 rounded-[4px] cursor-text transition-colors hover:bg-gray-50"
                    onClick={(event) => {
                      event.preventDefault()
                      event.stopPropagation()
                      openEditDialog(app)
                    }}
                    onMouseDown={(event) => event.stopPropagation()}
                  >
                    <p className="text-sm text-gray-500 truncate transition-colors hover:text-gray-600">
                      {(app.description || 'No description').slice(0, 60)}
                    </p>
                  </div>
                </div>

                <div className="flex items-center gap-3">
                  <span
                    className={`flex items-center gap-1.5 text-xs px-2 py-1 rounded-full ${
                      app.status === 'published'
                        ? 'bg-green-50 text-green-700 border border-green-200'
                        : 'bg-gray-100 text-gray-600 border border-gray-200'
                    }`}
                  >
                    {app.status === 'published' ? (
                      <CheckCircle className="h-3 w-3" />
                    ) : (
                      <Code2 className="h-3 w-3" />
                    )}
                    {app.status_display}
                  </span>
                  
                  <span className="flex items-center gap-1 text-xs text-gray-400">
                    <Clock className="h-3 w-3" />
                    {formatDistanceToNow(new Date(app.updated_at), { addSuffix: true })}
                  </span>
                </div>
                {deletingAppId === app.id && (
                  <p className="mt-2 text-xs text-red-500">Deleting…</p>
                )}
              </div>
            ))}
          </div>
        ) : (
          <div className="flex flex-col items-center justify-center py-16">
            <div className="h-16 w-16 rounded-xl bg-gray-100 flex items-center justify-center mb-4">
              <FolderOpen className="h-8 w-8 text-gray-400" />
            </div>
            <h2 className="text-lg font-medium text-gray-900 mb-1">
              No apps yet
            </h2>
            <p className="text-sm text-gray-500 mb-6 max-w-sm text-center">
              Create your first internal application to get started with building powerful tools.
            </p>
            <Button onClick={openCreateDialog}>
              <Plus className="h-4 w-4 mr-2" />
              Create Your First App
            </Button>
          </div>
        )}
      </div>

      {/* Create/Edit dialog */}
      <Dialog open={formDialogOpen} onOpenChange={handleFormOpenChange}>
        <DialogContent showCloseButton className="max-w-md">
          <form
            onSubmit={(event) => {
              event.preventDefault()
              handleFormSubmit()
            }}
          >
            <DialogHeader>
              <DialogTitle>{dialogTitle}</DialogTitle>
              <DialogDescription>{dialogSubtitle}</DialogDescription>
            </DialogHeader>
            <DialogBody>
              <div className="space-y-4">
                <div>
                  <label className="block text-sm font-medium text-gray-900 mb-1">
                    App title
                  </label>
                  <input
                    value={formName}
                    onChange={(event) => setFormName(event.target.value)}
                    placeholder="e.g., Customer Dashboard"
                    disabled={formSaving}
                    className="w-full h-10 px-3 rounded-md text-sm border border-gray-200 bg-white text-gray-900 placeholder:text-gray-400 focus:outline-none focus:ring-2 focus:ring-gray-900 focus:border-transparent disabled:opacity-50 disabled:cursor-not-allowed transition-shadow"
                    autoFocus
                  />
                </div>
                <div>
                  <label className="block text-sm font-medium text-gray-900 mb-1">
                    Description
                  </label>
                  <input
                    value={formDescription}
                    onChange={(event) => setFormDescription(event.target.value.slice(0, 60))}
                    maxLength={60}
                    placeholder="Short description (max 60 characters)"
                    disabled={formSaving}
                    className="w-full h-10 px-3 rounded-md text-sm border border-gray-200 bg-white text-gray-900 placeholder:text-gray-400 focus:outline-none focus:ring-2 focus:ring-gray-900 focus:border-transparent disabled:opacity-50 disabled:cursor-not-allowed transition-shadow"
                  />
                  <div className="mt-1 text-xs text-gray-400 text-right">
                    {formDescription.length}/60
                  </div>
                </div>
              </div>
            </DialogBody>
            <DialogFooter>
              <Button
                type="button"
                variant="outline"
                onClick={() => {
                  if (!formSaving) {
                    setFormDialogOpen(false)
                    resetFormDialog()
                  }
                }}
                disabled={formSaving}
                disableFocusRing
              >
                Cancel
              </Button>
              <Button
                type="submit"
                disabled={formSaving || !formName.trim()}
              >
                {formSaving ? 'Saving…' : dialogPrimaryText}
              </Button>
            </DialogFooter>
          </form>
        </DialogContent>
      </Dialog>
    </div>
  </>
  )
}
