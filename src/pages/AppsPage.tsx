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
import { 
  Plus, 
  Layers, 
  ArrowRight, 
  Clock,
  CheckCircle,
  Code2,
  FolderOpen,
  MoreVertical,
  PencilLine,
  AlignLeft,
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
  const { prompt, confirm } = useDialog()
  const navigate = useNavigate()

  const [editingAppId, setEditingAppId] = useState<string | null>(null)
  const [editingField, setEditingField] = useState<'name' | 'description' | null>(null)
  const [draftName, setDraftName] = useState('')
  const [draftDescription, setDraftDescription] = useState('')
  const [savingAppId, setSavingAppId] = useState<string | null>(null)
  const [deletingAppId, setDeletingAppId] = useState<string | null>(null)

  const handleCreateApp = async () => {
    const name = await prompt({
      title: 'Create New App',
      description: 'Enter a name for your new internal application.',
      placeholder: 'e.g., Customer Dashboard, Order Manager',
      confirmText: 'Create App',
      required: true,
    })
    
    if (name) {
      createApp.mutate({
        orgId: selectedOrgId!,
        data: { name, description: '', backend_connection: null },
      })
    }
  }

  const startEditing = (appId: string, field: 'name' | 'description', currentName: string, currentDescription: string) => {
    setEditingAppId(appId)
    setEditingField(field)
    setDraftName(currentName)
    setDraftDescription(currentDescription)
  }

  const cancelEditing = () => {
    setEditingAppId(null)
    setEditingField(null)
  }

  const handleSaveEdit = async (appId: string, field: 'name' | 'description', originalValue: string) => {
    const value = field === 'name' ? draftName : draftDescription
    const cleanedValue = value.trim()

    if (field === 'name' && !cleanedValue) {
      cancelEditing()
      return
    }

    if (cleanedValue === originalValue) {
      cancelEditing()
      return
    }

    setSavingAppId(appId)
    try {
      await updateApp.mutateAsync({ appId, data: { [field]: cleanedValue } })
    } catch (error) {
      console.error('Failed to update app', error)
    } finally {
      setSavingAppId(null)
      cancelEditing()
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

  return (
    <div className="min-h-full bg-gray-50">
      {/* Header */}
      <div className="bg-white border-b border-gray-200">
        <div className="max-w-6xl mx-auto px-6 py-6">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-xl font-semibold text-gray-900">Apps</h1>
              <p className="text-sm text-gray-500 mt-0.5">
                Build and manage your internal applications
              </p>
            </div>
            <Button onClick={handleCreateApp}>
              <Plus className="h-4 w-4 mr-2" />
              New App
            </Button>
          </div>
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
                            startEditing(app.id, 'name', app.name, app.description || '')
                          }}
                          className="gap-2"
                        >
                          <PencilLine className="h-4 w-4 text-gray-500" />
                          Rename
                        </DropdownMenuItem>
                        <DropdownMenuItem
                          onSelect={(event) => {
                            event.preventDefault()
                            startEditing(app.id, 'description', app.name, app.description || '')
                          }}
                          className="gap-2"
                        >
                          <AlignLeft className="h-4 w-4 text-gray-500" />
                          Edit description
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
                    <ArrowRight className="h-4 w-4 text-gray-300 group-hover:text-gray-500 
                                          group-hover:translate-x-0.5 transition-all" />
                  </div>
                </div>

                <div className="mb-1">
                  {editingAppId === app.id && editingField === 'name' ? (
                    <input
                      autoFocus
                      value={draftName}
                      onChange={(event) => setDraftName(event.target.value)}
                      onClick={(event) => event.stopPropagation()}
                      onBlur={() => handleSaveEdit(app.id, 'name', app.name)}
                      onKeyDown={(event) => {
                        if (event.key === 'Enter') {
                          event.preventDefault()
                          handleSaveEdit(app.id, 'name', app.name)
                        }
                        if (event.key === 'Escape') {
                          event.preventDefault()
                          cancelEditing()
                        }
                      }}
                      className="w-full bg-transparent border border-transparent p-0 m-0 text-base font-medium text-gray-900 outline-none focus:ring-0 focus:border-transparent"
                    />
                  ) : (
                    <div
                      className="group/name inline-flex items-center gap-1.5 -mx-1 px-1 rounded cursor-text transition-colors hover:bg-gray-50"
                      onClick={(event) => {
                        event.preventDefault()
                        event.stopPropagation()
                        startEditing(app.id, 'name', app.name, app.description || '')
                      }}
                    >
                      <span className="font-medium text-gray-900 group-hover/name:text-gray-700 truncate">
                        {app.name || 'Untitled app'}
                      </span>
                      <PencilLine className="h-3.5 w-3.5 text-gray-300 opacity-0 group-hover/name:opacity-100 transition-opacity" aria-hidden />
                    </div>
                  )}
                </div>
                <div className="mb-4">
                  {editingAppId === app.id && editingField === 'description' ? (
                    <textarea
                      autoFocus
                      value={draftDescription}
                      onChange={(event) => setDraftDescription(event.target.value)}
                      onClick={(event) => event.stopPropagation()}
                      onBlur={() => handleSaveEdit(app.id, 'description', app.description || '')}
                      onKeyDown={(event) => {
                        if (event.key === 'Enter' && !event.shiftKey) {
                          event.preventDefault()
                          handleSaveEdit(app.id, 'description', app.description || '')
                        }
                        if (event.key === 'Escape') {
                          event.preventDefault()
                          cancelEditing()
                        }
                      }}
                      rows={2}
                      className="w-full resize-none border border-transparent bg-transparent px-0 py-0 text-sm text-gray-600 leading-snug focus:border-gray-300 focus:ring-0"
                    />
                  ) : (
                    <div
                      className="group/description relative -mx-1 px-1 py-0.5 rounded cursor-text transition-colors hover:bg-gray-50"
                      onClick={(event) => {
                        event.preventDefault()
                        event.stopPropagation()
                        startEditing(app.id, 'description', app.name, app.description || '')
                      }}
                    >
                      <p className="text-sm text-gray-500 line-clamp-2 transition-colors group-hover/description:text-gray-600">
                        {app.description || 'No description'}
                      </p>
                      <PencilLine className="absolute right-1 top-1 h-3.5 w-3.5 text-gray-300 opacity-0 group-hover/description:opacity-100 transition-opacity" aria-hidden />
                    </div>
                  )}
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
            <Button onClick={handleCreateApp}>
              <Plus className="h-4 w-4 mr-2" />
              Create Your First App
            </Button>
          </div>
        )}
      </div>
    </div>
  )
}
