/**
 * Apps list page
 *
 * Clean, light-themed enterprise dashboard showing all internal apps.
 * Follows the established design system: white backgrounds, gray borders,
 * subtle shadows, and minimal design.
 */
import { useState, useMemo, useCallback } from 'react'
import { useNavigate } from 'react-router-dom'
import { useAppSelector } from '../store/hooks'
import { useApps, useCreateApp, useUpdateApp, useDeleteApp, useAppFavorites, useToggleFavorite } from '../hooks/useApps'
import { useOrgMembers } from '../hooks/useMembers'
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
  ExternalLink,
  Eye,
  Search,
  Star,
  X,
} from 'lucide-react'
import { formatDistanceToNow } from 'date-fns'
import { fuzzySearch } from '../utils/fuzzySearch'

type FilterMode = 'all' | 'created' | 'favorites'

export function AppsPage() {
  const selectedOrgId = useAppSelector((state) => state.ui.selectedOrgId)
  const currentUser = useAppSelector((state) => state.auth.user)
  const { data: apps, isLoading } = useApps(selectedOrgId || null)
  const { data: membersData } = useOrgMembers(selectedOrgId)
  const { data: favorites = new Set<string>(), isLoading: isLoadingFavorites } = useAppFavorites(selectedOrgId || null)
  const toggleFavoriteMutation = useToggleFavorite()
  const createApp = useCreateApp()
  const updateApp = useUpdateApp()
  const deleteApp = useDeleteApp()
  const { confirm } = useDialog()
  const navigate = useNavigate()
  
  // Permission checks
  const currentUserRole = membersData?.current_user_role
  const isViewer = currentUserRole === 'viewer'
  const canEditApps = currentUserRole === 'admin' || currentUserRole === 'editor'

  const [savingAppId, setSavingAppId] = useState<string | null>(null)
  const [deletingAppId, setDeletingAppId] = useState<string | null>(null)
  const [formDialogOpen, setFormDialogOpen] = useState(false)
  const [formMode, setFormMode] = useState<'create' | 'edit'>('create')
  const [formAppId, setFormAppId] = useState<string | null>(null)
  const [formName, setFormName] = useState('')
  const [formDescription, setFormDescription] = useState('')
  const [formSaving, setFormSaving] = useState(false)
  
  // Search and filter state
  const [searchQuery, setSearchQuery] = useState('')
  const [filterMode, setFilterMode] = useState<FilterMode>('all')
  
  // Track which app is being toggled for loading state
  const [togglingFavoriteId, setTogglingFavoriteId] = useState<string | null>(null)
  
  // Toggle favorite
  const toggleFavorite = useCallback((appId: string, e: React.MouseEvent) => {
    e.stopPropagation()
    if (!selectedOrgId) return
    
    setTogglingFavoriteId(appId)
    toggleFavoriteMutation.mutate(
      { orgId: selectedOrgId, appId },
      {
        onSettled: () => setTogglingFavoriteId(null),
      }
    )
  }, [selectedOrgId, toggleFavoriteMutation])
  
  // Filter and search apps
  const filteredApps = useMemo(() => {
    if (!apps) return []
    
    let result = apps
    
    // Viewers only see published apps
    if (isViewer) {
      result = result.filter(app => app.status === 'published')
    }
    
    // Apply filter mode
    if (filterMode === 'created' && currentUser) {
      result = result.filter(app => app.created_by === currentUser.id)
    } else if (filterMode === 'favorites') {
      result = result.filter(app => favorites.has(app.id))
    }
    
    // Apply fuzzy search
    if (searchQuery.trim()) {
      result = fuzzySearch(result, searchQuery, [
        { getValue: (app) => app.name, weight: 2 },
        { getValue: (app) => app.description, weight: 1 },
      ])
    }
    
    return result
  }, [apps, isViewer, filterMode, currentUser, favorites, searchQuery])
  
  // Count for tabs
  const counts = useMemo(() => {
    if (!apps) return { all: 0, created: 0, favorites: 0 }
    
    const visibleApps = isViewer ? apps.filter(app => app.status === 'published') : apps
    
    return {
      all: visibleApps.length,
      created: currentUser ? visibleApps.filter(app => app.created_by === currentUser.id).length : 0,
      favorites: visibleApps.filter(app => favorites.has(app.id)).length,
    }
  }, [apps, isViewer, currentUser, favorites])

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
          data: { name: cleanedName, description: cleanedDescription, backend_connection: undefined },
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

  if (isLoading || isLoadingFavorites) {
    return (
      <div className="flex items-center justify-center h-full bg-gray-50">
        <div className="flex flex-col items-center gap-3">
          <Loader2 className="h-6 w-6 animate-spin text-gray-400" />
          <p className="text-sm text-gray-500">Loading apps...</p>
        </div>
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
      {/* Content */}
      <div className="max-w-6xl mx-auto px-6 py-6 space-y-6">
        {/* Toolbar */}
        <div className="flex flex-col sm:flex-row gap-3">
          {/* Search */}
          <div className="relative flex-1">
            <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-gray-400" />
            <input
              type="text"
              placeholder="Search apps..."
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
              className="w-full pl-9 pr-3 py-2 border border-gray-200 rounded-lg text-sm text-gray-900 placeholder:text-gray-500 focus:outline-none focus:border-gray-300 transition-all"
            />
            {searchQuery && (
              <button
                onClick={() => setSearchQuery('')}
                className="absolute right-3 top-1/2 -translate-y-1/2 text-gray-400 hover:text-gray-600"
              >
                <X className="h-4 w-4" />
              </button>
            )}
          </div>
          
          {/* Controls */}
          <div className="flex items-center gap-2">
            {/* Filter Tabs */}
            <div className="flex bg-gray-100 rounded-md p-0.5">
              {([
                { key: 'all', label: 'All', count: counts.all },
                { key: 'created', label: 'Created', count: counts.created },
                { key: 'favorites', label: 'Favorites', count: counts.favorites },
              ] as const).map((tab) => (
                <button
                  key={tab.key}
                  onClick={() => setFilterMode(tab.key)}
                  className={`px-3 py-1.5 text-xs font-medium rounded transition-all ${
                    filterMode === tab.key
                      ? 'bg-white text-gray-900 shadow-sm'
                      : 'text-gray-600 hover:text-gray-900'
                  }`}
                >
                  {tab.label} ({tab.count})
                </button>
              ))}
            </div>
            
            {canEditApps && (
              <>
                <div className="w-px h-6 bg-gray-200" />
                <Button onClick={openCreateDialog} size="sm">
                  <Plus className="h-4 w-4 mr-1.5" />
                  New App
                </Button>
              </>
            )}
          </div>
        </div>
        {filteredApps.length > 0 ? (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
            {filteredApps.map((app) => {
                // Viewers click to go to published view, editors/admins to builder
                const handleAppClick = () => {
                  if (isViewer && app.published_url) {
                    navigate(app.published_url)
                  } else if (isViewer) {
                    // Published but no URL? Go to preview
                    navigate(`/preview/apps/${app.id}`)
                  } else {
                    navigate(`/apps/${app.id}`)
                  }
                }

                return (
              <div
                key={app.id}
                className="group block bg-white rounded-lg border border-gray-200 p-5 
                         hover:border-gray-300 hover:shadow-sm transition-all"
                role="button"
                tabIndex={0}
                onClick={handleAppClick}
                onKeyDown={(event) => {
                  if (event.key === 'Enter' || event.key === ' ') {
                    event.preventDefault()
                    handleAppClick()
                  }
                }}
              >
                <div className="flex items-start justify-between mb-3">
                  <div className="h-10 w-10 rounded-lg bg-gray-100 flex items-center justify-center">
                    <Layers className="h-5 w-5 text-gray-500" />
                  </div>
                  <div className="flex items-center gap-1" onClick={(event) => event.stopPropagation()}>
                    {/* Favorite button */}
                    <button
                      onClick={(e) => toggleFavorite(app.id, e)}
                      disabled={togglingFavoriteId === app.id}
                      className={`p-1 rounded-md transition-colors disabled:opacity-50 ${
                        favorites.has(app.id)
                          ? 'text-amber-500 hover:text-amber-600'
                          : 'text-gray-400 hover:text-gray-600 hover:bg-gray-100'
                      }`}
                      title={favorites.has(app.id) ? 'Remove from favorites' : 'Add to favorites'}
                    >
                      {togglingFavoriteId === app.id ? (
                        <Loader2 className="h-4 w-4 animate-spin" />
                      ) : (
                        <Star className={`h-4 w-4 ${favorites.has(app.id) ? 'fill-current' : ''}`} />
                      )}
                    </button>
                    {savingAppId === app.id && (
                      <Loader2 className="h-4 w-4 text-gray-400 animate-spin" />
                    )}
                    {/* Viewers see a simpler view without edit options */}
                    {isViewer ? (
                      app.published_url && (
                        <a
                          href={app.published_url}
                          target="_blank"
                          rel="noopener noreferrer"
                          className="p-1 rounded-md text-gray-400 hover:text-gray-600 hover:bg-gray-100 transition-colors"
                          title="Open in new tab"
                        >
                          <ExternalLink className="h-4 w-4" />
                        </a>
                      )
                    ) : (
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
                    )}
                  </div>
                </div>

                <div className="mb-1">
                  {canEditApps ? (
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
                  ) : (
                    <span className="font-medium text-gray-900 truncate">
                      {app.name || 'Untitled app'}
                    </span>
                  )}
                </div>
                <div className="mb-4">
                  {canEditApps ? (
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
                  ) : (
                    <p className="text-sm text-gray-500 truncate">
                      {(app.description || 'No description').slice(0, 60)}
                    </p>
                  )}
                </div>

                <div className="flex items-center gap-3">
                  {isViewer ? (
                    // Viewers see a simpler badge - just show it's available to run
                    <span className="flex items-center gap-1.5 text-xs px-2 py-1 rounded-full bg-green-50 text-green-700 border border-green-200">
                      <Eye className="h-3 w-3" />
                      Published
                    </span>
                  ) : (
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
                  )}
                  
                  <span className="flex items-center gap-1 text-xs text-gray-400">
                    <Clock className="h-3 w-3" />
                    {formatDistanceToNow(new Date(app.updated_at), { addSuffix: true })}
                  </span>
                </div>
                {deletingAppId === app.id && (
                  <p className="mt-2 text-xs text-red-500">Deleting…</p>
                )}
              </div>
                )
              })}
          </div>
        ) : apps && apps.length > 0 ? (
          // No results from search/filter
          <div className="flex flex-col items-center justify-center py-16">
            <div className="h-12 w-12 rounded-lg bg-gray-100 flex items-center justify-center mb-4">
              <Search className="h-6 w-6 text-gray-400" />
            </div>
            <h3 className="text-sm font-medium text-gray-900 mb-1">
              No matching apps
            </h3>
            <p className="text-sm text-gray-500 mb-4">
              {filterMode === 'favorites' 
                ? "You haven't favorited any apps yet"
                : filterMode === 'created'
                ? "You haven't created any apps yet"
                : 'Try adjusting your search criteria'
              }
            </p>
            <button
              onClick={() => { setSearchQuery(''); setFilterMode('all') }}
              className="text-sm text-gray-700 hover:text-gray-900 font-medium underline underline-offset-2"
            >
              Clear filters
            </button>
          </div>
        ) : (
          // No apps at all
          <div className="flex flex-col items-center justify-center py-16">
            <div className="h-16 w-16 rounded-xl bg-gray-100 flex items-center justify-center mb-4">
              <FolderOpen className="h-8 w-8 text-gray-400" />
            </div>
            <h2 className="text-lg font-medium text-gray-900 mb-1">
              {isViewer ? 'No published apps' : 'No apps yet'}
            </h2>
            <p className="text-sm text-gray-500 mb-6 max-w-sm text-center">
              {isViewer 
                ? 'There are no published apps available for you to use yet. Contact your admin to get started.'
                : 'Create your first internal application to get started with building powerful tools.'
              }
            </p>
            {canEditApps && (
              <Button onClick={openCreateDialog}>
                <Plus className="h-4 w-4 mr-2" />
                Create Your First App
              </Button>
            )}
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
                  <label className="block text-sm font-medium text-gray-700 mb-2">
                    App title
                  </label>
                  <input
                    value={formName}
                    onChange={(event) => setFormName(event.target.value)}
                    placeholder="e.g., Customer Dashboard"
                    disabled={formSaving}
                    className="w-full px-3 py-2 border border-gray-200 rounded-md text-sm text-gray-900 placeholder:text-gray-400 focus:outline-none focus:border-gray-400 disabled:opacity-50 disabled:cursor-not-allowed"
                    autoFocus
                  />
                </div>
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-2">
                    Description
                  </label>
                  <input
                    value={formDescription}
                    onChange={(event) => setFormDescription(event.target.value.slice(0, 60))}
                    maxLength={60}
                    placeholder="Short description (max 60 characters)"
                    disabled={formSaving}
                    className="w-full px-3 py-2 border border-gray-200 rounded-md text-sm text-gray-900 placeholder:text-gray-400 focus:outline-none focus:border-gray-400 disabled:opacity-50 disabled:cursor-not-allowed"
                  />
                  <div className="mt-1.5 text-xs text-gray-400 text-right">
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
