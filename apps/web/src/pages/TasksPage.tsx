/**
 * Tasks list page
 *
 * Clean, light-themed task list for managing user tasks.
 * Follows the established design system.
 */
import { useState, useMemo, useCallback } from 'react'
import { useAppSelector } from '../store/hooks'
import { useTasks, useCreateTask, useUpdateTask, useDeleteTask } from '../hooks/useTasks'
import { Button } from '../components/ui/button'
import { useDialog } from '../components/ui/dialog-provider'
import {
  Dialog,
  DialogBody,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '../components/ui/dialog'
import {
  Plus,
  CheckSquare,
  Square,
  Clock,
  Loader2,
  Search,
  X,
  Trash2,
  ListTodo,
} from 'lucide-react'
import { formatDistanceToNow } from 'date-fns'
import type { Task } from '@/services/apiService'

type FilterMode = 'all' | 'active' | 'completed'

export function TasksPage() {
  const selectedOrgId = useAppSelector((state) => state.ui.selectedOrgId)
  const { data: tasks, isLoading } = useTasks(selectedOrgId || null)
  const createTask = useCreateTask()
  const updateTask = useUpdateTask()
  const deleteTask = useDeleteTask()
  const { confirm } = useDialog()

  const [formDialogOpen, setFormDialogOpen] = useState(false)
  const [formTitle, setFormTitle] = useState('')
  const [formDescription, setFormDescription] = useState('')
  const [formSaving, setFormSaving] = useState(false)

  // Search and filter state
  const [searchQuery, setSearchQuery] = useState('')
  const [filterMode, setFilterMode] = useState<FilterMode>('all')

  // Track loading states
  const [togglingTaskId, setTogglingTaskId] = useState<string | null>(null)
  const [deletingTaskId, setDeletingTaskId] = useState<string | null>(null)

  // Filter and search tasks
  const filteredTasks = useMemo(() => {
    if (!tasks) return []

    let result = tasks

    // Apply filter mode
    if (filterMode === 'active') {
      result = result.filter((task) => !task.completed)
    } else if (filterMode === 'completed') {
      result = result.filter((task) => task.completed)
    }

    // Apply search
    if (searchQuery.trim()) {
      const query = searchQuery.toLowerCase()
      result = result.filter(
        (task) =>
          task.title.toLowerCase().includes(query) ||
          task.description.toLowerCase().includes(query)
      )
    }

    return result
  }, [tasks, filterMode, searchQuery])

  // Count for tabs
  const counts = useMemo(() => {
    if (!tasks) return { all: 0, active: 0, completed: 0 }
    return {
      all: tasks.length,
      active: tasks.filter((t) => !t.completed).length,
      completed: tasks.filter((t) => t.completed).length,
    }
  }, [tasks])

  const resetFormDialog = () => {
    setFormTitle('')
    setFormDescription('')
  }

  const openCreateDialog = () => {
    resetFormDialog()
    setFormDialogOpen(true)
  }

  const handleFormSubmit = async () => {
    const cleanedTitle = formTitle.trim()
    const cleanedDescription = formDescription.trim()

    if (!cleanedTitle || !selectedOrgId) return

    setFormSaving(true)

    try {
      await createTask.mutateAsync({
        orgId: selectedOrgId,
        data: { title: cleanedTitle, description: cleanedDescription },
      })
      setFormDialogOpen(false)
      resetFormDialog()
    } catch (error) {
      console.error('Failed to create task', error)
    } finally {
      setFormSaving(false)
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

  const toggleTaskCompleted = useCallback(
    async (task: Task) => {
      if (!selectedOrgId) return

      setTogglingTaskId(task.id)
      try {
        await updateTask.mutateAsync({
          orgId: selectedOrgId,
          taskId: task.id,
          data: { completed: !task.completed },
        })
      } catch (error) {
        console.error('Failed to toggle task', error)
      } finally {
        setTogglingTaskId(null)
      }
    },
    [selectedOrgId, updateTask]
  )

  const handleDeleteTask = async (task: Task) => {
    if (!selectedOrgId) return

    const confirmed = await confirm({
      title: 'Delete task?',
      description: `This will permanently delete "${task.title}".`,
      variant: 'destructive',
      confirmText: 'Delete',
    })

    if (!confirmed) return

    setDeletingTaskId(task.id)
    try {
      await deleteTask.mutateAsync({ orgId: selectedOrgId, taskId: task.id })
    } catch (error) {
      console.error('Failed to delete task', error)
    } finally {
      setDeletingTaskId(null)
    }
  }

  if (!selectedOrgId) {
    return (
      <div className="flex items-center justify-center h-full bg-gray-50">
        <div className="text-center">
          <ListTodo className="h-12 w-12 text-gray-300 mx-auto mb-4" />
          <p className="text-gray-500">Please select an organization</p>
        </div>
      </div>
    )
  }

  if (isLoading) {
    return (
      <div className="flex items-center justify-center h-full bg-gray-50">
        <div className="flex flex-col items-center gap-3">
          <Loader2 className="h-6 w-6 animate-spin text-gray-400" />
          <p className="text-sm text-gray-500">Loading tasks...</p>
        </div>
      </div>
    )
  }

  return (
    <>
      <div className="min-h-full bg-gray-50">
        {/* Content */}
        <div className="max-w-3xl mx-auto px-6 py-6 space-y-6">
          {/* Toolbar */}
          <div className="flex flex-col sm:flex-row gap-3">
            {/* Search */}
            <div className="relative flex-1">
              <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-gray-400" />
              <input
                type="text"
                placeholder="Search tasks..."
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
                {(
                  [
                    { key: 'all', label: 'All', count: counts.all },
                    { key: 'active', label: 'Active', count: counts.active },
                    { key: 'completed', label: 'Done', count: counts.completed },
                  ] as const
                ).map((tab) => (
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

              <div className="w-px h-6 bg-gray-200" />
              <Button onClick={openCreateDialog} size="sm">
                <Plus className="h-4 w-4 mr-1.5" />
                New Task
              </Button>
            </div>
          </div>

          {/* Task List */}
          {filteredTasks.length > 0 ? (
            <div className="space-y-2">
              {filteredTasks.map((task) => (
                <div
                  key={task.id}
                  className={`group flex items-start gap-3 bg-white rounded-lg border border-gray-200 p-4 hover:border-gray-300 transition-all ${
                    task.completed ? 'opacity-60' : ''
                  }`}
                >
                  {/* Checkbox */}
                  <button
                    onClick={() => toggleTaskCompleted(task)}
                    disabled={togglingTaskId === task.id}
                    className="flex-shrink-0 mt-0.5 text-gray-400 hover:text-gray-600 disabled:opacity-50"
                  >
                    {togglingTaskId === task.id ? (
                      <Loader2 className="h-5 w-5 animate-spin" />
                    ) : task.completed ? (
                      <CheckSquare className="h-5 w-5 text-green-600" />
                    ) : (
                      <Square className="h-5 w-5" />
                    )}
                  </button>

                  {/* Content */}
                  <div className="flex-1 min-w-0">
                    <p
                      className={`text-sm font-medium text-gray-900 ${
                        task.completed ? 'line-through text-gray-500' : ''
                      }`}
                    >
                      {task.title}
                    </p>
                    {task.description && (
                      <p className="text-sm text-gray-500 mt-0.5">{task.description}</p>
                    )}
                    <div className="flex items-center gap-2 mt-2">
                      <span className="flex items-center gap-1 text-xs text-gray-400">
                        <Clock className="h-3 w-3" />
                        {formatDistanceToNow(new Date(task.created_at), { addSuffix: true })}
                      </span>
                    </div>
                  </div>

                  {/* Delete button */}
                  <button
                    onClick={() => handleDeleteTask(task)}
                    disabled={deletingTaskId === task.id}
                    className="flex-shrink-0 p-1 rounded-md text-gray-400 hover:text-red-600 hover:bg-red-50 opacity-0 group-hover:opacity-100 transition-all disabled:opacity-50"
                  >
                    {deletingTaskId === task.id ? (
                      <Loader2 className="h-4 w-4 animate-spin" />
                    ) : (
                      <Trash2 className="h-4 w-4" />
                    )}
                  </button>
                </div>
              ))}
            </div>
          ) : tasks && tasks.length > 0 ? (
            // No results from search/filter
            <div className="flex flex-col items-center justify-center py-16">
              <div className="h-12 w-12 rounded-lg bg-gray-100 flex items-center justify-center mb-4">
                <Search className="h-6 w-6 text-gray-400" />
              </div>
              <h3 className="text-sm font-medium text-gray-900 mb-1">No matching tasks</h3>
              <p className="text-sm text-gray-500 mb-4">Try adjusting your search or filter</p>
              <button
                onClick={() => {
                  setSearchQuery('')
                  setFilterMode('all')
                }}
                className="text-sm text-gray-700 hover:text-gray-900 font-medium underline underline-offset-2"
              >
                Clear filters
              </button>
            </div>
          ) : (
            // No tasks at all
            <div className="flex flex-col items-center justify-center py-16">
              <div className="h-16 w-16 rounded-xl bg-gray-100 flex items-center justify-center mb-4">
                <ListTodo className="h-8 w-8 text-gray-400" />
              </div>
              <h2 className="text-lg font-medium text-gray-900 mb-1">No tasks yet</h2>
              <p className="text-sm text-gray-500 mb-6 max-w-sm text-center">
                Create your first task to start organizing your work.
              </p>
              <Button onClick={openCreateDialog}>
                <Plus className="h-4 w-4 mr-2" />
                Create Your First Task
              </Button>
            </div>
          )}
        </div>

        {/* Create dialog */}
        <Dialog open={formDialogOpen} onOpenChange={handleFormOpenChange}>
          <DialogContent showCloseButton className="max-w-md">
            <form
              onSubmit={(event) => {
                event.preventDefault()
                handleFormSubmit()
              }}
            >
              <DialogHeader>
                <DialogTitle>Create New Task</DialogTitle>
                <DialogDescription>Add a new task to your list.</DialogDescription>
              </DialogHeader>
              <DialogBody>
                <div className="space-y-4">
                  <div>
                    <label className="block text-sm font-medium text-gray-700 mb-2">
                      Task title
                    </label>
                    <input
                      value={formTitle}
                      onChange={(event) => setFormTitle(event.target.value)}
                      placeholder="e.g., Review project proposal"
                      disabled={formSaving}
                      className="w-full px-3 py-2 border border-gray-200 rounded-md text-sm text-gray-900 placeholder:text-gray-400 focus:outline-none focus:border-gray-400 disabled:opacity-50 disabled:cursor-not-allowed"
                      autoFocus
                    />
                  </div>
                  <div>
                    <label className="block text-sm font-medium text-gray-700 mb-2">
                      Description (optional)
                    </label>
                    <textarea
                      value={formDescription}
                      onChange={(event) => setFormDescription(event.target.value)}
                      placeholder="Add more details..."
                      disabled={formSaving}
                      rows={3}
                      className="w-full px-3 py-2 border border-gray-200 rounded-md text-sm text-gray-900 placeholder:text-gray-400 focus:outline-none focus:border-gray-400 disabled:opacity-50 disabled:cursor-not-allowed resize-none"
                    />
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
                <Button type="submit" disabled={formSaving || !formTitle.trim()}>
                  {formSaving ? 'Creating...' : 'Create Task'}
                </Button>
              </DialogFooter>
            </form>
          </DialogContent>
        </Dialog>
      </div>
    </>
  )
}
