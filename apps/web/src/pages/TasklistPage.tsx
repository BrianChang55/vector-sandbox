/**
 * Tasklist page
 *
 * A simple task management page for the organization.
 */
import { useState, useCallback } from 'react'
import { useAppSelector } from '../store/hooks'
import {
  useTasks,
  useCreateTask,
  useUpdateTask,
  useDeleteTask,
  useToggleTaskComplete,
} from '../hooks/useTasklist'
import { Button } from '../components/ui/button'
import { Input } from '../components/ui/input'
import {
  Plus,
  Trash2,
  Loader2,
  CheckCircle2,
  Circle,
  ListTodo,
} from 'lucide-react'
import { formatDistanceToNow } from 'date-fns'
import type { Task } from '../types/models'

export function TasklistPage() {
  const selectedOrgId = useAppSelector((state) => state.ui.selectedOrgId)
  const { data: tasks, isLoading } = useTasks(selectedOrgId || null)
  const createTask = useCreateTask()
  const updateTask = useUpdateTask()
  const deleteTask = useDeleteTask()
  const toggleComplete = useToggleTaskComplete()

  const [newTaskTitle, setNewTaskTitle] = useState('')
  const [editingTaskId, setEditingTaskId] = useState<string | null>(null)
  const [editingTitle, setEditingTitle] = useState('')

  const handleCreateTask = useCallback(
    async (e: React.FormEvent) => {
      e.preventDefault()
      if (!newTaskTitle.trim() || !selectedOrgId) return

      await createTask.mutateAsync({
        orgId: selectedOrgId,
        data: { title: newTaskTitle.trim() },
      })
      setNewTaskTitle('')
    },
    [newTaskTitle, selectedOrgId, createTask]
  )

  const handleToggleComplete = useCallback(
    async (task: Task) => {
      if (!selectedOrgId) return
      await toggleComplete.mutateAsync({
        orgId: selectedOrgId,
        taskId: task.id,
        isCompleted: !task.is_completed,
      })
    },
    [selectedOrgId, toggleComplete]
  )

  const handleDeleteTask = useCallback(
    async (taskId: string) => {
      if (!selectedOrgId) return
      await deleteTask.mutateAsync({
        orgId: selectedOrgId,
        taskId,
      })
    },
    [selectedOrgId, deleteTask]
  )

  const handleStartEdit = useCallback((task: Task) => {
    setEditingTaskId(task.id)
    setEditingTitle(task.title)
  }, [])

  const handleSaveEdit = useCallback(
    async (taskId: string) => {
      if (!selectedOrgId || !editingTitle.trim()) return
      await updateTask.mutateAsync({
        orgId: selectedOrgId,
        taskId,
        data: { title: editingTitle.trim() },
      })
      setEditingTaskId(null)
      setEditingTitle('')
    },
    [selectedOrgId, editingTitle, updateTask]
  )

  const handleCancelEdit = useCallback(() => {
    setEditingTaskId(null)
    setEditingTitle('')
  }, [])

  // Split tasks into completed and pending
  const pendingTasks = tasks?.filter((t) => !t.is_completed) || []
  const completedTasks = tasks?.filter((t) => t.is_completed) || []

  if (isLoading) {
    return (
      <div className="flex h-full items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-gray-400" />
      </div>
    )
  }

  return (
    <div className="h-full overflow-auto bg-gray-50">
      <div className="mx-auto max-w-2xl px-6 py-8">
        {/* Header */}
        <div className="mb-8">
          <div className="flex items-center gap-3">
            <div className="flex h-10 w-10 items-center justify-center rounded-lg bg-gray-900">
              <ListTodo className="h-5 w-5 text-white" />
            </div>
            <div>
              <h1 className="text-2xl font-semibold text-gray-900">Tasklist</h1>
              <p className="text-sm text-gray-500">
                {tasks?.length || 0} task{tasks?.length !== 1 ? 's' : ''} total
              </p>
            </div>
          </div>
        </div>

        {/* Add Task Form */}
        <form onSubmit={handleCreateTask} className="mb-6">
          <div className="flex gap-2">
            <Input
              type="text"
              placeholder="Add a new task..."
              value={newTaskTitle}
              onChange={(e) => setNewTaskTitle(e.target.value)}
              className="flex-1"
            />
            <Button
              type="submit"
              disabled={!newTaskTitle.trim() || createTask.isPending}
            >
              {createTask.isPending ? (
                <Loader2 className="h-4 w-4 animate-spin" />
              ) : (
                <Plus className="h-4 w-4" />
              )}
              <span className="ml-2">Add</span>
            </Button>
          </div>
        </form>

        {/* Task List */}
        <div className="space-y-6">
          {/* Pending Tasks */}
          {pendingTasks.length > 0 && (
            <div className="space-y-2">
              <h2 className="text-sm font-medium text-gray-500 uppercase tracking-wide">
                To Do ({pendingTasks.length})
              </h2>
              <div className="space-y-2">
                {pendingTasks.map((task) => (
                  <TaskItem
                    key={task.id}
                    task={task}
                    isEditing={editingTaskId === task.id}
                    editingTitle={editingTitle}
                    onToggleComplete={() => handleToggleComplete(task)}
                    onDelete={() => handleDeleteTask(task.id)}
                    onStartEdit={() => handleStartEdit(task)}
                    onSaveEdit={() => handleSaveEdit(task.id)}
                    onCancelEdit={handleCancelEdit}
                    onEditingTitleChange={setEditingTitle}
                  />
                ))}
              </div>
            </div>
          )}

          {/* Completed Tasks */}
          {completedTasks.length > 0 && (
            <div className="space-y-2">
              <h2 className="text-sm font-medium text-gray-500 uppercase tracking-wide">
                Completed ({completedTasks.length})
              </h2>
              <div className="space-y-2">
                {completedTasks.map((task) => (
                  <TaskItem
                    key={task.id}
                    task={task}
                    isEditing={editingTaskId === task.id}
                    editingTitle={editingTitle}
                    onToggleComplete={() => handleToggleComplete(task)}
                    onDelete={() => handleDeleteTask(task.id)}
                    onStartEdit={() => handleStartEdit(task)}
                    onSaveEdit={() => handleSaveEdit(task.id)}
                    onCancelEdit={handleCancelEdit}
                    onEditingTitleChange={setEditingTitle}
                  />
                ))}
              </div>
            </div>
          )}

          {/* Empty State */}
          {tasks?.length === 0 && (
            <div className="flex flex-col items-center justify-center py-16 text-center">
              <div className="mb-4 flex h-16 w-16 items-center justify-center rounded-full bg-gray-100">
                <ListTodo className="h-8 w-8 text-gray-400" />
              </div>
              <h3 className="text-lg font-medium text-gray-900">No tasks yet</h3>
              <p className="mt-1 text-sm text-gray-500">
                Add your first task above to get started.
              </p>
            </div>
          )}
        </div>
      </div>
    </div>
  )
}

interface TaskItemProps {
  task: Task
  isEditing: boolean
  editingTitle: string
  onToggleComplete: () => void
  onDelete: () => void
  onStartEdit: () => void
  onSaveEdit: () => void
  onCancelEdit: () => void
  onEditingTitleChange: (title: string) => void
}

function TaskItem({
  task,
  isEditing,
  editingTitle,
  onToggleComplete,
  onDelete,
  onStartEdit,
  onSaveEdit,
  onCancelEdit,
  onEditingTitleChange,
}: TaskItemProps) {
  return (
    <div className="group flex items-center gap-3 rounded-lg border border-gray-200 bg-white p-4 transition-shadow hover:shadow-sm">
      {/* Checkbox */}
      <button
        onClick={onToggleComplete}
        className="flex-shrink-0 text-gray-400 hover:text-gray-600"
      >
        {task.is_completed ? (
          <CheckCircle2 className="h-5 w-5 text-green-500" />
        ) : (
          <Circle className="h-5 w-5" />
        )}
      </button>

      {/* Content */}
      <div className="flex-1 min-w-0">
        {isEditing ? (
          <div className="flex items-center gap-2">
            <Input
              type="text"
              value={editingTitle}
              onChange={(e) => onEditingTitleChange(e.target.value)}
              onKeyDown={(e) => {
                if (e.key === 'Enter') onSaveEdit()
                if (e.key === 'Escape') onCancelEdit()
              }}
              autoFocus
              className="h-8"
            />
            <Button size="sm" onClick={onSaveEdit}>
              Save
            </Button>
            <Button size="sm" variant="ghost" onClick={onCancelEdit}>
              Cancel
            </Button>
          </div>
        ) : (
          <div
            className="cursor-pointer"
            onClick={onStartEdit}
          >
            <p
              className={`text-sm ${
                task.is_completed
                  ? 'text-gray-400 line-through'
                  : 'text-gray-900'
              }`}
            >
              {task.title}
            </p>
            <p className="text-xs text-gray-400 mt-1">
              {task.is_completed && task.completed_at
                ? `Completed ${formatDistanceToNow(new Date(task.completed_at), { addSuffix: true })}`
                : `Created ${formatDistanceToNow(new Date(task.created_at), { addSuffix: true })}`}
            </p>
          </div>
        )}
      </div>

      {/* Delete Button */}
      {!isEditing && (
        <button
          onClick={onDelete}
          className="flex-shrink-0 text-gray-300 opacity-0 transition-opacity group-hover:opacity-100 hover:text-red-500"
        >
          <Trash2 className="h-4 w-4" />
        </button>
      )}
    </div>
  )
}
