/**
 * React Query hooks for Tasklist
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { tasksApi } from '@/services/apiService'
import type { Task, CreateTaskRequest, UpdateTaskRequest } from '../types/models'

/**
 * Fetch all tasks for an organization
 */
export function useTasks(orgId: string | null) {
  return useQuery({
    queryKey: ['tasks', orgId],
    queryFn: async () => {
      if (!orgId) return []
      return tasksApi.list(orgId)
    },
    enabled: !!orgId,
  })
}

/**
 * Create a new task
 */
export function useCreateTask() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({ orgId, data }: { orgId: string; data: CreateTaskRequest }) => {
      return tasksApi.create(orgId, data)
    },
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['tasks', variables.orgId] })
    },
  })
}

/**
 * Update a task
 */
export function useUpdateTask() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({
      orgId,
      taskId,
      data,
    }: {
      orgId: string
      taskId: string
      data: UpdateTaskRequest
    }) => {
      return tasksApi.update(orgId, taskId, data)
    },
    onMutate: async ({ orgId, taskId, data }) => {
      // Cancel any outgoing refetches
      await queryClient.cancelQueries({ queryKey: ['tasks', orgId] })

      // Snapshot the previous value
      const previousTasks = queryClient.getQueryData<Task[]>(['tasks', orgId])

      // Optimistically update
      if (previousTasks) {
        queryClient.setQueryData<Task[]>(
          ['tasks', orgId],
          previousTasks.map((task) =>
            task.id === taskId ? { ...task, ...data } : task
          )
        )
      }

      return { previousTasks }
    },
    onError: (_err, { orgId }, context) => {
      // Rollback on error
      if (context?.previousTasks) {
        queryClient.setQueryData(['tasks', orgId], context.previousTasks)
      }
    },
    onSettled: (_data, _error, { orgId }) => {
      queryClient.invalidateQueries({ queryKey: ['tasks', orgId] })
    },
  })
}

/**
 * Delete a task
 */
export function useDeleteTask() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({ orgId, taskId }: { orgId: string; taskId: string }) => {
      await tasksApi.delete(orgId, taskId)
      return taskId
    },
    onMutate: async ({ orgId, taskId }) => {
      await queryClient.cancelQueries({ queryKey: ['tasks', orgId] })

      const previousTasks = queryClient.getQueryData<Task[]>(['tasks', orgId])

      if (previousTasks) {
        queryClient.setQueryData<Task[]>(
          ['tasks', orgId],
          previousTasks.filter((task) => task.id !== taskId)
        )
      }

      return { previousTasks }
    },
    onError: (_err, { orgId }, context) => {
      if (context?.previousTasks) {
        queryClient.setQueryData(['tasks', orgId], context.previousTasks)
      }
    },
    onSettled: (_data, _error, { orgId }) => {
      queryClient.invalidateQueries({ queryKey: ['tasks', orgId] })
    },
  })
}

/**
 * Toggle task completion status
 */
export function useToggleTaskComplete() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({
      orgId,
      taskId,
      isCompleted,
    }: {
      orgId: string
      taskId: string
      isCompleted: boolean
    }) => {
      return tasksApi.toggleComplete(orgId, taskId, isCompleted)
    },
    onMutate: async ({ orgId, taskId, isCompleted }) => {
      await queryClient.cancelQueries({ queryKey: ['tasks', orgId] })

      const previousTasks = queryClient.getQueryData<Task[]>(['tasks', orgId])

      if (previousTasks) {
        queryClient.setQueryData<Task[]>(
          ['tasks', orgId],
          previousTasks.map((task) =>
            task.id === taskId
              ? {
                  ...task,
                  is_completed: isCompleted,
                  completed_at: isCompleted ? new Date().toISOString() : null,
                }
              : task
          )
        )
      }

      return { previousTasks }
    },
    onError: (_err, { orgId }, context) => {
      if (context?.previousTasks) {
        queryClient.setQueryData(['tasks', orgId], context.previousTasks)
      }
    },
    onSettled: (_data, _error, { orgId }) => {
      queryClient.invalidateQueries({ queryKey: ['tasks', orgId] })
    },
  })
}
