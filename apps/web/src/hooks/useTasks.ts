/**
 * React Query hooks for Tasks
 *
 * Uses centralized apiService for all API calls.
 * @see {@link @/services/apiService} for API documentation.
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { tasksApi, type Task } from '@/services/apiService'

/**
 * Query key factory for tasks
 */
export const taskKeys = {
  all: ['tasks'] as const,
  lists: () => [...taskKeys.all, 'list'] as const,
  list: (orgId: string) => [...taskKeys.lists(), orgId] as const,
  details: () => [...taskKeys.all, 'detail'] as const,
  detail: (orgId: string, taskId: string) => [...taskKeys.details(), orgId, taskId] as const,
}

/**
 * Fetch all tasks for an organization
 */
export function useTasks(orgId: string | null) {
  return useQuery({
    queryKey: taskKeys.list(orgId || ''),
    queryFn: async () => {
      if (!orgId) return []
      return tasksApi.list(orgId)
    },
    enabled: !!orgId,
  })
}

/**
 * Fetch a single task
 */
export function useTask(orgId: string | null, taskId: string | null) {
  return useQuery({
    queryKey: taskKeys.detail(orgId || '', taskId || ''),
    queryFn: async () => {
      if (!orgId || !taskId) return null
      return tasksApi.get(orgId, taskId)
    },
    enabled: !!orgId && !!taskId,
  })
}

/**
 * Create a new task
 */
export function useCreateTask() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({
      orgId,
      data,
    }: {
      orgId: string
      data: { title: string; description?: string }
    }) => {
      return tasksApi.create(orgId, data)
    },
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: taskKeys.list(variables.orgId) })
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
      data: Partial<{ title: string; description: string; completed: boolean }>
    }) => {
      return tasksApi.update(orgId, taskId, data)
    },
    onMutate: async ({ orgId, taskId, data }) => {
      // Cancel outgoing refetches
      await queryClient.cancelQueries({ queryKey: taskKeys.list(orgId) })

      // Snapshot previous value
      const previousTasks = queryClient.getQueryData<Task[]>(taskKeys.list(orgId))

      // Optimistically update
      if (previousTasks) {
        const newTasks = previousTasks.map((task) =>
          task.id === taskId ? { ...task, ...data } : task
        )
        queryClient.setQueryData(taskKeys.list(orgId), newTasks)
      }

      return { previousTasks }
    },
    onError: (_err, { orgId }, context) => {
      // Rollback on error
      if (context?.previousTasks) {
        queryClient.setQueryData(taskKeys.list(orgId), context.previousTasks)
      }
    },
    onSettled: (_data, _error, { orgId }) => {
      queryClient.invalidateQueries({ queryKey: taskKeys.list(orgId) })
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
      // Cancel outgoing refetches
      await queryClient.cancelQueries({ queryKey: taskKeys.list(orgId) })

      // Snapshot previous value
      const previousTasks = queryClient.getQueryData<Task[]>(taskKeys.list(orgId))

      // Optimistically update
      if (previousTasks) {
        const newTasks = previousTasks.filter((task) => task.id !== taskId)
        queryClient.setQueryData(taskKeys.list(orgId), newTasks)
      }

      return { previousTasks }
    },
    onError: (_err, { orgId }, context) => {
      // Rollback on error
      if (context?.previousTasks) {
        queryClient.setQueryData(taskKeys.list(orgId), context.previousTasks)
      }
    },
    onSettled: (_data, _error, { orgId }) => {
      queryClient.invalidateQueries({ queryKey: taskKeys.list(orgId) })
    },
  })
}
