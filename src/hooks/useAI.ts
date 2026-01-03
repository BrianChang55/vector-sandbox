/**
 * React Query hooks for AI generation
 */
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { 
  getAvailableModels, 
  getChatSessions, 
  createChatSession,
  getChatMessages,
  generateCode,
  applyGeneratedCode,
} from '../services/aiService'

/**
 * Hook to get available AI models
 */
export function useAvailableModels() {
  return useQuery({
    queryKey: ['ai-models'],
    queryFn: getAvailableModels,
    staleTime: 1000 * 60 * 60, // 1 hour
  })
}

/**
 * Hook to get chat sessions for an app
 */
export function useChatSessions(appId: string | null) {
  return useQuery({
    queryKey: ['chat-sessions', appId],
    queryFn: () => getChatSessions(appId!),
    enabled: !!appId,
  })
}

/**
 * Hook to create a chat session
 */
export function useCreateChatSession() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: ({ appId, title, modelId }: { 
      appId: string
      title?: string
      modelId?: string 
    }) => createChatSession(appId, title, modelId),
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['chat-sessions', variables.appId] })
    },
  })
}

/**
 * Hook to get messages for a session
 */
export function useChatMessages(sessionId: string | null) {
  return useQuery({
    queryKey: ['chat-messages', sessionId],
    queryFn: () => getChatMessages(sessionId!),
    enabled: !!sessionId,
  })
}

/**
 * Hook to generate code (non-streaming)
 */
export function useGenerateCode() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: ({ appId, message, options }: {
      appId: string
      message: string
      options?: {
        sessionId?: string
        model?: string
        mode?: 'appspec' | 'code'
      }
    }) => generateCode(appId, message, options),
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['versions'] })
      if (data.session_id) {
        queryClient.invalidateQueries({ queryKey: ['chat-messages', data.session_id] })
      }
    },
  })
}

/**
 * Hook to apply generated code
 */
export function useApplyGeneratedCode() {
  const queryClient = useQueryClient()
  
  return useMutation({
    mutationFn: (messageId: string) => applyGeneratedCode(messageId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['versions'] })
    },
  })
}

