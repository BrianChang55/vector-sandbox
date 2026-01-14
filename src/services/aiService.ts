/**
 * AI Generation Service
 * 
 * Handles streaming code generation with Server-Sent Events.
 * Similar to Cursor, Lovable, Replit vibe coding experience.
 * 
 * Non-streaming API calls use the centralized apiService.
 * @see {@link @/services/apiService} for API documentation.
 */

import { api } from './api'
import { aiApi } from './apiService'

// Re-export types from apiService for backwards compatibility
export type {
  AIModel,
  ModelsResponse,
  ChatSession,
  ChatMessage,
} from './apiService'

// Event data types for each stream event
export interface SessionCreatedData {
  session_id: string
}

export interface UserMessageData {
  id: string
  content: string
}

export interface AssistantStartData {
  id: string
  model: string
}

export interface ContentData {
  chunk: string
}

export interface ThinkingData {
  chunk: string
}

export interface GenerationCompleteData {
  message_id: string
}

export interface VersionCreatedData {
  version_id: string
  version_number: number
}

export interface ValidationWarningData {
  message: string
  warnings: string[]
}

export interface VersionErrorData {
  message: string
}

export interface ErrorData {
  message: string
}

export type StreamEvent =
  | { type: 'connected'; data: Record<string, never> }
  | { type: 'session_created'; data: SessionCreatedData }
  | { type: 'user_message'; data: UserMessageData }
  | { type: 'assistant_start'; data: AssistantStartData }
  | { type: 'content'; data: ContentData }
  | { type: 'thinking'; data: ThinkingData }
  | { type: 'generation_complete'; data: GenerationCompleteData }
  | { type: 'version_created'; data: VersionCreatedData }
  | { type: 'validation_warning'; data: ValidationWarningData }
  | { type: 'version_error'; data: VersionErrorData }
  | { type: 'error'; data: ErrorData }
  | { type: 'done'; data: Record<string, never> }

export type StreamCallback = (event: StreamEvent) => void

/**
 * Get available AI models
 * @see {@link aiApi.getModels}
 */
export const getAvailableModels = aiApi.getModels

/**
 * Get chat sessions for an app
 * @see {@link aiApi.getChatSessions}
 */
export const getChatSessions = aiApi.getChatSessions

/**
 * Create a new chat session
 * @see {@link aiApi.createChatSession}
 */
export const createChatSession = aiApi.createChatSession

/**
 * Get messages for a chat session
 * @see {@link aiApi.getChatMessages}
 */
export const getChatMessages = aiApi.getChatMessages

/**
 * Generate code without streaming (fallback)
 * @see {@link aiApi.generateCode}
 */
export const generateCode = aiApi.generateCode

/**
 * Apply generated code from a message
 * @see {@link aiApi.applyGeneratedCode}
 */
export const applyGeneratedCode = aiApi.applyGeneratedCode

/**
 * Generate code with streaming updates
 * Uses Server-Sent Events for real-time feedback.
 * 
 * Note: This function uses fetch directly for SSE streaming,
 * as axios doesn't support streaming responses well.
 * 
 * @param appId - App ID
 * @param message - User's prompt
 * @param options - Streaming options including callbacks
 * @returns AbortController to cancel the stream
 */
export function generateCodeStreaming(
  appId: string,
  message: string,
  options: {
    sessionId?: string
    model?: string
    mode?: 'appspec' | 'code'
    onEvent: StreamCallback
    onComplete?: () => void
    onError?: (error: Error) => void
  }
): AbortController {
  const controller = new AbortController()
  
  const params = new URLSearchParams({
    message,
    model: options.model || 'anthropic/claude-sonnet-4.5',
    mode: options.mode || 'appspec',
  })
  
  if (options.sessionId) {
    params.set('session_id', options.sessionId)
  }
  
  const url = `${api.defaults.baseURL}/apps/${appId}/generate/stream/?${params.toString()}`
  
  // Get auth token from localStorage
  const token = localStorage.getItem('access_token')
  
  // Use fetch for authenticated requests (EventSource doesn't support custom headers)
  if (token) {
    // Use fetch for authenticated requests
    fetch(url, {
      method: 'GET',
      headers: {
        'Authorization': `Bearer ${token}`,
        'Accept': 'text/event-stream',
      },
      signal: controller.signal,
    }).then(async (response) => {
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`)
      }
      
      const reader = response.body?.getReader()
      if (!reader) {
        throw new Error('No response body')
      }
      
      const decoder = new TextDecoder()
      let buffer = ''
      
      while (true) {
        const { done, value } = await reader.read()
        
        if (done) {
          options.onComplete?.()
          break
        }
        
        buffer += decoder.decode(value, { stream: true })
        
        // Parse SSE events from buffer
        const lines = buffer.split('\n')
        buffer = lines.pop() || '' // Keep incomplete line in buffer
        
        let eventType = ''
        let eventData = ''
        
        for (const line of lines) {
          if (line.startsWith('event: ')) {
            eventType = line.slice(7).trim()
          } else if (line.startsWith('data: ')) {
            eventData = line.slice(6)
          } else if (line === '' && eventType && eventData) {
            // End of event
            try {
              const data = JSON.parse(eventData)
              options.onEvent({ type: eventType as StreamEvent['type'], data })
            } catch (e) {
              console.warn('Failed to parse SSE data:', eventData)
            }
            eventType = ''
            eventData = ''
          }
        }
      }
    }).catch((error) => {
      if (error.name !== 'AbortError') {
        options.onError?.(error)
      }
    })
  } else {
    // Fallback to EventSource for unauthenticated (shouldn't happen in prod)
    const eventSource = new EventSource(url)
    
    eventSource.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data)
        options.onEvent({ type: 'content', data })
      } catch (e) {
        console.warn('Failed to parse SSE message:', event.data)
      }
    }
    
    eventSource.onerror = () => {
      eventSource.close()
      options.onError?.(new Error('SSE connection error'))
    }
    
    // Override abort to close EventSource
    const originalAbort = controller.abort.bind(controller)
    controller.abort = () => {
      eventSource.close()
      originalAbort()
    }
  }
  
  return controller
}
