/**
 * AI Generation Service
 * 
 * Handles streaming code generation with Server-Sent Events.
 * Similar to Cursor, Lovable, Replit vibe coding experience.
 */

import { api } from './api'

// Types
export interface AIModel {
  id: string
  name: string
  description: string
  category: 'premium' | 'standard' | 'economy'
  context_length: number
  supports_streaming: boolean
  recommended_for: string[]
  cost: {
    input: number
    output: number
  }
}

export interface ModelsResponse {
  models: AIModel[]
  grouped: {
    premium: AIModel[]
    standard: AIModel[]
    economy: AIModel[]
  }
  default: string
}

export interface ChatSession {
  id: string
  title: string
  model_id: string
  created_at: string
  created_by?: string | null
  message_count: number
  last_message_at?: string | null
}

export interface ChatMessage {
  id: string
  role: 'user' | 'assistant' | 'system'
  content: string
  status: 'pending' | 'streaming' | 'complete' | 'error'
  model_id?: string
  created_at: string
  duration_ms?: number | null
  generated_files?: Record<string, string>
  generated_spec_json?: any
  version_created?: string
  error_message?: string
}

export interface StreamEvent {
  type: 'connected' | 'session_created' | 'user_message' | 'assistant_start' | 
        'content' | 'thinking' | 'generation_complete' | 'version_created' | 
        'validation_warning' | 'version_error' | 'error' | 'done'
  data: any
}

export type StreamCallback = (event: StreamEvent) => void

/**
 * Get available AI models
 */
export async function getAvailableModels(): Promise<ModelsResponse> {
  const response = await api.get<ModelsResponse>('/models/')
  return response.data
}

/**
 * Get chat sessions for an app
 */
export async function getChatSessions(appId: string): Promise<ChatSession[]> {
  const response = await api.get<{ sessions: ChatSession[] }>(`/apps/${appId}/chat-sessions/`)
  return response.data.sessions
}

/**
 * Create a new chat session
 */
export async function createChatSession(
  appId: string, 
  title?: string, 
  modelId?: string
): Promise<ChatSession> {
  const response = await api.post<ChatSession>(`/apps/${appId}/chat-sessions/`, {
    title,
    model_id: modelId,
  })
  return response.data
}

/**
 * Get messages for a chat session
 */
export async function getChatMessages(sessionId: string): Promise<ChatMessage[]> {
  const response = await api.get<{ messages: ChatMessage[] }>(
    `/chat-sessions/${sessionId}/messages/`
  )
  return response.data.messages
}

/**
 * Generate code with streaming updates
 * Uses Server-Sent Events for real-time feedback
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
    model: options.model || 'anthropic/claude-sonnet-4',
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

/**
 * Generate code without streaming (fallback)
 */
export async function generateCode(
  appId: string,
  message: string,
  options: {
    sessionId?: string
    model?: string
    mode?: 'appspec' | 'code'
  } = {}
): Promise<{
  session_id: string
  message_id: string
  spec_json: any
  version_id: string | null
  version_number: number | null
  duration_ms: number
  validation_errors: string[] | null
}> {
  const response = await api.post(`/apps/${appId}/generate/`, {
    message,
    session_id: options.sessionId,
    model: options.model,
    mode: options.mode,
  })
  return response.data
}

/**
 * Apply generated code from a message
 */
export async function applyGeneratedCode(messageId: string): Promise<{
  version_id: string
  version_number: number
  files_generated: number
}> {
  const response = await api.post(`/messages/${messageId}/apply/`)
  return response.data
}

