/**
 * Agent Service for Agentic Code Generation
 * 
 * Handles streaming SSE communication with the backend for
 * the Research → Plan → Execute → Validate workflow.
 */

import { api } from './api'
import { versionsApi } from './apiService'
import type {
  AgentEvent,
  AgentEventType,
  AgentState,
  FileChange,
} from '../types/agent'

// Helper type for accessing event data with any properties
 
type EventData = Record<string, any>

export function upsertFileChange(
  existing: FileChange[],
  change: FileChange & { 
    lines_added?: number
    lines_removed?: number
    hunks?: string[]
    previous_content?: string
  }
): FileChange[] {
  const index = existing.findIndex((f) => f.path === change.path)
  const previous = index >= 0 ? existing[index] : undefined
  
  // Use line counts from backend FileDiff (snake_case) or existing camelCase properties
  const addedLines = change.lines_added ?? change.addedLines ?? 0
  const removedLines = change.lines_removed ?? change.removedLines ?? 0

  // Use previous_content from backend (snake_case) if provided
  // Otherwise fall back to existing logic for multiple updates
  const hasPreviousContent = !!previous?.content
  const contentChanged = previous?.content !== change.content
  const previousContent = change.previous_content 
    ?? (hasPreviousContent && contentChanged ? previous.content : previous?.previousContent)

  const merged: FileChange = {
    ...previous,
    ...change,
    action: previous ? 'modify' : change.action || 'create',
    addedLines,
    removedLines,
    previousContent, // Store previous content for diff comparison
  }

  if (index >= 0) {
    return existing.map((file, i) => (i === index ? merged : file))
  }

  return [...existing, merged]
}

export type AgentEventCallback = (event: AgentEvent) => void

export interface AgentGenerateOptions {
  sessionId?: string
  model?: string
  /** If true, the prompt is from a template and should not be displayed to the user */
  isHiddenPrompt?: boolean
  onEvent: AgentEventCallback
  onComplete?: () => void
  onError?: (error: Error) => void
}

// Generation state from backend
export interface GenerationState {
  has_generation: boolean
  version_id?: string
  version_number?: number
  generation_status?: 'pending' | 'generating' | 'complete' | 'error'
  generation_plan?: {
    id: string
    goal: string
    reasoning: string
    steps: Array<{
      id: string
      type: string
      title: string
      description: string
      status: string
    }>
  }
  current_step?: number
  error?: string
  files?: Array<{
    path: string
    content: string
    action: string
    language: string
  }>
  file_count?: number
  is_complete?: boolean
  is_generating?: boolean
  created_at?: string
}

/**
 * Fetch the latest generation state for an app
 */
export async function fetchLatestGeneration(appId: string): Promise<GenerationState | null> {
  try {
    const response = await api.get<GenerationState>(`/apps/${appId}/latest-generation/`)
    return response.data
  } catch (error) {
    console.error('Failed to fetch generation state:', error)
    return null
  }
}

/**
 * Fetch generation state for a specific version
 */
export async function fetchGenerationState(versionId: string): Promise<GenerationState | null> {
  try {
    const response = await api.get<GenerationState>(`/versions/${versionId}/generation-state/`)
    return response.data
  } catch (error) {
    console.error('Failed to fetch generation state:', error)
    return null
  }
}

/**
 * Cancel a generating version when the user aborts
 * @see {@link versionsApi.cancel} in apiService for the underlying API call.
 */
export async function cancelGeneration(versionId: string): Promise<void> {
  try {
    await versionsApi.cancel(versionId)
  } catch (error) {
    // Log but don't throw - cancellation is best-effort cleanup
    console.warn('[agentService] Failed to cancel generation:', error)
  }
}

/**
 * Cancel a generation job by job ID
 */
export async function cancelJob(jobId: string): Promise<void> {
  try {
    await api.post(`/jobs/${jobId}/cancel/`)
  } catch (error) {
    console.warn('[agentService] Failed to cancel job:', error)
  }
}

/**
 * Job status response from the backend
 */
export interface JobStatus {
  job_id: string
  app_id: string
  status: 'questioning' | 'queued' | 'processing' | 'streaming' | 'complete' | 'failed' | 'cancelled'
  version_id: string | null
  event_count: number
  created_at: string
  started_at: string | null
  completed_at: string | null
  error_message: string | null
  is_active: boolean
}

/**
 * Fetch the status of a specific job
 */
export async function fetchJobStatus(jobId: string): Promise<JobStatus | null> {
  try {
    const response = await api.get<JobStatus>(`/jobs/${jobId}/`)
    return response.data
  } catch (error) {
    console.error('[agentService] Failed to fetch job status:', error)
    return null
  }
}

/**
 * Latest job response from the backend
 */
export interface LatestJobInfo {
  has_active_job: boolean
  job_id: string | null
  status?: string
  version_id?: string | null
  event_count?: number
  created_at?: string
  completed_at?: string | null
}

/**
 * Fetch the latest job for an app (for reconnection on page refresh)
 */
export async function fetchLatestJob(appId: string): Promise<LatestJobInfo | null> {
  try {
    const response = await api.get<LatestJobInfo>(`/apps/${appId}/latest-job/`)
    return response.data
  } catch (error) {
    console.error('[agentService] Failed to fetch latest job:', error)
    return null
  }
}

/**
 * Start agentic code generation with streaming progress
 * 
 * Uses job-based background processing for resilience:
 * - Generation runs in Celery worker, survives connection drops
 * - Events stored in DB for replay on reconnection
 * - Same SSE interface as before (no breaking changes)
 * 
 * Returns an object with:
 * - controller: AbortController to cancel the request
 * - getVersionId: function to get the current version ID (for cancellation)
 * - getJobId: function to get the job ID (for reconnection)
 */
export function startAgenticGeneration(
  appId: string,
  message: string,
  options: AgentGenerateOptions
): { controller: AbortController; getVersionId: () => string | null; getJobId: () => string | null } {
  const controller = new AbortController()
  
  // Track IDs when they're created
  let currentVersionId: string | null = null
  let currentJobId: string | null = null

  const params = new URLSearchParams({
    message,
    model: options.model || 'anthropic/claude-sonnet-4.5',
    mode: 'agentic',
  })

  if (options.sessionId) {
    params.set('session_id', options.sessionId)
  }

  // Use legacy GET endpoint which now creates a job and streams from it
  const url = `${api.defaults.baseURL}/apps/${appId}/generate/agentic/?${params.toString()}`
  const token = localStorage.getItem('access_token')

  if (token) {
    let eventCount = 0
    
    fetch(url, {
      method: 'GET',
      headers: {
        'Authorization': `Bearer ${token}`,
        'Accept': 'text/event-stream',
      },
      signal: controller.signal,
    })
      .then(async (response) => {
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

          const chunk = decoder.decode(value, { stream: true })
          buffer += chunk

          // Parse SSE events
          const lines = buffer.split('\n')
          buffer = lines.pop() || ''

          let eventType = ''
          let eventData = ''

          for (const line of lines) {
            if (line.startsWith('event: ')) {
              eventType = line.slice(7).trim()
            } else if (line.startsWith('data: ')) {
              eventData = line.slice(6)
            } else if (line === '' && eventType && eventData) {
              try {
                const data = JSON.parse(eventData)
                eventCount++
                
                // Track the version ID when a draft version is created
                if (eventType === 'version_draft' && data.version_id) {
                  currentVersionId = data.version_id
                }
                
                // Track job_id from any event that includes it (e.g., questioning_started)
                if (data.job_id && !currentJobId) {
                  currentJobId = data.job_id
                }
                
                const event: AgentEvent = {
                  type: eventType as AgentEventType,
                  timestamp: new Date().toISOString(),
                  data,
                }
                options.onEvent(event)
              } catch (e) {
                console.warn('[SSE] Failed to parse SSE data:', { eventType, eventData, error: e })
              }
              eventType = ''
              eventData = ''
            }
          }
        }
      })
      .catch(async (error) => {
        if (error.name === 'AbortError') {
          // User cancelled - clean up
          if (currentJobId) {
            await cancelJob(currentJobId)
          } else if (currentVersionId) {
            await cancelGeneration(currentVersionId)
          }
        } else {
          options.onError?.(error)
        }
      })
  } else {
    options.onError?.(new Error('Authentication required'))
  }

  return {
    controller,
    getVersionId: () => currentVersionId,
    getJobId: () => currentJobId,
  }
}

/**
 * Reconnect to an existing generation job
 * 
 * Use this to resume streaming events after a page refresh or browser switch.
 * All events are replayed from the start, then live streaming continues.
 */
export function reconnectToJob(
  jobId: string,
  options: AgentGenerateOptions & { lastEventIndex?: number }
): { controller: AbortController } {
  const controller = new AbortController()
  
  const params = new URLSearchParams()
  if (options.lastEventIndex !== undefined) {
    params.set('last_index', String(options.lastEventIndex))
  }

  const url = `${api.defaults.baseURL}/jobs/${jobId}/stream/?${params.toString()}`
  const token = localStorage.getItem('access_token')

  if (token) {
    let eventCount = 0
    
    fetch(url, {
      method: 'GET',
      headers: {
        'Authorization': `Bearer ${token}`,
        'Accept': 'text/event-stream',
      },
      signal: controller.signal,
    })
      .then(async (response) => {
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

          const chunk = decoder.decode(value, { stream: true })
          buffer += chunk

          // Parse SSE events
          const lines = buffer.split('\n')
          buffer = lines.pop() || ''

          let eventType = ''
          let eventData = ''

          for (const line of lines) {
            if (line.startsWith('event: ')) {
              eventType = line.slice(7).trim()
            } else if (line.startsWith('data: ')) {
              eventData = line.slice(6)
            } else if (line === '' && eventType && eventData) {
              try {
                const data = JSON.parse(eventData)
                eventCount++
                
                const event: AgentEvent = {
                  type: eventType as AgentEventType,
                  timestamp: new Date().toISOString(),
                  data,
                }
                options.onEvent(event)
              } catch (e) {
                console.warn('[SSE:reconnect] Failed to parse SSE data:', { eventType, eventData, error: e })
              }
              eventType = ''
              eventData = ''
            }
          }
        }
      })
      .catch((error) => {
        if (error.name !== 'AbortError') {
          options.onError?.(error)
        }
      })
  } else {
    options.onError?.(new Error('Authentication required'))
  }

  return { controller }
}

/**
 * Options for the fix errors stream
 */
export interface FixErrorsOptions {
  model?: string
  attempt?: number
  onEvent: AgentEventCallback
  onComplete?: () => void
  onError?: (error: Error) => void
}

/**
 * Bundler error structure from Sandpack
 */
export interface BundlerError {
  title: string
  message: string
  file?: string
  line?: number
  column?: number
}

/**
 * Start error fixing stream for bundler errors
 * 
 * This is a fallback when Sandpack detects errors that the backend
 * TypeScript validation didn't catch. Limited to 2 attempts.
 */
export function startFixErrors(
  versionId: string,
  errors: BundlerError[],
  options: FixErrorsOptions
): { controller: AbortController } {
  const controller = new AbortController()
  
  // Base64 encode the errors for URL transmission
  const errorsJson = JSON.stringify(errors)
  const errorsB64 = btoa(errorsJson)
  
  const params = new URLSearchParams({
    errors: errorsB64,
    model: options.model || 'anthropic/claude-sonnet-4.5',
    attempt: String(options.attempt || 1),
  })

  const url = `${api.defaults.baseURL}/versions/${versionId}/fix-errors/?${params.toString()}`
  const token = localStorage.getItem('access_token')

  if (token) {
    let eventCount = 0
    
    fetch(url, {
      method: 'GET',
      headers: {
        'Authorization': `Bearer ${token}`,
        'Accept': 'text/event-stream',
      },
      signal: controller.signal,
    })
      .then(async (response) => {
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

          const chunk = decoder.decode(value, { stream: true })
          buffer += chunk

          // Parse SSE events
          const lines = buffer.split('\n')
          buffer = lines.pop() || ''

          let eventType = ''
          let eventData = ''

          for (const line of lines) {
            if (line.startsWith('event: ')) {
              eventType = line.slice(7).trim()
            } else if (line.startsWith('data: ')) {
              eventData = line.slice(6)
            } else if (line === '' && eventType && eventData) {
              try {
                const data = JSON.parse(eventData)
                eventCount++
                
                const event: AgentEvent = {
                  type: eventType as AgentEventType,
                  timestamp: new Date().toISOString(),
                  data,
                }
                options.onEvent(event)
              } catch (e) {
                console.warn('[SSE:fixErrors] Failed to parse SSE data:', { eventType, eventData, error: e })
              }
              eventType = ''
              eventData = ''
            }
          }
        }
      })
      .catch((error) => {
        if (error.name !== 'AbortError') {
          options.onError?.(error)
        }
      })
  } else {
    options.onError?.(new Error('Authentication required'))
  }

  return { controller }
}

/**
 * Reducer for agent state updates from events
 */
export function agentStateReducer(
  state: AgentState,
  event: AgentEvent
): AgentState {
  const data = event.data as EventData
  
  switch (event.type) {
    case 'agent_start':
      return {
        ...state,
        phase: 'researching',
        startedAt: event.timestamp,
        error: null,
      }

    case 'phase_change':
      return {
        ...state,
        phase: data.phase,
      }

    case 'thinking':
      return {
        ...state,
        thinking: [
          ...state.thinking,
          {
            id: crypto.randomUUID(),
            timestamp: event.timestamp,
            phase: state.phase,
            content: data.content,
            type: data.type,
          },
        ],
      }

    case 'plan_created':
      return {
        ...state,
        plan: data.plan,
        phase: 'executing',
      }

    case 'step_start':
      return {
        ...state,
        currentStepIndex: data.stepIndex ?? data.step_index,
        plan: state.plan
          ? {
              ...state.plan,
              steps: state.plan.steps.map((step, i) =>
                i === (data.stepIndex ?? data.step_index)
                  ? { ...step, status: 'in_progress' as const }
                  : step
              ),
            }
          : null,
      }

    case 'step_complete': {
      const stepIndex = data.stepIndex ?? data.step_index
      return {
        ...state,
        plan: state.plan
          ? {
              ...state.plan,
              steps: state.plan.steps.map((step, i) =>
                i === stepIndex
                  ? {
                      ...step,
                      status: data.status || 'complete',
                      duration: data.duration,
                      output: data.output,
                    }
                  : step
              ),
            }
          : null,
      }
    }

    case 'file_generated': {
      const updatedFiles = upsertFileChange(state.generatedFiles, data.file)
      return {
        ...state,
        generatedFiles: updatedFiles,
      }
    }

    case 'preview_ready':
      return {
        ...state,
        previewReady: true,
        phase: 'complete',
      }

    case 'agent_complete':
      return {
        ...state,
        phase: 'complete',
        completedAt: event.timestamp,
      }

    case 'agent_error': {
      return {
        ...state,
        phase: 'error',
        error: data.message,
      }
    }

    // Questioning phase events
    case 'questioning_started':
      return {
        ...state,
        isQuestioning: true,
        questioningSessionId: data.session_id,
        currentQuestionNumber: 0,
      }

    case 'question_asked':
      return {
        ...state,
        currentQuestionNumber: data.question_number,
      }

    case 'questioning_complete':
    case 'questioning_skipped':
      return {
        ...state,
        isQuestioning: false,
      }

    default:
      return state
  }
}

/**
 * Get phase display info
 */
export function getPhaseInfo(phase: AgentState['phase']): {
  label: string
  icon: string
  color: string
} {
  const phases = {
    idle: { label: 'Ready', icon: '○', color: 'gray' },
    researching: { label: 'Researching', icon: '🔍', color: 'blue' },
    planning: { label: 'Planning', icon: '📋', color: 'purple' },
    executing: { label: 'Building', icon: '🔨', color: 'amber' },
    validating: { label: 'Validating', icon: '✓', color: 'green' },
    complete: { label: 'Complete', icon: '✅', color: 'green' },
    error: { label: 'Error', icon: '❌', color: 'red' },
  }
  return phases[phase]
}

/**
 * Calculate overall progress percentage
 */
export function calculateProgress(state: AgentState): number {
  if (state.phase === 'idle') return 0
  if (state.phase === 'complete') return 100
  if (!state.plan) return 5 // Just started

  const completedSteps = state.plan.steps.filter(
    (s) => s.status === 'complete' || s.status === 'skipped'
  ).length

  // Reserve 10% for research/planning, 80% for execution, 10% for validation
  const baseProgress = 10
  const executionProgress = (completedSteps / state.plan.steps.length) * 80
  const validationProgress = state.phase === 'validating' ? 5 : 0

  return Math.min(95, baseProgress + executionProgress + validationProgress)
}

