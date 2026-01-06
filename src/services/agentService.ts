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
// eslint-disable-next-line @typescript-eslint/no-explicit-any
type EventData = Record<string, any>

function lcsLength(a: string[], b: string[]): number {
  const n = b.length
  const dp = new Array(n + 1).fill(0)

  for (let i = 1; i <= a.length; i++) {
    let prev = 0
    for (let j = 1; j <= n; j++) {
      const temp = dp[j]
      dp[j] = a[i - 1] === b[j - 1] ? prev + 1 : Math.max(dp[j], dp[j - 1])
      prev = temp
    }
  }

  return dp[n]
}

function calculateLineDelta(prevContent?: string, nextContent?: string) {
  const toLines = (value?: string) =>
    value === undefined || value === '' ? [] : value.split('\n')

  const prevLines = toLines(prevContent)
  const nextLines = toLines(nextContent)
  const common = lcsLength(prevLines, nextLines)

  return {
    addedLines: Math.max(0, nextLines.length - common),
    removedLines: Math.max(0, prevLines.length - common),
  }
}

export function upsertFileChange(
  existing: FileChange[],
  change: FileChange
): FileChange[] {
  const index = existing.findIndex((f) => f.path === change.path)
  const previous = index >= 0 ? existing[index] : undefined
  const { addedLines, removedLines } = calculateLineDelta(
    previous?.content,
    change.content
  )

  const merged: FileChange = {
    ...previous,
    ...change,
    action: previous ? 'modify' : change.action || 'create',
    addedLines,
    removedLines,
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
 * Start agentic code generation with streaming progress
 * 
 * Returns an object with:
 * - controller: AbortController to cancel the request
 * - getVersionId: function to get the current version ID (for cancellation)
 */
export function startAgenticGeneration(
  appId: string,
  message: string,
  options: AgentGenerateOptions
): { controller: AbortController; getVersionId: () => string | null } {
  const controller = new AbortController()
  
  // Track the version ID when it's created so we can cancel it
  let currentVersionId: string | null = null

  const params = new URLSearchParams({
    message,
    model: options.model || 'anthropic/claude-sonnet-4',
    mode: 'agentic',
  })

  if (options.sessionId) {
    params.set('session_id', options.sessionId)
  }

  const url = `${api.defaults.baseURL}/apps/${appId}/generate/agentic/?${params.toString()}`
  const token = localStorage.getItem('access_token')

  if (token) {
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

          buffer += decoder.decode(value, { stream: true })

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
                
                // Track the version ID when a draft version is created
                if (eventType === 'version_draft' && data.version_id) {
                  currentVersionId = data.version_id
                }
                
                const event: AgentEvent = {
                  type: eventType as AgentEventType,
                  timestamp: new Date().toISOString(),
                  data,
                }
                options.onEvent(event)
              } catch (e) {
                console.warn('Failed to parse SSE data:', eventData)
              }
              eventType = ''
              eventData = ''
            }
          }
        }
      })
      .catch(async (error) => {
        if (error.name === 'AbortError') {
          // User cancelled - clean up the generating version
          if (currentVersionId) {
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
  }
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
    model: options.model || 'anthropic/claude-sonnet-4',
    attempt: String(options.attempt || 1),
  })

  const url = `${api.defaults.baseURL}/versions/${versionId}/fix-errors/?${params.toString()}`
  const token = localStorage.getItem('access_token')

  if (token) {
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

          buffer += decoder.decode(value, { stream: true })

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
                const event: AgentEvent = {
                  type: eventType as AgentEventType,
                  timestamp: new Date().toISOString(),
                  data,
                }
                options.onEvent(event)
              } catch (e) {
                console.warn('Failed to parse SSE data:', eventData)
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
      return {
        ...state,
        generatedFiles: upsertFileChange(state.generatedFiles, data.file),
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

