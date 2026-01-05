/**
 * Agentic Chat Panel Component
 * 
 * Cursor-style AI chat interface with inline thinking states,
 * task progress, and file generation previews.
 * 
 * Enterprise light theme design following STYLE_GUIDE.md
 */
import { useState, useRef, useEffect, useCallback, useReducer } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import {
  ArrowUp,
  Loader2,
  Sparkles,
  CheckCircle,
  XCircle,
  Lightbulb,
  FileCode2,
  ChevronDown,
  ChevronRight,
  Circle,
  Clock,
  Search,
  ListChecks,
} from 'lucide-react'
import { ModelSelector } from './ModelSelector'
import {
  startAgenticGeneration,
  agentStateReducer,
  fetchLatestGeneration,
  upsertFileChange,
  cancelGeneration,
} from '../../services/agentService'
import type { AgentEvent, PlanStep, FileChange } from '../../types/agent'
import { initialAgentState } from '../../types/agent'
import { useChatSessions, useCreateChatSession, useChatMessages } from '../../hooks/useAI'
import type { ChatMessage as ApiChatMessage } from '../../services/aiService'
import { cn } from '../../lib/utils'

// Helper type for accessing event data with any properties
// eslint-disable-next-line @typescript-eslint/no-explicit-any
type EventData = Record<string, any>

interface AgenticChatPanelProps {
  appId: string
  sessionId: string | null
  onSessionChange: (sessionId: string) => void
  onVersionCreated: (versionId: string, versionNumber: number) => void
  onFilesGenerated?: (files: FileChange[]) => void
  className?: string
}

interface LocalMessage {
  id: string
  role: 'user' | 'assistant' | 'system'
  content: string
  status: 'pending' | 'streaming' | 'complete' | 'error'
  isAgentic?: boolean
  thinkingDuration?: number
  tasks?: PlanStep[]
  files?: FileChange[]
  exploredInfo?: { directories: number; files: number; searches: number }
  error?: string
  createdAt: string
}

// Animated thinking indicator with timer
function ThinkingIndicator({ startTime }: { startTime: number }) {
  const [elapsed, setElapsed] = useState(0)

  useEffect(() => {
    const interval = setInterval(() => {
      setElapsed(Math.floor((Date.now() - startTime) / 1000))
    }, 1000)
    return () => clearInterval(interval)
  }, [startTime])

  return (
    <div className="flex items-center gap-3 py-3">
      <motion.div
        className="w-8 h-8 rounded-lg bg-gray-100 border border-gray-200 flex items-center justify-center"
        animate={{ 
          boxShadow: [
            '0 0 0 0 rgba(156, 163, 175, 0)',
            '0 0 0 4px rgba(156, 163, 175, 0.1)',
            '0 0 0 0 rgba(156, 163, 175, 0)',
          ]
        }}
        transition={{ duration: 2, repeat: Infinity, ease: 'easeInOut' }}
      >
        <Lightbulb className="h-4 w-4 text-gray-500" />
      </motion.div>
      <div>
        <span className="text-sm text-gray-600">Thought for </span>
        <span className="text-sm font-medium text-gray-900">{elapsed}s</span>
      </div>
    </div>
  )
}

// Inline todo/task list with progress
function TaskList({ tasks }: { tasks: PlanStep[] }) {
  const [isExpanded, setIsExpanded] = useState(true)
  const totalCount = tasks.length

  return (
    <div className="my-3">
      <button
        onClick={() => setIsExpanded(!isExpanded)}
        className="flex items-center gap-2 text-[11px] font-medium text-gray-600 hover:text-gray-900 transition-colors"
      >
        <ListChecks className="h-3.5 w-3.5" />
        <span>To-dos</span>
        <span className="text-gray-400">{totalCount}</span>
        {isExpanded ? (
          <ChevronDown className="h-3 w-3 text-gray-400" />
        ) : (
          <ChevronRight className="h-3 w-3 text-gray-400" />
        )}
      </button>

      <AnimatePresence>
        {isExpanded && (
          <motion.div
            initial={{ height: 0, opacity: 0 }}
            animate={{ height: 'auto', opacity: 1 }}
            exit={{ height: 0, opacity: 0 }}
            transition={{ duration: 0.15 }}
            className="overflow-hidden"
          >
            <div className="mt-2 space-y-1 pl-1">
              {tasks.map((task, i) => {
                const isComplete = task.status === 'complete'
                const isInProgress = task.status === 'in_progress'
                const isPending = task.status === 'pending'

                return (
                  <motion.div
                    key={task.id}
                    initial={{ opacity: 0, x: -8 }}
                    animate={{ opacity: 1, x: 0 }}
                    transition={{ delay: i * 0.03 }}
                    className="flex items-start gap-2 py-1"
                  >
                    <div className="flex-shrink-0 mt-0.5">
                      {isComplete && (
                        <CheckCircle className="h-4 w-4 text-green-600" />
                      )}
                      {isInProgress && (
                        <motion.div
                          animate={{ rotate: 360 }}
                          transition={{ duration: 1, repeat: Infinity, ease: 'linear' }}
                        >
                          <Loader2 className="h-4 w-4 text-gray-600" />
                        </motion.div>
                      )}
                      {isPending && (
                        <Circle className="h-4 w-4 text-gray-300" />
                      )}
                    </div>
                    <span className="relative inline-flex">
                      <span
                        className={cn(
                          'text-[12px]',
                          isComplete && 'text-gray-500 line-through',
                          isInProgress && 'text-gray-900 font-medium',
                          isPending && 'text-gray-500'
                        )}
                      >
                        {task.title}
                      </span>
                    </span>
                    {task.duration && isComplete && (
                      <span className="text-[10px] text-gray-400 flex items-center gap-0.5 ml-auto">
                        <Clock className="h-2.5 w-2.5" />
                        {(task.duration / 1000).toFixed(1)}s
                      </span>
                    )}
                  </motion.div>
                )
              })}
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  )
}

// File preview with expandable content
function FilePreview({ file, index }: { file: FileChange; index: number }) {
  const [isExpanded, setIsExpanded] = useState(false)
  const fileName = file.path.split('/').pop() || file.path
  const lineCount = file.content?.split('\n').length || 0
  const added = file.addedLines ?? (file.action === 'create' ? lineCount : undefined)
  const removed = file.removedLines ?? 0
  const displayAdded = added ?? lineCount

  return (
    <motion.div
      initial={{ opacity: 0, y: 8 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ delay: index * 0.05 }}
      className="border border-gray-200 rounded-lg overflow-hidden bg-white"
    >
      <button
        onClick={() => setIsExpanded(!isExpanded)}
        className="w-full flex items-center gap-2 px-2.5 py-1.5 hover:bg-gray-50 transition-colors text-left"
      >
        <FileCode2 className="h-3.5 w-3.5 text-gray-500" />
        <span className="text-xs font-medium text-gray-800 flex-1 font-mono">
          {fileName}
        </span>
        <span className="text-[9px] px-1.5 py-0.5 rounded font-medium flex items-center gap-1 bg-green-50 text-green-700 border border-green-200">
          <span className="text-green-700 font-semibold">
            +{displayAdded}
          </span>
          {removed > 0 && (
            <span className="text-red-600 font-semibold">
              -{removed}
            </span>
          )}
          {added === undefined && removed === 0 && `~${lineCount}`}
        </span>
        {isExpanded ? (
          <ChevronDown className="h-3 w-3 text-gray-400" />
        ) : (
          <ChevronRight className="h-3 w-3 text-gray-400" />
        )}
      </button>

      <AnimatePresence>
        {isExpanded && file.content && (
          <motion.div
            initial={{ height: 0 }}
            animate={{ height: 'auto' }}
            exit={{ height: 0 }}
            className="overflow-hidden border-t border-gray-200"
          >
            <pre className="p-2.5 text-[11px] font-mono text-gray-700 bg-gray-50 overflow-x-auto max-h-56 overflow-y-auto">
              <code>{file.content}</code>
            </pre>
          </motion.div>
        )}
      </AnimatePresence>
    </motion.div>
  )
}

// Explored info badge (like Cursor's "Explored 3 directories, 11 files, 1 search")
function ExploredBadge({ info }: { info: { directories: number; files: number; searches: number } }) {
  if (info.directories === 0 && info.files === 0 && info.searches === 0) return null

  return (
    <div className="flex items-center gap-1.5 text-xs text-gray-500 py-2">
      <Search className="h-3 w-3" />
      <span>
        Explored {info.directories} {info.directories === 1 ? 'directory' : 'directories'}{' '}
        {info.files} {info.files === 1 ? 'file' : 'files'}, and {' '}
        {info.searches > 0 && `${info.searches} ${info.searches === 1 ? 'search' : 'searches'}`}
      </span>
    </div>
  )
}

// Main component
export function AgenticChatPanel({
  appId,
  sessionId,
  onSessionChange,
  onVersionCreated,
  onFilesGenerated,
  className = '',
}: AgenticChatPanelProps) {
  const [messages, setMessages] = useState<LocalMessage[]>([])
  const [input, setInput] = useState('')
  const [isLoading, setIsLoading] = useState(false)
  const [selectedModel, setSelectedModel] = useState('anthropic/claude-sonnet-4')
  const [abortController, setAbortController] = useState<AbortController | null>(null)
  const [thinkingStartTime, setThinkingStartTime] = useState<number | null>(null)
  const [accumulatedFiles, setAccumulatedFiles] = useState<FileChange[]>([])
  // Track the generating version ID so we can cancel it properly
  const [generatingVersionId, setGeneratingVersionId] = useState<string | null>(null)
  const [hasLoadedState, setHasLoadedState] = useState(false)
  const [isScrolling, setIsScrolling] = useState(false)
  const { data: chatSessions, refetch: refetchChatSessions, isLoading: sessionsLoading } = useChatSessions(appId)
  const { mutateAsync: createChatSession, isPending: creatingSession } = useCreateChatSession()
  const { data: sessionMessages, isFetching: messagesFetching } = useChatMessages(sessionId)
  const [hydratedSessionId, setHydratedSessionId] = useState<string | null>(null)
  const [hasHydratedMessages, setHasHydratedMessages] = useState(false)
  const [minLoaderElapsed, setMinLoaderElapsed] = useState(false)
  const loaderHoldRef = useRef<ReturnType<typeof setTimeout> | null>(null)
  const mapApiMessageToLocal = useCallback((msg: ApiChatMessage): LocalMessage => {
    const gf = msg.generated_files
    let files: FileChange[] | undefined
    let tasks: PlanStep[] | undefined

    if (gf && typeof gf === 'object' && !Array.isArray(gf)) {
      if (Array.isArray((gf as any).files)) {
        files = (gf as any).files.map((f: any) => ({
          path: f.path,
          content: f.content,
          action: (f.action as FileChange['action']) || 'create',
          language: (f.language as FileChange['language']) || 'tsx',
        }))
      } else {
        files = Object.entries(gf).map(([path, content]) => ({
          path,
          content: typeof content === 'string' ? content : JSON.stringify(content, null, 2),
          action: 'create' as const,
          language: path.endsWith('.css')
            ? 'css'
            : path.endsWith('.json')
              ? 'json'
              : path.endsWith('.ts')
                ? 'ts'
                : 'tsx',
        }))
      }

      // Do not hydrate tasks or live progress from history; keep live view only
      tasks = undefined
    } else if (gf) {
      files = Object.entries(gf).map(([path, content]) => ({
        path,
        content: typeof content === 'string' ? content : JSON.stringify(content, null, 2),
        action: 'create' as const,
        language: path.endsWith('.css')
          ? 'css'
          : path.endsWith('.json')
            ? 'json'
            : path.endsWith('.ts')
              ? 'ts'
              : 'tsx',
      }))
    }

    return {
      id: msg.id,
      role: msg.role,
      content: msg.content,
      status: msg.status,
      createdAt: msg.created_at,
      files,
      tasks,
      error: msg.error_message || undefined,
    }
  }, [])

  // Agent state using reducer
  const [agentState, dispatchAgentEvent] = useReducer(
    agentStateReducer,
    initialAgentState
  )

  const messagesEndRef = useRef<HTMLDivElement>(null)
  const inputRef = useRef<HTMLTextAreaElement>(null)
  const scrollTimeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null)

  // Load existing generation state on mount (files only, not messages)
  // Chat messages come exclusively from session hydration to avoid race conditions.
  // This effect only restores files for the Sandpack preview.
  useEffect(() => {
    if (hasLoadedState) return
    
    const loadExistingState = async () => {
      try {
        const state = await fetchLatestGeneration(appId)
        if (state && state.has_generation && state.files && state.files.length > 0) {
          // Restore files to preview
          const restoredFiles: FileChange[] = state.files.map(f => ({
            path: f.path,
            content: f.content,
            action: f.action as 'create' | 'modify' | 'delete',
            language: f.language as FileChange['language'],
          }))
          
          setAccumulatedFiles(restoredFiles)
          onFilesGenerated?.(restoredFiles)
          
          if (state.version_id) {
            onVersionCreated(state.version_id, state.version_number || 1)
          }
          
          // Note: We intentionally do NOT set messages here.
          // Chat messages come from session hydration (useChatMessages hook).
          // Setting messages here would race with session hydration and cause
          // inconsistent behavior between page refresh and navigation from dashboard.
        }
      } catch (error) {
        console.error('Failed to load existing state:', error)
      } finally {
        setHasLoadedState(true)
      }
    }
    
    loadExistingState()
  }, [appId, hasLoadedState, onFilesGenerated, onVersionCreated])

  // Auto-select latest session for this app
  useEffect(() => {
    if (sessionId || !chatSessions || chatSessions.length === 0) return
    onSessionChange(chatSessions[0].id)
  }, [chatSessions, onSessionChange, sessionId])

  // Load persisted chat history for the active session
  useEffect(() => {
    if (!sessionId || !sessionMessages) return
    if (hydratedSessionId === sessionId) return
    // Avoid overwriting live streaming content; only hydrate when idle
    if (messages.length > 0 && messages[messages.length - 1]?.status === 'streaming') return
    const restored = sessionMessages.map(mapApiMessageToLocal)
    setMessages(restored)
    setHydratedSessionId(sessionId)
  }, [hydratedSessionId, mapApiMessageToLocal, messages, sessionId, sessionMessages])

  // Auto-create a session if none exists for this app
  useEffect(() => {
    // Wait for the initial session fetch before deciding to create one
    if (!chatSessions) return

    // Always pick the latest session; if none, create one.
    if (chatSessions && chatSessions.length > 0) {
      const latest = chatSessions[0]
      if (!sessionId || !chatSessions.some(s => s.id === sessionId)) {
        onSessionChange(latest.id)
      }
      return
    }
    if (sessionId || creatingSession) return
    let cancelled = false
    const ensureSession = async () => {
      try {
        const session = await createChatSession({ appId })
        if (!cancelled) {
          onSessionChange(session.id)
        }
      } catch (error) {
        console.error('Failed to auto-create chat session', error)
      }
    }
    ensureSession()
    return () => {
      cancelled = true
    }
  }, [appId, chatSessions, createChatSession, creatingSession, onSessionChange, sessionId])

  // Auto-scroll
  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' })
  }, [messages, agentState])

  // Handle agentic events - update messages with inline state
  const handleAgentEvent = useCallback(
    (event: AgentEvent) => {
      dispatchAgentEvent(event)
      const data = event.data as EventData

      switch (event.type) {
        case 'session_created':
          // Handle both session_id and sessionId for compatibility
          onSessionChange(data.sessionId || data.session_id)
          refetchChatSessions()
          break

        case 'agent_start':
          setThinkingStartTime(Date.now())
          setAccumulatedFiles([]) // Reset files for new generation
          break

        case 'phase_change':
          break

        case 'thinking':
          break

        case 'plan_created':
          // Update the last assistant message with tasks
          setMessages((prev) => {
            const updated = [...prev]
            const lastMsg = updated[updated.length - 1]
            if (lastMsg?.role === 'assistant') {
              lastMsg.tasks = data.steps || []
              lastMsg.exploredInfo = {
                directories: data.exploredDirectories || 0,
                files: data.exploredFiles || 0,
                searches: data.searches || 0,
              }
              if (thinkingStartTime) {
                lastMsg.thinkingDuration = Math.floor((Date.now() - thinkingStartTime) / 1000)
              }
            }
            return updated
          })
          setThinkingStartTime(null)
          break

        case 'step_start':
        case 'step_started': {
          const stepTitle =
            data.step?.title ||
            data.title ||
            data.stepTitle ||
            (typeof data.stepIndex === 'number' ? `Step ${data.stepIndex + 1}` : 'Step started')
          // Update task status in the message (alias to existing handler)
          setMessages((prev) => {
            const updated = [...prev]
            const lastMsg = updated[updated.length - 1]
            if (lastMsg?.tasks) {
              lastMsg.tasks = lastMsg.tasks.map((t) =>
                t.id === data.stepId || t.title === stepTitle
                  ? { ...t, status: 'in_progress' }
                  : t
              )
            }
            return updated
          })
          break
        }

        case 'step_completed':
          // Update task status in the message
          setMessages((prev) => {
            const updated = [...prev]
            const lastMsg = updated[updated.length - 1]
            if (lastMsg?.tasks) {
              lastMsg.tasks = lastMsg.tasks.map(t =>
                t.id === data.stepId
                  ? { ...t, status: event.type === 'step_completed' ? 'complete' : 'in_progress', duration: data.duration }
                  : t
              )
            }
            return updated
          })
          break

        case 'step_progress': {
          break
        }

        case 'step_complete': {
          break
        }

        case 'file_generated': {
          // Add file to accumulated files and notify parent
          const newFile = data.file
          setAccumulatedFiles((prev) => {
            const updated = upsertFileChange(prev, newFile)
            onFilesGenerated?.(updated)
            return updated
          })
          // Also update the message
          setMessages((prev) => {
            const updated = [...prev]
            const lastMsg = updated[updated.length - 1]
            if (lastMsg?.role === 'assistant') {
              lastMsg.files = upsertFileChange(lastMsg.files || [], newFile)
            }
            return updated
          })
          break
        }

        case 'table_created': {
          // A new data table was created by the agent
          setMessages((prev) => {
            const updated = [...prev]
            const lastMsg = updated[updated.length - 1]
            if (lastMsg?.role === 'assistant') {
              // Add table creation info to the message
              const tableInfo = `Created table ${data.name} with ${data.columns} columns`
              lastMsg.content = lastMsg.content 
                ? `${lastMsg.content}\n\n ${tableInfo}`
                : `${tableInfo}`
            }
            return updated
          })
          break
        }

        case 'table_updated': {
          // An existing data table was updated by the agent
          setMessages((prev) => {
            const updated = [...prev]
            const lastMsg = updated[updated.length - 1]
            if (lastMsg?.role === 'assistant') {
              const changes = data.changes || {}
              const changeInfo = []
              if (changes.added?.length) changeInfo.push(`added: ${changes.added.join(', ')}`)
              if (changes.removed?.length) changeInfo.push(`removed: ${changes.removed.join(', ')}`)
              if (changes.modified?.length) changeInfo.push(`modified: ${changes.modified.join(', ')}`)
              
              const tableInfo = changeInfo.length > 0
                ? `Updated table "${data.name}" - ${changeInfo.join('; ')}`
                : `Updated table "${data.name}"`
              lastMsg.content = lastMsg.content 
                ? `${lastMsg.content}\n\n📊 ${tableInfo}`
                : `📊 ${tableInfo}`
            }
            return updated
          })
          break
        }

        case 'code_chunk': {
          // Too chatty for users; skip
          break
        }

        case 'validation_result': {
          break
        }

        case 'preview_ready': {
          onVersionCreated(data.versionId || data.version_id, data.versionNumber || data.version_number)
          const finalFiles =
            (Array.isArray(data.files)
              ? data.files.reduce(
                  (list: FileChange[], file: FileChange) =>
                    upsertFileChange(list, file),
                  accumulatedFiles
                )
              : accumulatedFiles)
          setAccumulatedFiles(finalFiles)
          if (finalFiles.length > 0) {
            onFilesGenerated?.(finalFiles)
          }
          break
        }
        
        case 'done': {
          // Generation complete - ensure we've passed files
          if (accumulatedFiles.length > 0) {
            onFilesGenerated?.(accumulatedFiles)
          }
          break
        }

        case 'version_draft':
          // Track the generating version ID so we can cancel it if needed
          if (data.version_id) {
            setGeneratingVersionId(data.version_id)
          }
          break

        case 'version_created':
          onVersionCreated(data.versionId || data.version_id, data.versionNumber || data.version_number)
          // Clear the generating version ID since generation is complete
          setGeneratingVersionId(null)
          break

        case 'agent_complete':
          setMessages((prev) => {
            const updated = [...prev]
            const lastMsg = updated[updated.length - 1]
            if (lastMsg?.role === 'assistant') {
              lastMsg.status = 'complete'
              lastMsg.content = data.summary || 'App generated successfully!'
            }
            return updated
          })
          setThinkingStartTime(null)
          break

        case 'agent_error':
          setMessages((prev) => {
            const updated = [...prev]
            const lastMsg = updated[updated.length - 1]
            if (lastMsg?.role === 'assistant') {
              lastMsg.status = 'error'
              lastMsg.error = data.message
            }
            return updated
          })
          setThinkingStartTime(null)
          break
      }
    },
    [
      onSessionChange,
      onVersionCreated,
      onFilesGenerated,
      thinkingStartTime,
      accumulatedFiles,
    ]
  )

  const handleSubmit = async () => {
    if (!input.trim() || isLoading) return

    const message = input.trim()
    setInput('')
    setIsLoading(true)
    setThinkingStartTime(Date.now())

    // Reset agent state
    dispatchAgentEvent({
      type: 'agent_start',
      timestamp: new Date().toISOString(),
      data: { sessionId: '', messageId: '', goal: message },
    })

    // Reset the generating version ID for a new generation
    setGeneratingVersionId(null)

    // Add user message
    setMessages((prev) => [
      ...prev,
      {
        id: Date.now().toString(),
        role: 'user',
        content: message,
        status: 'complete',
        createdAt: new Date().toISOString(),
      },
    ])

    // Add assistant placeholder
    setMessages((prev) => [
      ...prev,
      {
        id: (Date.now() + 1).toString(),
        role: 'assistant',
        content: '',
        status: 'streaming',
        isAgentic: true,
        tasks: [],
        files: [],
        createdAt: new Date().toISOString(),
      },
    ])

    const { controller } = startAgenticGeneration(appId, message, {
      sessionId: sessionId || undefined,
      model: selectedModel,
      onEvent: handleAgentEvent,
      onComplete: () => {
        setIsLoading(false)
        setAbortController(null)
        setGeneratingVersionId(null)  // Clear on completion
      },
      onError: (error) => {
        console.error('Agent error:', error)
        setIsLoading(false)
        setAbortController(null)
        setThinkingStartTime(null)
        setGeneratingVersionId(null)  // Clear on error
        setMessages((prev) => {
          const updated = [...prev]
          const lastMsg = updated[updated.length - 1]
          if (lastMsg?.role === 'assistant') {
            lastMsg.status = 'error'
            lastMsg.error = error.message
          }
          return updated
        })
      },
    })

    setAbortController(controller)
  }

  const handleCancel = async () => {
    // Abort the SSE connection
    abortController?.abort()
    setIsLoading(false)
    setAbortController(null)
    setThinkingStartTime(null)

    // Cancel the generating version on the backend to clean up
    if (generatingVersionId) {
      try {
        await cancelGeneration(generatingVersionId)
        console.log('[AgenticChatPanel] Cancelled generating version:', generatingVersionId)
      } catch (error) {
        console.warn('[AgenticChatPanel] Failed to cancel generating version:', error)
      }
      setGeneratingVersionId(null)
    }

    dispatchAgentEvent({
      type: 'agent_error',
      timestamp: new Date().toISOString(),
      data: {
        message: 'Generation cancelled',
        phase: 'error',
        recoverable: true,
      },
    })
  }

  const handleSendClick = () => {
    if (isLoading) {
      handleCancel()
    } else {
      handleSubmit()
    }
  }

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault()
      handleSubmit()
    }
  }

  const waitingOnSessionData =
    sessionsLoading ||
    creatingSession ||
    !chatSessions ||
    chatSessions.length === 0 ||
    !sessionId

  const waitingOnMessages = !hasHydratedMessages
  const showLoadingOverlay = waitingOnSessionData || waitingOnMessages || !minLoaderElapsed

  const handleMessagesScroll = useCallback(() => {
    if (!isScrolling) {
      setIsScrolling(true)
    }

    if (scrollTimeoutRef.current) {
      clearTimeout(scrollTimeoutRef.current)
    }

    scrollTimeoutRef.current = setTimeout(() => {
      setIsScrolling(false)
    }, 600)
  }, [isScrolling])

  // Clear any pending scroll timers on unmount
  useEffect(() => {
    return () => {
      if (scrollTimeoutRef.current) {
        clearTimeout(scrollTimeoutRef.current)
      }
    }
  }, [])

  // Track message hydration so we don't flash empty state before data arrives
  useEffect(() => {
    setHasHydratedMessages(false)
    setMinLoaderElapsed(false)
    if (loaderHoldRef.current) {
      clearTimeout(loaderHoldRef.current)
      loaderHoldRef.current = null
    }
  }, [sessionId])

  useEffect(() => {
    if (!sessionId) return
    // Consider messages hydrated only after we've loaded the current session's
    // history (even if empty) and our hydrate effect has run.
    if (
      !messagesFetching &&
      sessionMessages &&
      (hydratedSessionId === sessionId || sessionMessages.length === 0)
    ) {
      setHasHydratedMessages(true)
    }
  }, [sessionId, sessionMessages, messagesFetching, hydratedSessionId])

  // Add a small minimum loader duration to avoid flicker
  useEffect(() => {
    if (waitingOnSessionData || waitingOnMessages) {
      setMinLoaderElapsed(false)
      if (loaderHoldRef.current) {
        clearTimeout(loaderHoldRef.current)
        loaderHoldRef.current = null
      }
      return
    }
    loaderHoldRef.current = setTimeout(() => setMinLoaderElapsed(true), 300)
    return () => {
      if (loaderHoldRef.current) {
        clearTimeout(loaderHoldRef.current)
        loaderHoldRef.current = null
      }
    }
  }, [waitingOnSessionData, waitingOnMessages])

  return (
    <div className={cn('relative flex flex-col h-full bg-white', className)}>
      <AnimatePresence initial={false} mode="wait">
        {showLoadingOverlay && (
          <motion.div
            key="chat-loading"
            className="absolute inset-0 z-10 flex items-center justify-center bg-white"
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
            exit={{ opacity: 0 }}
          >
            <div className="flex items-center gap-3 text-sm text-gray-600">
              <Loader2 className="h-4 w-4 animate-spin" />
              <span>Preparing chat…</span>
            </div>
          </motion.div>
        )}
      </AnimatePresence>

      {/* Messages */}
      <div
        className={cn(
          'flex-1 overflow-y-auto scrollbar-auto-hide relative',
          isScrolling && 'scrolling'
        )}
        onScroll={handleMessagesScroll}
      >
        <div className="max-w-3xl mx-auto px-4 py-6 space-y-6">
          <AnimatePresence initial={false}>
            {waitingOnMessages && (
              <motion.div
                key="messages-loading"
                className="flex items-center gap-2 text-xs text-gray-500"
                initial={{ opacity: 0 }}
                animate={{ opacity: 1 }}
                exit={{ opacity: 0 }}
              >
                <Loader2 className="h-4 w-4 animate-spin" />
                <span>Loading chat history…</span>
              </motion.div>
            )}
          </AnimatePresence>

          <AnimatePresence initial={false}>
            {messages.length === 0 && !waitingOnMessages && !waitingOnSessionData && (
              <motion.div
                key="empty-state"
                className="flex flex-col items-center justify-center py-16 text-center"
                initial={{ opacity: 0 }}
                animate={{ opacity: 1 }}
                exit={{ opacity: 0 }}
              >
                <div className="h-16 w-16 rounded-xl bg-gray-100 border border-gray-200 
                               flex items-center justify-center mb-6">
                  <Sparkles className="h-8 w-8 text-gray-400" />
                </div>
                <h3 className="text-lg font-medium text-gray-900 mb-2">
                  What would you like to build?
                </h3>
                <p className="text-sm text-gray-600 max-w-sm mb-8">
                  Describe your app and I'll research, plan, and generate the code step by step.
                </p>
                <div className="flex flex-wrap gap-2 justify-center max-w-md">
                  {[
                    'Build a dashboard with user stats',
                    'Create an order management table',
                    'Design a settings page',
                  ].map((suggestion) => (
                    <button
                      key={suggestion}
                      onClick={() => setInput(suggestion)}
                      className="px-3 py-2 text-sm text-gray-700 bg-white border border-gray-200 
                               rounded-lg hover:border-gray-300 hover:bg-gray-50 transition-all"
                    >
                      {suggestion}
                    </button>
                  ))}
                </div>
              </motion.div>
            )}
          </AnimatePresence>

          <AnimatePresence initial={false}>
            {messages.map((message) => (
              <motion.div
                key={message.id}
                className="space-y-3"
                layout={false}
                initial={{ opacity: 0 }}
                animate={{ opacity: 1 }}
                exit={{ opacity: 0 }}
                transition={{ duration: 0.2, ease: 'easeOut' }}
              >
                {message.role === 'user' ? (
                  // User message - simple right-aligned bubble
                  <div className="flex justify-end">
                    <div className="max-w-[80%] bg-gray-900 text-white rounded-2xl px-4 py-3">
                      <p className="text-sm">{message.content}</p>
                    </div>
                  </div>
                ) : (
                  // Assistant message with inline agentic content
                  <div className="space-y-3">
                    {/* Thinking indicator */}
                    {message.status === 'streaming' && thinkingStartTime && !message.tasks?.length && (
                      <ThinkingIndicator startTime={thinkingStartTime} />
                    )}

                    {/* Thought duration badge (after thinking completes) */}
                    {message.thinkingDuration && message.thinkingDuration > 0 && (
                      <div className="flex items-center gap-2 text-sm text-gray-600">
                        <Lightbulb className="h-4 w-4 text-gray-400" />
                        <span>Thought for {message.thinkingDuration}s</span>
                      </div>
                    )}

                    {/* Explored info */}
                    {message.exploredInfo && (
                      <ExploredBadge info={message.exploredInfo} />
                    )}

                    {/* Task list */}
                    {message.tasks && message.tasks.length > 0 && (
                      <TaskList 
                        tasks={message.tasks} 
                      />
                    )}

                    {/* Summary text content */}
                    {message.content && (
                      <div className="text-sm text-gray-700 leading-relaxed whitespace-pre-wrap">
                        {message.content}
                      </div>
                    )}

                    {/* Generated files */}
                    {message.files && message.files.length > 0 && (
                      <div className="space-y-2 pt-2">
                        {message.files.map((file, i) => (
                          <FilePreview key={file.path} file={file} index={i} />
                        ))}
                      </div>
                    )}

                    {/* Completion badge */}
                    {message.status === 'complete' && message.files && message.files.length > 0 && (
                      <div className="flex items-center gap-2 pt-3 text-sm text-green-700">
                        <CheckCircle className="h-4 w-4" />
                        <span>Generated {message.files.length} files</span>
                      </div>
                    )}

                    {/* Error display */}
                    {message.error && (
                      <div className="flex items-start gap-2 p-3 bg-red-50 border border-red-200 rounded-lg">
                        <XCircle className="h-4 w-4 text-red-600 flex-shrink-0 mt-0.5" />
                        <p className="text-sm text-red-700">{message.error}</p>
                      </div>
                    )}
                  </div>
                )}
              </motion.div>
            ))}
          </AnimatePresence>

          <div ref={messagesEndRef} />
        </div>
      </div>

      {/* Input area */}
      <div className="border-t border-gray-200 bg-gray-50">
        <div className="max-w-3xl mx-auto px-4 py-5">
          <div className="rounded-2xl border border-gray-200 bg-white shadow-sm">
            <div className="px-4 pt-4">
              <textarea
                ref={inputRef}
                value={input}
                onChange={(e) => setInput(e.target.value)}
                onKeyDown={handleKeyDown}
                placeholder="Describe what you want to build..."
                rows={1}
                disabled={isLoading}
                className="w-full bg-transparent border-none text-sm text-gray-900 placeholder-gray-500 resize-none
                           focus:outline-none focus:ring-0 disabled:opacity-60"
                style={{ minHeight: '48px', maxHeight: '140px' }}
                onInput={(e) => {
                  const target = e.target as HTMLTextAreaElement
                  target.style.height = 'auto'
                  target.style.height = Math.min(target.scrollHeight, 140) + 'px'
                }}
              />
            </div>

            <div className="mt-3 flex items-center justify-between gap-3 px-4 py-3 bg-white rounded-b-2xl">
              <ModelSelector
                selectedModel={selectedModel}
                onModelChange={setSelectedModel}
                className="min-w-[180px]"
                size="sm"
                placement="up"
              />
              <button
                onClick={handleSendClick}
                disabled={!isLoading && !input.trim()}
                className="inline-flex items-center justify-center h-6 w-6 rounded-full bg-gray-900 text-white
                           hover:bg-gray-800 disabled:opacity-30 disabled:cursor-not-allowed transition-all shadow-sm"
                title={isLoading ? 'Stop generation' : 'Send message'}
              >
                {isLoading ? (
                  <Loader2 className="h-4 w-4 animate-spin" />
                ) : (
                  <ArrowUp className="h-4 w-4" />
                )}
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}
