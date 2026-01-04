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
} from '../../services/agentService'
import type { AgentEvent, PlanStep, FileChange, AgentState } from '../../types/agent'
import { initialAgentState } from '../../types/agent'
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
  progressUpdates?: ProgressUpdate[]
  error?: string
  createdAt: string
}

type ProgressVariant = 'info' | 'phase' | 'thinking' | 'step' | 'file' | 'preview' | 'error'

interface ProgressUpdate {
  id: string
  text: string
  timestamp: string
  variant: ProgressVariant
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
        {info.files} {info.files === 1 ? 'file' : 'files'}{' '}
        {info.searches > 0 && `${info.searches} ${info.searches === 1 ? 'search' : 'searches'}`}
      </span>
    </div>
  )
}

function formatTimeLabel(timestamp: string) {
  try {
    return new Intl.DateTimeFormat(undefined, {
      hour: '2-digit',
      minute: '2-digit',
    }).format(new Date(timestamp))
  } catch {
    return ''
  }
}

function ProgressFeed({
  updates,
  isStreaming,
}: {
  updates: ProgressUpdate[]
  isStreaming: boolean
}) {
  const [isOpen, setIsOpen] = useState(false)
  if (!updates.length) return null

  const latestId = updates[updates.length - 1]?.id
  const shimmerStyle = {
    backgroundImage:
      'linear-gradient(90deg, rgba(200,200,200,0.1) 0%, rgba(230,230,230,0.8) 50%, rgba(200,200,200,0.1) 100%)',
    backgroundSize: '260% 100%',
  } as const

  return (
    <div className="space-y-1">
      <button
        onClick={() => setIsOpen((v) => !v)}
        className="inline-flex items-center gap-2 text-xs font-medium text-gray-700 hover:text-gray-900"
      >
        <ChevronDown
          className={cn(
            'h-3 w-3 transition-transform',
            isOpen ? 'rotate-0' : '-rotate-90'
          )}
        />
        <span className="relative inline-flex items-center gap-1 text-gray-800">
          <span>Live activity</span>
          <motion.span
            className="pointer-events-none absolute inset-0 rounded-sm opacity-80 mix-blend-screen"
            style={shimmerStyle}
            animate={
              isStreaming
                ? { backgroundPosition: ['-40% 50%', '140% 50%', '-40% 50%'] }
                : { backgroundPosition: '-40% 50%' }
            }
            transition={
              isStreaming
                ? { duration: 1.6, repeat: Infinity, ease: 'linear', repeatDelay: 0.25 }
                : undefined
            }
          />
        </span>
      </button>

      {!isOpen && latestId && (
        <div className="pl-5 text-[12px] text-gray-800 relative inline-flex">
          <span>{updates[updates.length - 1]?.text}</span>
          <motion.span
            className="pointer-events-none absolute inset-0 opacity-80 mix-blend-screen"
            style={shimmerStyle}
            animate={
              isStreaming
                ? { backgroundPosition: ['-40% 50%', '140% 50%', '-40% 50%'] }
                : { backgroundPosition: '-40% 50%' }
            }
            transition={
              isStreaming
                ? { duration: 1.6, repeat: Infinity, ease: 'linear', repeatDelay: 0.25 }
                : undefined
            }
          />
        </div>
      )}

      <AnimatePresence initial={false}>
        {isOpen && (
          <motion.ul
            initial={{ height: 0, opacity: 0 }}
            animate={{ height: 'auto', opacity: 1 }}
            exit={{ height: 0, opacity: 0 }}
            transition={{ duration: 0.2 }}
            className="space-y-2 pl-5 text-xs text-gray-700"
          >
            {updates.map((update, idx) => {
              const isLatest = update.id === latestId
              return (
                <motion.li
                  key={update.id}
                  initial={{ opacity: 0, x: -6 }}
                  animate={{ opacity: 1, x: 0 }}
                  exit={{ opacity: 0, x: -6 }}
                  transition={{ duration: 0.12, delay: idx * 0.02 }}
                  className="leading-relaxed"
                >
                  <span className="text-[11px] text-gray-500 mr-2">
                    {formatTimeLabel(update.timestamp)}
                  </span>
                  <span className="relative inline-flex text-gray-800">
                    {update.text}
                    {isLatest && (
                      <motion.span
                        className="pointer-events-none absolute inset-0 opacity-80 mix-blend-screen"
                        style={shimmerStyle}
                        animate={{ backgroundPosition: ['-40% 50%', '140% 50%', '-40% 50%'] }}
                        transition={{ duration: 1.6, repeat: Infinity, ease: 'linear', repeatDelay: 0.25 }}
                      />
                    )}
                  </span>
                </motion.li>
              )
            })}
          </motion.ul>
        )}
      </AnimatePresence>
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
  const [hasLoadedState, setHasLoadedState] = useState(false)
  const [isScrolling, setIsScrolling] = useState(false)
  const progressMilestonesRef = useRef<Set<number>>(new Set())
  const progressUpdateCountRef = useRef(0)
  const progressLastPctRef = useRef(-1)
  const progressFinalEmittedRef = useRef(false)
  const progressThresholdIndexRef = useRef(0)

  // Agent state using reducer
  const [agentState, dispatchAgentEvent] = useReducer(
    agentStateReducer,
    initialAgentState
  )

  const messagesEndRef = useRef<HTMLDivElement>(null)
  const inputRef = useRef<HTMLTextAreaElement>(null)
  const scrollTimeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null)

  const appendProgressUpdate = useCallback(
    (
      text: string,
      variant: ProgressVariant = 'info',
      opts?: { dedupeWindowMs?: number }
    ) => {
      const timestamp = new Date().toISOString()
      setMessages((prev) => {
        if (!prev.length) return prev
        const updated = [...prev]
        const lastMsg = updated[updated.length - 1]
        if (lastMsg?.role === 'assistant') {
          const existing = lastMsg.progressUpdates || []

          const dedupeWindow = opts?.dedupeWindowMs ?? 4000
          const last = existing[existing.length - 1]
          const withinDedupeWindow =
            last &&
            Math.abs(new Date(timestamp).getTime() - new Date(last.timestamp).getTime()) <
              dedupeWindow
          if (last && last.text === text && last.variant === variant && withinDedupeWindow) {
            return updated // skip noisy duplicate
          }

          const next = [
            ...existing,
            {
              id: `${timestamp}-${existing.length}`,
              text,
              timestamp,
              variant,
            },
          ]
          lastMsg.progressUpdates = next // append only; don't rewrite prior rows
        }
        return updated
      })
    },
    []
  )

  // Load existing generation state on mount
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
          
          // If there's an in-progress generation, show status message
          if (state.is_generating) {
            setMessages([{
              id: 'restored',
              role: 'system',
              content: `Resuming generation... ${state.file_count || 0} files saved so far.`,
              status: 'complete',
              createdAt: new Date().toISOString(),
            }])
            
            // Restore tasks from plan if available
            if (state.generation_plan?.steps) {
              const restoredMessage: LocalMessage = {
                id: 'restored-assistant',
                role: 'assistant',
                content: 'Generation in progress (resumed from saved state)',
                status: 'streaming',
                createdAt: new Date().toISOString(),
                tasks: state.generation_plan.steps.map((s, i) => ({
                  id: s.id,
                  type: s.type as PlanStep['type'],
                  title: s.title,
                  description: s.description,
                  status: i < (state.current_step || 0) ? 'complete' as const : s.status as PlanStep['status'],
                })),
                files: restoredFiles,
              }
              setMessages([restoredMessage])
            }
          } else if (state.is_complete) {
            // Show completed generation
            const completedMessage: LocalMessage = {
              id: 'restored-complete',
              role: 'assistant',
              content: `✅ Generated ${state.file_count || 0} files`,
              status: 'complete',
              createdAt: state.created_at || new Date().toISOString(),
              files: restoredFiles,
              tasks: state.generation_plan?.steps?.map(s => ({
                id: s.id,
                type: s.type as PlanStep['type'],
                title: s.title,
                description: s.description,
                status: 'complete' as const,
              })),
            }
            setMessages([completedMessage])
          }
        }
      } catch (error) {
        console.error('Failed to load existing state:', error)
      } finally {
        setHasLoadedState(true)
      }
    }
    
    loadExistingState()
  }, [appId, hasLoadedState, onFilesGenerated, onVersionCreated])

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
          break

        case 'agent_start':
          setThinkingStartTime(Date.now())
          setAccumulatedFiles([]) // Reset files for new generation
          progressMilestonesRef.current = new Set()
          progressUpdateCountRef.current = 0
          progressLastPctRef.current = -1
          progressFinalEmittedRef.current = false
          progressThresholdIndexRef.current = 0
          appendProgressUpdate('Starting agentic build: setting up session and tools', 'phase')
          break

        case 'phase_change':
          const phase = (data.phase as AgentState['phase']) || 'working'
          const phaseDescriptions: Record<AgentState['phase'] | 'working', string> = {
            researching: 'reviewing code and context',
            planning: 'drafting an execution plan',
            executing: 'writing and wiring code',
            validating: 'running checks and validations',
            complete: 'wrapping up',
            error: 'encountered an issue',
            idle: 'waiting',
            working: 'working',
          }
          appendProgressUpdate(
            `Now ${phase}: ${phaseDescriptions[phase] || 'working'}`,
            'phase',
            { dedupeWindowMs: 4000 }
          )
          break

        case 'thinking':
          if (data.content) {
            appendProgressUpdate(`Thinking: ${data.content}`, 'thinking', {
              dedupeWindowMs: 6000,
            })
          }
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
          appendProgressUpdate(
            `Plan ready (${(data.steps || data.plan?.steps || []).length || 0} steps): moving to execution`,
            'step',
            { dedupeWindowMs: 4000 }
          )
          setThinkingStartTime(null)
          break

        case 'step_start':
        case 'step_started': {
          const stepTitle =
            data.step?.title ||
            data.title ||
            data.stepTitle ||
            (typeof data.stepIndex === 'number' ? `Step ${data.stepIndex + 1}` : 'Step started')
          const stepHint = data.step?.description || ''
          appendProgressUpdate(
            `Starting ${stepTitle}${stepHint ? `: ${stepHint}` : ''}`,
            'step',
            { dedupeWindowMs: 2000 }
          )
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
          appendProgressUpdate('Step completed: output captured', 'step', { dedupeWindowMs: 2000 })
          break

        case 'step_progress': {
          const pct = typeof data.progress === 'number' ? data.progress : null
          if (pct === null) break
          // Emit only when crossing ordered thresholds (real progress), with slight display jitter
          const thresholds = [6, 21, 53, 72, 100]
          const idx = progressThresholdIndexRef.current
          if (idx >= thresholds.length) break

          const target = thresholds[idx]
          const reached = pct >= target

          // Always allow final even if prior ones were skipped
          if (!reached && !(idx === thresholds.length - 1 && pct >= 99.5)) break

          // Display the backend percent with a light ±3 jitter for a less "clean" feel
          const jitteredPct = Math.max(
            0,
            Math.min(100, pct + (Math.floor(Math.random() * 7) - 5)) // ±5 jitter
          )
          const progressText = data.message || 'Working...'
          const pctLabel = ` (${jitteredPct}%)`
          appendProgressUpdate(`Progress${pctLabel}: ${progressText}`, 'step', {
            dedupeWindowMs: 4000,
          })

          if (idx === thresholds.length - 1) {
            progressFinalEmittedRef.current = true
          } else {
            progressThresholdIndexRef.current = idx + 1
          }
          progressUpdateCountRef.current += 1
          progressLastPctRef.current = pct
          break
        }

        case 'step_complete': {
          const duration = data.duration ? ` in ${(data.duration / 1000).toFixed(1)}s` : ''
          const stepTitle =
            data.step?.title ||
            data.title ||
            data.stepTitle ||
            (typeof data.stepIndex === 'number' ? `Step ${data.stepIndex + 1}` : 'Step')
          appendProgressUpdate(`Finished ${stepTitle}${duration}`, 'step')
          break
        }

        case 'file_generated': {
          // Add file to accumulated files and notify parent
          const newFile = data.file
          appendProgressUpdate(`Generated ${newFile?.path || 'a file'}`, 'file', {
            dedupeWindowMs: 1500,
          })
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

        case 'code_chunk': {
          // Too chatty for users; skip
          break
        }

        case 'validation_result': {
          const label = data.passed
            ? 'Validation passed'
            : `Validation failed: ${(data.errors || []).slice(0, 2).join('; ')}`
          appendProgressUpdate(
            data.passed ? `${label}: ready for preview` : `${label}: needs attention`,
            data.passed ? 'preview' : 'error'
          )
          break
        }

        case 'preview_ready': {
          onVersionCreated(data.versionId || data.version_id, data.versionNumber || data.version_number)
          appendProgressUpdate(
            `Preview ready${data.versionNumber ? ` (v${data.versionNumber})` : ''}`,
            'preview',
            { dedupeWindowMs: 4000 }
          )
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
          const duration = data.duration ? ` in ${(data.duration / 1000).toFixed(1)}s` : ''
          appendProgressUpdate(`Run finished${duration}`, 'preview', { dedupeWindowMs: 4000 })
          break
        }

        case 'version_created':
          onVersionCreated(data.versionId, data.versionNumber)
          appendProgressUpdate(`Saved version ${data.versionNumber}`, 'preview', { dedupeWindowMs: 4000 })
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
          appendProgressUpdate('Generation complete: summarizing results', 'preview')
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
          appendProgressUpdate(`Error: ${data.message}`, 'error')
          setThinkingStartTime(null)
          break
      }
    },
    [
      appendProgressUpdate,
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
        progressUpdates: [],
        createdAt: new Date().toISOString(),
      },
    ])

    const controller = startAgenticGeneration(appId, message, {
      sessionId: sessionId || undefined,
      model: selectedModel,
      onEvent: handleAgentEvent,
      onComplete: () => {
        setIsLoading(false)
        setAbortController(null)
      },
      onError: (error) => {
        console.error('Agent error:', error)
        setIsLoading(false)
        setAbortController(null)
        setThinkingStartTime(null)
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

  const handleCancel = () => {
    abortController?.abort()
    setIsLoading(false)
    setAbortController(null)
    setThinkingStartTime(null)

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

  return (
    <div className={cn('flex flex-col h-full bg-white', className)}>
      {/* Messages */}
      <div
        className={cn(
          'flex-1 overflow-y-auto scrollbar-auto-hide',
          isScrolling && 'scrolling'
        )}
        onScroll={handleMessagesScroll}
      >
        <div className="max-w-3xl mx-auto px-4 py-6 space-y-6">
          {messages.length === 0 && (
            <div className="flex flex-col items-center justify-center py-16 text-center">
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
            </div>
          )}

          {messages.map((message) => (
            <div key={message.id} className="space-y-3">
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

                  {/* Live progress feed */}
                  {message.progressUpdates && message.progressUpdates.length > 0 && (
                    <ProgressFeed updates={message.progressUpdates} isStreaming={isLoading} />
                  )}

                  {/* Summary text content */}
                  {message.content && message.status === 'complete' && (
                    <div className="text-sm text-gray-700 leading-relaxed">
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
            </div>
          ))}

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
