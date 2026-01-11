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
import Editor, { DiffEditor } from '@monaco-editor/react'
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
  fetchLatestJob,
  reconnectToJob,
  upsertFileChange,
  cancelGeneration,
  cancelJob,
  startFixErrors,
} from '../../services/agentService'
import type { BundlerError } from '../../hooks/useSandpackValidation'
import type { AgentEvent, PlanStep, FileChange, AgentState } from '../../types/agent'
import { initialAgentState } from '../../types/agent'
import { useQueryClient } from '@tanstack/react-query'
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
  onGeneratingVersionChange?: (versionId: string | null) => void
  /** Bundler errors detected from Sandpack - triggers auto-fix */
  bundlerErrors?: BundlerError[]
  /** Current version ID for error fixing */
  currentVersionId?: string
  /** Callback when errors are successfully fixed (clears bundlerErrors in parent) */
  onErrorsCleared?: () => void
  /** Initial prompt to auto-submit (e.g., from landing page) - VISIBLE in input */
  initialPrompt?: string
  /** Callback when initial prompt has been consumed */
  onInitialPromptConsumed?: () => void
  /** Hidden prompt to auto-submit (from templates) - NEVER displayed to user */
  hiddenPrompt?: string
  /** Callback when hidden prompt has been consumed */
  onHiddenPromptConsumed?: () => void
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

// Helper to get Monaco language from file path
function getMonacoLanguage(path: string): string {
  const ext = path.split('.').pop() || ''
  const langMap: Record<string, string> = {
    tsx: 'typescript',
    ts: 'typescript',
    jsx: 'javascript',
    js: 'javascript',
    css: 'css',
    json: 'json',
    md: 'markdown',
    html: 'html',
    py: 'python',
    yaml: 'yaml',
    yml: 'yaml',
  }
  return langMap[ext] || 'plaintext'
}

// File preview with expandable content
function FilePreview({ 
  file, 
  index, 
  previousFile 
}: { 
  file: FileChange
  index: number
  previousFile?: FileChange 
}) {
  const [isExpanded, setIsExpanded] = useState(false)
  const [viewMode, setViewMode] = useState<'editor' | 'diff'>('editor')
  const fileName = file.path.split('/').pop() || file.path
  const lineCount = file.content?.split('\n').length || 0
  const added = file.addedLines ?? (file.action === 'create' ? lineCount : undefined)
  const removed = file.removedLines ?? 0
  const displayAdded = added ?? lineCount
  const canShowDiff = !!previousFile && !!previousFile.content && previousFile.path === file.path

  const handleToggle = () => {
    setIsExpanded(!isExpanded)
  }

  const monacoLanguage = getMonacoLanguage(file.path)
  const editorOptions = {
    readOnly: true,
    minimap: { enabled: false },
    fontSize: 11,
    fontFamily: 'JetBrains Mono, Menlo, Monaco, Consolas, monospace',
    lineNumbers: 'on' as const,
    scrollBeyondLastLine: false,
    wordWrap: 'on' as const,
    tabSize: 2,
    padding: { top: 8, bottom: 8 },
    renderLineHighlight: 'none' as const,
    automaticLayout: true,
  }

  return (
    <motion.div
      initial={{ opacity: 0, y: 8 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ delay: index * 0.05 }}
      className="border border-gray-200 rounded-lg overflow-hidden bg-white"
    >
      <button
        onClick={handleToggle}
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
            {/* Diff toggle button */}
            {canShowDiff && (
              <div className="px-2.5 py-1.5 border-b border-gray-200 bg-gray-50 flex items-center justify-end">
                <button
                  onClick={() => setViewMode(viewMode === 'diff' ? 'editor' : 'diff')}
                  className="text-[10px] px-2 py-1 rounded text-gray-600 hover:text-gray-900 hover:bg-gray-200 transition-colors font-medium"
                >
                  {viewMode === 'diff' ? 'Show Code' : 'Show Diff'}
                </button>
              </div>
            )}

            {viewMode === 'diff' && canShowDiff && previousFile ? (
              <div className="h-[300px] border-t border-gray-200">
                <DiffEditor
                  original={previousFile.content || ''}
                  modified={file.content || ''}
                  language={monacoLanguage}
                  theme="light"
                  options={{
                    ...editorOptions,
                    renderSideBySide: false,
                  }}
                  height="300px"
                />
              </div>
            ) : (
              <div className="h-[300px] border-t border-gray-200">
                <Editor
                  value={file.content}
                  language={monacoLanguage}
                  theme="light"
                  options={editorOptions}
                  height="300px"
                />
              </div>
            )}
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

// Maximum fix attempts for bundler errors
const MAX_FIX_ATTEMPTS = 2

// Main component
export function AgenticChatPanel({
  appId,
  sessionId,
  onSessionChange,
  onVersionCreated,
  onFilesGenerated,
  onGeneratingVersionChange,
  bundlerErrors,
  currentVersionId,
  onErrorsCleared,
  initialPrompt,
  onInitialPromptConsumed,
  hiddenPrompt,
  onHiddenPromptConsumed,
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
  // Track active job ID for reconnection support
  const [activeJobId, setActiveJobId] = useState<string | null>(null)
  const [hasCheckedForActiveJob, setHasCheckedForActiveJob] = useState(false)
  
  // Error fixing state
  const [isFixingErrors, setIsFixingErrors] = useState(false)
  const [fixAttempt, setFixAttempt] = useState(0)
  const fixControllerRef = useRef<AbortController | null>(null)
  const lastFixedErrorSignatureRef = useRef<string>('')
  
  // Notify parent component of generating version changes
  useEffect(() => {
    onGeneratingVersionChange?.(generatingVersionId)
  }, [generatingVersionId, onGeneratingVersionChange])
  
  const [hasLoadedState, setHasLoadedState] = useState(false)
  const [isScrolling, setIsScrolling] = useState(false)
  const progressMilestonesRef = useRef<Set<number>>(new Set())
  const progressUpdateCountRef = useRef(0)
  const progressLastPctRef = useRef(-1)
  const progressFinalEmittedRef = useRef(false)
  const progressThresholdIndexRef = useRef(0)
  const { data: chatSessions, refetch: refetchChatSessions, isLoading: sessionsLoading } = useChatSessions(appId)
  const { mutateAsync: createChatSession, isPending: creatingSession } = useCreateChatSession()
  const { data: sessionMessages, isFetching: messagesFetching } = useChatMessages(sessionId)
  const queryClient = useQueryClient()
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
  
  // Track whether we've auto-submitted the initial prompt
  const hasAutoSubmittedInitialPrompt = useRef(false)
  
  // Track whether we've auto-submitted the hidden prompt (from templates)
  const hasAutoSubmittedHiddenPrompt = useRef(false)
  
  // Track if currently executing a hidden prompt (to suppress user message display)
  const [isHiddenPromptExecution, setIsHiddenPromptExecution] = useState(false)
  
  // Store hidden prompts to filter from display - persists to localStorage per app
  const HIDDEN_PROMPTS_KEY = `hidden_prompts_${appId}`
  
  // Get hidden prompts for this app from localStorage
  const getHiddenPrompts = useCallback((): string[] => {
    try {
      const stored = localStorage.getItem(HIDDEN_PROMPTS_KEY)
      return stored ? JSON.parse(stored) : []
    } catch {
      return []
    }
  }, [HIDDEN_PROMPTS_KEY])
  
  // Add a hidden prompt to the list
  const addHiddenPrompt = useCallback((content: string) => {
    const existing = getHiddenPrompts()
    if (!existing.includes(content)) {
      existing.push(content)
      localStorage.setItem(HIDDEN_PROMPTS_KEY, JSON.stringify(existing))
    }
  }, [HIDDEN_PROMPTS_KEY, getHiddenPrompts])
  
  // Check if a message content is a hidden prompt
  const isHiddenPromptContent = useCallback((content: string): boolean => {
    const hiddenPrompts = getHiddenPrompts()
    return hiddenPrompts.includes(content)
  }, [getHiddenPrompts])
  
  // Store the current hidden prompt from prop
  useEffect(() => {
    if (hiddenPrompt) {
      addHiddenPrompt(hiddenPrompt)
    }
  }, [hiddenPrompt, addHiddenPrompt])

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
    
    // Filter out hidden prompt messages from session history
    const restored = sessionMessages
      .map(mapApiMessageToLocal)
      .filter(msg => {
        // Skip user messages that match any hidden prompt content
        if (msg.role === 'user' && isHiddenPromptContent(msg.content)) {
          return false
        }
        return true
      })
    
    setMessages(restored)
    setHydratedSessionId(sessionId)
  }, [hydratedSessionId, mapApiMessageToLocal, messages, sessionId, sessionMessages, isHiddenPromptContent])

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

  // Create error signature for deduplication
  const createErrorSignature = useCallback((errors: BundlerError[]) => {
    return errors
      .map(e => `${e.file || ''}:${e.line || 0}:${e.message}`)
      .sort()
      .join('|')
  }, [])

  // Handle bundler errors - trigger fix and show in Live Activity
  useEffect(() => {
    if (
      !bundlerErrors ||
      bundlerErrors.length === 0 ||
      !currentVersionId ||
      isFixingErrors ||
      isLoading || // Don't fix while generating
      fixAttempt >= MAX_FIX_ATTEMPTS
    ) {
      return
    }

    // Check if these are new errors (avoid fixing the same errors again)
    const signature = createErrorSignature(bundlerErrors)
    if (signature === lastFixedErrorSignatureRef.current) {
      return
    }

    // Start fixing errors
    lastFixedErrorSignatureRef.current = signature
    setIsFixingErrors(true)
    setFixAttempt(prev => prev + 1)

    console.log(`[AgenticChatPanel] Starting fix attempt ${fixAttempt + 1}/${MAX_FIX_ATTEMPTS}`, bundlerErrors)

    // Add an assistant message for the fix activity
    const fixMessageId = `fix-${Date.now()}`
    setMessages(prev => [
      ...prev,
      {
        id: fixMessageId,
        role: 'assistant',
        content: '',
        status: 'streaming',
        isAgentic: true,
        progressUpdates: [{
          id: `${Date.now()}-0`,
          text: `Fixing ${bundlerErrors.length} compilation error${bundlerErrors.length !== 1 ? 's' : ''} (attempt ${fixAttempt + 1}/${MAX_FIX_ATTEMPTS})`,
          timestamp: new Date().toISOString(),
          variant: 'step',
        }],
        createdAt: new Date().toISOString(),
      },
    ])

    const { controller } = startFixErrors(currentVersionId, bundlerErrors, {
      model: selectedModel,
      attempt: fixAttempt + 1,
      onEvent: (event) => {
        const data = event.data as EventData

        // Collect fixed files from events
        if (event.type === 'file_generated') {
          const fileData = data.file
          if (fileData) {
            const fixedFile: FileChange = {
              path: fileData.path,
              action: (fileData.action as FileChange['action']) || 'modify',
              language: (fileData.language as FileChange['language']) || 'tsx',
              content: fileData.content,
            }
            
            // Update accumulated files and notify parent
            setAccumulatedFiles(prev => {
              const updated = upsertFileChange(prev, fixedFile)
              onFilesGenerated?.(updated)
              return updated
            })

            // Add progress update
            setMessages(prev => {
              const lastIdx = prev.length - 1
              if (lastIdx < 0) return prev
              const lastMsg = prev[lastIdx]
              if (lastMsg?.id !== fixMessageId) return prev

              const existing = lastMsg.progressUpdates || []
              return [
                ...prev.slice(0, lastIdx),
                {
                  ...lastMsg,
                  progressUpdates: [
                    ...existing,
                    {
                      id: `${Date.now()}-${existing.length}`,
                      text: `Fixed ${fileData.path.split('/').pop() || fileData.path}`,
                      timestamp: new Date().toISOString(),
                      variant: 'file' as ProgressVariant,
                    },
                  ],
                },
              ]
            })
          }
        }

        // Handle other fix events for progress updates
        if (event.type === 'fix_progress' && data.message) {
          setMessages(prev => {
            const lastIdx = prev.length - 1
            if (lastIdx < 0) return prev
            const lastMsg = prev[lastIdx]
            if (lastMsg?.id !== fixMessageId) return prev

            const existing = lastMsg.progressUpdates || []
            return [
              ...prev.slice(0, lastIdx),
              {
                ...lastMsg,
                progressUpdates: [
                  ...existing,
                  {
                    id: `${Date.now()}-${existing.length}`,
                    text: data.message,
                    timestamp: new Date().toISOString(),
                    variant: 'step' as ProgressVariant,
                  },
                ],
              },
            ]
          })
        }
      },
      onComplete: () => {
        console.log('[AgenticChatPanel] Fix completed')
        setIsFixingErrors(false)
        
        // Notify parent that errors are cleared so bundlerErrors state is reset
        onErrorsCleared?.()

        // Update the fix message to complete
        setMessages(prev => {
          const lastIdx = prev.length - 1
          if (lastIdx < 0) return prev
          const lastMsg = prev[lastIdx]
          if (lastMsg?.id !== fixMessageId) return prev

          const existing = lastMsg.progressUpdates || []
          return [
            ...prev.slice(0, lastIdx),
            {
              ...lastMsg,
              status: 'complete' as const,
              content: 'Fixed compilation errors',
              progressUpdates: [
                ...existing,
                {
                  id: `${Date.now()}-${existing.length}`,
                  text: 'Errors fixed successfully',
                  timestamp: new Date().toISOString(),
                  variant: 'preview' as ProgressVariant,
                },
              ],
            },
          ]
        })
      },
      onError: (error) => {
        console.error('[AgenticChatPanel] Fix error:', error)
        setIsFixingErrors(false)

        // Update the fix message to show error
        setMessages(prev => {
          const lastIdx = prev.length - 1
          if (lastIdx < 0) return prev
          const lastMsg = prev[lastIdx]
          if (lastMsg?.id !== fixMessageId) return prev

          const existing = lastMsg.progressUpdates || []
          return [
            ...prev.slice(0, lastIdx),
            {
              ...lastMsg,
              status: 'error' as const,
              error: error.message,
              progressUpdates: [
                ...existing,
                {
                  id: `${Date.now()}-${existing.length}`,
                  text: `Fix failed: ${error.message}`,
                  timestamp: new Date().toISOString(),
                  variant: 'error' as ProgressVariant,
                },
              ],
            },
          ]
        })
      },
    })

    fixControllerRef.current = controller
  }, [bundlerErrors, currentVersionId, isFixingErrors, isLoading, fixAttempt, selectedModel, createErrorSignature, onFilesGenerated, onErrorsCleared])

  // Reset fix attempt counter when errors are cleared (compilation succeeds)
  useEffect(() => {
    if (!bundlerErrors || bundlerErrors.length === 0) {
      setFixAttempt(0)
      lastFixedErrorSignatureRef.current = ''
    }
  }, [bundlerErrors])

  // Cleanup fix controller on unmount
  useEffect(() => {
    return () => {
      fixControllerRef.current?.abort()
    }
  }, [])

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

        case 'user_message':
          // Skip user_message events during hidden prompt execution (template builds)
          // The prompt is intentionally never shown to the user
          if (isHiddenPromptExecution) {
            break
          }
          
          // Also skip if the message content matches any hidden prompt
          // This handles cases where the message comes from backend history or reconnection
          if (isHiddenPromptContent(data.content)) {
            break
          }
          
          // Add user message to the chat (handles reconnection/replay scenarios)
          setMessages(prev => {
            // Check if we already have this user message (avoid duplicates during live generation)
            const existingIdx = prev.findIndex(m => m.id === data.id)
            if (existingIdx >= 0) return prev
            
            // Also check if the last user message has the same content (for live generation)
            const lastUserMsg = [...prev].reverse().find(m => m.role === 'user')
            if (lastUserMsg?.content === data.content) return prev
            
            // Find the insertion point (before any assistant messages for this generation)
            const lastIdx = prev.length - 1
            const lastMsg = prev[lastIdx]
            
            const userMessage: LocalMessage = {
              id: data.id || `user-${Date.now()}`,
              role: 'user',
              content: data.content,
              status: 'complete',
              createdAt: event.timestamp,
            }
            
            // If last message is a streaming assistant, insert user message before it
            if (lastMsg?.role === 'assistant' && lastMsg?.status === 'streaming') {
              return [...prev.slice(0, lastIdx), userMessage, lastMsg]
            }
            
            return [...prev, userMessage]
          })
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

        case 'phase_change': {
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
        }

        case 'thinking':
          if (data.content) {
            appendProgressUpdate(`Thinking. ${data.content}`, 'thinking', {
              dedupeWindowMs: 6000,
            })
          }
          break

        case 'plan_created':
          // Update the last assistant message with tasks
          setMessages((prev) => {
            const lastIdx = prev.length - 1
            if (lastIdx < 0) return prev
            const lastMsg = prev[lastIdx]
            if (lastMsg?.role !== 'assistant') return prev
            
            return [
              ...prev.slice(0, lastIdx),
              {
                ...lastMsg,
                tasks: data.steps || [],
                exploredInfo: {
                  directories: data.exploredDirectories || 0,
                  files: data.exploredFiles || 0,
                  searches: data.searches || 0,
                },
                thinkingDuration: thinkingStartTime 
                  ? Math.floor((Date.now() - thinkingStartTime) / 1000)
                  : lastMsg.thinkingDuration,
              },
            ]
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
          // Update task status in the message - properly clone
          setMessages((prev) => {
            const lastIdx = prev.length - 1
            if (lastIdx < 0) return prev
            const lastMsg = prev[lastIdx]
            if (!lastMsg?.tasks) return prev
            
            const updatedTasks = lastMsg.tasks.map((t) =>
              t.id === data.stepId || t.title === stepTitle
                ? { ...t, status: 'in_progress' as const }
                : t
            )
            return [
              ...prev.slice(0, lastIdx),
              { ...lastMsg, tasks: updatedTasks },
            ]
          })
          break
        }

        case 'step_completed':
          // Update task status in the message - properly clone
          setMessages((prev) => {
            const lastIdx = prev.length - 1
            if (lastIdx < 0) return prev
            const lastMsg = prev[lastIdx]
            if (!lastMsg?.tasks) return prev
            
            const updatedTasks = lastMsg.tasks.map(t =>
              t.id === data.stepId
                ? { ...t, status: 'complete' as const, duration: data.duration }
                : t
            )
            return [
              ...prev.slice(0, lastIdx),
              { ...lastMsg, tasks: updatedTasks },
            ]
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
          // Also update the message - properly clone to trigger re-render
          setMessages((prev) => {
            const lastIdx = prev.length - 1
            if (lastIdx < 0) return prev
            const lastMsg = prev[lastIdx]
            if (lastMsg?.role !== 'assistant') return prev

            const updatedFiles = upsertFileChange(lastMsg.files || [], newFile)
            return [
              ...prev.slice(0, lastIdx),
              { ...lastMsg, files: updatedFiles },
            ]
          })
          break
        }

        case 'table_created': {
          // A new data table was created by the agent
          setMessages((prev) => {
            const lastIdx = prev.length - 1
            if (lastIdx < 0) return prev
            const lastMsg = prev[lastIdx]
            if (lastMsg?.role !== 'assistant') return prev
            
            const tableInfo = `Created table ${data.name} with ${data.columns} columns`
            const newContent = lastMsg.content 
              ? `${lastMsg.content}\n\n ${tableInfo}`
              : `${tableInfo}`
            return [
              ...prev.slice(0, lastIdx),
              { ...lastMsg, content: newContent },
            ]
          })
          break
        }

        case 'table_updated': {
          // An existing data table was updated by the agent
          setMessages((prev) => {
            const lastIdx = prev.length - 1
            if (lastIdx < 0) return prev
            const lastMsg = prev[lastIdx]
            if (lastMsg?.role !== 'assistant') return prev
            
            const changes = data.changes || {}
            const changeInfo = []
            if (changes.added?.length) changeInfo.push(`added: ${changes.added.join(', ')}`)
            if (changes.removed?.length) changeInfo.push(`removed: ${changes.removed.join(', ')}`)
            if (changes.modified?.length) changeInfo.push(`modified: ${changes.modified.join(', ')}`)
            
            const tableInfo = changeInfo.length > 0
              ? `Updated table "${data.name}" - ${changeInfo.join('; ')}`
              : `Updated table "${data.name}"`
            const newContent = lastMsg.content 
              ? `${lastMsg.content}\n\n📊 ${tableInfo}`
              : `📊 ${tableInfo}`
            return [
              ...prev.slice(0, lastIdx),
              { ...lastMsg, content: newContent },
            ]
          })
          break
        }

        case 'code_chunk': {
          // Too chatty for users; skip
          break
        }

        case 'validation_result': {
          const errors = data.errors || []
          const errorSummary = errors.slice(0, 2).map((e: string | { message?: string }) => 
            typeof e === 'string' ? e : e.message || 'Unknown error'
          ).join('; ')
          const label = data.passed
            ? 'Validation passed'
            : `Validation failed: ${errorSummary}`
          appendProgressUpdate(
            data.passed ? `${label}: ready for preview` : `${label}: needs attention`,
            data.passed ? 'preview' : 'error'
          )
          break
        }

        // Error fixing events
        case 'fix_started': {
          const attempt = data.attempt || 1
          const maxAttempts = data.max_attempts || 2
          const errorCount = data.error_count || 0
          const errorType = data.error_type || 'compilation'
          appendProgressUpdate(
            `Fixing ${errorCount} ${errorType} error${errorCount !== 1 ? 's' : ''} (attempt ${attempt}/${maxAttempts})`,
            'step'
          )
          break
        }

        case 'fix_progress': {
          if (data.message) {
            appendProgressUpdate(`Fix in progress: ${data.message}`, 'step', { dedupeWindowMs: 60000 })
          }
          break
        }

        case 'fix_file_updated': {
          const filePath = data.file_path || 'a file'
          const fileName = filePath.split('/').pop() || filePath
          appendProgressUpdate(`Fixed ${fileName}`, 'file')
          break
        }

        case 'fix_complete': {
          const attempts = data.fix_attempts || 1
          appendProgressUpdate(
            `Errors fixed successfully${attempts > 1 ? ` (${attempts} attempts)` : ''}`,
            'preview'
          )
          break
        }

        case 'fix_failed': {
          const remaining = data.remaining_errors || 0
          const attempts = data.fix_attempts || 2
          appendProgressUpdate(
            `Could not fix all errors after ${attempts} attempts (${remaining} remaining)`,
            'error'
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

        case 'version_draft':
          // Track the generating version ID so we can cancel it if needed
          if (data.version_id) {
            setGeneratingVersionId(data.version_id)
          }
          break

        case 'version_created':
          onVersionCreated(data.versionId || data.version_id, data.versionNumber || data.version_number)
          appendProgressUpdate(`Saved version ${data.versionNumber}`, 'preview', { dedupeWindowMs: 4000 })
          // Clear the generating version ID since generation is complete
          setGeneratingVersionId(null)
          break

        case 'agent_complete':
          setMessages((prev) => {
            const lastIdx = prev.length - 1
            if (lastIdx < 0) return prev
            const lastMsg = prev[lastIdx]
            if (lastMsg?.role !== 'assistant') return prev
            
            return [
              ...prev.slice(0, lastIdx),
              {
                ...lastMsg,
                status: 'complete' as const,
                content: data.summary || 'App generated successfully!',
              },
            ]
          })
          appendProgressUpdate('Generation complete: summarizing results', 'preview')
          setThinkingStartTime(null)
          break

        case 'agent_error':
          setMessages((prev) => {
            const lastIdx = prev.length - 1
            if (lastIdx < 0) return prev
            const lastMsg = prev[lastIdx]
            if (lastMsg?.role !== 'assistant') return prev
            
            return [
              ...prev.slice(0, lastIdx),
              {
                ...lastMsg,
                status: 'error' as const,
                error: data.message,
              },
            ]
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
      isHiddenPromptExecution,
      isHiddenPromptContent,
    ]
  )

  // Check for active jobs on mount and reconnect if one is in progress
  // This enables seamless continuation after page refresh
  useEffect(() => {
    if (hasCheckedForActiveJob) return
    // Wait for session messages to be loaded first to preserve chat history
    if (messagesFetching || sessionMessages === undefined) return
    
    const checkForActiveJob = async () => {
      try {
        const jobInfo = await fetchLatestJob(appId)
        
        if (jobInfo?.has_active_job && jobInfo.job_id) {
          console.log('[AgenticChatPanel] Found active job, reconnecting:', jobInfo.job_id)
          
          // Set loading state
          setIsLoading(true)
          setActiveJobId(jobInfo.job_id)
          setThinkingStartTime(Date.now())
          
          if (jobInfo.version_id) {
            setGeneratingVersionId(jobInfo.version_id)
            onGeneratingVersionChange?.(jobInfo.version_id)
          }
          
          // Add placeholder message for the reconnected generation
          // Use sessionMessages as the base to preserve chat history
          const historicalMessages = sessionMessages 
            ? sessionMessages.map(mapApiMessageToLocal)
            : []
          
          // Mark as hydrated since we're using session messages
          if (sessionId) {
            setHydratedSessionId(sessionId)
          }
          
          setMessages(prev => {
            // If we haven't hydrated yet, start with historical messages
            const baseMessages = prev.length === 0 ? historicalMessages : prev
            
            // Check if we already have a streaming message
            const lastMsg = baseMessages[baseMessages.length - 1]
            if (lastMsg?.status === 'streaming') return baseMessages
            
            return [
              ...baseMessages,
              {
                id: `reconnect-${Date.now()}`,
                role: 'assistant' as const,
                content: '',
                status: 'streaming' as const,
                isAgentic: true,
                tasks: [],
                files: [],
                progressUpdates: [{
                  id: 'reconnect',
                  text: 'Reconnecting to generation in progress...',
                  timestamp: new Date().toISOString(),
                  variant: 'info' as const,
                }],
                createdAt: new Date().toISOString(),
              },
            ]
          })
          
          // Reconnect to the job stream
          const { controller } = reconnectToJob(jobInfo.job_id, {
            onEvent: handleAgentEvent,
            onComplete: () => {
              setIsLoading(false)
              setAbortController(null)
              setActiveJobId(null)
              setGeneratingVersionId(null)
              // Invalidate and refetch messages to ensure we have the latest
              if (sessionId) {
                queryClient.invalidateQueries({ queryKey: ['chat-messages', sessionId] })
              }
            },
            onError: (error) => {
              console.error('[AgenticChatPanel] Reconnection error:', error)
              setIsLoading(false)
              setAbortController(null)
              setActiveJobId(null)
              setGeneratingVersionId(null)
            },
          })
          
          setAbortController(controller)
        }
      } catch (error) {
        console.error('[AgenticChatPanel] Failed to check for active jobs:', error)
      } finally {
        setHasCheckedForActiveJob(true)
      }
    }
    
    checkForActiveJob()
  }, [appId, hasCheckedForActiveJob, handleAgentEvent, onGeneratingVersionChange, messagesFetching, sessionMessages, mapApiMessageToLocal, sessionId])

  const handleSubmit = async (overrideMessage?: string) => {
    const messageToSend = overrideMessage ?? input.trim()
    if (!messageToSend || isLoading) return

    const message = messageToSend
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
        progressUpdates: [],
        createdAt: new Date().toISOString(),
      },
    ])

    const { controller, getJobId } = startAgenticGeneration(appId, message, {
      sessionId: sessionId || undefined,
      model: selectedModel,
      onEvent: (event) => {
        // Track job ID from version_draft event (job creates version)
        const data = event.data as Record<string, unknown>
        if (event.type === 'version_draft' && data.version_id) {
          // The job ID is tracked internally, but we use version_id for cancellation
          const jobId = getJobId()
          if (jobId) {
            setActiveJobId(jobId)
          }
        }
        handleAgentEvent(event)
      },
      onComplete: () => {
        setIsLoading(false)
        setAbortController(null)
        setGeneratingVersionId(null)
        setActiveJobId(null)
        // Invalidate and refetch messages to ensure we have the latest
        if (sessionId) {
          queryClient.invalidateQueries({ queryKey: ['chat-messages', sessionId] })
        }
      },
      onError: (error) => {
        console.error('Agent error:', error)
        setIsLoading(false)
        setAbortController(null)
        setThinkingStartTime(null)
        setGeneratingVersionId(null)
        setActiveJobId(null)
        setMessages((prev) => {
          const lastIdx = prev.length - 1
          if (lastIdx < 0) return prev
          const lastMsg = prev[lastIdx]
          if (lastMsg?.role !== 'assistant') return prev
          
          return [
            ...prev.slice(0, lastIdx),
            {
              ...lastMsg,
              status: 'error' as const,
              error: error.message,
            },
          ]
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

    // Cancel the job on the backend (this also cancels the version)
    if (activeJobId) {
      try {
        await cancelJob(activeJobId)
      } catch (error) {
        console.warn('[AgenticChatPanel] Failed to cancel job:', error)
      }
      setActiveJobId(null)
    }
    
    // Fallback: cancel the version directly if no job ID
    if (generatingVersionId && !activeJobId) {
      try {
        await cancelGeneration(generatingVersionId)
      } catch (error) {
        console.warn('[AgenticChatPanel] Failed to cancel generating version:', error)
      }
    }
    setGeneratingVersionId(null)

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
    // Consider messages hydrated once the fetch completes and we've either:
    // 1. Already hydrated this session
    // 2. No messages to hydrate (empty session)
    // 3. sessionMessages are available (hydration effect will run)
    if (!messagesFetching && sessionMessages !== undefined) {
      // Give the hydration effect a chance to run, then mark as hydrated
      // This handles the case where sessionMessages has data
      const timer = setTimeout(() => {
        setHasHydratedMessages(true)
      }, 50)
      return () => clearTimeout(timer)
    }
  }, [sessionId, sessionMessages, messagesFetching])

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

  // Pre-fill input with initial prompt from landing page when session is ready
  useEffect(() => {
    // Only process once per component instance
    if (hasAutoSubmittedInitialPrompt.current) return
    // Need a session ID and the initial prompt
    if (!sessionId || !initialPrompt) return
    // Wait for session to be hydrated and ready
    if (waitingOnSessionData || waitingOnMessages) return
    
    // Mark as processed immediately to prevent double-processing
    hasAutoSubmittedInitialPrompt.current = true
    
    // Pre-fill the input with the prompt (don't auto-submit)
    setInput(initialPrompt)
    
    // Focus the input after a small delay
    const focusTimeout = setTimeout(() => {
      inputRef.current?.focus()
      // Notify parent that we've consumed the initial prompt
      onInitialPromptConsumed?.()
    }, 200)
    
    return () => clearTimeout(focusTimeout)
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [sessionId, initialPrompt, waitingOnSessionData, waitingOnMessages])

  // Auto-submit hidden prompt from templates (NEVER display to user)
  useEffect(() => {
    // Only process once per component instance
    if (hasAutoSubmittedHiddenPrompt.current) return
    // Need a session ID and the hidden prompt
    if (!sessionId || !hiddenPrompt) return
    // Wait for session to be hydrated and ready
    if (waitingOnSessionData || waitingOnMessages) return
    // Don't submit if already loading
    if (isLoading) return
    
    // Mark as processed immediately to prevent double-processing
    hasAutoSubmittedHiddenPrompt.current = true
    
    console.log('[AgenticChatPanel] Auto-submitting hidden prompt from template')
    
    // Set flag to indicate this is a hidden prompt execution
    setIsHiddenPromptExecution(true)
    
    // Store the hidden prompt content to filter it from display (persists in localStorage)
    addHiddenPrompt(hiddenPrompt)
    
    // Auto-submit the hidden prompt after a brief delay for smooth UX
    const submitTimeout = setTimeout(() => {
      handleHiddenPromptSubmit(hiddenPrompt)
      onHiddenPromptConsumed?.()
    }, 300)
    
    return () => clearTimeout(submitTimeout)
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [sessionId, hiddenPrompt, waitingOnSessionData, waitingOnMessages, isLoading])
  
  // Handle hidden prompt submission - same as regular but doesn't add user message
  const handleHiddenPromptSubmit = async (promptText: string) => {
    if (!promptText || isLoading) return

    setIsLoading(true)
    setThinkingStartTime(Date.now())

    // Reset agent state
    dispatchAgentEvent({
      type: 'agent_start',
      timestamp: new Date().toISOString(),
      data: { sessionId: '', messageId: '', goal: promptText },
    })

    // Reset the generating version ID for a new generation
    setGeneratingVersionId(null)

    // DON'T add user message - this is a hidden prompt execution
    // Instead, add a special "building from template" assistant message
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
        progressUpdates: [{
          id: `${Date.now()}-0`,
          text: 'Building your app from template...',
          timestamp: new Date().toISOString(),
          variant: 'phase' as ProgressVariant,
        }],
        createdAt: new Date().toISOString(),
      },
    ])

    const { controller, getJobId } = startAgenticGeneration(appId, promptText, {
      sessionId: sessionId || undefined,
      model: selectedModel,
      isHiddenPrompt: true, // Flag to tell backend not to echo the prompt
      onEvent: (event) => {
        // Track job ID from version_draft event (job creates version)
        const data = event.data as Record<string, unknown>
        if (event.type === 'version_draft' && data.version_id) {
          const jobId = getJobId()
          if (jobId) {
            setActiveJobId(jobId)
          }
        }
        
        // For hidden prompts, skip user_message events
        if (event.type === 'user_message' && isHiddenPromptExecution) {
          return
        }
        
        handleAgentEvent(event)
      },
      onComplete: () => {
        setIsLoading(false)
        setAbortController(null)
        setGeneratingVersionId(null)
        setActiveJobId(null)
        setIsHiddenPromptExecution(false)
        // Invalidate and refetch messages to ensure we have the latest
        if (sessionId) {
          queryClient.invalidateQueries({ queryKey: ['chat-messages', sessionId] })
        }
      },
      onError: (error) => {
        console.error('Agent error:', error)
        setIsLoading(false)
        setAbortController(null)
        setThinkingStartTime(null)
        setGeneratingVersionId(null)
        setActiveJobId(null)
        setIsHiddenPromptExecution(false)
        setMessages((prev) => {
          const lastIdx = prev.length - 1
          if (lastIdx < 0) return prev
          const lastMsg = prev[lastIdx]
          if (lastMsg?.role !== 'assistant') return prev
          
          return [
            ...prev.slice(0, lastIdx),
            {
              ...lastMsg,
              status: 'error' as const,
              error: error.message,
            },
          ]
        })
      },
    })

    setAbortController(controller)
  }

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
            {messages.map((message, messageIndex) => {
              // Only show thinking indicator on the very last message
              const isLastMessage = messageIndex === messages.length - 1
              
              return (
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
                    <div className="max-w-[80%] bg-gray-100 text-gray-700 rounded-xl px-4 py-3">
                      <p className="text-sm whitespace-pre-wrap break-words">{message.content}</p>
                    </div>
                  </div>
                ) : (
                  // Assistant message with inline agentic content
                  <div className="space-y-3">
                    {/* Thinking indicator - only show on the last message while actively streaming */}
                    {isLastMessage && message.status === 'streaming' && thinkingStartTime && !message.tasks?.length && (
                      <ThinkingIndicator startTime={thinkingStartTime} />
                    )}

                    {/* Thought duration badge (after thinking completes) */}
                    {message.thinkingDuration && message.thinkingDuration > 0 && (
                      <div className="flex items-center gap-1 text-[10px] text-gray-400">
                        <Lightbulb className="h-2.5 w-2.5 flex-shrink-0" />
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
                        {message.files.map((file, i) => {
                          // Use previousContent stored in the file, or construct previousFile object
                          const previousFile = file.previousContent 
                            ? {
                                path: file.path,
                                content: file.previousContent,
                                action: 'modify' as const,
                                language: file.language,
                              }
                            : undefined
                          
                          return (
                            <FilePreview 
                              key={file.path} 
                              file={file} 
                              index={i}
                              previousFile={previousFile}
                            />
                          )
                        })}
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
            )})}
          </AnimatePresence>

          <div ref={messagesEndRef} />
        </div>
      </div>

      {/* Input area */}
      <div className="border-t border-gray-200 bg-gray-50">
        <div className="max-w-3xl mx-auto px-4 py-5">
          <div className="rounded-xl border border-gray-200 bg-white shadow-sm">
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
                           focus:outline-none focus:ring-0 disabled:opacity-60 whitespace-pre-wrap break-words"
                style={{ minHeight: '48px', maxHeight: '140px', wordWrap: 'break-word', overflowWrap: 'break-word' }}
                onInput={(e) => {
                  const target = e.target as HTMLTextAreaElement
                  target.style.height = 'auto'
                  target.style.height = Math.min(target.scrollHeight, 140) + 'px'
                }}
              />
            </div>

            <div className="mt-3 flex items-center justify-between gap-3 px-4 py-3 bg-white rounded-b-xl">
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
