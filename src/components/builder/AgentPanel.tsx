/**
 * Agent Panel Component
 * 
 * Displays the agentic coding workflow with:
 * - Phase indicator (Research → Plan → Execute → Validate)
 * - Plan visualization with step progress
 * - Collapsible thinking/reasoning section
 * - Real-time file generation feedback
 * 
 * Enterprise light theme design.
 */
import { useState, useRef, useEffect, useMemo } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import {
  Search,
  ListTodo,
  Hammer,
  CheckCircle2,
  XCircle,
  ChevronDown,
  ChevronRight,
  Brain,
  Sparkles,
  FileCode2,
  Loader2,
  CircleDot,
  Circle,
  Clock,
  Lightbulb,
  Eye,
} from 'lucide-react'
import { cn } from '../../lib/utils'
import type {
  AgentState,
  AgentPhase,
  PlanStep,
  ThinkingEntry,
  FileChange,
} from '../../types/agent'
import { calculateProgress, getPhaseInfo } from '../../services/agentService'

interface AgentPanelProps {
  state: AgentState
  className?: string
}

// Phase progress bar
function PhaseIndicator({ phase, progress }: { phase: AgentPhase; progress: number }) {
  const phases: { key: AgentPhase; icon: typeof Search; label: string }[] = [
    { key: 'researching', icon: Search, label: 'Research' },
    { key: 'planning', icon: ListTodo, label: 'Plan' },
    { key: 'executing', icon: Hammer, label: 'Build' },
    { key: 'validating', icon: CheckCircle2, label: 'Validate' },
  ]

  const currentIndex = phases.findIndex((p) => p.key === phase)

  return (
    <div className="px-4 py-3 border-b border-gray-200 bg-gray-50">
      {/* Progress bar */}
      <div className="relative h-1 bg-gray-200 rounded-full mb-4 overflow-hidden">
        <motion.div
          className="absolute inset-y-0 left-0 bg-gray-900 rounded-full"
          initial={{ width: 0 }}
          animate={{ width: `${progress}%` }}
          transition={{ duration: 0.3, ease: 'easeOut' }}
        />
      </div>

      {/* Phase steps */}
      <div className="flex items-center justify-between">
        {phases.map((p, i) => {
          const Icon = p.icon
          const isActive = p.key === phase
          const isComplete = i < currentIndex || phase === 'complete'
          const isPending = i > currentIndex

          return (
            <div key={p.key} className="flex items-center">
              <div className="flex flex-col items-center">
                <div
                  className={cn(
                    'w-8 h-8 rounded-full flex items-center justify-center transition-all duration-300',
                    isComplete && 'bg-green-50 text-green-700 border border-green-200',
                    isActive && 'bg-gray-900 text-white',
                    isPending && 'bg-gray-100 text-gray-400 border border-gray-200'
                  )}
                >
                  {isComplete ? (
                    <CheckCircle2 className="h-4 w-4" />
                  ) : isActive ? (
                    <motion.div
                      animate={{ rotate: 360 }}
                      transition={{ duration: 2, repeat: Infinity, ease: 'linear' }}
                    >
                      <Icon className="h-4 w-4" />
                    </motion.div>
                  ) : (
                    <Icon className="h-4 w-4" />
                  )}
                </div>
                <span
                  className={cn(
                    'text-[10px] font-medium mt-1 transition-colors',
                    isComplete && 'text-green-700',
                    isActive && 'text-gray-900',
                    isPending && 'text-gray-400'
                  )}
                >
                  {p.label}
                </span>
              </div>

              {i < phases.length - 1 && (
                <div
                  className={cn(
                    'h-px w-8 mx-2 transition-colors',
                    i < currentIndex ? 'bg-green-400' : 'bg-gray-200'
                  )}
                />
              )}
            </div>
          )
        })}
      </div>
    </div>
  )
}

// Thinking section with collapsible entries
function ThinkingSection({ thinking }: { thinking: ThinkingEntry[] }) {
  const [isExpanded, setIsExpanded] = useState(true)
  const containerRef = useRef<HTMLDivElement>(null)

  // Auto-scroll to latest
  useEffect(() => {
    if (containerRef.current && isExpanded) {
      containerRef.current.scrollTop = containerRef.current.scrollHeight
    }
  }, [thinking, isExpanded])

  if (thinking.length === 0) return null

  const latestThinking = thinking[thinking.length - 1]

  return (
    <div className="border-b border-gray-200">
      <button
        onClick={() => setIsExpanded(!isExpanded)}
        className="w-full flex items-center gap-2 px-4 py-2.5 bg-yellow-50 hover:bg-yellow-100 
                 transition-colors text-left border-b border-yellow-200"
      >
        <Brain className="h-4 w-4 text-yellow-700" />
        <span className="text-xs font-medium text-yellow-900 flex-1">
          Agent Thinking
        </span>
        <span className="text-[10px] text-yellow-700 bg-yellow-100 border border-yellow-200 px-1.5 py-0.5 rounded-full">
          {thinking.length}
        </span>
        {isExpanded ? (
          <ChevronDown className="h-3.5 w-3.5 text-yellow-700" />
        ) : (
          <ChevronRight className="h-3.5 w-3.5 text-yellow-700" />
        )}
      </button>

      <AnimatePresence>
        {isExpanded && (
          <motion.div
            initial={{ height: 0, opacity: 0 }}
            animate={{ height: 'auto', opacity: 1 }}
            exit={{ height: 0, opacity: 0 }}
            transition={{ duration: 0.2 }}
            className="overflow-hidden"
          >
            <div
              ref={containerRef}
              className="max-h-32 overflow-y-auto px-4 py-2 space-y-2 bg-yellow-50/50"
            >
              {thinking.map((entry) => (
                <div key={entry.id} className="flex gap-2 text-xs">
                  <span className="text-yellow-600 flex-shrink-0">
                    {entry.type === 'observation' && '👁️'}
                    {entry.type === 'reasoning' && '💭'}
                    {entry.type === 'decision' && '⚡'}
                    {entry.type === 'reflection' && '🔄'}
                  </span>
                  <span className="text-gray-700">{entry.content}</span>
                </div>
              ))}
            </div>
          </motion.div>
        )}
      </AnimatePresence>

      {!isExpanded && latestThinking && (
        <div className="px-4 py-1.5 bg-yellow-50/50 text-xs text-gray-600 truncate">
          💭 {latestThinking.content}
        </div>
      )}
    </div>
  )
}

// Plan steps list
function PlanSteps({
  steps,
  currentStepIndex,
}: {
  steps: PlanStep[]
  currentStepIndex: number
}) {
  return (
    <div className="px-4 py-3 space-y-2">
      <div className="flex items-center gap-2 mb-3">
        <ListTodo className="h-4 w-4 text-gray-500" />
        <span className="text-xs font-semibold text-gray-900">Execution Plan</span>
        <span className="text-[10px] text-gray-600">
          {steps.filter((s) => s.status === 'complete').length}/{steps.length} complete
        </span>
      </div>

      <div className="space-y-1">
        {steps.map((step, i) => {
          const isActive = i === currentStepIndex
          const isComplete = step.status === 'complete'
          const isError = step.status === 'error'
          const isPending = step.status === 'pending'

          return (
            <motion.div
              key={step.id}
              initial={{ opacity: 0, x: -10 }}
              animate={{ opacity: 1, x: 0 }}
              transition={{ delay: i * 0.05 }}
              className={cn(
                'flex items-start gap-3 p-2.5 rounded-lg transition-colors border',
                isActive && 'bg-gray-100 border-gray-300',
                isComplete && 'bg-green-50 border-green-200',
                isError && 'bg-red-50 border-red-200',
                isPending && 'opacity-60 border-transparent'
              )}
            >
              <div className="flex-shrink-0 mt-0.5">
                {isComplete && (
                  <CheckCircle2 className="h-4 w-4 text-green-700" />
                )}
                {isError && <XCircle className="h-4 w-4 text-red-700" />}
                {isActive && (
                  <motion.div
                    animate={{ scale: [1, 1.2, 1] }}
                    transition={{ duration: 1, repeat: Infinity }}
                  >
                    <CircleDot className="h-4 w-4 text-gray-900" />
                  </motion.div>
                )}
                {isPending && <Circle className="h-4 w-4 text-gray-300" />}
              </div>

              <div className="flex-1 min-w-0">
                <div className="flex items-center gap-2">
                  <span
                    className={cn(
                      'text-xs font-medium',
                      isActive && 'text-gray-900',
                      isComplete && 'text-green-900',
                      isError && 'text-red-900',
                      isPending && 'text-gray-500'
                    )}
                  >
                    {step.title}
                  </span>
                  {step.duration && (
                    <span className="text-[10px] text-gray-500 flex items-center gap-0.5">
                      <Clock className="h-2.5 w-2.5" />
                      {(step.duration / 1000).toFixed(1)}s
                    </span>
                  )}
                </div>
                <p className="text-[10px] text-gray-600 mt-0.5 line-clamp-1">
                  {step.description}
                </p>
              </div>

              <span
                className={cn(
                  'text-[9px] px-1.5 py-0.5 rounded-full font-medium flex-shrink-0 border',
                  step.type === 'design' && 'bg-blue-50 text-blue-700 border-blue-200',
                  step.type === 'code' && 'bg-gray-100 text-gray-700 border-gray-200',
                  step.type === 'component' && 'bg-blue-50 text-blue-700 border-blue-200',
                  step.type === 'styling' && 'bg-gray-100 text-gray-700 border-gray-200',
                  step.type === 'integration' && 'bg-yellow-50 text-yellow-700 border-yellow-200',
                  step.type === 'validation' && 'bg-green-50 text-green-700 border-green-200'
                )}
              >
                {step.type}
              </span>
            </motion.div>
          )
        })}
      </div>
    </div>
  )
}

// Generated files list
function GeneratedFiles({ files }: { files: FileChange[] }) {
  const [isExpanded, setIsExpanded] = useState(true)

  if (files.length === 0) return null

  return (
    <div className="border-t border-gray-200">
      <button
        onClick={() => setIsExpanded(!isExpanded)}
        className="w-full flex items-center gap-2 px-4 py-2.5 bg-blue-50 hover:bg-blue-100 
                 transition-colors text-left border-b border-blue-200"
      >
        <FileCode2 className="h-4 w-4 text-blue-700" />
        <span className="text-xs font-medium text-blue-900 flex-1">
          Generated Files
        </span>
        <span className="text-[10px] text-blue-700 bg-blue-100 border border-blue-200 px-1.5 py-0.5 rounded-full">
          {files.length}
        </span>
        {isExpanded ? (
          <ChevronDown className="h-3.5 w-3.5 text-blue-700" />
        ) : (
          <ChevronRight className="h-3.5 w-3.5 text-blue-700" />
        )}
      </button>

      <AnimatePresence>
        {isExpanded && (
          <motion.div
            initial={{ height: 0, opacity: 0 }}
            animate={{ height: 'auto', opacity: 1 }}
            exit={{ height: 0, opacity: 0 }}
            transition={{ duration: 0.2 }}
            className="overflow-hidden"
          >
            <div className="px-4 py-2 space-y-1 bg-blue-50/50">
              {files.map((file, i) => (
                <motion.div
                  key={file.path}
                  initial={{ opacity: 0, x: -5 }}
                  animate={{ opacity: 1, x: 0 }}
                  transition={{ delay: i * 0.05 }}
                  className="flex items-center gap-2 text-xs"
                >
                  <span className="text-green-600 font-medium">
                    {file.action === 'create' && '+'}
                    {file.action === 'modify' && '~'}
                    {file.action === 'delete' && '-'}
                  </span>
                  <span className="text-gray-800 font-mono text-[10px] truncate">
                    {file.path}
                  </span>
                  <span className="text-[9px] text-gray-500 ml-auto">
                    {file.language}
                  </span>
                </motion.div>
              ))}
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  )
}

// Main Agent Panel
export function AgentPanel({ state, className = '' }: AgentPanelProps) {
  const progress = useMemo(() => calculateProgress(state), [state])
  const phaseInfo = useMemo(() => getPhaseInfo(state.phase), [state.phase])

  const isIdle = state.phase === 'idle'
  const isComplete = state.phase === 'complete'
  const isError = state.phase === 'error'
  const isWorking = !isIdle && !isComplete && !isError

  return (
    <div className={cn('flex flex-col h-full bg-white', className)}>
      {/* Header with current status */}
      <div className="flex items-center justify-between px-4 py-3 border-b border-gray-200">
        <div className="flex items-center gap-2">
          <div
            className={cn(
              'h-8 w-8 rounded-lg flex items-center justify-center',
              isWorking && 'bg-gray-900',
              isComplete && 'bg-green-50 border border-green-200',
              isError && 'bg-red-50 border border-red-200',
              isIdle && 'bg-gray-100 border border-gray-200'
            )}
          >
            {isWorking ? (
              <motion.div
                animate={{ rotate: 360 }}
                transition={{ duration: 3, repeat: Infinity, ease: 'linear' }}
              >
                <Sparkles className="h-4 w-4 text-white" />
              </motion.div>
            ) : isComplete ? (
              <CheckCircle2 className="h-4 w-4 text-green-700" />
            ) : isError ? (
              <XCircle className="h-4 w-4 text-red-700" />
            ) : (
              <Lightbulb className="h-4 w-4 text-gray-500" />
            )}
          </div>
          <div>
            <h3 className="text-sm font-semibold text-gray-900">
              {isIdle && 'Ready to Build'}
              {isWorking && 'Building Your App'}
              {isComplete && 'App Generated'}
              {isError && 'Generation Failed'}
            </h3>
            <p className="text-[10px] text-gray-600">
              {isWorking && phaseInfo.label}
              {isComplete && state.startedAt && state.completedAt && `Completed in ${((new Date(state.completedAt).getTime() - new Date(state.startedAt).getTime()) / 1000).toFixed(1)}s`}
              {isError && state.error}
            </p>
          </div>
        </div>

        {isWorking && (
          <div className="flex items-center gap-2">
            <Loader2 className="h-4 w-4 text-gray-700 animate-spin" />
            <span className="text-xs font-medium text-gray-700">
              {progress.toFixed(0)}%
            </span>
          </div>
        )}

        {state.previewReady && (
          <button
            className="flex items-center gap-1.5 px-3 py-1.5 text-xs font-medium
                     bg-gray-900 text-white rounded-md hover:bg-gray-800 transition-colors"
          >
            <Eye className="h-3.5 w-3.5" />
            Preview
          </button>
        )}
      </div>

      {/* Phase indicator (only when working) */}
      {isWorking && <PhaseIndicator phase={state.phase} progress={progress} />}

      {/* Scrollable content */}
      <div className="flex-1 overflow-y-auto">
        {/* Thinking section */}
        <ThinkingSection thinking={state.thinking} />

        {/* Plan steps */}
        {state.plan && (
          <PlanSteps steps={state.plan.steps} currentStepIndex={state.currentStepIndex} />
        )}

        {/* Generated files */}
        <GeneratedFiles files={state.generatedFiles} />

        {/* Idle state */}
        {isIdle && (
          <div className="flex flex-col items-center justify-center h-full text-center p-6">
            <div className="w-16 h-16 rounded-xl bg-gray-100 border border-gray-200
                          flex items-center justify-center mb-4">
              <Sparkles className="h-8 w-8 text-gray-500" />
            </div>
            <h4 className="text-sm font-medium text-gray-900 mb-1">
              Agentic Code Generation
            </h4>
            <p className="text-xs text-gray-600 max-w-[200px]">
              Describe your app and watch as the AI researches, plans, and builds it step by step.
            </p>
          </div>
        )}

        {/* Error state details */}
        {isError && state.error && (
          <div className="p-4">
            <div className="p-3 bg-red-50 border border-red-200 rounded-lg">
              <p className="text-xs text-red-700">{state.error}</p>
            </div>
          </div>
        )}

        {/* Complete state summary */}
        {isComplete && (
          <div className="p-4">
            <div className="p-4 bg-green-50 border border-green-200 rounded-lg">
              <div className="flex items-center gap-2 mb-2">
                <CheckCircle2 className="h-5 w-5 text-green-700" />
                <span className="text-sm font-medium text-green-900">
                  Generation Complete
                </span>
              </div>
              <p className="text-xs text-green-700">
                Generated {state.generatedFiles.length} files.
                Your app is ready to preview!
              </p>
            </div>
          </div>
        )}
      </div>
    </div>
  )
}
