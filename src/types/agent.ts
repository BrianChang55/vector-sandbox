/**
 * Agent Types for Agentic Coding Workflow
 * 
 * Defines the structure for the Research → Plan → Execute → Validate loop
 * that powers the AI app builder with visible progress and thinking.
 */

// Agent Phases
export type AgentPhase = 
  | 'idle'
  | 'researching'
  | 'planning'
  | 'executing'
  | 'validating'
  | 'complete'
  | 'error'

// Step status
export type StepStatus = 'pending' | 'in_progress' | 'complete' | 'error' | 'skipped'

// Plan step definition
export interface PlanStep {
  id: string
  type: 'research' | 'design' | 'code' | 'component' | 'styling' | 'integration' | 'validation'
  title: string
  description: string
  status: StepStatus
  duration?: number
  output?: string
  error?: string
  fileChanges?: FileChange[]
}

// File change during execution
export interface FileChange {
  path: string
  action: 'create' | 'modify' | 'delete'
  language: 'tsx' | 'ts' | 'css' | 'json' | 'html'
  content?: string
  diff?: string
  addedLines?: number
  removedLines?: number
  previousContent?: string // Preserve previous content for diff view
}

// Agent plan structure
export interface AgentPlan {
  id: string
  goal: string
  reasoning: string
  steps: PlanStep[]
  estimatedDuration: number
  createdAt: string
}

// Thinking entry for visibility
export interface ThinkingEntry {
  id: string
  timestamp: string
  phase: AgentPhase
  content: string
  type: 'observation' | 'reasoning' | 'decision' | 'reflection'
}

// Agent state for UI
export interface AgentState {
  phase: AgentPhase
  plan: AgentPlan | null
  currentStepIndex: number
  thinking: ThinkingEntry[]
  generatedFiles: FileChange[]
  previewReady: boolean
  error: string | null
  startedAt: string | null
  completedAt: string | null
}

// SSE Event types from backend
export type AgentEventType =
  | 'agent_start'
  | 'session_created'
  | 'user_message'
  | 'phase_change'
  | 'thinking'
  | 'plan_created'
  | 'step_start'
  | 'step_started'
  | 'step_progress'
  | 'step_complete'
  | 'step_completed'
  | 'file_generated'
  | 'table_created'
  | 'table_updated'
  | 'code_chunk'
  | 'validation_result'
  | 'preview_ready'
  | 'version_draft'
  | 'version_created'
  | 'agent_complete'
  | 'agent_error'
  | 'done'
  | 'connected'
  // Error fix events
  | 'fix_started'
  | 'fix_progress'
  | 'fix_file_updated'
  | 'fix_complete'
  | 'fix_failed'
  // Verification events
  | 'verification_started'
  | 'verification_passed'
  | 'verification_failed'
  | 'verification_skipped'
  | 'verification_retry_started'

// SSE Event structure
export interface AgentEvent {
  type: AgentEventType
  timestamp: string
  data: AgentEventData
}

// Event data types
export type AgentEventData =
  | AgentStartData
  | SessionCreatedData
  | UserMessageData
  | PhaseChangeData
  | ThinkingData
  | PlanCreatedData
  | StepStartData
  | StepStartedData
  | StepProgressData
  | StepCompleteData
  | StepCompletedData
  | FileGeneratedData
  | TableCreatedData
  | TableUpdatedData
  | CodeChunkData
  | ValidationResultData
  | PreviewReadyData
  | VersionDraftData
  | VersionCreatedData
  | AgentCompleteData
  | AgentErrorData
  | DoneData
  // Fix event data types
  | FixStartedData
  | FixProgressData
  | FixFileUpdatedData
  | FixCompleteData
  | FixFailedData
  // Verification event data types
  | VerificationStartedData
  | VerificationPassedData
  | VerificationFailedData
  | VerificationSkippedData
  | VerificationRetryStartedData

export interface DoneData {
  success: boolean
  filesGenerated?: number
  duration?: number
  validated?: boolean
  fix_attempts?: number
}

export interface AgentStartData {
  sessionId: string
  messageId: string
  goal: string
}

export interface SessionCreatedData {
  sessionId: string
}

export interface UserMessageData {
  id: string
  content: string
}

export interface PhaseChangeData {
  phase: AgentPhase
  message: string
}

export interface ThinkingData {
  content: string
  type: ThinkingEntry['type']
}

export interface PlanCreatedData {
  plan?: AgentPlan
  steps?: PlanStep[]
  exploredDirectories?: number
  exploredFiles?: number
  searches?: number
}

export interface StepStartData {
  stepIndex: number
  step: PlanStep
}

export interface StepProgressData {
  stepIndex: number
  progress: number
  message: string
}

export interface StepCompleteData {
  stepIndex: number
  stepId?: string
  status: StepStatus
  duration: number
  output?: string
}

export interface StepStartedData {
  stepId: string
  stepIndex: number
}

export interface StepCompletedData {
  stepId: string
  stepIndex: number
  duration?: number
}

export interface FileGeneratedData {
  file: FileChange
}

export interface TableCreatedData {
  slug: string
  name: string
  columns: number
}

export interface TableUpdatedData {
  slug: string
  name: string
  changes: {
    added?: string[]
    removed?: string[]
    modified?: string[]
  }
}

export interface CodeChunkData {
  filePath: string
  chunk: string
  accumulated: number
}

// Compilation error structure from backend
export interface CompilationError {
  file: string
  line: number
  column: number
  message: string
  code?: string
}

export interface ValidationResultData {
  passed: boolean
  errors: (string | CompilationError)[]
  warnings: string[]
  fix_attempts?: number
}

// Fix event data interfaces
export interface FixStartedData {
  attempt: number
  max_attempts: number
  error_count: number
  error_type?: 'typescript' | 'bundler'
}

export interface FixProgressData {
  attempt: number
  message: string
}

export interface FixFileUpdatedData {
  file_path: string
  attempt: number
}

export interface FixCompleteData {
  success: boolean
  fix_attempts: number
}

export interface FixFailedData {
  remaining_errors: number
  fix_attempts: number
}

// Verification event data interfaces
export interface VerificationStartedData {
  file_path: string
  verifier: string
}

export interface VerificationPassedData {
  file_path: string
  verifier: string
}

export interface VerificationFailedData {
  file_path: string
  verifier: string
  error_message: string
  is_blocking: boolean
}

export interface VerificationSkippedData {
  file_path: string
  reason: string
}

export interface VerificationRetryStartedData {
  file_path: string
  attempt_number: number
  max_attempts: number
  previous_error: string
}

// Verification state for tracking per-file status
export interface VerificationState {
  isVerifying: boolean
  totalFiles: number
  verifiedFiles: number
  currentFile: string | null
  results: VerificationFileResult[]
}

export interface VerificationFileResult {
  file_path: string
  status: 'verifying' | 'passed' | 'failed' | 'skipped' | 'retrying'
  verifier?: string
  error_message?: string
  attempt_number?: number
}

export interface PreviewReadyData {
  versionId: string
  versionNumber: number
  previewUrl: string
  files: FileChange[]
}

export interface AgentCompleteData {
  versionId?: string
  versionNumber?: number
  duration?: number
  filesGenerated?: number
  summary?: string
  validated?: boolean
  fix_attempts?: number
}

export interface VersionDraftData {
  version_id: string
  version_number: number
  status: string
}

export interface VersionCreatedData {
  versionId: string
  versionNumber: number
}

export interface AgentErrorData {
  message: string
  phase: AgentPhase
  stepIndex?: number
  recoverable: boolean
}

// Initial agent state
export const initialAgentState: AgentState = {
  phase: 'idle',
  plan: null,
  currentStepIndex: -1,
  thinking: [],
  generatedFiles: [],
  previewReady: false,
  error: null,
  startedAt: null,
  completedAt: null,
}

// Helper to create a new plan step
export function createPlanStep(
  type: PlanStep['type'],
  title: string,
  description: string
): PlanStep {
  return {
    id: crypto.randomUUID(),
    type,
    title,
    description,
    status: 'pending',
  }
}

// Helper to update step status
export function updateStepStatus(
  plan: AgentPlan,
  stepIndex: number,
  status: StepStatus,
  output?: string
): AgentPlan {
  return {
    ...plan,
    steps: plan.steps.map((step, i) =>
      i === stepIndex ? { ...step, status, output } : step
    ),
  }
}

