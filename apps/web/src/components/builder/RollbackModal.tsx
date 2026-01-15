/**
 * Rollback Modal Component
 * 
 * Enterprise-grade rollback confirmation modal with full preview of changes,
 * schema compatibility warnings, and migration hints.
 */
import { useState, useEffect, useCallback } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import {
  X,
  RotateCcw,
  AlertTriangle,
  Info,
  Check,
  ChevronDown,
  ChevronRight,
  FileCode,
  Database,
  GitCompare,
  Loader2,
  Sparkles,
  Code2,
  History,
} from 'lucide-react'
import { formatDistanceToNow } from 'date-fns'
import type { AppVersion } from '../../types/models'
import { api } from '../../services/api'
import { cn } from '../../lib/utils'

interface RollbackModalProps {
  isOpen: boolean
  onClose: () => void
  targetVersion: AppVersion
  currentVersion: AppVersion | null
  onConfirm: (options: { include_schema: boolean }) => Promise<void>
}

interface RollbackPreview {
  diff: {
    tables: {
      added: Array<{ name: string; slug: string; schema?: any }>
      removed: Array<{ name: string; slug: string }>
      modified: Array<{ slug: string; from: any; to: any }>
    }
    resources: {
      added: any[]
      removed: any[]
      modified: any[]
    }
    files: {
      added: string[]
      removed: string[]
      modified: string[]
      from_count: number
      to_count: number
    }
    versions: {
      from: { id: string; version_number: number; source: string; created_at: string }
      to: { id: string; version_number: number; source: string; created_at: string }
    }
  } | null
  warnings: Array<{
    type: string
    severity: 'info' | 'warning' | 'error'
    message: string
    details?: any
  }>
  can_revert: boolean
  target_version: {
    id: string
    version_number: number
    source: string
    created_at: string
    intent_message?: string | null
  }
  schema_compatibility?: {
    is_compatible: boolean
    risk_level: 'safe' | 'warning' | 'danger'
    tables: Array<{
      slug: string
      name: string
      change: 'added' | 'removed' | 'modified'
      risk: string
      compatibility?: any
    }>
    summary: {
      tables_removed: number
      tables_added: number
      tables_modified: number
      total_warnings: number
      total_errors: number
    }
  }
  migration_hints?: Array<{
    column: string
    type: string
    severity: string
    suggestion: string
    action: string
  }>
}

const sourceIcons: Record<string, typeof Sparkles> = {
  ai: Sparkles,
  ai_edit: Sparkles,
  code: Code2,
  code_edit: Code2,
  rollback: History,
}

export function RollbackModal({
  isOpen,
  onClose,
  targetVersion,
  currentVersion,
  onConfirm,
}: RollbackModalProps) {
  const [preview, setPreview] = useState<RollbackPreview | null>(null)
  const [loading, setLoading] = useState(true)
  const [confirming, setConfirming] = useState(false)
  const [includeSchema, setIncludeSchema] = useState(true)
  const [expandedSections, setExpandedSections] = useState<Set<string>>(new Set(['files', 'tables']))
  const [error, setError] = useState<string | null>(null)

  // Fetch preview on mount
  const fetchPreview = useCallback(async () => {
    setLoading(true)
    setError(null)
    try {
      const response = await api.post<RollbackPreview>(
        `/versions/${targetVersion.id}/rollback/`,
        { dry_run: true, include_schema: includeSchema }
      )
      setPreview(response.data)
    } catch (err: any) {
      setError(err.response?.data?.error || 'Failed to load rollback preview')
    } finally {
      setLoading(false)
    }
  }, [targetVersion.id, includeSchema])

  useEffect(() => {
    if (isOpen) {
      fetchPreview()
    }
  }, [isOpen, fetchPreview])

  // Toggle section expansion
  const toggleSection = (section: string) => {
    setExpandedSections(prev => {
      const next = new Set(prev)
      if (next.has(section)) {
        next.delete(section)
      } else {
        next.add(section)
      }
      return next
    })
  }

  // Handle confirm
  const handleConfirm = async () => {
    setConfirming(true)
    try {
      await onConfirm({ include_schema: includeSchema })
      onClose()
    } catch (err: any) {
      setError(err.response?.data?.error || 'Rollback failed')
    } finally {
      setConfirming(false)
    }
  }

  if (!isOpen) return null

  const TargetIcon = sourceIcons[targetVersion.source] || Sparkles

  return (
    <AnimatePresence>
      <div className="fixed inset-0 z-50 flex items-center justify-center">
        {/* Backdrop */}
        <motion.div
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          exit={{ opacity: 0 }}
          className="absolute inset-0 bg-black/40 backdrop-blur-sm"
          onClick={onClose}
        />

        {/* Modal */}
        <motion.div
          initial={{ opacity: 0, scale: 0.95, y: 20 }}
          animate={{ opacity: 1, scale: 1, y: 0 }}
          exit={{ opacity: 0, scale: 0.95, y: 20 }}
          transition={{ type: 'spring', duration: 0.3 }}
          className="relative bg-white rounded-xl shadow-2xl max-w-2xl w-full mx-4 max-h-[85vh] flex flex-col"
        >
          {/* Header */}
          <div className="flex items-center justify-between px-6 py-4 border-b border-gray-200">
            <div className="flex items-center gap-3">
              <div className="p-2 bg-yellow-50 rounded-lg">
                <RotateCcw className="h-5 w-5 text-yellow-600" />
              </div>
              <div>
                <h2 className="text-lg font-semibold text-gray-900">
                  Rollback to Version {targetVersion.version_number}
                </h2>
                <p className="text-sm text-gray-500">
                  Review changes before reverting
                </p>
              </div>
            </div>
            <button
              onClick={onClose}
              className="p-2 text-gray-400 hover:text-gray-600 hover:bg-gray-100 
                       rounded-lg transition-colors"
            >
              <X className="h-5 w-5" />
            </button>
          </div>

          {/* Content */}
          <div className="flex-1 overflow-y-auto px-6 py-4">
            {loading ? (
              <div className="space-y-6">
                {/* Skeleton: Version comparison */}
                <div className="flex items-center justify-between p-4 bg-gray-50 rounded-lg animate-pulse">
                  <div className="flex-1">
                    <div className="h-3 w-16 bg-gray-200 rounded mb-2" />
                    <div className="h-5 w-24 bg-gray-200 rounded" />
                  </div>
                  <div className="px-4">
                    <div className="h-5 w-5 bg-gray-200 rounded" />
                  </div>
                  <div className="flex-1 text-right">
                    <div className="h-3 w-16 bg-gray-200 rounded mb-2 ml-auto" />
                    <div className="h-5 w-24 bg-gray-200 rounded ml-auto" />
                  </div>
                </div>

                {/* Skeleton: Stats cards */}
                <div className="grid grid-cols-2 gap-4 animate-pulse">
                  <div className="p-4 bg-gray-50 border border-gray-200 rounded-lg">
                    <div className="h-4 w-16 bg-gray-200 rounded mb-2" />
                    <div className="h-6 w-12 bg-gray-200 rounded" />
                  </div>
                  <div className="p-4 bg-gray-50 border border-gray-200 rounded-lg">
                    <div className="h-4 w-16 bg-gray-200 rounded mb-2" />
                    <div className="h-6 w-12 bg-gray-200 rounded" />
                  </div>
                </div>

                {/* Skeleton: Sections */}
                <div className="space-y-3 animate-pulse">
                  <div className="h-12 bg-gray-100 rounded-lg" />
                  <div className="h-12 bg-gray-100 rounded-lg" />
                </div>

                <div className="text-center">
                  <Loader2 className="h-5 w-5 text-gray-400 animate-spin mx-auto mb-2" />
                  <p className="text-sm text-gray-500">Analyzing changes...</p>
                </div>
              </div>
            ) : error ? (
              <div className="flex flex-col items-center justify-center py-12">
                <AlertTriangle className="h-8 w-8 text-red-500 mb-3" />
                <p className="text-sm text-red-600">{error}</p>
                <button
                  onClick={fetchPreview}
                  className="mt-3 text-sm text-gray-600 hover:text-gray-900"
                >
                  Try again
                </button>
              </div>
            ) : preview ? (
              <div className="space-y-6">
                {/* Version comparison header */}
                <div className="flex items-center justify-between p-4 bg-gray-50 rounded-lg">
                  {/* Current version */}
                  <div className="flex-1">
                    <div className="text-xs text-gray-500 uppercase tracking-wider mb-1">
                      Current
                    </div>
                    {currentVersion ? (
                      <div className="flex items-center gap-2">
                        <span className="font-medium text-gray-900">
                          v{currentVersion.version_number}
                        </span>
                        <span className="text-xs text-gray-500">
                          {formatDistanceToNow(new Date(currentVersion.created_at), { addSuffix: true })}
                        </span>
                      </div>
                    ) : (
                      <span className="text-gray-500">No version</span>
                    )}
                  </div>

                  {/* Arrow */}
                  <div className="px-4">
                    <GitCompare className="h-5 w-5 text-gray-400" />
                  </div>

                  {/* Target version */}
                  <div className="flex-1 text-right">
                    <div className="text-xs text-gray-500 uppercase tracking-wider mb-1">
                      Target
                    </div>
                    <div className="flex items-center justify-end gap-2">
                      <TargetIcon className="h-4 w-4 text-gray-500" />
                      <span className="font-medium text-gray-900">
                        v{targetVersion.version_number}
                      </span>
                      <span className="text-xs text-gray-500">
                        {formatDistanceToNow(new Date(targetVersion.created_at), { addSuffix: true })}
                      </span>
                    </div>
                    {targetVersion.intent_message && (
                      <p className="text-xs text-gray-500 mt-1 line-clamp-1">
                        "{targetVersion.intent_message}"
                      </p>
                    )}
                  </div>
                </div>

                {/* Snapshot Stats */}
                {preview.diff && (
                  <div className="grid grid-cols-2 gap-4">
                    <div className="p-4 bg-gray-50 border border-gray-200 rounded-lg">
                      <div className="flex items-center gap-2 text-sm text-gray-600 mb-1">
                        <FileCode className="h-4 w-4" />
                        <span>Files</span>
                      </div>
                      <div className="flex items-center gap-3">
                        <span className="text-lg font-semibold text-gray-900">
                          {preview.diff.files.from_count}
                        </span>
                        <span className="text-gray-400">→</span>
                        <span className="text-lg font-semibold text-gray-900">
                          {preview.diff.files.to_count}
                        </span>
                        {preview.diff.files.modified.length + preview.diff.files.added.length + preview.diff.files.removed.length > 0 && (
                          <span className="text-xs px-1.5 py-0.5 bg-yellow-100 text-yellow-700 rounded-full">
                            {preview.diff.files.modified.length + preview.diff.files.added.length + preview.diff.files.removed.length} changes
                          </span>
                        )}
                      </div>
                    </div>
                    <div className="p-4 bg-gray-50 border border-gray-200 rounded-lg">
                      <div className="flex items-center gap-2 text-sm text-gray-600 mb-1">
                        <Database className="h-4 w-4" />
                        <span>Tables</span>
                      </div>
                      <div className="flex items-center gap-3">
                        <span className="text-lg font-semibold text-gray-900">
                          {(preview.schema_compatibility?.summary?.tables_removed ?? 0) + 
                           (preview.schema_compatibility?.summary?.tables_modified ?? 0)}
                        </span>
                        {preview.diff.tables.modified.length + preview.diff.tables.added.length + preview.diff.tables.removed.length > 0 && (
                          <span className="text-xs px-1.5 py-0.5 bg-yellow-100 text-yellow-700 rounded-full">
                            {preview.diff.tables.modified.length + preview.diff.tables.added.length + preview.diff.tables.removed.length} changes
                          </span>
                        )}
                      </div>
                    </div>
                  </div>
                )}

                {/* Warnings */}
                {preview.warnings.length > 0 && (
                  <div className="space-y-2">
                    {preview.warnings.map((warning, i) => (
                      <div
                        key={i}
                        className={cn(
                          'flex items-start gap-3 p-3 rounded-lg',
                          warning.severity === 'warning' && 'bg-yellow-50 border border-yellow-200',
                          warning.severity === 'error' && 'bg-red-50 border border-red-200',
                          warning.severity === 'info' && 'bg-blue-50 border border-blue-200'
                        )}
                      >
                        {warning.severity === 'warning' || warning.severity === 'error' ? (
                          <AlertTriangle className={cn(
                            'h-4 w-4 flex-shrink-0 mt-0.5',
                            warning.severity === 'warning' && 'text-yellow-600',
                            warning.severity === 'error' && 'text-red-600'
                          )} />
                        ) : (
                          <Info className="h-4 w-4 flex-shrink-0 mt-0.5 text-blue-600" />
                        )}
                        <p className={cn(
                          'text-sm',
                          warning.severity === 'warning' && 'text-yellow-800',
                          warning.severity === 'error' && 'text-red-800',
                          warning.severity === 'info' && 'text-blue-800'
                        )}>
                          {warning.message}
                        </p>
                      </div>
                    ))}
                  </div>
                )}

                {/* Schema compatibility */}
                {preview.schema_compatibility && (
                  <div className={cn(
                    'p-4 rounded-lg border',
                    preview.schema_compatibility.risk_level === 'safe' && 'bg-green-50 border-green-200',
                    preview.schema_compatibility.risk_level === 'warning' && 'bg-yellow-50 border-yellow-200',
                    preview.schema_compatibility.risk_level === 'danger' && 'bg-red-50 border-red-200'
                  )}>
                    <div className="flex items-center gap-2 mb-2">
                      {preview.schema_compatibility.risk_level === 'safe' ? (
                        <Check className="h-4 w-4 text-green-600" />
                      ) : (
                        <AlertTriangle className={cn(
                          'h-4 w-4',
                          preview.schema_compatibility.risk_level === 'warning' && 'text-yellow-600',
                          preview.schema_compatibility.risk_level === 'danger' && 'text-red-600'
                        )} />
                      )}
                      <span className={cn(
                        'text-sm font-medium',
                        preview.schema_compatibility.risk_level === 'safe' && 'text-green-800',
                        preview.schema_compatibility.risk_level === 'warning' && 'text-yellow-800',
                        preview.schema_compatibility.risk_level === 'danger' && 'text-red-800'
                      )}>
                        Schema Compatibility: {preview.schema_compatibility.risk_level.charAt(0).toUpperCase() + preview.schema_compatibility.risk_level.slice(1)}
                      </span>
                    </div>
                    <div className="text-xs text-gray-600 space-y-1">
                      <p>{preview.schema_compatibility.summary.tables_modified} table(s) modified</p>
                      {preview.schema_compatibility.summary.tables_added > 0 && (
                        <p>{preview.schema_compatibility.summary.tables_added} table(s) added</p>
                      )}
                      {preview.schema_compatibility.summary.tables_removed > 0 && (
                        <p>{preview.schema_compatibility.summary.tables_removed} table(s) removed</p>
                      )}
                    </div>
                  </div>
                )}

                {/* File changes section */}
                {preview.diff && (
                  <CollapsibleSection
                    title="File Changes"
                    icon={FileCode}
                    isExpanded={expandedSections.has('files')}
                    onToggle={() => toggleSection('files')}
                    summary={`${preview.diff.files.modified.length + preview.diff.files.added.length + preview.diff.files.removed.length} changes`}
                  >
                    <div className="space-y-2">
                      {preview.diff.files.added.map((file) => (
                        <div key={file} className="flex items-center gap-2 text-sm">
                          <span className="text-green-600 font-mono">+</span>
                          <span className="text-gray-700 font-mono text-xs">{file}</span>
                        </div>
                      ))}
                      {preview.diff.files.removed.map((file) => (
                        <div key={file} className="flex items-center gap-2 text-sm">
                          <span className="text-red-600 font-mono">-</span>
                          <span className="text-gray-700 font-mono text-xs">{file}</span>
                        </div>
                      ))}
                      {preview.diff.files.modified.map((file) => (
                        <div key={file} className="flex items-center gap-2 text-sm">
                          <span className="text-yellow-600 font-mono">~</span>
                          <span className="text-gray-700 font-mono text-xs">{file}</span>
                        </div>
                      ))}
                      {preview.diff.files.added.length === 0 &&
                       preview.diff.files.removed.length === 0 &&
                       preview.diff.files.modified.length === 0 && (
                        <p className="text-sm text-gray-500">No file changes</p>
                      )}
                    </div>
                  </CollapsibleSection>
                )}

                {/* Table changes section */}
                {preview.diff && (
                  <CollapsibleSection
                    title="Table Schema Changes"
                    icon={Database}
                    isExpanded={expandedSections.has('tables')}
                    onToggle={() => toggleSection('tables')}
                    summary={`${preview.diff.tables.modified.length + preview.diff.tables.added.length + preview.diff.tables.removed.length} changes`}
                  >
                    <div className="space-y-2">
                      {preview.diff.tables.added.map((table) => (
                        <div key={table.slug} className="flex items-center gap-2 text-sm">
                          <span className="text-green-600 font-mono">+</span>
                          <span className="text-gray-700">{table.name}</span>
                          <span className="text-gray-400 text-xs">({table.slug})</span>
                        </div>
                      ))}
                      {preview.diff.tables.removed.map((table) => (
                        <div key={table.slug} className="flex items-center gap-2 text-sm">
                          <span className="text-red-600 font-mono">-</span>
                          <span className="text-gray-700">{table.name}</span>
                          <span className="text-gray-400 text-xs">({table.slug})</span>
                        </div>
                      ))}
                      {preview.diff.tables.modified.map((table) => (
                        <div key={table.slug} className="flex items-center gap-2 text-sm">
                          <span className="text-yellow-600 font-mono">~</span>
                          <span className="text-gray-700">{table.slug}</span>
                          <span className="text-gray-400 text-xs">(schema modified)</span>
                        </div>
                      ))}
                      {preview.diff.tables.added.length === 0 &&
                       preview.diff.tables.removed.length === 0 &&
                       preview.diff.tables.modified.length === 0 && (
                        <p className="text-sm text-gray-500">No table changes</p>
                      )}
                    </div>
                  </CollapsibleSection>
                )}

                {/* Migration hints */}
                {preview.migration_hints && preview.migration_hints.length > 0 && (
                  <CollapsibleSection
                    title="Migration Hints"
                    icon={Info}
                    isExpanded={expandedSections.has('hints')}
                    onToggle={() => toggleSection('hints')}
                    summary={`${preview.migration_hints.length} suggestions`}
                  >
                    <div className="space-y-2">
                      {preview.migration_hints.map((hint, i) => (
                        <div key={i} className="p-3 bg-gray-50 rounded-lg">
                          <div className="flex items-center gap-2 text-xs text-gray-500 mb-1">
                            <span className="font-mono">{hint.column}</span>
                            <span>•</span>
                            <span>{hint.type}</span>
                          </div>
                          <p className="text-sm text-gray-700">{hint.suggestion}</p>
                        </div>
                      ))}
                    </div>
                  </CollapsibleSection>
                )}

                {/* Options */}
                <div className="p-4 bg-gray-50 rounded-lg">
                  <label className="flex items-start gap-3 cursor-pointer">
                    <input
                      type="checkbox"
                      checked={includeSchema}
                      onChange={(e) => setIncludeSchema(e.target.checked)}
                      className="mt-0.5 rounded border-gray-300 text-gray-900 focus:ring-gray-500"
                    />
                    <div>
                      <span className="text-sm font-medium text-gray-900">
                        Revert schema changes
                      </span>
                      <p className="text-xs text-gray-500 mt-0.5">
                        Restore table schemas to match the target version. Your data will be preserved.
                      </p>
                    </div>
                  </label>
                </div>
              </div>
            ) : null}
          </div>

          {/* Footer */}
          <div className="flex items-center justify-between px-6 py-4 border-t border-gray-200 bg-gray-50 rounded-b-xl">
            <p className="text-xs text-gray-500">
              A new version will be created. This action cannot be undone.
            </p>
            <div className="flex gap-3">
              <button
                onClick={onClose}
                className="px-4 py-2 text-sm font-medium text-gray-700 bg-white 
                         border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors"
              >
                Cancel
              </button>
              <button
                onClick={handleConfirm}
                disabled={loading || confirming || !preview}
                className="flex items-center gap-2 px-4 py-2 text-sm font-medium text-white
                         bg-yellow-600 hover:bg-yellow-700 rounded-lg transition-colors
                         disabled:opacity-50 disabled:cursor-not-allowed"
              >
                {confirming ? (
                  <Loader2 className="h-4 w-4 animate-spin" />
                ) : (
                  <RotateCcw className="h-4 w-4" />
                )}
                Confirm Rollback
              </button>
            </div>
          </div>
        </motion.div>
      </div>
    </AnimatePresence>
  )
}

// Collapsible section component
interface CollapsibleSectionProps {
  title: string
  icon: typeof FileCode
  isExpanded: boolean
  onToggle: () => void
  summary: string
  children: React.ReactNode
}

function CollapsibleSection({
  title,
  icon: Icon,
  isExpanded,
  onToggle,
  summary,
  children,
}: CollapsibleSectionProps) {
  return (
    <div className="border border-gray-200 rounded-lg overflow-hidden">
      <button
        onClick={onToggle}
        className="w-full flex items-center justify-between px-4 py-3 bg-gray-50 
                 hover:bg-gray-100 transition-colors"
      >
        <div className="flex items-center gap-2">
          <Icon className="h-4 w-4 text-gray-500" />
          <span className="text-sm font-medium text-gray-900">{title}</span>
          <span className="text-xs text-gray-500 bg-white px-2 py-0.5 rounded-full">
            {summary}
          </span>
        </div>
        {isExpanded ? (
          <ChevronDown className="h-4 w-4 text-gray-400" />
        ) : (
          <ChevronRight className="h-4 w-4 text-gray-400" />
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
            <div className="px-4 py-3 bg-white">
              {children}
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  )
}

