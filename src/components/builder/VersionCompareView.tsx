/**
 * Version Compare View Component
 * 
 * Side-by-side comparison of two versions showing file and schema diffs.
 * Light enterprise theme following STYLE_GUIDE.md
 */
import { useState, useMemo } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import {
  X,
  GitCompare,
  FileCode,
  Database,
  ChevronDown,
  ChevronRight,
  Plus,
  Minus,
  ArrowRight,
  Loader2,
  AlertTriangle,
  Check,
} from 'lucide-react'
import { formatDistanceToNow } from 'date-fns'
import type { AppVersion, VersionDiff } from '../../types/models'
import { useVersionDiff } from '../../hooks/useApps'
import { cn } from '../../lib/utils'

interface VersionCompareViewProps {
  isOpen: boolean
  onClose: () => void
  fromVersion: AppVersion
  toVersion: AppVersion
}

export function VersionCompareView({
  isOpen,
  onClose,
  fromVersion,
  toVersion,
}: VersionCompareViewProps) {
  const [expandedSections, setExpandedSections] = useState<Set<string>>(
    new Set(['files', 'tables'])
  )
  
  const { data: diff, isLoading } = useVersionDiff(
    isOpen ? fromVersion.id : null,
    isOpen ? toVersion.id : null
  )

  const toggleSection = (section: string) => {
    setExpandedSections((prev) => {
      const next = new Set(prev)
      if (next.has(section)) {
        next.delete(section)
      } else {
        next.add(section)
      }
      return next
    })
  }

  // Calculate summary stats
  const stats = useMemo(() => {
    if (!diff) return null
    return {
      filesAdded: diff.files.added.length,
      filesRemoved: diff.files.removed.length,
      filesModified: diff.files.modified.length,
      tablesAdded: diff.tables.added.length,
      tablesRemoved: diff.tables.removed.length,
      tablesModified: diff.tables.modified.length,
    }
  }, [diff])

  if (!isOpen) return null

  return (
    <AnimatePresence>
      <div className="fixed inset-0 z-50 flex items-center justify-center">
        {/* Backdrop */}
        <motion.div
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          exit={{ opacity: 0 }}
          className="absolute inset-0 bg-black/40"
          onClick={onClose}
        />

        {/* Modal */}
        <motion.div
          initial={{ opacity: 0, scale: 0.95, y: 20 }}
          animate={{ opacity: 1, scale: 1, y: 0 }}
          exit={{ opacity: 0, scale: 0.95, y: 20 }}
          transition={{ type: 'spring', duration: 0.3 }}
          className="relative bg-white rounded-xl shadow-2xl max-w-4xl w-full mx-4 max-h-[85vh] flex flex-col"
        >
          {/* Header */}
          <div className="flex items-center justify-between px-6 py-4 border-b border-gray-200">
            <div className="flex items-center gap-3">
              <div className="p-2 bg-gray-100 rounded-lg">
                <GitCompare className="h-5 w-5 text-gray-700" />
              </div>
              <div>
                <h2 className="text-lg font-semibold text-gray-900">Compare Versions</h2>
                <p className="text-sm text-gray-500">
                  v{fromVersion.version_number} → v{toVersion.version_number}
                </p>
              </div>
            </div>
            <button
              onClick={onClose}
              className="p-2 text-gray-400 hover:text-gray-600 hover:bg-gray-100 rounded-lg transition-colors"
            >
              <X className="h-5 w-5" />
            </button>
          </div>

          {/* Version Headers */}
          <div className="grid grid-cols-2 gap-4 px-6 py-4 bg-gray-50 border-b border-gray-200">
            <VersionHeader version={fromVersion} label="From" />
            <VersionHeader version={toVersion} label="To" />
          </div>

          {/* Content */}
          <div className="flex-1 overflow-y-auto px-6 py-4">
            {isLoading ? (
              <div className="flex flex-col items-center justify-center py-12">
                <Loader2 className="h-8 w-8 text-gray-400 animate-spin mb-3" />
                <p className="text-sm text-gray-500">Comparing versions...</p>
              </div>
            ) : diff ? (
              <div className="space-y-6">
                {/* Summary Stats */}
                {stats && (
                  <div className="grid grid-cols-2 gap-4">
                    <SummaryCard
                      title="Files"
                      icon={FileCode}
                      added={stats.filesAdded}
                      removed={stats.filesRemoved}
                      modified={stats.filesModified}
                    />
                    <SummaryCard
                      title="Tables"
                      icon={Database}
                      added={stats.tablesAdded}
                      removed={stats.tablesRemoved}
                      modified={stats.tablesModified}
                    />
                  </div>
                )}

                {/* File Changes */}
                <CollapsibleDiffSection
                  title="File Changes"
                  icon={FileCode}
                  addedCount={diff.files.added.length}
                  removedCount={diff.files.removed.length}
                  modifiedCount={diff.files.modified.length}
                  isExpanded={expandedSections.has('files')}
                  onToggle={() => toggleSection('files')}
                >
                  <div className="space-y-2">
                    {diff.files.added.map((file) => (
                      <DiffItem key={file} type="added" label={file} />
                    ))}
                    {diff.files.removed.map((file) => (
                      <DiffItem key={file} type="removed" label={file} />
                    ))}
                    {diff.files.modified.map((file) => (
                      <DiffItem key={file} type="modified" label={file} />
                    ))}
                    {diff.files.added.length === 0 &&
                      diff.files.removed.length === 0 &&
                      diff.files.modified.length === 0 && (
                        <p className="text-sm text-gray-500 text-center py-4">
                          No file changes
                        </p>
                      )}
                  </div>
                </CollapsibleDiffSection>

                {/* Table Changes */}
                <CollapsibleDiffSection
                  title="Table Schema Changes"
                  icon={Database}
                  addedCount={diff.tables.added.length}
                  removedCount={diff.tables.removed.length}
                  modifiedCount={diff.tables.modified.length}
                  isExpanded={expandedSections.has('tables')}
                  onToggle={() => toggleSection('tables')}
                >
                  <div className="space-y-2">
                    {diff.tables.added.map((table) => (
                      <DiffItem
                        key={table.slug}
                        type="added"
                        label={table.name}
                        sublabel={table.slug}
                      />
                    ))}
                    {diff.tables.removed.map((table) => (
                      <DiffItem
                        key={table.slug}
                        type="removed"
                        label={table.name}
                        sublabel={table.slug}
                      />
                    ))}
                    {diff.tables.modified.map((table) => (
                      <TableDiffItem key={table.slug} table={table} />
                    ))}
                    {diff.tables.added.length === 0 &&
                      diff.tables.removed.length === 0 &&
                      diff.tables.modified.length === 0 && (
                        <p className="text-sm text-gray-500 text-center py-4">
                          No table changes
                        </p>
                      )}
                  </div>
                </CollapsibleDiffSection>
              </div>
            ) : (
              <div className="flex flex-col items-center justify-center py-12">
                <AlertTriangle className="h-8 w-8 text-yellow-500 mb-3" />
                <p className="text-sm text-gray-600">Failed to load version comparison</p>
              </div>
            )}
          </div>

          {/* Footer */}
          <div className="flex items-center justify-end px-6 py-4 border-t border-gray-200 bg-gray-50">
            <button
              onClick={onClose}
              className="px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors"
            >
              Close
            </button>
          </div>
        </motion.div>
      </div>
    </AnimatePresence>
  )
}

// Version Header Component
function VersionHeader({ version, label }: { version: AppVersion; label: string }) {
  return (
    <div className="p-3 bg-white border border-gray-200 rounded-lg">
      <div className="text-xs text-gray-500 uppercase tracking-wider mb-1">{label}</div>
      <div className="flex items-center justify-between">
        <span className="font-medium text-gray-900">v{version.version_number}</span>
        <span className="text-xs text-gray-500">
          {formatDistanceToNow(new Date(version.created_at), { addSuffix: true })}
        </span>
      </div>
      {version.intent_message && (
        <p className="text-xs text-gray-600 mt-1 line-clamp-1">"{version.intent_message}"</p>
      )}
    </div>
  )
}

// Summary Card Component
interface SummaryCardProps {
  title: string
  icon: typeof FileCode
  added: number
  removed: number
  modified: number
}

function SummaryCard({ title, icon: Icon, added, removed, modified }: SummaryCardProps) {
  const total = added + removed + modified
  const hasChanges = total > 0

  return (
    <div className="p-4 bg-gray-50 border border-gray-200 rounded-lg">
      <div className="flex items-center gap-2 mb-3">
        <Icon className="h-4 w-4 text-gray-500" />
        <span className="text-sm font-medium text-gray-900">{title}</span>
        <span className="text-xs text-gray-500 bg-gray-100 px-2 py-0.5 rounded-full">
          {total} changes
        </span>
      </div>
      {hasChanges ? (
        <div className="flex gap-4 text-xs">
          {added > 0 && (
            <span className="flex items-center gap-1 text-green-600">
              <Plus className="h-3 w-3" />
              {added} added
            </span>
          )}
          {removed > 0 && (
            <span className="flex items-center gap-1 text-red-600">
              <Minus className="h-3 w-3" />
              {removed} removed
            </span>
          )}
          {modified > 0 && (
            <span className="flex items-center gap-1 text-yellow-600">
              <ArrowRight className="h-3 w-3" />
              {modified} modified
            </span>
          )}
        </div>
      ) : (
        <div className="flex items-center gap-1 text-xs text-gray-500">
          <Check className="h-3 w-3" />
          No changes
        </div>
      )}
    </div>
  )
}

// Collapsible Diff Section
interface CollapsibleDiffSectionProps {
  title: string
  icon: typeof FileCode
  addedCount: number
  removedCount: number
  modifiedCount: number
  isExpanded: boolean
  onToggle: () => void
  children: React.ReactNode
}

function CollapsibleDiffSection({
  title,
  icon: Icon,
  addedCount,
  removedCount,
  modifiedCount,
  isExpanded,
  onToggle,
  children,
}: CollapsibleDiffSectionProps) {
  const total = addedCount + removedCount + modifiedCount

  return (
    <div className="border border-gray-200 rounded-lg overflow-hidden">
      <button
        onClick={onToggle}
        className="w-full flex items-center justify-between px-4 py-3 bg-white hover:bg-gray-50 transition-colors"
      >
        <div className="flex items-center gap-2">
          <Icon className="h-4 w-4 text-gray-500" />
          <span className="text-sm font-medium text-gray-900">{title}</span>
          <span className="text-xs text-gray-500 bg-gray-100 px-2 py-0.5 rounded-full">
            {total} changes
          </span>
        </div>
        <div className="flex items-center gap-3">
          {addedCount > 0 && (
            <span className="text-xs text-green-600">+{addedCount}</span>
          )}
          {removedCount > 0 && (
            <span className="text-xs text-red-600">-{removedCount}</span>
          )}
          {modifiedCount > 0 && (
            <span className="text-xs text-yellow-600">~{modifiedCount}</span>
          )}
          {isExpanded ? (
            <ChevronDown className="h-4 w-4 text-gray-400" />
          ) : (
            <ChevronRight className="h-4 w-4 text-gray-400" />
          )}
        </div>
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
            <div className="px-4 py-3 bg-white border-t border-gray-100">{children}</div>
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  )
}

// Diff Item Component
interface DiffItemProps {
  type: 'added' | 'removed' | 'modified'
  label: string
  sublabel?: string
}

function DiffItem({ type, label, sublabel }: DiffItemProps) {
  return (
    <div
      className={cn(
        'flex items-center gap-2 px-3 py-2 rounded-md',
        type === 'added' && 'bg-green-50 border border-green-200',
        type === 'removed' && 'bg-red-50 border border-red-200',
        type === 'modified' && 'bg-yellow-50 border border-yellow-200'
      )}
    >
      {type === 'added' && <Plus className="h-3.5 w-3.5 text-green-600" />}
      {type === 'removed' && <Minus className="h-3.5 w-3.5 text-red-600" />}
      {type === 'modified' && <ArrowRight className="h-3.5 w-3.5 text-yellow-600" />}
      <span
        className={cn(
          'text-sm font-mono',
          type === 'added' && 'text-green-700',
          type === 'removed' && 'text-red-700',
          type === 'modified' && 'text-yellow-700'
        )}
      >
        {label}
      </span>
      {sublabel && <span className="text-xs text-gray-500">({sublabel})</span>}
    </div>
  )
}

// Table Diff Item with schema details
function TableDiffItem({
  table,
}: {
  table: { slug: string; name: string; from: any; to: any }
}) {
  const [expanded, setExpanded] = useState(false)

  return (
    <div className="bg-yellow-50 border border-yellow-200 rounded-md overflow-hidden">
      <button
        onClick={() => setExpanded(!expanded)}
        className="w-full flex items-center justify-between px-3 py-2 hover:bg-yellow-100 transition-colors"
      >
        <div className="flex items-center gap-2">
          <ArrowRight className="h-3.5 w-3.5 text-yellow-600" />
          <span className="text-sm font-mono text-yellow-700">{table.name}</span>
          <span className="text-xs text-gray-500">({table.slug})</span>
        </div>
        {expanded ? (
          <ChevronDown className="h-4 w-4 text-yellow-600" />
        ) : (
          <ChevronRight className="h-4 w-4 text-yellow-600" />
        )}
      </button>
      {expanded && (
        <div className="px-3 py-2 border-t border-yellow-200 bg-white">
          <div className="grid grid-cols-2 gap-4 text-xs">
            <div>
              <div className="text-gray-500 uppercase tracking-wider mb-1">From</div>
              <pre className="p-2 bg-gray-50 rounded text-gray-600 overflow-x-auto">
                {JSON.stringify(table.from, null, 2)}
              </pre>
            </div>
            <div>
              <div className="text-gray-500 uppercase tracking-wider mb-1">To</div>
              <pre className="p-2 bg-gray-50 rounded text-gray-600 overflow-x-auto">
                {JSON.stringify(table.to, null, 2)}
              </pre>
            </div>
          </div>
        </div>
      )}
    </div>
  )
}

