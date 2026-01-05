/**
 * Version Details Drawer Component
 * 
 * Wide, spacious drawer for comprehensive version details.
 * Enterprise-grade design with clean typography and generous spacing.
 * Light theme following STYLE_GUIDE.md
 */
import { useState } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import {
  X,
  FileCode,
  Database,
  ChevronDown,
  ChevronRight,
  RotateCcw,
  Sparkles,
  Code2,
  Upload,
  History,
  Loader2,
  Layers,
  Calendar,
  User,
  MessageSquare,
  Hash,
  Clock,
} from 'lucide-react'
import { formatDistanceToNow, format } from 'date-fns'
import type { AppVersion } from '../../types/models'
import { useVersionSnapshot } from '../../hooks/useApps'
import { cn } from '../../lib/utils'

interface VersionDetailsDrawerProps {
  isOpen: boolean
  onClose: () => void
  version: AppVersion
  onRollback?: (versionId: string) => void
  isLatestVersion?: boolean
}

const sourceConfig: Record<string, { icon: typeof Sparkles; label: string; bg: string; text: string }> = {
  ai: { icon: Sparkles, label: 'AI Generated', bg: 'bg-gray-100', text: 'text-gray-700' },
  ai_edit: { icon: Sparkles, label: 'AI Edit', bg: 'bg-purple-50', text: 'text-purple-700' },
  code: { icon: Code2, label: 'Code Edit', bg: 'bg-yellow-50', text: 'text-yellow-700' },
  code_edit: { icon: Code2, label: 'Code Edit', bg: 'bg-yellow-50', text: 'text-yellow-700' },
  publish: { icon: Upload, label: 'Published', bg: 'bg-green-50', text: 'text-green-700' },
  rollback: { icon: History, label: 'Rollback', bg: 'bg-blue-50', text: 'text-blue-700' },
}

export function VersionDetailsDrawer({
  isOpen,
  onClose,
  version,
  onRollback,
  isLatestVersion = false,
}: VersionDetailsDrawerProps) {
  const [expandedSections, setExpandedSections] = useState<Set<string>>(
    new Set(['files', 'tables'])
  )
  const { data: snapshot, isLoading: snapshotLoading } = useVersionSnapshot(
    isOpen ? version.id : null
  )

  const toggleSection = (section: string) => {
    setExpandedSections((prev) => {
      const next = new Set(prev)
      next.has(section) ? next.delete(section) : next.add(section)
      return next
    })
  }

  const config = sourceConfig[version.source] || sourceConfig.ai
  const Icon = config.icon

  if (!isOpen) return null

  return (
    <AnimatePresence>
      <div className="fixed inset-0 z-50 flex justify-end">
        {/* Backdrop */}
        <motion.div
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          exit={{ opacity: 0 }}
          className="absolute inset-0 bg-black/15"
          onClick={onClose}
        />

        {/* Drawer - Extra wide for spacious feel */}
        <motion.div
          initial={{ x: '100%' }}
          animate={{ x: 0 }}
          exit={{ x: '100%' }}
          transition={{ type: 'spring', damping: 28, stiffness: 280 }}
          className="relative w-full max-w-4xl bg-gray-50 flex flex-col h-full border-l border-gray-200"
        >
          {/* Header */}
          <header className="flex-shrink-0 bg-white border-b border-gray-200">
            <div className="px-10 py-8">
              <div className="flex items-start justify-between">
                <div className="flex items-center gap-5">
                  <div className={cn('p-4 rounded-2xl', config.bg)}>
                    <Icon className={cn('h-7 w-7', config.text)} />
                  </div>
                  <div>
                    <div className="flex items-center gap-4">
                      <h1 className="text-2xl font-semibold text-gray-900">
                        Version {version.version_number}
                      </h1>
                      {isLatestVersion && (
                        <span className="text-xs font-medium px-3 py-1.5 bg-green-50 text-green-700 border border-green-200 rounded-full">
                          Current
                        </span>
                      )}
                    </div>
                    <div className="flex items-center gap-4 mt-2">
                      <span className={cn('text-sm font-medium px-3 py-1 rounded-full', config.bg, config.text)}>
                        {config.label}
                      </span>
                      <span className="text-sm text-gray-500">
                        {formatDistanceToNow(new Date(version.created_at), { addSuffix: true })}
                      </span>
                    </div>
                  </div>
                </div>
                <button
                  onClick={onClose}
                  className="p-3 text-gray-400 hover:text-gray-600 hover:bg-gray-100 rounded-xl transition-colors -mt-1 -mr-1"
                >
                  <X className="h-5 w-5" />
                </button>
              </div>
            </div>
          </header>

          {/* Content */}
          <div className="flex-1 overflow-y-auto">
            <div className="px-10 py-8">
              
              {/* Two-column metadata grid */}
              <div className="grid grid-cols-2 gap-6 mb-10">
                <MetadataCard
                  icon={Calendar}
                  label="Created"
                  value={format(new Date(version.created_at), 'MMMM d, yyyy')}
                  subvalue={format(new Date(version.created_at), 'h:mm a')}
                />
                <MetadataCard
                  icon={User}
                  label="Author"
                  value={version.created_by_email || 'Unknown'}
                />
              </div>

              {/* Intent Message */}
              {version.intent_message && (
                <div className="mb-10">
                  <div className="flex items-center gap-2 mb-4">
                    <MessageSquare className="h-4 w-4 text-gray-400" />
                    <h3 className="text-sm font-medium text-gray-900">Intent</h3>
                  </div>
                  <div className="bg-white border border-gray-200 rounded-xl p-6">
                    <p className="text-gray-700 leading-relaxed">
                      "{version.intent_message}"
                    </p>
                  </div>
                </div>
              )}

              {/* Stats */}
              {snapshotLoading ? (
                <div className="flex items-center justify-center py-16">
                  <div className="text-center">
                    <Loader2 className="h-8 w-8 text-gray-300 animate-spin mx-auto mb-4" />
                    <p className="text-sm text-gray-500">Loading version data...</p>
                  </div>
                </div>
              ) : (
                <>
                  {/* Stats Row */}
                  <div className="grid grid-cols-3 gap-6 mb-10">
                    <StatCard
                      icon={FileCode}
                      value={snapshot?.file_count || version.files?.length || 0}
                      label="Files"
                    />
                    <StatCard
                      icon={Database}
                      value={snapshot?.total_tables || 0}
                      label="Tables"
                    />
                    <StatCard
                      icon={Layers}
                      value={snapshot?.total_resources || 0}
                      label="Resources"
                    />
                  </div>

                  {/* Files */}
                  <div className="mb-6">
                    <CollapsibleSection
                      title="Files"
                      icon={FileCode}
                      count={version.files?.length || 0}
                      isExpanded={expandedSections.has('files')}
                      onToggle={() => toggleSection('files')}
                    >
                      {version.files && version.files.length > 0 ? (
                        <div className="grid grid-cols-2 gap-3">
                          {version.files.map((file) => (
                            <div
                              key={file.id}
                              className="flex items-center gap-3 px-4 py-3 bg-gray-50 hover:bg-gray-100 rounded-lg transition-colors group"
                            >
                              <FileCode className="h-4 w-4 text-gray-400 group-hover:text-gray-500 flex-shrink-0" />
                              <span className="text-sm font-mono text-gray-700 truncate">
                                {file.path}
                              </span>
                            </div>
                          ))}
                        </div>
                      ) : (
                        <EmptyState message="No files in this version" />
                      )}
                    </CollapsibleSection>
                  </div>

                  {/* Tables */}
                  {snapshot?.tables_json && snapshot.tables_json.length > 0 && (
                    <div className="mb-6">
                      <CollapsibleSection
                        title="Data Tables"
                        icon={Database}
                        count={snapshot.tables_json.length}
                        isExpanded={expandedSections.has('tables')}
                        onToggle={() => toggleSection('tables')}
                      >
                        <div className="grid grid-cols-2 gap-4">
                          {snapshot.tables_json.map((table) => (
                            <div
                              key={table.id}
                              className="p-5 bg-gray-50 border border-gray-200 rounded-xl hover:border-gray-300 transition-colors"
                            >
                              <div className="flex items-start justify-between mb-3">
                                <div>
                                  <h4 className="text-sm font-medium text-gray-900">{table.name}</h4>
                                  <p className="text-xs text-gray-500 mt-0.5">{table.slug}</p>
                                </div>
                                <span className="text-xs font-medium text-gray-600 bg-white border border-gray-200 px-2.5 py-1 rounded-full">
                                  {table.row_count.toLocaleString()} rows
                                </span>
                              </div>
                              {table.description && (
                                <p className="text-xs text-gray-600 mb-4 line-clamp-2">
                                  {table.description}
                                </p>
                              )}
                              {table.schema_json?.columns && table.schema_json.columns.length > 0 && (
                                <div className="flex flex-wrap gap-1.5">
                                  {table.schema_json.columns.slice(0, 6).map((col) => (
                                    <span
                                      key={col.name}
                                      className="inline-flex items-center gap-1 text-xs px-2 py-1 bg-white border border-gray-200 rounded text-gray-600"
                                    >
                                      <Hash className="h-2.5 w-2.5 text-gray-400" />
                                      {col.name}
                                    </span>
                                  ))}
                                  {table.schema_json.columns.length > 6 && (
                                    <span className="text-xs px-2 py-1 text-gray-400">
                                      +{table.schema_json.columns.length - 6}
                                    </span>
                                  )}
                                </div>
                              )}
                            </div>
                          ))}
                        </div>
                      </CollapsibleSection>
                    </div>
                  )}
                </>
              )}
            </div>
          </div>

          {/* Footer */}
          {!isLatestVersion && onRollback && (
            <footer className="flex-shrink-0 bg-white border-t border-gray-200">
              <div className="px-10 py-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm font-medium text-gray-900">
                      Restore this version?
                    </p>
                    <p className="text-xs text-gray-500 mt-0.5">
                      A new version will be created based on this snapshot
                    </p>
                  </div>
                  <button
                    onClick={() => onRollback(version.id)}
                    className="flex items-center gap-2 px-6 py-2.5 text-sm font-medium
                             bg-gray-900 text-white hover:bg-gray-800
                             rounded-lg transition-colors"
                  >
                    <RotateCcw className="h-4 w-4" />
                    Rollback to v{version.version_number}
                  </button>
                </div>
              </div>
            </footer>
          )}
        </motion.div>
      </div>
    </AnimatePresence>
  )
}

// Metadata Card
interface MetadataCardProps {
  icon: typeof Calendar
  label: string
  value: string
  subvalue?: string
}

function MetadataCard({ icon: Icon, label, value, subvalue }: MetadataCardProps) {
  return (
    <div className="bg-white border border-gray-200 rounded-xl p-5">
      <div className="flex items-center gap-3 mb-3">
        <div className="p-2 bg-gray-50 rounded-lg">
          <Icon className="h-4 w-4 text-gray-500" />
        </div>
        <span className="text-xs font-medium text-gray-500 uppercase tracking-wider">
          {label}
        </span>
      </div>
      <p className="text-base font-medium text-gray-900">{value}</p>
      {subvalue && <p className="text-sm text-gray-500 mt-0.5">{subvalue}</p>}
    </div>
  )
}

// Stat Card
interface StatCardProps {
  icon: typeof FileCode
  value: number
  label: string
}

function StatCard({ icon: Icon, value, label }: StatCardProps) {
  return (
    <div className="bg-white border border-gray-200 rounded-xl p-6 text-center">
      <div className="inline-flex p-3 bg-gray-50 rounded-xl mb-4">
        <Icon className="h-5 w-5 text-gray-500" />
      </div>
      <div className="text-3xl font-semibold text-gray-900 mb-1">
        {value.toLocaleString()}
      </div>
      <div className="text-sm text-gray-500">{label}</div>
    </div>
  )
}

// Collapsible Section
interface CollapsibleSectionProps {
  title: string
  icon: typeof FileCode
  count?: number
  isExpanded: boolean
  onToggle: () => void
  children: React.ReactNode
}

function CollapsibleSection({
  title,
  icon: Icon,
  count,
  isExpanded,
  onToggle,
  children,
}: CollapsibleSectionProps) {
  return (
    <div className="bg-white border border-gray-200 rounded-xl overflow-hidden">
      <button
        onClick={onToggle}
        className="w-full flex items-center justify-between px-6 py-5 hover:bg-gray-50 transition-colors"
      >
        <div className="flex items-center gap-3">
          <Icon className="h-5 w-5 text-gray-500" />
          <span className="text-sm font-medium text-gray-900">{title}</span>
          {count !== undefined && (
            <span className="text-xs font-medium text-gray-600 bg-gray-100 px-2.5 py-1 rounded-full">
              {count}
            </span>
          )}
        </div>
        {isExpanded ? (
          <ChevronDown className="h-5 w-5 text-gray-400" />
        ) : (
          <ChevronRight className="h-5 w-5 text-gray-400" />
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
            <div className="px-6 py-5 border-t border-gray-100">{children}</div>
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  )
}

// Empty State
function EmptyState({ message }: { message: string }) {
  return (
    <div className="py-10 text-center">
      <p className="text-sm text-gray-500">{message}</p>
    </div>
  )
}
