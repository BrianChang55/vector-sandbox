/**
 * Versions Panel Component
 * 
 * Displays version history with clean, minimal, enterprise-grade design.
 * Features rollback preview, diff visualization, and professional UX.
 * Light theme following STYLE_GUIDE.md
 */
import { useState } from 'react'
import { 
  Clock, 
  RotateCcw, 
  Check,
  Loader2,
  ChevronDown,
} from 'lucide-react'
import { formatDistanceToNow, format } from 'date-fns'
import type { AppVersion } from '../../types/models'
import { cn } from '../../lib/utils'

interface VersionsPanelProps {
  versions: AppVersion[]
  selectedVersionId: string | null
  onVersionSelect: (versionId: string) => void
  onRollback: (versionId: string, options?: { include_schema?: boolean }) => void
  className?: string
}

const statusConfig: Record<string, { 
  bgClass: string
  textClass: string
  borderClass: string
  label: string
}> = {
  pending: { 
    bgClass: 'bg-gray-50', 
    textClass: 'text-gray-600', 
    borderClass: 'border-gray-200',
    label: 'Pending'
  },
  generating: { 
    bgClass: 'bg-blue-50', 
    textClass: 'text-blue-600', 
    borderClass: 'border-blue-200',
    label: 'Generating'
  },
  complete: { 
    bgClass: 'bg-green-50', 
    textClass: 'text-green-600', 
    borderClass: 'border-green-200',
    label: 'Complete'
  },
  error: { 
    bgClass: 'bg-red-50', 
    textClass: 'text-red-600', 
    borderClass: 'border-red-200',
    label: 'Error'
  },
}

export function VersionsPanel({
  versions,
  selectedVersionId,
  onVersionSelect,
  onRollback,
  className = '',
}: VersionsPanelProps) {
  const [expandedVersion, setExpandedVersion] = useState<string | null>(null)
  const [rollbackLoading, setRollbackLoading] = useState(false)

  // Execute rollback
  const handleRollback = async (e: React.MouseEvent, versionId: string) => {
    e.stopPropagation()
    setRollbackLoading(true)
    try {
      await onRollback(versionId, { include_schema: true })
      setExpandedVersion(null)
    } finally {
      setRollbackLoading(false)
    }
  }

  // Toggle expand
  const handleExpand = (e: React.MouseEvent, versionId: string) => {
    e.stopPropagation()
    setExpandedVersion(expandedVersion === versionId ? null : versionId)
  }

  return (
    <div className={cn('flex flex-col h-full bg-white', className)}>
      {/* Header */}
      <div className="flex-shrink-0 px-5 py-[11px] border-b border-gray-200">
        <div className="flex items-center gap-3">
          <div className="p-2 bg-gray-100 rounded-lg">
            <Clock className="h-4 w-4 text-gray-500" />
          </div>
          <div>
            <h2 className="text-sm font-semibold text-gray-900">Version History</h2>
            <p className="text-xs text-gray-500 mt-0.5">
              {versions.length} {versions.length === 1 ? 'version' : 'versions'}
            </p>
          </div>
        </div>
      </div>

      {/* Versions List */}
      <div className="flex-1 overflow-y-auto">
        {versions.length === 0 ? (
          <EmptyState />
        ) : (
          <div className="px-4 py-4">
            <div className="space-y-2">
            {versions.map((version, index) => {
              const isSelected = selectedVersionId === version.id
              const isLatest = index === 0
              const isExpanded = expandedVersion === version.id
                const statusCfg = version.generation_status ? statusConfig[version.generation_status] : null

              return (
                  <div key={version.id} className="relative">
                    {/* Version Card */}
                  <button
                    onClick={() => onVersionSelect(version.id)}
                    className={cn(
                        'w-full text-left rounded-lg transition-all duration-150',
                        'border',
                      isSelected
                          ? 'bg-gray-50 border-gray-200'
                          : 'bg-white border-transparent hover:bg-gray-50'
                      )}
                    >
                      <div className="px-4 py-4">
                        {/* Top row */}
                        <div className="flex items-center gap-3">
                          {/* Content */}
                      <div className="flex-1 min-w-0">
                            {/* Version number and badges */}
                        <div className="flex items-center gap-2">
                          <span className="text-sm font-medium text-gray-900">
                            v{version.version_number}
                          </span>
                              
                          {isLatest && (
                                <span className="text-[10px] font-medium px-1.5 py-0.5 
                                               bg-green-50 text-green-600 rounded">
                                  Current
                            </span>
                          )}
                              
                              {statusCfg && version.generation_status !== 'complete' && (
                            <span className={cn(
                                  'inline-flex items-center gap-1 text-[10px] font-medium px-1.5 py-0.5 rounded',
                                  statusCfg.bgClass,
                                  statusCfg.textClass
                            )}>
                              {version.generation_status === 'generating' && (
                                <Loader2 className="h-2.5 w-2.5 animate-spin" />
                              )}
                                  {statusCfg.label}
                            </span>
                          )}

                          {isSelected && (
                                <Check className="h-3 w-3 text-gray-400" />
                          )}
                        </div>

                            {/* Metadata row */}
                            <div className="flex items-center gap-1.5 mt-1.5 text-[11px] text-gray-500">
                              <span>{formatDistanceToNow(new Date(version.created_at), { addSuffix: true })}</span>
                          {version.files && version.files.length > 0 && (
                            <>
                                  <span>·</span>
                                  <span>{version.files.length} files</span>
                            </>
                          )}
                        </div>
                      </div>

                          {/* Expand button */}
                      <button
                            onClick={(e) => handleExpand(e, version.id)}
                            className={cn(
                              'flex-shrink-0 p-1.5 rounded transition-colors',
                              'text-gray-400 hover:text-gray-600 hover:bg-gray-100'
                            )}
                      >
                        <ChevronDown 
                          className={cn(
                                'h-4 w-4 transition-transform duration-150',
                            isExpanded && 'rotate-180'
                          )} 
                        />
                      </button>
                    </div>

                        {/* Expanded content */}
                    {isExpanded && (
                          <div className="mt-4 pt-4 border-t border-gray-100">
                            {/* Author and date */}
                            <div className="flex items-center gap-3 mb-4 text-[11px] text-gray-500">
                          <span>{version.created_by_email || 'Unknown'}</span>
                              <span>·</span>
                              <span>{format(new Date(version.created_at), 'MMM d, h:mm a')}</span>
                        </div>

                            {/* Intent message */}
                        {version.intent_message && (
                              <div className="mb-4 p-3 bg-gray-50 rounded border border-gray-100">
                                <p className="text-xs text-gray-600 leading-relaxed">
                              {version.intent_message}
                            </p>
                          </div>
                        )}

                            {/* Rollback button for non-latest versions */}
                        {!isLatest && (
                              <button
                                onClick={(e) => handleRollback(e, version.id)}
                                disabled={rollbackLoading}
                                className={cn(
                                  'w-full flex items-center justify-center gap-2 px-4 py-2.5',
                                  'text-xs font-medium rounded-md transition-colors',
                                  'bg-gray-900 hover:bg-gray-800 text-white',
                                  'disabled:opacity-50 disabled:cursor-not-allowed'
                                )}
                              >
                                {rollbackLoading ? (
                                  <Loader2 className="h-3.5 w-3.5 animate-spin" />
                                ) : (
                                  <RotateCcw className="h-3.5 w-3.5" />
                                )}
                                Rollback to this version
                              </button>
                            )}
                          </div>
                        )}
                      </div>
                  </button>
                </div>
              )
            })}
            </div>
          </div>
        )}
      </div>
    </div>
  )
}

// Empty State Component
function EmptyState() {
  return (
    <div className="flex flex-col items-center justify-center h-full px-6 py-12">
      <div className="h-12 w-12 rounded-lg bg-gray-100 flex items-center justify-center mb-3">
        <Clock className="h-5 w-5 text-gray-400" />
      </div>
      <p className="text-sm text-gray-600 mb-0.5">No versions yet</p>
      <p className="text-xs text-gray-400">Start chatting to create one</p>
    </div>
  )
}
