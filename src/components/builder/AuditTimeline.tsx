/**
 * Audit Timeline Component
 * 
 * Displays a chronological timeline of version audit events.
 * Light enterprise theme following STYLE_GUIDE.md
 */
import { useState, useMemo } from 'react'
import {
  CheckCircle,
  Upload,
  History,
  Database,
  AlertCircle,
  User,
  Clock,
  ChevronDown,
  ChevronRight,
  Filter,
  Loader2,
} from 'lucide-react'
import { format, formatDistanceToNow } from 'date-fns'
import type { VersionAuditLog, AuditEventType } from '../../types/models'
import { useAuditTrail } from '../../hooks/useApps'
import { cn } from '../../lib/utils'

interface AuditTimelineProps {
  appId: string
  versionId?: string // Optional: filter to specific version
  limit?: number
  className?: string
}

const eventIcons: Record<AuditEventType, typeof CheckCircle> = {
  created: CheckCircle,
  published: Upload,
  rolled_back: History,
  schema_migrated: Database,
  data_migrated: Database,
  error: AlertCircle,
}

const eventColors: Record<AuditEventType, { bg: string; icon: string; border: string }> = {
  created: { bg: 'bg-green-50', icon: 'text-green-600', border: 'border-green-200' },
  published: { bg: 'bg-blue-50', icon: 'text-blue-600', border: 'border-blue-200' },
  rolled_back: { bg: 'bg-yellow-50', icon: 'text-yellow-600', border: 'border-yellow-200' },
  schema_migrated: { bg: 'bg-purple-50', icon: 'text-purple-600', border: 'border-purple-200' },
  data_migrated: { bg: 'bg-purple-50', icon: 'text-purple-600', border: 'border-purple-200' },
  error: { bg: 'bg-red-50', icon: 'text-red-600', border: 'border-red-200' },
}

const eventLabels: Record<AuditEventType, string> = {
  created: 'Version Created',
  published: 'Published',
  rolled_back: 'Rolled Back',
  schema_migrated: 'Schema Migrated',
  data_migrated: 'Data Migrated',
  error: 'Error',
}

export function AuditTimeline({
  appId,
  versionId,
  limit = 50,
  className = '',
}: AuditTimelineProps) {
  const { data: auditLogs, isLoading } = useAuditTrail(appId, { limit })
  const [expandedEvents, setExpandedEvents] = useState<Set<string>>(new Set())
  const [filterType, setFilterType] = useState<AuditEventType | 'all'>('all')

  // Filter logs by version and type
  const filteredLogs = useMemo(() => {
    let logs = auditLogs || []
    
    if (versionId) {
      logs = logs.filter((log) => log.app_version === versionId)
    }
    
    if (filterType !== 'all') {
      logs = logs.filter((log) => log.event_type === filterType)
    }
    
    return logs
  }, [auditLogs, versionId, filterType])

  const toggleEvent = (eventId: string) => {
    setExpandedEvents((prev) => {
      const next = new Set(prev)
      if (next.has(eventId)) {
        next.delete(eventId)
      } else {
        next.add(eventId)
      }
      return next
    })
  }

  if (isLoading) {
    return (
      <div className={cn('flex items-center justify-center py-8', className)}>
        <Loader2 className="h-6 w-6 text-gray-400 animate-spin" />
      </div>
    )
  }

  return (
    <div className={cn('flex flex-col', className)}>
      {/* Filter */}
      {!versionId && (
        <div className="flex items-center gap-2 mb-4">
          <Filter className="h-4 w-4 text-gray-400" />
          <select
            value={filterType}
            onChange={(e) => setFilterType(e.target.value as AuditEventType | 'all')}
            className="text-sm text-gray-700 bg-white border border-gray-200 rounded-md px-2 py-1 focus:outline-none focus:ring-2 focus:ring-gray-200"
          >
            <option value="all">All Events</option>
            <option value="created">Created</option>
            <option value="published">Published</option>
            <option value="rolled_back">Rolled Back</option>
            <option value="schema_migrated">Schema Migrated</option>
            <option value="error">Errors</option>
          </select>
        </div>
      )}

      {/* Timeline */}
      {filteredLogs.length === 0 ? (
        <div className="flex flex-col items-center justify-center py-8 text-center">
          <div className="h-12 w-12 rounded-xl bg-gray-100 flex items-center justify-center mb-3">
            <Clock className="h-6 w-6 text-gray-400" />
          </div>
          <p className="text-sm text-gray-600">No audit events found</p>
        </div>
      ) : (
        <div className="relative">
          {/* Timeline line */}
          <div className="absolute left-4 top-0 bottom-0 w-px bg-gray-200" />

          {/* Events */}
          <div className="space-y-4">
            {filteredLogs.map((log) => {
              const Icon = eventIcons[log.event_type] || CheckCircle
              const colors = eventColors[log.event_type] || eventColors.created
              const isExpanded = expandedEvents.has(log.id)
              const hasDetails = log.details_json && Object.keys(log.details_json).length > 0

              return (
                <div key={log.id} className="relative pl-10">
                  {/* Icon */}
                  <div
                    className={cn(
                      'absolute left-0 w-8 h-8 rounded-full flex items-center justify-center border-2 bg-white',
                      colors.border
                    )}
                  >
                    <Icon className={cn('h-4 w-4', colors.icon)} />
                  </div>

                  {/* Content */}
                  <div
                    className={cn(
                      'p-3 rounded-lg border',
                      colors.bg,
                      colors.border
                    )}
                  >
                    <div className="flex items-center justify-between">
                      <div className="flex items-center gap-2">
                        <span className="text-sm font-medium text-gray-900">
                          {log.event_type_display || eventLabels[log.event_type]}
                        </span>
                        <span className="text-xs text-gray-500">
                          v{log.app_version.split('-')[0]}
                        </span>
                      </div>
                      {hasDetails && (
                        <button
                          onClick={() => toggleEvent(log.id)}
                          className="p-1 text-gray-400 hover:text-gray-600 rounded transition-colors"
                        >
                          {isExpanded ? (
                            <ChevronDown className="h-4 w-4" />
                          ) : (
                            <ChevronRight className="h-4 w-4" />
                          )}
                        </button>
                      )}
                    </div>

                    <div className="flex items-center gap-3 mt-1 text-xs text-gray-600">
                      <span className="flex items-center gap-1">
                        <Clock className="h-3 w-3" />
                        {formatDistanceToNow(new Date(log.created_at), { addSuffix: true })}
                      </span>
                      {log.created_by_email && (
                        <span className="flex items-center gap-1">
                          <User className="h-3 w-3" />
                          {log.created_by_email}
                        </span>
                      )}
                    </div>

                    {/* Expanded details */}
                    {isExpanded && hasDetails && (
                      <div className="mt-3 pt-3 border-t border-gray-200">
                        <pre className="text-xs text-gray-600 bg-white p-2 rounded overflow-x-auto">
                          {JSON.stringify(log.details_json, null, 2)}
                        </pre>
                      </div>
                    )}
                  </div>

                  {/* Timestamp tooltip */}
                  <div className="mt-1 text-[10px] text-gray-400">
                    {format(new Date(log.created_at), 'MMM d, yyyy h:mm:ss a')}
                  </div>
                </div>
              )
            })}
          </div>
        </div>
      )}
    </div>
  )
}

// Compact version for embedding in other components
interface AuditTimelineCompactProps {
  logs: VersionAuditLog[]
  className?: string
}

export function AuditTimelineCompact({ logs, className = '' }: AuditTimelineCompactProps) {
  if (logs.length === 0) {
    return (
      <p className="text-sm text-gray-500 text-center py-4">No audit events</p>
    )
  }

  return (
    <div className={cn('space-y-2', className)}>
      {logs.slice(0, 5).map((log) => {
        const Icon = eventIcons[log.event_type] || CheckCircle
        const colors = eventColors[log.event_type] || eventColors.created

        return (
          <div
            key={log.id}
            className="flex items-center gap-3 p-2 bg-gray-50 rounded-md"
          >
            <div
              className={cn(
                'w-6 h-6 rounded-full flex items-center justify-center',
                colors.bg
              )}
            >
              <Icon className={cn('h-3 w-3', colors.icon)} />
            </div>
            <div className="flex-1 min-w-0">
              <span className="text-xs font-medium text-gray-900">
                {eventLabels[log.event_type]}
              </span>
              <span className="text-[10px] text-gray-500 ml-2">
                {formatDistanceToNow(new Date(log.created_at), { addSuffix: true })}
              </span>
            </div>
          </div>
        )
      })}
      {logs.length > 5 && (
        <p className="text-xs text-gray-400 text-center">
          +{logs.length - 5} more events
        </p>
      )}
    </div>
  )
}

