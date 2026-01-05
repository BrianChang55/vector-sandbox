/**
 * Versions Panel Component
 * 
 * Displays version history with diffs and rollback capabilities.
 * Light enterprise theme.
 */
import { useState } from 'react'
import { 
  Clock, 
  Sparkles, 
  Code2, 
  Upload, 
  RotateCcw, 
  Check,
  ChevronDown,
  History,
  User
} from 'lucide-react'
import { formatDistanceToNow } from 'date-fns'
import type { AppVersion } from '../../types/models'
import { cn } from '../../lib/utils'

interface VersionsPanelProps {
  versions: AppVersion[]
  selectedVersionId: string | null
  onVersionSelect: (versionId: string) => void
  onRollback: (versionId: string) => void
  className?: string
}

const sourceIcons: Record<string, typeof Sparkles> = {
  ai: Sparkles,
  ai_edit: Sparkles,
  code: Code2,
  code_edit: Code2,
  publish: Upload,
  rollback: History,
}

const sourceLabels: Record<string, string> = {
  ai: 'AI Generated',
  ai_edit: 'AI Edit',
  code: 'Code Edit',
  code_edit: 'Code Edit',
  publish: 'Published',
  rollback: 'Rollback',
}

const sourceColors: Record<string, string> = {
  ai: 'text-gray-600 bg-gray-100',
  ai_edit: 'text-purple-700 bg-purple-50',
  code: 'text-yellow-700 bg-yellow-50',
  code_edit: 'text-yellow-700 bg-yellow-50',
  publish: 'text-green-700 bg-green-50',
  rollback: 'text-blue-700 bg-blue-50',
}

export function VersionsPanel({
  versions,
  selectedVersionId,
  onVersionSelect,
  onRollback,
  className = '',
}: VersionsPanelProps) {
  const [expandedVersion, setExpandedVersion] = useState<string | null>(null)

  const handleRollback = async (e: React.MouseEvent, versionId: string) => {
    e.stopPropagation()
    if (confirm('Roll back to this version? This will create a new version.')) {
      onRollback(versionId)
    }
  }

  return (
    <div className={`flex flex-col h-full bg-white ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between px-4 py-3 border-b border-gray-200">
        <div className="flex items-center gap-2">
          <Clock className="h-4 w-4 text-gray-500" />
          <span className="text-sm font-semibold text-gray-900">Versions</span>
          <span className="text-xs text-gray-700 bg-gray-100 px-2 py-0.5 rounded-full">
            {versions.length}
          </span>
        </div>
      </div>

      {/* Versions List */}
      <div className="flex-1 overflow-y-auto">
        {versions.length === 0 ? (
          <div className="flex flex-col items-center justify-center h-full p-4 text-center">
            <div className="h-12 w-12 rounded-xl bg-gray-100 flex items-center justify-center mb-3">
              <Clock className="h-6 w-6 text-gray-500" />
            </div>
            <p className="text-sm text-gray-900 font-medium">No versions yet</p>
            <p className="text-xs text-gray-700 mt-1">
              Start a chat to create your first version
            </p>
          </div>
        ) : (
          <div className="p-2 space-y-1">
            {versions.map((version, index) => {
              const Icon = sourceIcons[version.source] || Sparkles
              const isSelected = selectedVersionId === version.id
              const isLatest = index === 0
              const isExpanded = expandedVersion === version.id

              return (
                <div key={version.id}>
                  <button
                    onClick={() => onVersionSelect(version.id)}
                    className={cn(
                      'w-full text-left rounded-lg transition-all',
                      isSelected
                        ? 'bg-gray-100 border border-gray-200'
                        : 'hover:bg-gray-50 border border-transparent'
                    )}
                  >
                    {/* Main row */}
                    <div className="flex items-center gap-3 px-3 py-2.5">
                      <div className={cn('p-1.5 rounded-md', sourceColors[version.source])}>
                        <Icon className="h-3.5 w-3.5" />
                      </div>

                      <div className="flex-1 min-w-0">
                        <div className="flex items-center gap-2">
                          <span className="text-sm font-medium text-gray-900">
                            v{version.version_number}
                          </span>
                          {isLatest && (
                            <span className="text-[10px] px-1.5 py-0.5 bg-green-50 
                                           text-green-700 border border-green-200 rounded-full font-medium">
                              Latest
                            </span>
                          )}
                          {isSelected && (
                            <Check className="h-3.5 w-3.5 text-gray-700" />
                          )}
                        </div>
                        <div className="flex items-center gap-2 mt-0.5">
                          <span className="text-[10px] text-gray-700">
                            {sourceLabels[version.source]}
                          </span>
                          <span className="text-[10px] text-gray-500">•</span>
                          <span className="text-[10px] text-gray-700">
                            {formatDistanceToNow(new Date(version.created_at), { addSuffix: true })}
                          </span>
                        </div>
                      </div>

                      <button
                        onClick={(e) => {
                          e.stopPropagation()
                          setExpandedVersion(isExpanded ? null : version.id)
                        }}
                        className="p-1 text-gray-400 hover:text-gray-600 rounded-md 
                                 hover:bg-gray-200 transition-colors"
                      >
                        <ChevronDown 
                          className={cn(
                            'h-4 w-4 transition-transform',
                            isExpanded && 'rotate-180'
                          )} 
                        />
                      </button>
                    </div>

                    {/* Expanded details */}
                    {isExpanded && (
                      <div className="px-3 pb-3 pt-1 border-t border-gray-100 mt-1">
                        {/* Author */}
                        <div className="flex items-center gap-2 text-xs text-gray-700 mb-2">
                          <User className="h-3 w-3" />
                          <span>{version.created_by_email || 'Unknown'}</span>
                        </div>

                        {/* Spec preview */}
                        {version.spec_json && (
                          <div className="mb-3">
                            <span className="text-[10px] text-gray-600 uppercase tracking-wider">
                              App Spec
                            </span>
                            <div className="mt-1 p-2 bg-gray-50 border border-gray-200 rounded-md text-[10px] 
                                          font-mono text-gray-800 max-h-24 overflow-hidden">
                              {JSON.stringify(version.spec_json, null, 2).slice(0, 200)}...
                            </div>
                          </div>
                        )}

                        {/* Actions */}
                        {!isLatest && (
                          <button
                            onClick={(e) => handleRollback(e, version.id)}
                            className="flex items-center gap-1.5 px-2.5 py-1.5 text-xs
                                     text-yellow-700 bg-yellow-50 hover:bg-yellow-100 border border-yellow-200
                                     rounded-md transition-colors"
                          >
                            <RotateCcw className="h-3 w-3" />
                            Rollback to this version
                          </button>
                        )}
                      </div>
                    )}
                  </button>
                </div>
              )
            })}
          </div>
        )}
      </div>
    </div>
  )
}
