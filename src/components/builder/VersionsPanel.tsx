/**
 * Versions Panel Component
 * 
 * Displays version history with diffs and rollback capabilities.
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
  User
} from 'lucide-react'
import { motion, AnimatePresence } from 'framer-motion'
import { formatDistanceToNow } from 'date-fns'
import type { AppVersion } from '../../types/models'

interface VersionsPanelProps {
  versions: AppVersion[]
  selectedVersionId: string | null
  onVersionSelect: (versionId: string) => void
  onRollback: (versionId: string) => void
  className?: string
}

const sourceIcons = {
  ai: Sparkles,
  code: Code2,
  publish: Upload,
}

const sourceLabels = {
  ai: 'AI Generated',
  code: 'Code Edit',
  publish: 'Published',
}

const sourceColors = {
  ai: 'text-violet-400 bg-violet-500/10',
  code: 'text-amber-400 bg-amber-500/10',
  publish: 'text-emerald-400 bg-emerald-500/10',
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
    <div className={`flex flex-col h-full bg-zinc-950 ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between px-4 py-3 border-b border-zinc-800/50">
        <div className="flex items-center gap-2">
          <Clock className="h-4 w-4 text-blue-400" />
          <span className="text-sm font-semibold text-zinc-200">Versions</span>
          <span className="text-xs text-zinc-500 bg-zinc-800/50 px-2 py-0.5 rounded-full">
            {versions.length}
          </span>
        </div>
      </div>

      {/* Versions List */}
      <div className="flex-1 overflow-y-auto">
        {versions.length === 0 ? (
          <div className="flex flex-col items-center justify-center h-full p-4 text-center">
            <Clock className="h-10 w-10 text-zinc-700 mb-3" />
            <p className="text-sm text-zinc-500">No versions yet</p>
            <p className="text-xs text-zinc-600 mt-1">
              Start a chat to create your first version
            </p>
          </div>
        ) : (
          <div className="p-2 space-y-1">
            <AnimatePresence mode="popLayout">
              {versions.map((version, index) => {
                const Icon = sourceIcons[version.source] || Sparkles
                const isSelected = selectedVersionId === version.id
                const isLatest = index === 0
                const isExpanded = expandedVersion === version.id

                return (
                  <motion.div
                    key={version.id}
                    initial={{ opacity: 0, y: -10 }}
                    animate={{ opacity: 1, y: 0 }}
                    exit={{ opacity: 0, y: -10 }}
                    layout
                  >
                    <button
                      onClick={() => onVersionSelect(version.id)}
                      className={`w-full text-left rounded-lg transition-all ${
                        isSelected
                          ? 'bg-zinc-800 border border-zinc-700'
                          : 'hover:bg-zinc-800/50 border border-transparent'
                      }`}
                    >
                      {/* Main row */}
                      <div className="flex items-center gap-3 px-3 py-2.5">
                        <div className={`p-1.5 rounded-md ${sourceColors[version.source]}`}>
                          <Icon className="h-3.5 w-3.5" />
                        </div>

                        <div className="flex-1 min-w-0">
                          <div className="flex items-center gap-2">
                            <span className="text-sm font-medium text-zinc-200">
                              v{version.version_number}
                            </span>
                            {isLatest && (
                              <span className="text-[10px] px-1.5 py-0.5 bg-emerald-500/20 
                                             text-emerald-400 rounded-full font-medium">
                                Latest
                              </span>
                            )}
                            {isSelected && (
                              <Check className="h-3.5 w-3.5 text-violet-400" />
                            )}
                          </div>
                          <div className="flex items-center gap-2 mt-0.5">
                            <span className="text-[10px] text-zinc-500">
                              {sourceLabels[version.source]}
                            </span>
                            <span className="text-[10px] text-zinc-600">•</span>
                            <span className="text-[10px] text-zinc-500">
                              {formatDistanceToNow(new Date(version.created_at), { addSuffix: true })}
                            </span>
                          </div>
                        </div>

                        <button
                          onClick={(e) => {
                            e.stopPropagation()
                            setExpandedVersion(isExpanded ? null : version.id)
                          }}
                          className="p-1 text-zinc-500 hover:text-zinc-300 rounded-md 
                                   hover:bg-zinc-700/50 transition-colors"
                        >
                          <ChevronDown 
                            className={`h-4 w-4 transition-transform ${
                              isExpanded ? 'rotate-180' : ''
                            }`} 
                          />
                        </button>
                      </div>

                      {/* Expanded details */}
                      <AnimatePresence>
                        {isExpanded && (
                          <motion.div
                            initial={{ height: 0, opacity: 0 }}
                            animate={{ height: 'auto', opacity: 1 }}
                            exit={{ height: 0, opacity: 0 }}
                            className="overflow-hidden"
                          >
                            <div className="px-3 pb-3 pt-1 border-t border-zinc-800/50 mt-1">
                              {/* Author */}
                              <div className="flex items-center gap-2 text-xs text-zinc-500 mb-2">
                                <User className="h-3 w-3" />
                                <span>{version.created_by_email || 'Unknown'}</span>
                              </div>

                              {/* Spec preview */}
                              {version.spec_json && (
                                <div className="mb-3">
                                  <span className="text-[10px] text-zinc-600 uppercase tracking-wider">
                                    App Spec
                                  </span>
                                  <div className="mt-1 p-2 bg-zinc-900 rounded-md text-[10px] 
                                                font-mono text-zinc-400 max-h-24 overflow-hidden">
                                    {JSON.stringify(version.spec_json, null, 2).slice(0, 200)}...
                                  </div>
                                </div>
                              )}

                              {/* Actions */}
                              {!isLatest && (
                                <button
                                  onClick={(e) => handleRollback(e, version.id)}
                                  className="flex items-center gap-1.5 px-2.5 py-1.5 text-xs
                                           text-amber-400 bg-amber-500/10 hover:bg-amber-500/20
                                           rounded-md transition-colors"
                                >
                                  <RotateCcw className="h-3 w-3" />
                                  Rollback to this version
                                </button>
                              )}
                            </div>
                          </motion.div>
                        )}
                      </AnimatePresence>
                    </button>
                  </motion.div>
                )
              })}
            </AnimatePresence>
          </div>
        )}
      </div>
    </div>
  )
}

