/**
 * Publish Popover Component
 * 
 * A popover that shows the publish status, copy URL functionality,
 * and publish/republish actions for an internal app.
 */
import { useState, useRef, useEffect } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import {
  Rocket,
  CheckCircle,
  Link2,
  Copy,
  Check,
  Loader2,
  ExternalLink,
  Globe,
  Clock,
} from 'lucide-react'
import { Button } from '../ui/button'
import { cn } from '../../lib/utils'
import type { InternalApp } from '../../types/models'

interface PublishPopoverProps {
  app: InternalApp
  isPublishing: boolean
  canPublish: boolean
  onPublish: () => void
  className?: string
}

export function PublishPopover({
  app,
  isPublishing,
  canPublish,
  onPublish,
  className,
}: PublishPopoverProps) {
  const [isOpen, setIsOpen] = useState(false)
  const [copied, setCopied] = useState(false)
  const popoverRef = useRef<HTMLDivElement>(null)
  const triggerRef = useRef<HTMLButtonElement>(null)

  const isPublished = app.status === 'published' && app.published_url
  const publishedUrl = app.published_url
    ? `${window.location.origin}${app.published_url}`
    : null

  // Close popover when clicking outside
  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (
        popoverRef.current &&
        !popoverRef.current.contains(event.target as Node) &&
        triggerRef.current &&
        !triggerRef.current.contains(event.target as Node)
      ) {
        setIsOpen(false)
      }
    }

    if (isOpen) {
      document.addEventListener('mousedown', handleClickOutside)
    }

    return () => {
      document.removeEventListener('mousedown', handleClickOutside)
    }
  }, [isOpen])

  const handleCopyUrl = async () => {
    if (!publishedUrl) return
    try {
      await navigator.clipboard.writeText(publishedUrl)
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    } catch (error) {
      console.error('Failed to copy URL:', error)
    }
  }

  const handlePublish = () => {
    onPublish()
    // Keep popover open to show publishing state
  }

  const handleOpenPublishedApp = () => {
    if (publishedUrl) {
      window.open(publishedUrl, '_blank')
    }
  }

  const formatDate = (dateString: string | null) => {
    if (!dateString) return null
    const date = new Date(dateString)
    if (isNaN(date.getTime())) return null
    return date.toLocaleDateString('en-US', {
      month: 'short',
      day: 'numeric',
      year: 'numeric',
      hour: 'numeric',
      minute: '2-digit',
    })
  }

  return (
    <div className={cn('relative', className)}>
      {/* Trigger Button */}
      <Button
        ref={triggerRef}
        size="sm"
        className="gap-1.5 text-xs"
        onClick={() => setIsOpen(!isOpen)}
        disabled={isPublishing}
      >
        {isPublishing ? (
          <Loader2 className="h-3.5 w-3.5 animate-spin" />
        ) : isPublished ? (
          <Globe className="h-3.5 w-3.5" />
        ) : (
          <Rocket className="h-3.5 w-3.5" />
        )}
        {isPublishing ? 'Publishing...' : isPublished ? 'Published' : 'Publish'}
      </Button>

      {/* Popover */}
      <AnimatePresence>
        {isOpen && (
          <motion.div
            ref={popoverRef}
            initial={{ opacity: 0, y: -4, scale: 0.98 }}
            animate={{ opacity: 1, y: 0, scale: 1 }}
            exit={{ opacity: 0, y: -4, scale: 0.98 }}
            transition={{ duration: 0.15 }}
            className="absolute right-0 top-full mt-2 w-80 z-50"
          >
            <div className="bg-white rounded-lg border border-gray-200 shadow-lg overflow-hidden">
              {/* Header */}
              <div className="px-4 py-3 border-b border-gray-100 bg-gray-50">
                <div className="flex items-center gap-2">
                  {isPublished ? (
                    <>
                      <div className="w-2 h-2 rounded-full bg-green-500" />
                      <span className="text-sm font-medium text-gray-900">
                        Live
                      </span>
                    </>
                  ) : (
                    <>
                      <div className="w-2 h-2 rounded-full bg-gray-400" />
                      <span className="text-sm font-medium text-gray-900">
                        Draft
                      </span>
                    </>
                  )}
                </div>
                {isPublished && app.published_at && (
                  <div className="flex items-center gap-1.5 mt-1 text-xs text-gray-500">
                    <Clock className="h-3 w-3" />
                    <span>Published {formatDate(app.published_at)}</span>
                  </div>
                )}
              </div>

              {/* Content */}
              <div className="p-4 space-y-4">
                {/* Published URL (if published) */}
                {isPublished && publishedUrl && (
                  <div className="space-y-2">
                    <label className="text-xs font-medium text-gray-600 uppercase tracking-wide">
                      Live URL
                    </label>
                    <div className="flex items-center gap-2">
                      <div className="flex-1 bg-gray-50 border border-gray-200 rounded-md px-3 py-2 text-sm text-gray-700 truncate">
                        {publishedUrl}
                      </div>
                      <button
                        onClick={handleCopyUrl}
                        className="p-2 text-gray-500 hover:text-gray-700 hover:bg-gray-100 rounded-md transition-colors"
                        title={copied ? 'Copied!' : 'Copy URL'}
                      >
                        {copied ? (
                          <Check className="h-4 w-4 text-green-600" />
                        ) : (
                          <Copy className="h-4 w-4" />
                        )}
                      </button>
                      <button
                        onClick={handleOpenPublishedApp}
                        className="p-2 text-gray-500 hover:text-gray-700 hover:bg-gray-100 rounded-md transition-colors"
                        title="Open in new tab"
                      >
                        <ExternalLink className="h-4 w-4" />
                      </button>
                    </div>
                  </div>
                )}

                {/* Version info */}
                {isPublished && app.published_version_number && (
                  <div className="flex items-center gap-2 text-xs text-gray-500">
                    <CheckCircle className="h-3.5 w-3.5 text-green-600" />
                    <span>Version {app.published_version_number} is live</span>
                  </div>
                )}

                {/* Actions */}
                <div className="pt-2 border-t border-gray-100">
                  <Button
                    onClick={handlePublish}
                    disabled={!canPublish || isPublishing}
                    className="w-full gap-2"
                    size="sm"
                  >
                    {isPublishing ? (
                      <>
                        <Loader2 className="h-4 w-4 animate-spin" />
                        Publishing...
                      </>
                    ) : isPublished ? (
                      <>
                        <Rocket className="h-4 w-4" />
                        Republish Latest Version
                      </>
                    ) : (
                      <>
                        <Rocket className="h-4 w-4" />
                        Publish App
                      </>
                    )}
                  </Button>
                  {!canPublish && !isPublishing && (
                    <p className="mt-2 text-xs text-gray-500 text-center">
                      {app.status === 'draft'
                        ? 'Create at least one version before publishing'
                        : 'Latest version is still generating'}
                    </p>
                  )}
                </div>
              </div>
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  )
}

