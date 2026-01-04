/**
 * Model Selector Component
 * 
 * Allows users to select AI models for code generation.
 * Groups models by category with cost and capability info.
 * Light enterprise theme.
 */
import { useState, useEffect } from 'react'
import { ChevronDown, Check } from 'lucide-react'
import { getAvailableModels, type AIModel } from '../../services/aiService'
import { cn } from '../../lib/utils'

type SelectorSize = 'sm' | 'md'
type DropdownPlacement = 'up' | 'down'

interface ModelSelectorProps {
  selectedModel: string
  onModelChange: (modelId: string) => void
  className?: string
  size?: SelectorSize
  placement?: DropdownPlacement
}

export function ModelSelector({
  selectedModel,
  onModelChange,
  className = '',
  size = 'md',
  placement = 'down',
}: ModelSelectorProps) {
  const [isOpen, setIsOpen] = useState(false)
  const [models, setModels] = useState<AIModel[]>([])
  const [grouped, setGrouped] = useState<Record<string, AIModel[]>>({})
  useEffect(() => {
    async function loadModels() {
      try {
        const data = await getAvailableModels()
        setModels(data.models)
        setGrouped(data.grouped)
      } catch (error) {
        console.error('Failed to load models:', error)
      }
    }
    loadModels()
  }, [])

  const selectedModelData = models.find(m => m.id === selectedModel)

  const sizeStyles: Record<SelectorSize, {
    button: string
    chevron: string
    item: string
    itemText: string
    meta: string
  }> = {
    sm: {
      button: 'px-2.5 py-1.5 text-xs',
      chevron: 'h-3.5 w-3.5',
      item: 'px-3 py-1.5',
      itemText: 'text-xs',
      meta: 'text-[10px]',
    },
    md: {
      button: 'px-3 py-2 text-sm',
      chevron: 'h-4 w-4',
      item: 'px-3 py-2.5',
      itemText: 'text-sm',
      meta: 'text-[10px]',
    },
  }

  const s = sizeStyles[size]
  const placementClasses =
    placement === 'up'
      ? 'bottom-full mb-2 origin-bottom'
      : 'top-full mt-2 origin-top'

  return (
    <div className={`relative ${className}`}>
      <button
        onClick={() => setIsOpen(!isOpen)}
        className={cn(
          'flex items-center rounded-2xl gap-1 bg-white border border-gray-200 hover:border-gray-300 transition-all',
          s.button
        )}
      >
        {selectedModelData ? (
          <span className={cn('text-gray-900 font-medium', s.itemText)}>
            {selectedModelData.name}
          </span>
        ) : (
          <span className={cn('text-gray-700', s.itemText)}>Select Model</span>
        )}
        <ChevronDown className={cn(s.chevron, 'text-gray-400 transition-transform', isOpen && 'rotate-180')} />
      </button>

      {isOpen && (
        <>
          {/* Backdrop */}
          <div 
            className="fixed inset-0 z-40" 
            onClick={() => setIsOpen(false)} 
          />
          
          {/* Dropdown */}
          <div
            className={cn(
              'absolute left-0 w-80 bg-white border border-gray-200 rounded-xl shadow-lg z-50 overflow-hidden',
              placementClasses
            )}
          >
            <div className="p-2 max-h-96 overflow-y-auto">
              {(['premium', 'standard', 'economy'] as const).map((category) => {
                const categoryModels = grouped[category] || []

                if (categoryModels.length === 0) return null

                return (
                  <div key={category} className="mb-1 last:mb-0">
                    <div className="space-y-0.5">
                      {categoryModels.map((model) => (
                        <button
                          key={model.id}
                          onClick={() => {
                            onModelChange(model.id)
                            setIsOpen(false)
                          }}
                          className={cn(
                            'w-full flex items-start gap-3 rounded-lg transition-all',
                            s.item,
                            selectedModel === model.id 
                              ? 'bg-gray-100 border border-gray-200' 
                              : 'hover:bg-gray-50 border border-transparent'
                          )}
                        >
                          <div className="flex-1 text-left">
                            <div className="flex items-center gap-2">
                              <span className={cn('font-medium text-gray-900', s.itemText)}>
                                {model.name}
                              </span>
                              {selectedModel === model.id && (
                                <Check className={cn(s.chevron, 'text-green-600')} />
                              )}
                            </div>
                            <p className={cn('text-gray-700 mt-0.5', size === 'sm' ? 'text-[11px]' : 'text-xs')}>
                              {model.description}
                            </p>
                            <div className="flex items-center gap-3 mt-1.5">
                              <span className={cn('text-gray-600', s.meta)}>
                                {(model.context_length / 1000).toFixed(0)}K context
                              </span>
                              <span className={cn('text-gray-600', s.meta)}>
                                ${model.cost.input}/${model.cost.output} per 1M tokens
                              </span>
                            </div>
                          </div>
                        </button>
                      ))}
                    </div>
                  </div>
                )
              })}
            </div>
          </div>
        </>
      )}
    </div>
  )
}
