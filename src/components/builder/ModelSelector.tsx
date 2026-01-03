/**
 * Model Selector Component
 * 
 * Allows users to select AI models for code generation.
 * Groups models by category with cost and capability info.
 * Light enterprise theme.
 */
import { useState, useEffect } from 'react'
import { ChevronDown, Sparkles, Zap, DollarSign, Check } from 'lucide-react'
import { getAvailableModels, type AIModel } from '../../services/aiService'
import { cn } from '../../lib/utils'

interface ModelSelectorProps {
  selectedModel: string
  onModelChange: (modelId: string) => void
  className?: string
}

const categoryConfig = {
  premium: {
    label: 'Premium',
    icon: Sparkles,
    color: 'text-yellow-700',
    bgColor: 'bg-yellow-50 border-yellow-200',
    description: 'Best for complex apps',
  },
  standard: {
    label: 'Standard',
    icon: Zap,
    color: 'text-green-700',
    bgColor: 'bg-green-50 border-green-200',
    description: 'Great balance',
  },
  economy: {
    label: 'Economy',
    icon: DollarSign,
    color: 'text-blue-700',
    bgColor: 'bg-blue-50 border-blue-200',
    description: 'Cost effective',
  },
}

export function ModelSelector({ selectedModel, onModelChange, className = '' }: ModelSelectorProps) {
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

  return (
    <div className={`relative ${className}`}>
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="flex items-center gap-2 px-3 py-2 rounded-md bg-white border border-gray-200 
                   hover:border-gray-300 transition-all text-sm"
      >
        {selectedModelData ? (
          <>
            {(() => {
              const config = categoryConfig[selectedModelData.category]
              const Icon = config.icon
              return <Icon className={cn('h-4 w-4', config.color)} />
            })()}
            <span className="text-gray-900 font-medium">
              {selectedModelData.name}
            </span>
          </>
        ) : (
          <span className="text-gray-700">Select Model</span>
        )}
        <ChevronDown className={cn('h-4 w-4 text-gray-400 transition-transform', isOpen && 'rotate-180')} />
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
            className="absolute top-full left-0 mt-2 w-80 bg-white border border-gray-200 
                       rounded-lg shadow-lg z-50 overflow-hidden"
          >
            <div className="p-2 max-h-96 overflow-y-auto">
              {(['premium', 'standard', 'economy'] as const).map((category) => {
                const config = categoryConfig[category]
                const categoryModels = grouped[category] || []
                const Icon = config.icon

                if (categoryModels.length === 0) return null

                return (
                  <div key={category} className="mb-3 last:mb-0">
                    <div className={cn('flex items-center gap-2 px-3 py-2 rounded-md mb-1 border', config.bgColor)}>
                      <Icon className={cn('h-4 w-4', config.color)} />
                      <span className={cn('text-xs font-semibold uppercase tracking-wider', config.color)}>
                        {config.label}
                      </span>
                      <span className="text-xs text-gray-700 ml-auto">
                        {config.description}
                      </span>
                    </div>
                    
                    <div className="space-y-0.5">
                      {categoryModels.map((model) => (
                        <button
                          key={model.id}
                          onClick={() => {
                            onModelChange(model.id)
                            setIsOpen(false)
                          }}
                          className={cn(
                            'w-full flex items-start gap-3 px-3 py-2.5 rounded-md transition-all',
                            selectedModel === model.id 
                              ? 'bg-gray-100 border border-gray-200' 
                              : 'hover:bg-gray-50 border border-transparent'
                          )}
                        >
                          <div className="flex-1 text-left">
                            <div className="flex items-center gap-2">
                              <span className="text-sm font-medium text-gray-900">
                                {model.name}
                              </span>
                              {selectedModel === model.id && (
                                <Check className="h-4 w-4 text-green-600" />
                              )}
                            </div>
                            <p className="text-xs text-gray-700 mt-0.5">
                              {model.description}
                            </p>
                            <div className="flex items-center gap-3 mt-1.5">
                              <span className="text-[10px] text-gray-600">
                                {(model.context_length / 1000).toFixed(0)}K context
                              </span>
                              <span className="text-[10px] text-gray-600">
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
