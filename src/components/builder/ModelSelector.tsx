/**
 * Model Selector Component
 * 
 * Allows users to select AI models for code generation.
 * Groups models by category with cost and capability info.
 */
import { useState, useEffect } from 'react'
import { ChevronDown, Sparkles, Zap, DollarSign, Check } from 'lucide-react'
import { motion, AnimatePresence } from 'framer-motion'
import { getAvailableModels, type AIModel } from '../../services/aiService'

interface ModelSelectorProps {
  selectedModel: string
  onModelChange: (modelId: string) => void
  className?: string
}

const categoryConfig = {
  premium: {
    label: 'Premium',
    icon: Sparkles,
    color: 'text-amber-500',
    bgColor: 'bg-amber-500/10',
    description: 'Best for complex apps',
  },
  standard: {
    label: 'Standard',
    icon: Zap,
    color: 'text-emerald-500',
    bgColor: 'bg-emerald-500/10',
    description: 'Great balance',
  },
  economy: {
    label: 'Economy',
    icon: DollarSign,
    color: 'text-blue-500',
    bgColor: 'bg-blue-500/10',
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
        className="flex items-center gap-2 px-3 py-2 rounded-lg bg-zinc-800/50 border border-zinc-700/50 
                   hover:border-zinc-600 transition-all text-sm"
      >
        {selectedModelData ? (
          <>
            {(() => {
              const config = categoryConfig[selectedModelData.category]
              const Icon = config.icon
              return <Icon className={`h-4 w-4 ${config.color}`} />
            })()}
            <span className="text-zinc-200 font-medium">
              {selectedModelData.name}
            </span>
          </>
        ) : (
          <span className="text-zinc-400">Select Model</span>
        )}
        <ChevronDown className={`h-4 w-4 text-zinc-500 transition-transform ${isOpen ? 'rotate-180' : ''}`} />
      </button>

      <AnimatePresence>
        {isOpen && (
          <>
            {/* Backdrop */}
            <div 
              className="fixed inset-0 z-40" 
              onClick={() => setIsOpen(false)} 
            />
            
            {/* Dropdown */}
            <motion.div
              initial={{ opacity: 0, y: -10, scale: 0.95 }}
              animate={{ opacity: 1, y: 0, scale: 1 }}
              exit={{ opacity: 0, y: -10, scale: 0.95 }}
              transition={{ duration: 0.15 }}
              className="absolute top-full left-0 mt-2 w-80 bg-zinc-900 border border-zinc-700/50 
                         rounded-xl shadow-2xl z-50 overflow-hidden"
            >
              <div className="p-2 max-h-96 overflow-y-auto">
                {(['premium', 'standard', 'economy'] as const).map((category) => {
                  const config = categoryConfig[category]
                  const categoryModels = grouped[category] || []
                  const Icon = config.icon

                  if (categoryModels.length === 0) return null

                  return (
                    <div key={category} className="mb-3 last:mb-0">
                      <div className={`flex items-center gap-2 px-3 py-2 ${config.bgColor} rounded-lg mb-1`}>
                        <Icon className={`h-4 w-4 ${config.color}`} />
                        <span className={`text-xs font-semibold uppercase tracking-wider ${config.color}`}>
                          {config.label}
                        </span>
                        <span className="text-xs text-zinc-500 ml-auto">
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
                            className={`w-full flex items-start gap-3 px-3 py-2.5 rounded-lg transition-all
                                      ${selectedModel === model.id 
                                        ? 'bg-zinc-700/50 border border-zinc-600' 
                                        : 'hover:bg-zinc-800/50 border border-transparent'}`}
                          >
                            <div className="flex-1 text-left">
                              <div className="flex items-center gap-2">
                                <span className="text-sm font-medium text-zinc-200">
                                  {model.name}
                                </span>
                                {selectedModel === model.id && (
                                  <Check className="h-4 w-4 text-emerald-500" />
                                )}
                              </div>
                              <p className="text-xs text-zinc-500 mt-0.5">
                                {model.description}
                              </p>
                              <div className="flex items-center gap-3 mt-1.5">
                                <span className="text-[10px] text-zinc-600">
                                  {(model.context_length / 1000).toFixed(0)}K context
                                </span>
                                <span className="text-[10px] text-zinc-600">
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
            </motion.div>
          </>
        )}
      </AnimatePresence>
    </div>
  )
}

