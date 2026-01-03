/**
 * Chat Panel Component
 * 
 * AI chat interface for building internal apps.
 * Supports streaming responses with real-time updates.
 */
import { useState, useRef, useEffect, useCallback } from 'react'
import { Send, Loader2, Sparkles, Code2, CheckCircle, XCircle } from 'lucide-react'
import { motion, AnimatePresence } from 'framer-motion'
import { ModelSelector } from './ModelSelector'
import { 
  generateCodeStreaming, 
  applyGeneratedCode,
  type ChatMessage,
  type StreamEvent 
} from '../../services/aiService'

interface ChatPanelProps {
  appId: string
  sessionId: string | null
  onSessionChange: (sessionId: string) => void
  onVersionCreated: (versionId: string, versionNumber: number) => void
  className?: string
}

interface LocalMessage extends ChatMessage {
  isStreaming?: boolean
  streamContent?: string
}

export function ChatPanel({
  appId,
  sessionId,
  onSessionChange,
  onVersionCreated,
  className = '',
}: ChatPanelProps) {
  const [messages, setMessages] = useState<LocalMessage[]>([])
  const [input, setInput] = useState('')
  const [isLoading, setIsLoading] = useState(false)
  const [selectedModel, setSelectedModel] = useState('anthropic/claude-3.5-sonnet')
  const [abortController, setAbortController] = useState<AbortController | null>(null)
  
  const messagesEndRef = useRef<HTMLDivElement>(null)
  const inputRef = useRef<HTMLTextAreaElement>(null)

  // Auto-scroll to bottom
  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' })
  }, [messages])

  // Handle streaming events
  const handleStreamEvent = useCallback((event: StreamEvent) => {
    switch (event.type) {
      case 'session_created':
        onSessionChange(event.data.session_id)
        break
        
      case 'user_message':
        setMessages(prev => [...prev, {
          id: event.data.id,
          role: 'user',
          content: event.data.content,
          status: 'complete',
          created_at: new Date().toISOString(),
        }])
        break
        
      case 'assistant_start':
        setMessages(prev => [...prev, {
          id: event.data.id,
          role: 'assistant',
          content: '',
          status: 'streaming',
          model_id: event.data.model,
          created_at: new Date().toISOString(),
          isStreaming: true,
          streamContent: '',
        }])
        break
        
      case 'content':
        setMessages(prev => {
          const updated = [...prev]
          const lastMsg = updated[updated.length - 1]
          if (lastMsg?.isStreaming) {
            lastMsg.streamContent = (lastMsg.streamContent || '') + event.data.chunk
            lastMsg.content = lastMsg.streamContent
          }
          return updated
        })
        break
        
      case 'thinking':
        // Could show thinking indicator
        break
        
      case 'generation_complete':
        setMessages(prev => {
          const updated = [...prev]
          const lastMsg = updated[updated.length - 1]
          if (lastMsg?.isStreaming) {
            lastMsg.status = 'complete'
            lastMsg.isStreaming = false
          }
          return updated
        })
        break
        
      case 'version_created':
        onVersionCreated(event.data.version_id, event.data.version_number)
        setMessages(prev => {
          const updated = [...prev]
          const lastMsg = updated[updated.length - 1]
          if (lastMsg) {
            lastMsg.version_created = event.data.version_id
          }
          return updated
        })
        break
        
      case 'error':
        setMessages(prev => {
          const updated = [...prev]
          const lastMsg = updated[updated.length - 1]
          if (lastMsg?.isStreaming) {
            lastMsg.status = 'error'
            lastMsg.error_message = event.data.message
            lastMsg.isStreaming = false
          }
          return updated
        })
        break
    }
  }, [onSessionChange, onVersionCreated])

  const handleSubmit = async () => {
    if (!input.trim() || isLoading) return
    
    const message = input.trim()
    setInput('')
    setIsLoading(true)
    
    const controller = generateCodeStreaming(appId, message, {
      sessionId: sessionId || undefined,
      model: selectedModel,
      mode: 'appspec',
      onEvent: handleStreamEvent,
      onComplete: () => {
        setIsLoading(false)
        setAbortController(null)
      },
      onError: (error) => {
        console.error('Stream error:', error)
        setIsLoading(false)
        setAbortController(null)
        setMessages(prev => [...prev, {
          id: Date.now().toString(),
          role: 'assistant',
          content: `Error: ${error.message}`,
          status: 'error',
          created_at: new Date().toISOString(),
        }])
      },
    })
    
    setAbortController(controller)
  }

  const handleCancel = () => {
    abortController?.abort()
    setIsLoading(false)
    setAbortController(null)
  }

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault()
      handleSubmit()
    }
  }

  const handleApplyCode = async (messageId: string) => {
    try {
      const result = await applyGeneratedCode(messageId)
      onVersionCreated(result.version_id, result.version_number)
    } catch (error) {
      console.error('Failed to apply code:', error)
    }
  }

  return (
    <div className={`flex flex-col h-full bg-zinc-950 ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between px-4 py-3 border-b border-zinc-800/50">
        <div className="flex items-center gap-2">
          <Sparkles className="h-4 w-4 text-violet-400" />
          <span className="text-sm font-semibold text-zinc-200">AI Builder</span>
        </div>
        <ModelSelector
          selectedModel={selectedModel}
          onModelChange={setSelectedModel}
        />
      </div>

      {/* Messages */}
      <div className="flex-1 overflow-y-auto p-4 space-y-4">
        {messages.length === 0 && (
          <div className="flex flex-col items-center justify-center h-full text-center">
            <div className="w-16 h-16 rounded-2xl bg-gradient-to-br from-violet-500/20 to-fuchsia-500/20 
                          flex items-center justify-center mb-4">
              <Code2 className="h-8 w-8 text-violet-400" />
            </div>
            <h3 className="text-lg font-semibold text-zinc-200 mb-2">
              Start Building
            </h3>
            <p className="text-sm text-zinc-500 max-w-xs">
              Describe the internal app you want to build. I'll generate the UI and connect it to your data.
            </p>
            <div className="mt-6 flex flex-wrap gap-2 justify-center">
              {[
                'Create a user management dashboard',
                'Build an order tracking view',
                'Design a CRM for customers',
              ].map((suggestion) => (
                <button
                  key={suggestion}
                  onClick={() => setInput(suggestion)}
                  className="px-3 py-1.5 text-xs text-zinc-400 bg-zinc-800/50 rounded-full
                           hover:bg-zinc-800 hover:text-zinc-300 transition-all"
                >
                  {suggestion}
                </button>
              ))}
            </div>
          </div>
        )}

        <AnimatePresence mode="popLayout">
          {messages.map((message) => (
            <motion.div
              key={message.id}
              initial={{ opacity: 0, y: 10 }}
              animate={{ opacity: 1, y: 0 }}
              exit={{ opacity: 0, y: -10 }}
              className={`flex ${message.role === 'user' ? 'justify-end' : 'justify-start'}`}
            >
              <div
                className={`max-w-[85%] rounded-2xl px-4 py-3 ${
                  message.role === 'user'
                    ? 'bg-violet-600 text-white'
                    : 'bg-zinc-800/50 border border-zinc-700/50'
                }`}
              >
                {message.role === 'assistant' && (
                  <div className="flex items-center gap-2 mb-2 text-xs text-zinc-500">
                    <Sparkles className="h-3 w-3 text-violet-400" />
                    <span>{message.model_id?.split('/')[1] || 'AI'}</span>
                    {message.isStreaming && (
                      <Loader2 className="h-3 w-3 animate-spin text-violet-400" />
                    )}
                    {message.status === 'complete' && !message.isStreaming && (
                      <CheckCircle className="h-3 w-3 text-emerald-400" />
                    )}
                    {message.status === 'error' && (
                      <XCircle className="h-3 w-3 text-red-400" />
                    )}
                  </div>
                )}
                
                <div className="text-sm whitespace-pre-wrap">
                  {message.content || (
                    <span className="text-zinc-500 italic">Generating...</span>
                  )}
                </div>

                {message.role === 'assistant' && message.status === 'complete' && (
                  <div className="mt-3 pt-3 border-t border-zinc-700/50 flex items-center gap-2">
                    {message.version_created ? (
                      <span className="text-xs text-emerald-400 flex items-center gap-1">
                        <CheckCircle className="h-3 w-3" />
                        Applied as version
                      </span>
                    ) : message.generated_spec_json ? (
                      <button
                        onClick={() => handleApplyCode(message.id)}
                        className="text-xs px-3 py-1 bg-violet-600 hover:bg-violet-500 
                                 text-white rounded-full transition-colors"
                      >
                        Apply Changes
                      </button>
                    ) : null}
                  </div>
                )}

                {message.error_message && (
                  <div className="mt-2 p-2 bg-red-500/10 border border-red-500/20 rounded-lg">
                    <p className="text-xs text-red-400">{message.error_message}</p>
                  </div>
                )}
              </div>
            </motion.div>
          ))}
        </AnimatePresence>

        <div ref={messagesEndRef} />
      </div>

      {/* Input */}
      <div className="p-4 border-t border-zinc-800/50">
        <div className="relative">
          <textarea
            ref={inputRef}
            value={input}
            onChange={(e) => setInput(e.target.value)}
            onKeyDown={handleKeyDown}
            placeholder="Describe what you want to build..."
            rows={3}
            disabled={isLoading}
            className="w-full px-4 py-3 pr-24 bg-zinc-900 border border-zinc-700/50 rounded-xl
                     text-sm text-zinc-200 placeholder-zinc-500 resize-none
                     focus:outline-none focus:border-violet-500/50 focus:ring-1 focus:ring-violet-500/25
                     disabled:opacity-50"
          />
          
          <div className="absolute right-2 bottom-2 flex items-center gap-2">
            {isLoading ? (
              <button
                onClick={handleCancel}
                className="p-2 bg-zinc-700 hover:bg-zinc-600 rounded-lg transition-colors"
              >
                <XCircle className="h-4 w-4 text-zinc-300" />
              </button>
            ) : (
              <button
                onClick={handleSubmit}
                disabled={!input.trim()}
                className="p-2 bg-violet-600 hover:bg-violet-500 disabled:opacity-50 
                         disabled:cursor-not-allowed rounded-lg transition-colors"
              >
                <Send className="h-4 w-4 text-white" />
              </button>
            )}
          </div>
        </div>
        
        <p className="mt-2 text-[10px] text-zinc-600 text-center">
          Press Enter to send, Shift+Enter for new line
        </p>
      </div>
    </div>
  )
}

