/**
 * Code Editor Component
 * 
 * Monaco-based code editor for viewing and editing generated code.
 * Supports syntax highlighting, diff view, and multiple files.
 */
import { useState, useMemo } from 'react'
import Editor, { DiffEditor } from '@monaco-editor/react'
import { 
  FileCode2, 
  Copy, 
  Check, 
  ChevronRight, 
  Download,
  GitCompare,
  Edit3
} from 'lucide-react'

interface VersionFile {
  id: string
  path: string
  content: string
}

interface CodeEditorProps {
  files: VersionFile[]
  previousFiles?: VersionFile[]
  onFileChange?: (path: string, content: string) => void
  readOnly?: boolean
  className?: string
}

const fileIcons: Record<string, string> = {
  tsx: '⚛️',
  ts: '📘',
  jsx: '⚛️',
  js: '📙',
  css: '🎨',
  json: '📋',
  md: '📝',
}

const getLanguage = (path: string): string => {
  const ext = path.split('.').pop() || ''
  const langMap: Record<string, string> = {
    tsx: 'typescript',
    ts: 'typescript',
    jsx: 'javascript',
    js: 'javascript',
    css: 'css',
    json: 'json',
    md: 'markdown',
    html: 'html',
  }
  return langMap[ext] || 'plaintext'
}

export function CodeEditor({
  files,
  previousFiles,
  onFileChange,
  readOnly = true,
  className = '',
}: CodeEditorProps) {
  const [selectedFile, setSelectedFile] = useState<string>(files[0]?.path || '')
  const [viewMode, setViewMode] = useState<'editor' | 'diff'>('editor')
  const [copied, setCopied] = useState(false)

  const currentFile = useMemo(
    () => files.find(f => f.path === selectedFile),
    [files, selectedFile]
  )

  const previousFile = useMemo(
    () => previousFiles?.find(f => f.path === selectedFile),
    [previousFiles, selectedFile]
  )

  const handleCopy = async () => {
    if (currentFile) {
      await navigator.clipboard.writeText(currentFile.content)
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    }
  }

  const handleDownload = () => {
    if (currentFile) {
      const blob = new Blob([currentFile.content], { type: 'text/plain' })
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = currentFile.path.split('/').pop() || 'file'
      document.body.appendChild(a)
      a.click()
      document.body.removeChild(a)
      URL.revokeObjectURL(url)
    }
  }

  const editorOptions = {
    readOnly,
    minimap: { enabled: false },
    fontSize: 13,
    fontFamily: 'JetBrains Mono, Menlo, Monaco, Consolas, monospace',
    lineNumbers: 'on' as const,
    scrollBeyondLastLine: false,
    wordWrap: 'on' as const,
    tabSize: 2,
    padding: { top: 16, bottom: 16 },
    renderLineHighlight: 'all' as const,
    bracketPairColorization: { enabled: true },
    smoothScrolling: true,
  }

  return (
    <div className={`flex flex-col h-full bg-zinc-950 ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between px-4 py-3 border-b border-zinc-800/50">
        <div className="flex items-center gap-2">
          <FileCode2 className="h-4 w-4 text-emerald-400" />
          <span className="text-sm font-semibold text-zinc-200">Code</span>
          <span className="text-xs text-zinc-500 bg-zinc-800/50 px-2 py-0.5 rounded-full">
            {files.length} files
          </span>
        </div>
        
        <div className="flex items-center gap-1">
          {previousFiles && (
            <button
              onClick={() => setViewMode(viewMode === 'diff' ? 'editor' : 'diff')}
              className={`p-1.5 rounded-md transition-colors ${
                viewMode === 'diff'
                  ? 'bg-emerald-500/20 text-emerald-400'
                  : 'text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800'
              }`}
              title="Toggle diff view"
            >
              <GitCompare className="h-4 w-4" />
            </button>
          )}
          <button
            onClick={handleCopy}
            className="p-1.5 text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800 rounded-md transition-colors"
            title="Copy code"
          >
            {copied ? (
              <Check className="h-4 w-4 text-emerald-400" />
            ) : (
              <Copy className="h-4 w-4" />
            )}
          </button>
          <button
            onClick={handleDownload}
            className="p-1.5 text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800 rounded-md transition-colors"
            title="Download file"
          >
            <Download className="h-4 w-4" />
          </button>
        </div>
      </div>

      <div className="flex flex-1 overflow-hidden">
        {/* File Tree */}
        <div className="w-48 border-r border-zinc-800/50 overflow-y-auto">
          <div className="p-2 space-y-0.5">
            {files.map((file) => {
              const fileName = file.path.split('/').pop() || file.path
              const ext = fileName.split('.').pop() || ''
              const icon = fileIcons[ext] || '📄'
              const isSelected = selectedFile === file.path
              const hasChanges = previousFiles && 
                previousFiles.find(pf => pf.path === file.path)?.content !== file.content

              return (
                <button
                  key={file.id}
                  onClick={() => setSelectedFile(file.path)}
                  className={`w-full flex items-center gap-2 px-2 py-1.5 rounded-md text-left
                            transition-colors text-xs ${
                              isSelected
                                ? 'bg-zinc-800 text-zinc-200'
                                : 'text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800/50'
                            }`}
                >
                  <span>{icon}</span>
                  <span className="flex-1 truncate">{fileName}</span>
                  {hasChanges && (
                    <span className="w-1.5 h-1.5 rounded-full bg-amber-500" />
                  )}
                </button>
              )
            })}
          </div>
        </div>

        {/* Editor */}
        <div className="flex-1 flex flex-col">
          {/* File path breadcrumb */}
          {currentFile && (
            <div className="flex items-center gap-1 px-4 py-2 border-b border-zinc-800/50 text-xs text-zinc-500">
              {currentFile.path.split('/').map((part, i, arr) => (
                <span key={i} className="flex items-center gap-1">
                  {i > 0 && <ChevronRight className="h-3 w-3" />}
                  <span className={i === arr.length - 1 ? 'text-zinc-300' : ''}>
                    {part}
                  </span>
                </span>
              ))}
              {!readOnly && (
                <span className="ml-auto flex items-center gap-1 text-amber-500">
                  <Edit3 className="h-3 w-3" />
                  Editing
                </span>
              )}
            </div>
          )}

          {/* Monaco Editor */}
          <div className="flex-1">
            {currentFile ? (
              viewMode === 'diff' && previousFile ? (
                <DiffEditor
                  original={previousFile.content}
                  modified={currentFile.content}
                  language={getLanguage(currentFile.path)}
                  theme="vs-dark"
                  options={{
                    ...editorOptions,
                    renderSideBySide: false,
                  }}
                />
              ) : (
                <Editor
                  value={currentFile.content}
                  language={getLanguage(currentFile.path)}
                  theme="vs-dark"
                  options={editorOptions}
                  onChange={(value) => {
                    if (!readOnly && onFileChange && value !== undefined) {
                      onFileChange(currentFile.path, value)
                    }
                  }}
                />
              )
            ) : (
              <div className="flex items-center justify-center h-full text-zinc-500">
                <p className="text-sm">No file selected</p>
              </div>
            )}
          </div>
        </div>
      </div>
    </div>
  )
}

