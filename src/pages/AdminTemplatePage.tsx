/**
 * Admin Template Testing Page
 * 
 * Testing page for template components, dataStore, and MCP integrations.
 * Accessible at /admin/template
 */
import { useState } from 'react'
import {
  CheckCircle,
  XCircle,
  Loader2,
  Database,
  Plug,
  Palette,
  Clock,
  LayoutGrid,
  Bell,
  PanelRightOpen,
  AlertCircle,
  Plus,
  RefreshCw,
  ChevronRight,
} from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { useToast } from '@/components/ui/toast'
import {
  Drawer,
  DrawerContent,
  DrawerHeader,
  DrawerTitle,
  DrawerBody,
  DrawerFooter,
} from '@/components/ui/drawer'
import { cn } from '@/lib/utils'

// API base URL for runtime proxy testing
const API_BASE_URL = import.meta.env.VITE_API_URL || 'http://localhost:8001/api/v1'

/**
 * Get auth headers for authenticated requests
 */
function getAuthHeaders(): Record<string, string> {
  const token = localStorage.getItem('access_token')
  const headers: Record<string, string> = {
    'Content-Type': 'application/json',
  }
  if (token) {
    headers['Authorization'] = `Bearer ${token}`
  }
  return headers
}

/**
 * Runtime Data Proxy - calls the same endpoint that generated apps use
 * Includes auth token so backend can verify user has access to the app
 */
async function runtimeDataCall<T>(
  appId: string,
  operation: string,
  tableSlug: string | null = null,
  params: Record<string, unknown> = {}
): Promise<T> {
  const body: Record<string, unknown> = {
    appId,
    operation,
    params,
  }
  
  if (tableSlug) {
    body.tableSlug = tableSlug
  }
  
  const response = await fetch(`${API_BASE_URL}/runtime/data/`, {
    method: 'POST',
    headers: getAuthHeaders(),
    body: JSON.stringify(body),
  })
  
  const result = await response.json()
  
  if (!response.ok) {
    throw new Error(result.error || `Runtime data operation failed: ${operation}`)
  }
  
  return result as T
}

/**
 * Runtime Connector Proxy - calls the same endpoint that generated apps use for MCP
 * Includes auth token so backend can verify user has access to the app
 */
async function runtimeConnectorCall<T>(
  appId: string,
  connectorId: string,
  toolId: string,
  params: Record<string, unknown> = {}
): Promise<T> {
  const body = {
    appId,
    connectorId,
    toolId,
    params,
  }
  
  const response = await fetch(`${API_BASE_URL}/runtime/connectors/`, {
    method: 'POST',
    headers: getAuthHeaders(),
    body: JSON.stringify(body),
  })
  
  const result = await response.json()
  
  if (!response.ok) {
    throw new Error(result.error || `Runtime connector operation failed: ${toolId}`)
  }
  
  return result as T
}

// ============================================================================
// Types
// ============================================================================

interface TestResult {
  operation: string
  success: boolean
  duration: number
  error?: string
  data?: any
}

interface ConnectorTool {
  name: string
  description: string
  inputSchema?: Record<string, any>
}

// ============================================================================
// Component Showcase Sections
// ============================================================================

function ButtonShowcase() {
  const [loading, setLoading] = useState(false)

  const handleClick = () => {
    setLoading(true)
    setTimeout(() => setLoading(false), 1500)
  }

  return (
    <div className="bg-white rounded-lg border border-gray-200 p-6">
      <h3 className="font-medium text-gray-900 mb-4">Buttons</h3>
      <div className="flex flex-wrap gap-3">
        <Button>Default</Button>
        <Button variant="secondary">Secondary</Button>
        <Button variant="outline">Outline</Button>
        <Button variant="ghost">Ghost</Button>
        <Button variant="destructive">Destructive</Button>
        <Button variant="link">Link</Button>
      </div>
      <div className="flex flex-wrap gap-3 mt-4">
        <Button size="sm">Small</Button>
        <Button size="default">Default</Button>
        <Button size="lg">Large</Button>
        <Button size="icon"><Plus className="h-4 w-4" /></Button>
      </div>
      <div className="flex flex-wrap gap-3 mt-4">
        <Button onClick={handleClick} disabled={loading}>
          {loading && <Loader2 className="h-4 w-4 mr-2 animate-spin" />}
          {loading ? 'Loading...' : 'Click to Load'}
        </Button>
        <Button disabled>Disabled</Button>
      </div>
    </div>
  )
}

function InputShowcase() {
  const [value, setValue] = useState('')
  const [error, setError] = useState('')

  return (
    <div className="bg-white rounded-lg border border-gray-200 p-6">
      <h3 className="font-medium text-gray-900 mb-4">Form Inputs</h3>
      <div className="grid grid-cols-2 gap-4">
        <div>
          <label className="block text-sm font-medium text-gray-700 mb-1.5">Default Input</label>
          <Input
            placeholder="Enter text..."
            value={value}
            onChange={(e) => setValue(e.target.value)}
          />
        </div>
        <div>
          <label className="block text-sm font-medium text-gray-700 mb-1.5">With Error</label>
          <Input
            placeholder="Email address"
            value={error}
            onChange={(e) => setError(e.target.value)}
            className="border-red-300 focus:border-red-400"
          />
          <p className="mt-1 text-sm text-red-600">Please enter a valid email</p>
        </div>
        <div>
          <label className="block text-sm font-medium text-gray-700 mb-1.5">Disabled</label>
          <Input placeholder="Disabled input" disabled />
        </div>
        <div>
          <label className="block text-sm font-medium text-gray-700 mb-1.5">Read Only</label>
          <Input
            value="Read only value"
            readOnly
            className="bg-gray-50"
          />
        </div>
      </div>
    </div>
  )
}

function BadgeShowcase() {
  return (
    <div className="bg-white rounded-lg border border-gray-200 p-6">
      <h3 className="font-medium text-gray-900 mb-4">Badges & Status</h3>
      <div className="flex flex-wrap gap-3">
        <span className="flex items-center gap-1.5 text-xs px-2 py-1 rounded-full bg-gray-100 text-gray-600 border border-gray-200">
          Default
        </span>
        <span className="flex items-center gap-1.5 text-xs px-2 py-1 rounded-full bg-green-50 text-green-700 border border-green-200">
          <CheckCircle className="h-3 w-3" />
          Success
        </span>
        <span className="flex items-center gap-1.5 text-xs px-2 py-1 rounded-full bg-yellow-50 text-yellow-700 border border-yellow-200">
          <AlertCircle className="h-3 w-3" />
          Warning
        </span>
        <span className="flex items-center gap-1.5 text-xs px-2 py-1 rounded-full bg-red-50 text-red-700 border border-red-200">
          <XCircle className="h-3 w-3" />
          Error
        </span>
        <span className="flex items-center gap-1.5 text-xs px-2 py-1 rounded-full bg-blue-50 text-blue-700 border border-blue-200">
          Info
        </span>
      </div>
    </div>
  )
}

function CardShowcase() {
  return (
    <div className="bg-white rounded-lg border border-gray-200 p-6">
      <h3 className="font-medium text-gray-900 mb-4">Cards</h3>
      <div className="grid grid-cols-3 gap-4">
        {/* Basic Card */}
        <div className="bg-white rounded-lg border border-gray-200 p-5">
          <h4 className="font-medium text-gray-900 mb-2">Basic Card</h4>
          <p className="text-sm text-gray-500">Simple card with content.</p>
        </div>
        
        {/* Interactive Card */}
        <div className="bg-white rounded-lg border border-gray-200 p-5 hover:border-gray-300 hover:shadow-sm transition-all cursor-pointer">
          <h4 className="font-medium text-gray-900 mb-2">Interactive Card</h4>
          <p className="text-sm text-gray-500">Hover to see effect.</p>
        </div>
        
        {/* Stat Card */}
        <div className="bg-white rounded-lg border border-gray-200 p-5">
          <p className="text-sm font-medium text-gray-500 mb-1">Revenue</p>
          <p className="text-2xl font-semibold text-gray-900">$12,450</p>
          <div className="mt-2 flex items-center gap-1 text-green-600">
            <svg className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 10l7-7m0 0l7 7m-7-7v18" />
            </svg>
            <span className="text-sm font-medium">+12.5%</span>
            <span className="text-sm text-gray-500">vs last month</span>
          </div>
        </div>
      </div>
    </div>
  )
}

function TableShowcase() {
  const sampleData = [
    { id: '1', name: 'John Doe', email: 'john@example.com', status: 'Active' },
    { id: '2', name: 'Jane Smith', email: 'jane@example.com', status: 'Pending' },
    { id: '3', name: 'Bob Wilson', email: 'bob@example.com', status: 'Inactive' },
  ]

  return (
    <div className="bg-white rounded-lg border border-gray-200 p-6">
      <h3 className="font-medium text-gray-900 mb-4">Data Table</h3>
      <div className="bg-white rounded-lg border border-gray-200 overflow-hidden">
        <table className="w-full">
          <thead>
            <tr className="border-b border-gray-200 bg-gray-50">
              <th className="text-left text-xs font-medium text-gray-500 uppercase tracking-wider px-4 py-3">Name</th>
              <th className="text-left text-xs font-medium text-gray-500 uppercase tracking-wider px-4 py-3">Email</th>
              <th className="text-left text-xs font-medium text-gray-500 uppercase tracking-wider px-4 py-3">Status</th>
              <th className="text-right text-xs font-medium text-gray-500 uppercase tracking-wider px-4 py-3">Actions</th>
            </tr>
          </thead>
          <tbody className="divide-y divide-gray-200">
            {sampleData.map((row) => (
              <tr key={row.id} className="hover:bg-gray-50 transition-colors">
                <td className="px-4 py-3 text-sm text-gray-900">{row.name}</td>
                <td className="px-4 py-3 text-sm text-gray-500">{row.email}</td>
                <td className="px-4 py-3">
                  <span className={cn(
                    'text-xs px-2 py-1 rounded-full border',
                    row.status === 'Active' && 'bg-green-50 text-green-700 border-green-200',
                    row.status === 'Pending' && 'bg-yellow-50 text-yellow-700 border-yellow-200',
                    row.status === 'Inactive' && 'bg-gray-100 text-gray-600 border-gray-200'
                  )}>
                    {row.status}
                  </span>
                </td>
                <td className="px-4 py-3 text-right">
                  <Button variant="ghost" size="sm">Edit</Button>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  )
}

function ToastShowcase() {
  const { addToast } = useToast()

  return (
    <div className="bg-white rounded-lg border border-gray-200 p-6">
      <h3 className="font-medium text-gray-900 mb-4">Toast Notifications</h3>
      <div className="flex flex-wrap gap-3">
        <Button
          variant="outline"
          onClick={() => addToast({ type: 'success', title: 'Success!', description: 'Operation completed successfully.' })}
        >
          <CheckCircle className="h-4 w-4 mr-2 text-green-600" />
          Success Toast
        </Button>
        <Button
          variant="outline"
          onClick={() => addToast({ type: 'error', title: 'Error!', description: 'Something went wrong.' })}
        >
          <XCircle className="h-4 w-4 mr-2 text-red-600" />
          Error Toast
        </Button>
        <Button
          variant="outline"
          onClick={() => addToast({ type: 'warning', title: 'Warning', description: 'Please review this item.' })}
        >
          <AlertCircle className="h-4 w-4 mr-2 text-yellow-600" />
          Warning Toast
        </Button>
        <Button
          variant="outline"
          onClick={() => addToast({ type: 'info', title: 'Info', description: 'Here is some information.' })}
        >
          <Bell className="h-4 w-4 mr-2 text-blue-600" />
          Info Toast
        </Button>
      </div>
    </div>
  )
}

function DrawerShowcase() {
  const [open, setOpen] = useState(false)

  return (
    <div className="bg-white rounded-lg border border-gray-200 p-6">
      <h3 className="font-medium text-gray-900 mb-4">Drawer / Side Panel</h3>
      <Button variant="outline" onClick={() => setOpen(true)}>
        <PanelRightOpen className="h-4 w-4 mr-2" />
        Open Drawer
      </Button>
      
      <Drawer open={open} onOpenChange={setOpen}>
        <DrawerContent>
          <DrawerHeader>
            <DrawerTitle>Sample Drawer</DrawerTitle>
          </DrawerHeader>
          <DrawerBody>
            <p className="text-sm text-gray-600 mb-4">
              This is a sample drawer component. Use it for forms, details, and side panels.
            </p>
            <div className="space-y-4">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1.5">Name</label>
                <Input placeholder="Enter name..." />
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1.5">Email</label>
                <Input type="email" placeholder="Enter email..." />
              </div>
            </div>
          </DrawerBody>
          <DrawerFooter>
            <Button variant="outline" onClick={() => setOpen(false)}>Cancel</Button>
            <Button onClick={() => setOpen(false)}>Save</Button>
          </DrawerFooter>
        </DrawerContent>
      </Drawer>
    </div>
  )
}

function EmptyStateShowcase() {
  return (
    <div className="bg-white rounded-lg border border-gray-200 p-6">
      <h3 className="font-medium text-gray-900 mb-4">Empty State</h3>
      <div className="flex flex-col items-center justify-center py-12 border border-dashed border-gray-200 rounded-lg">
        <div className="h-16 w-16 rounded-xl bg-gray-100 flex items-center justify-center mb-4">
          <Database className="h-8 w-8 text-gray-400" />
        </div>
        <h4 className="text-lg font-medium text-gray-900 mb-1">No data yet</h4>
        <p className="text-sm text-gray-500 mb-6 max-w-sm text-center">
          Get started by creating your first item.
        </p>
        <Button>
          <Plus className="h-4 w-4 mr-2" />
          Create First Item
        </Button>
      </div>
    </div>
  )
}

function LoadingStateShowcase() {
  return (
    <div className="bg-white rounded-lg border border-gray-200 p-6">
      <h3 className="font-medium text-gray-900 mb-4">Loading States</h3>
      <div className="grid grid-cols-2 gap-4">
        {/* Card Skeleton */}
        <div className="bg-white rounded-lg border border-gray-200 p-6">
          <div className="animate-pulse space-y-4">
            <div className="h-4 bg-gray-200 rounded w-1/4" />
            <div className="h-4 bg-gray-200 rounded w-full" />
            <div className="h-4 bg-gray-200 rounded w-3/4" />
          </div>
        </div>
        
        {/* Table Skeleton */}
        <div className="bg-white rounded-lg border border-gray-200 overflow-hidden">
          <div className="animate-pulse">
            <div className="border-b border-gray-200 bg-gray-50 h-10" />
            {[1, 2, 3].map((i) => (
              <div key={i} className="border-b border-gray-200 h-12 px-4 flex items-center gap-4">
                <div className="h-4 bg-gray-200 rounded w-1/4" />
                <div className="h-4 bg-gray-200 rounded w-1/3" />
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  )
}

// ============================================================================
// DataStore Test Panel - Tests the Runtime Data Proxy endpoint
// ============================================================================

function DataStoreTestPanel() {
  const [results, setResults] = useState<TestResult[]>([])
  const [running, setRunning] = useState(false)
  const [testAppId, setTestAppId] = useState<string>('')

  const runTest = async (operation: string, fn: () => Promise<unknown>): Promise<TestResult> => {
    const start = Date.now()
    try {
      const data = await fn()
      return {
        operation,
        success: true,
        duration: Date.now() - start,
        data,
      }
    } catch (error) {
      return {
        operation,
        success: false,
        duration: Date.now() - start,
        error: error instanceof Error ? error.message : String(error),
      }
    }
  }

  const runAllTests = async () => {
    const appId = testAppId.trim()
    if (!appId) return
    
    setRunning(true)
    setResults([])
    const newResults: TestResult[] = []

    // Test 1: Runtime Proxy - List Tables (same endpoint generated apps use)
    newResults.push(await runTest('Runtime Proxy: listTables', async () => {
      const response = await runtimeDataCall<{ tables: unknown[] }>(
        appId,
        'listTables'
      )
      return response
    }))
    setResults([...newResults])

    // Test 2: Get first table's schema if any tables exist
    const tablesResult = newResults[0]
    if (tablesResult.success && tablesResult.data?.tables?.length > 0) {
      const firstTableSlug = tablesResult.data.tables[0].slug
      
      newResults.push(await runTest(`Runtime Proxy: getSchema (${firstTableSlug})`, async () => {
        const response = await runtimeDataCall(
          appId,
          'getSchema',
          firstTableSlug
        )
        return response
      }))
      setResults([...newResults])

      // Test 3: Query rows from the table
      newResults.push(await runTest(`Runtime Proxy: query (${firstTableSlug})`, async () => {
        const response = await runtimeDataCall(
          appId,
          'query',
          firstTableSlug,
          { limit: 5 }
        )
        return response
      }))
      setResults([...newResults])
    } else {
      // No tables - show info result
      newResults.push({
        operation: 'getSchema / query',
        success: true,
        duration: 0,
        data: { message: 'No tables in app - create a table to test more operations' },
      })
      setResults([...newResults])
    }

    setRunning(false)
  }

  return (
    <div className="bg-white rounded-lg border border-gray-200 p-6">
      <div className="flex items-center justify-between mb-4">
        <h3 className="font-medium text-gray-900 flex items-center gap-2">
          <Database className="h-5 w-5 text-gray-500" />
          DataStore Integration Test
        </h3>
        <Button 
          onClick={runAllTests} 
          disabled={running || !testAppId.trim()}
        >
          {running ? (
            <>
              <Loader2 className="h-4 w-4 mr-2 animate-spin" />
              Running...
            </>
          ) : (
            <>
              <RefreshCw className="h-4 w-4 mr-2" />
              Run All Tests
            </>
          )}
        </Button>
      </div>

      {/* App ID Input */}
      <div className="mb-4">
        <label className="block text-xs font-medium text-gray-700 mb-1">App ID</label>
        <Input
          type="text"
          value={testAppId}
          onChange={(e) => setTestAppId(e.target.value)}
          placeholder="Paste an app ID (e.g. abc12345-1234-5678-9abc-def012345678)"
          className="font-mono text-xs"
        />
      </div>

      <p className="text-xs text-gray-500 mb-4">
        Tests the <code className="bg-gray-100 px-1 rounded">/api/v1/runtime/data/</code> endpoint 
        — the same proxy that generated apps use at runtime.
      </p>

      {results.length > 0 && (
        <div className="space-y-2">
          {results.map((result, index) => (
            <div
              key={index}
              className={cn(
                'flex items-center justify-between p-3 rounded-lg border',
                result.success
                  ? 'bg-green-50 border-green-200'
                  : 'bg-red-50 border-red-200'
              )}
            >
              <div className="flex items-center gap-3">
                {result.success ? (
                  <CheckCircle className="h-5 w-5 text-green-600" />
                ) : (
                  <XCircle className="h-5 w-5 text-red-600" />
                )}
                <div>
                  <p className="text-sm font-medium text-gray-900">{result.operation}</p>
                  {result.error && (
                    <p className="text-xs text-red-600">{result.error}</p>
                  )}
                </div>
              </div>
              <div className="flex items-center gap-2 text-xs text-gray-500">
                <Clock className="h-3 w-3" />
                {result.duration}ms
              </div>
            </div>
          ))}
        </div>
      )}

      {results.length === 0 && !running && (
        <p className="text-sm text-gray-500 text-center py-8">
          Click "Run All Tests" to test DataStore operations
        </p>
      )}
    </div>
  )
}

// ============================================================================
// MCP Test Panel - Tests the Runtime Connector Proxy endpoint
// ============================================================================

interface RuntimeConnector {
  id: string
  name: string
  category: string
  icon_url?: string
  tool_count: number
  is_connected: boolean
}

function MCPTestPanel() {
  const [testAppId, setTestAppId] = useState<string>('')
  const [connectors, setConnectors] = useState<RuntimeConnector[]>([])
  const [selectedConnector, setSelectedConnector] = useState<RuntimeConnector | null>(null)
  const [tools, setTools] = useState<ConnectorTool[]>([])
  const [loadingConnectors, setLoadingConnectors] = useState(false)
  const [loadingTools, setLoadingTools] = useState(false)
  const [testResult, setTestResult] = useState<TestResult | null>(null)
  const [error, setError] = useState<string | null>(null)

  const loadConnectors = async () => {
    const appId = testAppId.trim()
    if (!appId) return
    
    setLoadingConnectors(true)
    setError(null)
    setConnectors([])
    setSelectedConnector(null)
    setTools([])
    
    try {
      // Call runtime connector proxy to list connectors (same as generated apps)
      const response = await runtimeConnectorCall<{ connectors: RuntimeConnector[] }>(
        appId,
        '_meta',
        'list'
      )
      setConnectors(response.connectors || [])
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err)
      if (message.includes('No integration provider') || message.includes('not configured')) {
        setError('No integration provider configured for this organization.')
      } else {
        setError(message)
      }
    } finally {
      setLoadingConnectors(false)
    }
  }

  const handleSelectConnector = async (connector: RuntimeConnector) => {
    const appId = testAppId.trim()
    if (!appId) return
    
    setSelectedConnector(connector)
    setLoadingTools(true)
    setTools([])
    setTestResult(null)

    try {
      // Call runtime connector proxy to get tools for this connector
      const response = await runtimeConnectorCall<{ tools: ConnectorTool[] }>(
        appId,
        '_meta',
        'tools',
        { connectorId: connector.id }
      )
      setTools(response.tools || [])
    } catch (err) {
      setTestResult({
        operation: 'Load Tools',
        success: false,
        duration: 0,
        error: err instanceof Error ? err.message : String(err),
      })
    } finally {
      setLoadingTools(false)
    }
  }

  const handleTestTool = async (tool: ConnectorTool) => {
    const appId = testAppId.trim()
    if (!appId || !selectedConnector) return
    
    setTestResult(null)
    const start = Date.now()

    try {
      // Execute the tool via runtime connector proxy
      const response = await runtimeConnectorCall<{ success: boolean; data?: unknown; error?: string }>(
        appId,
        selectedConnector.id,
        tool.name,
        {}  // Empty params for test
      )
      
      setTestResult({
        operation: tool.name,
        success: response.success,
        duration: Date.now() - start,
        data: response.data,
        error: response.error,
      })
    } catch (err) {
      setTestResult({
        operation: tool.name,
        success: false,
        duration: Date.now() - start,
        error: err instanceof Error ? err.message : String(err),
      })
    }
  }

  return (
    <div className="bg-white rounded-lg border border-gray-200 p-6">
      <div className="flex items-center justify-between mb-4">
        <h3 className="font-medium text-gray-900 flex items-center gap-2">
          <Plug className="h-5 w-5 text-gray-500" />
          MCP Integration Test
        </h3>
      </div>

      {/* App ID Input */}
      <div className="mb-4">
        <label className="block text-xs font-medium text-gray-700 mb-1">App ID</label>
        <div className="flex gap-2">
          <Input
            type="text"
            value={testAppId}
            onChange={(e) => setTestAppId(e.target.value)}
            placeholder="Paste an app ID (e.g. abc12345-1234-5678-9abc-def012345678)"
            className="font-mono text-xs flex-1"
          />
          <Button 
            onClick={loadConnectors} 
            disabled={loadingConnectors || !testAppId.trim()}
            variant="outline"
          >
            {loadingConnectors ? (
              <Loader2 className="h-4 w-4 animate-spin" />
            ) : (
              'Load Connectors'
            )}
          </Button>
        </div>
      </div>

      <p className="text-xs text-gray-500 mb-4">
        Tests the <code className="bg-gray-100 px-1 rounded">/api/v1/runtime/connectors/</code> endpoint 
        — the same proxy that generated apps use for MCP integrations.
      </p>

      {error && (
        <div className="p-3 rounded-lg bg-yellow-50 border border-yellow-200 text-sm text-yellow-700 mb-4">
          <AlertCircle className="h-4 w-4 inline mr-2" />
          {error}
        </div>
      )}

      {!loadingConnectors && connectors.length > 0 && (
        <div className="grid grid-cols-2 gap-6">
          {/* Connector List */}
          <div>
            <h4 className="text-sm font-medium text-gray-700 mb-3">
              Available Connectors ({connectors.length})
            </h4>
            <div className="space-y-2 max-h-64 overflow-y-auto">
              {connectors.map((connector) => (
                <button
                  key={connector.id}
                  onClick={() => handleSelectConnector(connector)}
                  className={cn(
                    'w-full flex items-center justify-between p-3 rounded-lg border text-left transition-colors',
                    selectedConnector?.id === connector.id
                      ? 'border-gray-400 bg-gray-50'
                      : 'border-gray-200 hover:border-gray-300'
                  )}
                >
                  <div className="flex items-center gap-2">
                    {connector.icon_url && (
                      <img src={connector.icon_url} alt="" className="h-5 w-5 rounded" />
                    )}
                    <div>
                      <span className="text-sm font-medium text-gray-900">{connector.name}</span>
                      <span className="text-xs text-gray-500 ml-2">
                        {connector.tool_count} tools
                      </span>
                    </div>
                  </div>
                  <div className="flex items-center gap-2">
                    {connector.is_connected ? (
                      <span className="text-xs text-green-600">Connected</span>
                    ) : (
                      <span className="text-xs text-gray-400">Not connected</span>
                    )}
                    <ChevronRight className="h-4 w-4 text-gray-400" />
                  </div>
                </button>
              ))}
            </div>
          </div>

          {/* Tool List & Execution */}
          <div>
            {selectedConnector ? (
              <>
                <h4 className="text-sm font-medium text-gray-700 mb-3">
                  {selectedConnector.name} Tools
                </h4>
              {loadingTools ? (
                <div className="flex items-center gap-2 text-sm text-gray-500">
                  <Loader2 className="h-4 w-4 animate-spin" />
                  Loading tools...
                </div>
              ) : (
                <div className="space-y-2">
                  {tools.map((tool) => (
                    <div
                      key={tool.name}
                      className="p-3 rounded-lg border border-gray-200 hover:border-gray-300 transition-colors"
                    >
                      <div className="flex items-center justify-between mb-1">
                        <span className="text-sm font-medium text-gray-900">{tool.name}</span>
                        <Button size="sm" variant="outline" onClick={() => handleTestTool(tool)}>
                          Test
                        </Button>
                      </div>
                      <p className="text-xs text-gray-500">{tool.description}</p>
                    </div>
                  ))}
                </div>
              )}

              {testResult && (
                <div
                  className={cn(
                    'mt-4 p-3 rounded-lg border',
                    testResult.success
                      ? 'bg-green-50 border-green-200'
                      : 'bg-red-50 border-red-200'
                  )}
                >
                  <div className="flex items-center gap-2 mb-2">
                    {testResult.success ? (
                      <CheckCircle className="h-4 w-4 text-green-600" />
                    ) : (
                      <XCircle className="h-4 w-4 text-red-600" />
                    )}
                    <span className="text-sm font-medium text-gray-900">{testResult.operation}</span>
                    <span className="text-xs text-gray-500 ml-auto">{testResult.duration}ms</span>
                  </div>
                  {testResult.data && (
                    <pre className="text-xs text-gray-600 bg-white p-2 rounded border border-gray-200 overflow-x-auto">
                      {JSON.stringify(testResult.data, null, 2)}
                    </pre>
                  )}
                </div>
              )}
            </>
          ) : (
            <div className="flex items-center justify-center h-full text-sm text-gray-500">
              Select a connector to view available tools
            </div>
          )}
        </div>
      </div>
      )}
    </div>
  )
}

// ============================================================================
// Main Page Component
// ============================================================================

function AdminTemplatePageContent() {
  return (
    <div className="min-h-screen bg-gray-50">
      {/* Header */}
      <div className="bg-white border-b border-gray-200">
        <div className="max-w-6xl mx-auto px-6 py-6">
          <h1 className="text-xl font-semibold text-gray-900">Template Component Gallery</h1>
          <p className="text-sm text-gray-500 mt-0.5">
            Test all template components, dataStore, and MCP integrations
          </p>
        </div>
      </div>

      <div className="max-w-6xl mx-auto p-6 space-y-8">
        {/* Component Sections */}
        <section>
          <h2 className="text-lg font-medium text-gray-900 mb-4 flex items-center gap-2">
            <Palette className="h-5 w-5 text-gray-500" />
            Core Components
          </h2>
          <div className="space-y-4">
            <ButtonShowcase />
            <InputShowcase />
            <BadgeShowcase />
          </div>
        </section>

        <section>
          <h2 className="text-lg font-medium text-gray-900 mb-4 flex items-center gap-2">
            <LayoutGrid className="h-5 w-5 text-gray-500" />
            Layout Components
          </h2>
          <div className="space-y-4">
            <CardShowcase />
            <TableShowcase />
          </div>
        </section>

        <section>
          <h2 className="text-lg font-medium text-gray-900 mb-4 flex items-center gap-2">
            <Bell className="h-5 w-5 text-gray-500" />
            Feedback Components
          </h2>
          <div className="space-y-4">
            <ToastShowcase />
            <DrawerShowcase />
            <EmptyStateShowcase />
            <LoadingStateShowcase />
          </div>
        </section>

        {/* Integration Tests */}
        <section>
          <h2 className="text-lg font-medium text-gray-900 mb-4 flex items-center gap-2">
            <Database className="h-5 w-5 text-gray-500" />
            Integration Tests
          </h2>
          <div className="space-y-4">
            <DataStoreTestPanel />
            <MCPTestPanel />
          </div>
        </section>
      </div>
    </div>
  )
}

export function AdminTemplatePage() {
  return <AdminTemplatePageContent />
}

export default AdminTemplatePage

