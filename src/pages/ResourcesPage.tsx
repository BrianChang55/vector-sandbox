/**
 * Resources page - Manage backend connections, resource registry, and integrations
 */
import { useState } from 'react'
import { useAppSelector } from '../store/hooks'
import { useBackends, useCreateBackend, useTestBackend } from '../hooks/useBackends'
import { useResources, useUpdateResource, useDiscoverResources } from '../hooks/useResources'
import { Button } from '../components/ui/button'
import { CustomDialog } from '../components/ui/dialog'
import { useDialog } from '../components/ui/dialog-provider'
import { Plus, Database, RefreshCw, ChevronRight, Check, X, TestTube2, Loader2, Plug } from 'lucide-react'
import { cn } from '../lib/utils'
import { IntegrationsPanel } from '../components/settings/IntegrationsPanel'

type ResourceTab = 'backends' | 'integrations'
type AdapterType = 'supabase' | 'postgresql' | 'mysql'

interface ConnectionFormData {
  adapter_type: AdapterType
  display_name: string
  // Supabase
  supabase_url?: string
  service_role_key?: string
  anon_key?: string
  // Direct DB
  host?: string
  port?: number
  database?: string
  username?: string
  password?: string
  ssl_mode?: string
}

const adapterOptions: { value: AdapterType; label: string; description: string }[] = [
  { value: 'supabase', label: 'Supabase', description: 'PostgreSQL with built-in auth & RLS' },
  { value: 'postgresql', label: 'PostgreSQL', description: 'Direct PostgreSQL connection' },
  { value: 'mysql', label: 'MySQL', description: 'Direct MySQL connection' },
]

export function ResourcesPage() {
  const selectedOrgId = useAppSelector((state) => state.ui.selectedOrgId)
  const { data: backends, isLoading: backendsLoading } = useBackends(selectedOrgId || null)
  const createBackend = useCreateBackend()
  const testBackend = useTestBackend()
  const discoverResources = useDiscoverResources()
  const { alert } = useDialog()
  
  const [selectedBackendId, setSelectedBackendId] = useState<string | null>(null)
  const { data: resources, isLoading: resourcesLoading } = useResources(selectedBackendId)
  const updateResource = useUpdateResource()
  
  // Tab state
  const [activeTab, setActiveTab] = useState<ResourceTab>('backends')
  
  // Connection dialog state
  const [showConnectionDialog, setShowConnectionDialog] = useState(false)
  const [formData, setFormData] = useState<ConnectionFormData>({
    adapter_type: 'postgresql',
    display_name: '',
    host: '',
    port: 5432,
    database: '',
    username: '',
    password: '',
    ssl_mode: 'disable',
  })
  const [isSubmitting, setIsSubmitting] = useState(false)
  const [isTesting, setIsTesting] = useState(false)
  const [testResult, setTestResult] = useState<{ success: boolean; message: string } | null>(null)

  const resetForm = () => {
    setFormData({
      adapter_type: 'postgresql',
      display_name: '',
      host: '',
      port: 5432,
      database: '',
      username: '',
      password: '',
      ssl_mode: 'disable',
    })
    setTestResult(null)
  }

  const handleAdapterChange = (adapter: AdapterType) => {
    setFormData(prev => ({
      ...prev,
      adapter_type: adapter,
      port: adapter === 'mysql' ? 3306 : 5432,
    }))
    setTestResult(null)
  }

  const handleSubmit = async () => {
    if (!selectedOrgId) return
    
    setIsSubmitting(true)
    try {
      await createBackend.mutateAsync({
        orgId: selectedOrgId,
        data: formData,
      })
      setShowConnectionDialog(false)
      resetForm()
      await alert({
        title: 'Backend Connected',
        description: 'Your database has been connected successfully. Click "Discover" to fetch resources.',
        variant: 'success',
      })
    } catch (error: any) {
      const errorMsg = error?.response?.data?.error || 
                       Object.values(error?.response?.data || {}).flat().join(', ') ||
                       'Failed to connect backend.'
      await alert({
        title: 'Connection Failed',
        description: errorMsg,
        variant: 'destructive',
      })
    } finally {
      setIsSubmitting(false)
    }
  }

  const handleTestConnection = async () => {
    if (!selectedOrgId) return
    
    setIsTesting(true)
    setTestResult(null)
    
    try {
      // First create a temporary connection to test
      const result = await createBackend.mutateAsync({
        orgId: selectedOrgId,
        data: formData,
      })
      
      // Then test it
      const testRes = await testBackend.mutateAsync(result.id)
      setTestResult({
        success: testRes.success,
        message: testRes.message + (testRes.version ? ` (${testRes.version})` : ''),
      })
      
      // If test failed, we might want to delete the connection
      // For now, keep it - user can manually delete
      
    } catch (error: any) {
      setTestResult({
        success: false,
        message: error?.response?.data?.message || error?.message || 'Connection test failed',
      })
    } finally {
      setIsTesting(false)
    }
  }

  const handleDiscover = async (backendId: string) => {
    try {
      await discoverResources.mutateAsync(backendId)
      await alert({
        title: 'Discovery Complete',
        description: 'Resources have been discovered from your backend.',
        variant: 'success',
      })
    } catch (error: any) {
      await alert({
        title: 'Discovery Failed',
        description: error?.response?.data?.error || error?.message || 'Failed to discover resources.',
        variant: 'destructive',
      })
    }
  }

  const handleToggleResource = async (resourceId: string, enabled: boolean) => {
    try {
      await updateResource.mutateAsync({
        resourceId,
        data: { enabled },
      })
    } catch (error) {
      console.error('Failed to toggle resource:', error)
    }
  }

  if (!selectedOrgId) {
    return (
      <div className="flex items-center justify-center h-full">
        <p className="text-gray-500">Please select an organization</p>
      </div>
    )
  }

  return (
    <div className="flex flex-col h-full">
      {/* Tab Navigation */}
      <div className="border-b border-gray-200 bg-white px-4">
        <div className="flex items-center gap-6 h-14">
          <button
            onClick={() => setActiveTab('backends')}
            className={cn(
              'flex items-center gap-2 h-full border-b-2 text-sm font-medium transition-colors',
              activeTab === 'backends'
                ? 'border-gray-900 text-gray-900'
                : 'border-transparent text-gray-500 hover:text-gray-700'
            )}
          >
            <Database className="h-4 w-4" />
            Backends
          </button>
          <button
            onClick={() => setActiveTab('integrations')}
            className={cn(
              'flex items-center gap-2 h-full border-b-2 text-sm font-medium transition-colors',
              activeTab === 'integrations'
                ? 'border-gray-900 text-gray-900'
                : 'border-transparent text-gray-500 hover:text-gray-700'
            )}
          >
            <Plug className="h-4 w-4" />
            Integrations
          </button>
        </div>
      </div>

      {/* Tab Content */}
      {activeTab === 'integrations' ? (
        <div className="flex-1 overflow-auto bg-gray-50 p-6">
          <div className="max-w-4xl mx-auto">
            <IntegrationsPanel orgId={selectedOrgId} />
          </div>
        </div>
      ) : (
        <div className="flex flex-1 overflow-hidden">
          {/* Left panel: Backend connections */}
          <div className="w-72 border-r border-gray-200 bg-white flex flex-col">
            <div className="px-4 py-4 min-h-[60px] border-b border-gray-200 flex items-center justify-between">
              <h2 className="font-semibold text-sm text-gray-900">Backends</h2>
              <Button size="sm" variant="ghost" onClick={() => setShowConnectionDialog(true)}>
                <Plus className="h-4 w-4" />
              </Button>
            </div>
        
        <div className="flex-1 overflow-y-auto">
          {backendsLoading ? (
            <div className="p-4 text-sm text-gray-500">Loading...</div>
          ) : backends?.length === 0 ? (
            <div className="p-4 text-center">
              <Database className="h-8 w-8 text-gray-300 mx-auto mb-2" />
              <p className="text-sm text-gray-500 mb-3">No backends connected</p>
              <Button size="sm" onClick={() => setShowConnectionDialog(true)}>
                <Plus className="h-4 w-4 mr-1" />
                Add Backend
              </Button>
            </div>
          ) : (
            <div className="p-2">
              {backends?.map((backend) => (
                <button
                  key={backend.id}
                  onClick={() => setSelectedBackendId(backend.id)}
                  className={cn(
                    'w-full flex items-center gap-3 px-3 py-2 rounded-md text-left transition-colors',
                    selectedBackendId === backend.id
                      ? 'bg-gray-100 text-gray-900'
                      : 'text-gray-600 hover:bg-gray-50'
                  )}
                >
                  <Database className="h-4 w-4 flex-shrink-0" />
                  <div className="flex-1 min-w-0">
                    <div className="text-sm font-medium truncate">{backend.display_name}</div>
                    <div className="text-xs text-gray-400">{backend.adapter_type_display}</div>
                  </div>
                  <ChevronRight className="h-4 w-4 text-gray-400" />
                </button>
              ))}
            </div>
          )}
        </div>
      </div>

      {/* Right panel: Resources */}
      <div className="flex-1 bg-gray-50">
        {!selectedBackendId ? (
          <div className="flex items-center justify-center h-full">
            <div className="text-center">
              <Database className="h-12 w-12 text-gray-300 mx-auto mb-3" />
              <p className="text-gray-500">Select a backend to view resources</p>
            </div>
          </div>
        ) : (
          <div className="h-full flex flex-col">
            <div className="p-4 bg-white border-b border-gray-200 flex items-center justify-between min-h-[75px]">
              <div>
                <h2 className="font-semibold text-gray-900">
                  {backends?.find((b) => b.id === selectedBackendId)?.display_name}
                </h2>
                <p className="text-sm text-gray-500">
                  {resources?.length || 0} resources discovered
                </p>
              </div>
              <Button
                size="sm"
                variant="outline"
                onClick={() => handleDiscover(selectedBackendId)}
                disabled={discoverResources.isPending}
              >
                <RefreshCw className={cn('h-4 w-4 mr-1', discoverResources.isPending && 'animate-spin')} />
                Discover
              </Button>
            </div>

            <div className="flex-1 overflow-y-auto p-4">
              {resourcesLoading ? (
                <div className="text-sm text-gray-500">Loading resources...</div>
              ) : resources?.length === 0 ? (
                <div className="text-center py-12">
                  <Database className="h-12 w-12 text-gray-300 mx-auto mb-3" />
                  <p className="text-gray-500 mb-4">No resources discovered yet</p>
                  <Button onClick={() => handleDiscover(selectedBackendId)}>
                    <RefreshCw className="h-4 w-4 mr-1" />
                    Discover Resources
                  </Button>
                </div>
              ) : (
                <div className="bg-white rounded-lg border border-gray-200 overflow-hidden">
                  <table className="w-full">
                    <thead>
                      <tr className="border-b border-gray-200 bg-gray-50">
                        <th className="text-left text-xs font-medium text-gray-500 uppercase px-4 py-3">
                          Resource
                        </th>
                        <th className="text-left text-xs font-medium text-gray-500 uppercase px-4 py-3">
                          Fields
                        </th>
                        <th className="text-left text-xs font-medium text-gray-500 uppercase px-4 py-3">
                          Actions
                        </th>
                        <th className="text-center text-xs font-medium text-gray-500 uppercase px-4 py-3 w-24">
                          Enabled
                        </th>
                      </tr>
                    </thead>
                    <tbody className="divide-y divide-gray-200">
                      {resources?.map((resource) => (
                        <tr key={resource.id} className="hover:bg-gray-50">
                          <td className="px-4 py-3">
                            <div className="font-medium text-sm text-gray-900">
                              {resource.resource_name}
                            </div>
                            <div className="text-xs text-gray-400">{resource.resource_id}</div>
                          </td>
                          <td className="px-4 py-3">
                            <div className="text-sm text-gray-600">
                              {resource.schema_json?.fields?.length || 0} fields
                            </div>
                          </td>
                          <td className="px-4 py-3">
                            <div className="text-sm text-gray-600">
                              {resource.allowed_actions_json?.length || 0} actions
                            </div>
                          </td>
                          <td className="px-4 py-3 text-center">
                            <button
                              onClick={() => handleToggleResource(resource.id, !resource.enabled)}
                              className={cn(
                                'inline-flex items-center justify-center h-6 w-6 rounded-full transition-colors',
                                resource.enabled
                                  ? 'bg-green-100 text-green-600'
                                  : 'bg-gray-100 text-gray-400'
                              )}
                            >
                              {resource.enabled ? (
                                <Check className="h-3.5 w-3.5" />
                              ) : (
                                <X className="h-3.5 w-3.5" />
                              )}
                            </button>
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              )}
            </div>
          </div>
        )}
      </div>
    </div>
      )}

      {/* Connection Dialog */}
      <CustomDialog
        open={showConnectionDialog}
        onOpenChange={(open) => {
          setShowConnectionDialog(open)
          if (!open) resetForm()
        }}
        title="Add Backend Connection"
        description="Connect a database to discover resources and build apps."
        className="max-w-lg"
      >
        <div className="space-y-4">
          {/* Adapter Type Selection */}
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">Database Type</label>
            <div className="grid grid-cols-3 gap-2">
              {adapterOptions.map((option) => (
                <button
                  key={option.value}
                  type="button"
                  onClick={() => handleAdapterChange(option.value)}
                  className={cn(
                    'p-3 rounded-lg border text-left transition-all',
                    formData.adapter_type === option.value
                      ? 'border-gray-900 bg-gray-50 ring-1 ring-gray-900'
                      : 'border-gray-200 hover:border-gray-300'
                  )}
                >
                  <div className="font-medium text-sm">{option.label}</div>
                  <div className="text-xs text-gray-500 mt-0.5">{option.description}</div>
                </button>
              ))}
            </div>
          </div>

          {/* Display Name */}
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">Display Name</label>
            <input
              type="text"
              value={formData.display_name}
              onChange={(e) => setFormData(prev => ({ ...prev, display_name: e.target.value }))}
              placeholder="e.g., Production DB, Staging"
              className="w-full px-3 py-2 border border-gray-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-gray-900"
            />
          </div>

          {/* Supabase-specific fields */}
          {formData.adapter_type === 'supabase' && (
            <>
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1">Supabase URL</label>
                <input
                  type="url"
                  value={formData.supabase_url || ''}
                  onChange={(e) => setFormData(prev => ({ ...prev, supabase_url: e.target.value }))}
                  placeholder="https://xxxxx.supabase.co"
                  className="w-full px-3 py-2 border border-gray-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-gray-900"
                />
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1">Service Role Key</label>
                <input
                  type="password"
                  value={formData.service_role_key || ''}
                  onChange={(e) => setFormData(prev => ({ ...prev, service_role_key: e.target.value }))}
                  placeholder="eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
                  className="w-full px-3 py-2 border border-gray-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-gray-900"
                />
                <p className="text-xs text-gray-500 mt-1">Used for schema discovery only, never for data queries.</p>
              </div>
            </>
          )}

          {/* PostgreSQL/MySQL-specific fields */}
          {(formData.adapter_type === 'postgresql' || formData.adapter_type === 'mysql') && (
            <>
              <div className="grid grid-cols-3 gap-3">
                <div className="col-span-2">
                  <label className="block text-sm font-medium text-gray-700 mb-1">Host</label>
                  <input
                    type="text"
                    value={formData.host || ''}
                    onChange={(e) => setFormData(prev => ({ ...prev, host: e.target.value }))}
                    placeholder="localhost or db.example.com"
                    className="w-full px-3 py-2 border border-gray-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-gray-900"
                  />
                </div>
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">Port</label>
                  <input
                    type="number"
                    value={formData.port || ''}
                    onChange={(e) => setFormData(prev => ({ ...prev, port: parseInt(e.target.value) || undefined }))}
                    placeholder={formData.adapter_type === 'mysql' ? '3306' : '5432'}
                    className="w-full px-3 py-2 border border-gray-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-gray-900"
                  />
                </div>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1">Database</label>
                <input
                  type="text"
                  value={formData.database || ''}
                  onChange={(e) => setFormData(prev => ({ ...prev, database: e.target.value }))}
                  placeholder="mydb"
                  className="w-full px-3 py-2 border border-gray-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-gray-900"
                />
              </div>
              <div className="grid grid-cols-2 gap-3">
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">Username</label>
                  <input
                    type="text"
                    value={formData.username || ''}
                    onChange={(e) => setFormData(prev => ({ ...prev, username: e.target.value }))}
                    placeholder="postgres"
                    className="w-full px-3 py-2 border border-gray-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-gray-900"
                  />
                </div>
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">Password</label>
                  <input
                    type="password"
                    value={formData.password || ''}
                    onChange={(e) => setFormData(prev => ({ ...prev, password: e.target.value }))}
                    placeholder="••••••••"
                    className="w-full px-3 py-2 border border-gray-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-gray-900"
                  />
                </div>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1">SSL Mode</label>
                <select
                  value={formData.ssl_mode || 'disable'}
                  onChange={(e) => setFormData(prev => ({ ...prev, ssl_mode: e.target.value }))}
                  className="w-full px-3 py-2 border border-gray-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-gray-900"
                >
                  <option value="disable">Disable</option>
                  <option value="require">Require</option>
                  <option value="verify-full">Verify Full</option>
                </select>
              </div>
            </>
          )}

          {/* Test Result */}
          {testResult && (
            <div className={cn(
              'p-3 rounded-lg text-sm',
              testResult.success ? 'bg-green-50 text-green-700' : 'bg-red-50 text-red-700'
            )}>
              {testResult.success ? '✓ ' : '✗ '}{testResult.message}
            </div>
          )}

          {/* Actions */}
          <div className="flex justify-end gap-2 pt-2">
            <Button
              variant="outline"
              onClick={() => {
                setShowConnectionDialog(false)
                resetForm()
              }}
            >
              Cancel
            </Button>
            <Button
              variant="outline"
              onClick={handleTestConnection}
              disabled={isTesting || !formData.display_name}
            >
              {isTesting ? (
                <Loader2 className="h-4 w-4 mr-1 animate-spin" />
              ) : (
                <TestTube2 className="h-4 w-4 mr-1" />
              )}
              Test
            </Button>
            <Button
              onClick={handleSubmit}
              disabled={isSubmitting || !formData.display_name}
            >
              {isSubmitting ? (
                <Loader2 className="h-4 w-4 mr-1 animate-spin" />
              ) : null}
              Connect
            </Button>
          </div>
        </div>
      </CustomDialog>
    </div>
  )
}
