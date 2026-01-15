/**
 * App Connectors Panel
 * 
 * Displays organization's connected integrations available for use in the app.
 * Connections can be initiated directly from here (admin only).
 * 
 * Organization-level integration model:
 * - Connectors are connected once per organization
 * - All org members share the connected integrations
 */
import { useState, useCallback, useEffect } from 'react'
import { useNavigate } from 'react-router-dom'
import { useAgentHandlerLink } from '@mergeapi/react-agent-handler-link'
import {
  Plug,
  Loader2,
  Check,
  AlertCircle,
  Clock,
  Zap,
  RefreshCw,
  User,
  Link,
} from 'lucide-react'
import { useApp } from '@/hooks/useApps'
import {
  useHasIntegrations,
  useConnectors,
  useGenerateLinkToken,
  useHandleLinkCallback,
} from '@/hooks/useIntegrations'
import { useOrgMembers } from '@/hooks/useMembers'
import { Button } from '@/components/ui/button'
import type { Connector } from '@/types/models'

interface AppConnectorsPanelProps {
  appId: string
  className?: string
}

export function AppConnectorsPanel({ appId, className = '' }: AppConnectorsPanelProps) {
  const navigate = useNavigate()
  const { data: app } = useApp(appId)
  const orgId = app?.organization || null
  const { hasIntegrations, provider, isLoading: loadingProvider } = useHasIntegrations(orgId)
  
  const { data: connectors, isLoading: loadingConnectors, refetch: refetchConnectors } = useConnectors(provider?.id || null)
  
  // Check if user is admin
  const { data: membersData } = useOrgMembers(orgId)
  const isAdmin = membersData?.current_user_role === 'admin'
  
  const [selectedConnector, setSelectedConnector] = useState<Connector | null>(null)

  // Auto-select first connected connector, or first connector if none connected
  if (connectors && connectors.length > 0 && !selectedConnector) {
    const firstConnected = connectors.find(c => c.is_connected)
    setSelectedConnector(firstConnected || connectors[0])
  }

  if (loadingProvider || loadingConnectors) {
    return (
      <div className={`flex items-center justify-center h-full bg-gray-50 ${className}`}>
        <Loader2 className="h-8 w-8 text-gray-400 animate-spin" />
      </div>
    )
  }

  if (!hasIntegrations) {
    return (
      <EmptyState 
        message="No integration provider configured"
        description="Configure an integration provider in your organization's Resources page to connect external services like Jira, Slack, and Linear."
      />
    )
  }

  if (!connectors || connectors.length === 0) {
    return (
      <EmptyState 
        message="No connectors available"
        description="Sync connectors from your integration provider in the Resources page."
      />
    )
  }

  const connectedCount = connectors.filter(c => c.is_connected).length

  return (
    <div className={`flex h-full bg-gray-50 ${className}`}>
      {/* Left Panel - Connector List */}
      <div className="w-64 flex-shrink-0 border-r border-gray-200 bg-white flex flex-col">
        <div className="p-3.5 border-b border-gray-200">
          <div className="flex items-center gap-2 mb-1">
            <Plug className="h-4 w-4 text-gray-500" />
            <h2 className="font-medium text-sm text-gray-900">Connectors</h2>
          </div>
          <p className="text-xs text-gray-500">
            {connectedCount} of {connectors.length} connected
          </p>
        </div>

        <div className="flex-1 overflow-y-auto">
          <ConnectorList
            connectors={connectors}
            selectedConnector={selectedConnector}
            onSelectConnector={setSelectedConnector}
          />
        </div>
        
        {/* Link to manage connections */}
        <div className="p-3 border-t border-gray-200">
          <Button
            variant="ghost"
            size="sm"
            className="w-full justify-start text-xs text-gray-500 hover:text-gray-700"
            onClick={() => navigate('/integrations')}
          >
            <Plug className="h-3 w-3 mr-2" />
            Manage Integrations
          </Button>
        </div>
      </div>

      {/* Right Panel - Connector Details */}
      <div className="flex-1 min-w-0 flex flex-col">
        {selectedConnector ? (
          <ConnectorDetails 
            connector={selectedConnector} 
            providerId={provider?.id || ''}
            isAdmin={isAdmin}
            onConnected={() => refetchConnectors()}
          />
        ) : (
          <div className="flex flex-col items-center justify-center h-full">
            <div className="h-16 w-16 rounded-xl bg-gray-100 flex items-center justify-center mb-4">
              <Plug className="h-8 w-8 text-gray-400" />
            </div>
            <h3 className="text-lg font-medium text-gray-900 mb-1">
              Select a connector
            </h3>
            <p className="text-sm text-gray-500 max-w-sm text-center">
              Choose a connector from the left panel to view its details and available tools.
            </p>
          </div>
        )}
      </div>
    </div>
  )
}

// ============================================================================
// Empty State
// ============================================================================

interface EmptyStateProps {
  message: string
  description: string
}

function EmptyState({ message, description }: EmptyStateProps) {
  const navigate = useNavigate()
  
  return (
    <div className="flex flex-col items-center justify-center h-full bg-gray-50">
      <div className="h-16 w-16 rounded-xl bg-gray-100 flex items-center justify-center mb-4">
        <Plug className="h-8 w-8 text-gray-400" />
      </div>
      <h3 className="text-lg font-medium text-gray-900 mb-1">
        {message}
      </h3>
      <p className="text-sm text-gray-500 max-w-sm text-center mb-6">
        {description}
      </p>
      <Button
        variant="outline"
        onClick={() => navigate('/integrations')}
        className="gap-2"
      >
        <Plug className="h-4 w-4" />
        Go to Integrations
      </Button>
    </div>
  )
}

// ============================================================================
// Not Connected Banner
// ============================================================================

interface NotConnectedBannerProps {
  connector: Connector
  providerId: string
  isAdmin: boolean
  onConnected: () => void
}

function NotConnectedBanner({ connector, providerId, isAdmin, onConnected }: NotConnectedBannerProps) {
  const [connecting, setConnecting] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [linkToken, setLinkToken] = useState<string | undefined>(undefined)
  
  const generateLinkToken = useGenerateLinkToken()
  const handleCallback = useHandleLinkCallback()
  
  // Use the Merge Agent Handler Link React hook
  const { open: openLink, isReady } = useAgentHandlerLink({
    linkToken: linkToken || '',
    onSuccess: async () => {
      // Verify connection with the backend
      try {
        const result = await handleCallback.mutateAsync({
          providerId,
          connectorId: connector.connector_id,
        })
        
        if (result.success && result.is_connected) {
          onConnected()
        } else {
          setError(result.message || 'Connection was not completed. Please try again.')
        }
      } catch (err: any) {
        const errorMsg = err.response?.data?.error || err.response?.data?.message || 'Failed to verify connection'
        setError(errorMsg)
      }
      setConnecting(false)
      setLinkToken(undefined)
    },
    onExit: () => {
      // User closed without completing
      setConnecting(false)
      setLinkToken(undefined)
    },
  })
  
  // Open link when it's ready and we have a token
  useEffect(() => {
    if (isReady && linkToken && connecting) {
      openLink()
    }
  }, [isReady, linkToken, connecting, openLink])
  
  const handleConnect = useCallback(async () => {
    if (!isAdmin) return
    
    setConnecting(true)
    setError(null)
    try {
      // Generate link token from backend
      const { link_token } = await generateLinkToken.mutateAsync({
        providerId,
        connectorId: connector.connector_id,
      })
      
      // Set the link token - this will trigger the useEffect to open the link
      setLinkToken(link_token)
    } catch (err: any) {
      console.error('Failed to connect:', err)
      setError(err.response?.data?.error || err.message || 'Failed to start connection flow')
      setConnecting(false)
    }
  }, [connector.connector_id, generateLinkToken, providerId, isAdmin])
  
  return (
    <div className="mx-4 mt-4 p-3 bg-amber-50 border border-amber-200 rounded-lg">
      <p className="text-sm text-amber-800">
        <strong>Not connected:</strong> This integration needs to be connected by an organization 
        admin before it can be used in apps.
      </p>
      {error && (
        <p className="mt-2 text-sm text-red-600">{error}</p>
      )}
      <div className="relative inline-block mt-2">
        <Button
          variant="outline"
          size="sm"
          className={`text-amber-800 border-amber-300 hover:bg-amber-100 ${
            !isAdmin ? 'opacity-50 cursor-not-allowed' : ''
          }`}
          onClick={handleConnect}
          disabled={!isAdmin || connecting}
          title={!isAdmin ? 'Only organization admins can connect integrations' : undefined}
        >
          {connecting ? (
            <Loader2 className="h-3 w-3 mr-1.5 animate-spin" />
          ) : (
            <Link className="h-3 w-3 mr-1.5" />
          )}
          {connecting ? 'Connecting...' : 'Connect'}
        </Button>
        {!isAdmin && (
          <div className="absolute left-0 bottom-full mb-1 px-2 py-1 bg-gray-900 text-white text-xs rounded whitespace-nowrap opacity-0 pointer-events-none group-hover:opacity-100 transition-opacity z-10">
            Only admins can connect integrations
          </div>
        )}
      </div>
      {!isAdmin && (
        <p className="mt-1.5 text-xs text-amber-700">
          Contact an organization admin to connect this integration.
        </p>
      )}
    </div>
  )
}

// ============================================================================
// Connector List
// ============================================================================

interface ConnectorListProps {
  connectors: Connector[]
  selectedConnector: Connector | null
  onSelectConnector: (connector: Connector) => void
}

function ConnectorList({ 
  connectors, 
  selectedConnector, 
  onSelectConnector 
}: ConnectorListProps) {
  // Group by category
  const byCategory = connectors.reduce((acc, connector) => {
    const category = connector.category || 'other'
    if (!acc[category]) acc[category] = []
    acc[category].push(connector)
    return acc
  }, {} as Record<string, Connector[]>)

  return (
    <div className="p-2">
      {Object.entries(byCategory).map(([category, categoryConnectors]) => (
        <div key={category} className="mb-4">
          <div className="px-2 py-1 text-[10px] font-semibold text-gray-400 uppercase tracking-wider">
            {formatCategory(category)}
          </div>
          <div className="space-y-0.5">
            {categoryConnectors.map(connector => {
              const isSelected = selectedConnector?.id === connector.id
              
              return (
                <button
                  key={connector.id}
                  onClick={() => onSelectConnector(connector)}
                  className={`
                    w-full flex items-center gap-2.5 px-2.5 py-2 rounded-md text-left transition-colors
                    ${isSelected 
                      ? 'bg-gray-100 text-gray-900' 
                      : 'text-gray-600 hover:bg-gray-50'
                    }
                  `}
                >
                  <div className="w-7 h-7 rounded-md bg-gray-100 flex items-center justify-center flex-shrink-0">
                    {connector.icon_url ? (
                      <img 
                        src={connector.icon_url} 
                        alt={connector.name} 
                        className="w-4 h-4 object-contain"
                      />
                    ) : (
                      <Plug className="w-3.5 h-3.5 text-gray-400" />
                    )}
                  </div>
                  <div className="flex-1 min-w-0">
                    <div className="text-sm font-medium truncate">{connector.name}</div>
                    <div className="text-[10px] text-gray-400">{connector.tool_count} tools</div>
                  </div>
                  {connector.is_connected ? (
                    <div className="w-2 h-2 rounded-full bg-green-500 flex-shrink-0" />
                  ) : (
                    <div className="w-2 h-2 rounded-full bg-gray-300 flex-shrink-0" />
                  )}
                </button>
              )
            })}
          </div>
        </div>
      ))}
    </div>
  )
}

// ============================================================================
// Connector Details
// ============================================================================

interface ConnectorDetailsProps {
  connector: Connector
  providerId: string
  isAdmin: boolean
  onConnected: () => void
}

function ConnectorDetails({ connector, providerId, isAdmin, onConnected }: ConnectorDetailsProps) {
  return (
    <div className="h-full flex flex-col">
      {/* Header */}
      <div className="p-4 bg-white border-b border-gray-200">
        <div className="flex items-start gap-4">
          <div className="w-12 h-12 rounded-lg bg-gray-100 flex items-center justify-center flex-shrink-0">
            {connector.icon_url ? (
              <img 
                src={connector.icon_url} 
                alt={connector.name} 
                className="w-7 h-7 object-contain"
              />
            ) : (
              <Plug className="w-6 h-6 text-gray-400" />
            )}
          </div>
          <div className="flex-1 min-w-0">
            <div className="flex items-center gap-2 mb-1">
              <h2 className="text-lg font-semibold text-gray-900">{connector.name}</h2>
              {connector.is_connected ? (
                <span className="inline-flex items-center gap-1 px-2 py-0.5 bg-green-50 text-green-700 text-xs font-medium rounded-full">
                  <Check className="h-3 w-3" />
                  Connected
                </span>
              ) : (
                <span className="inline-flex items-center gap-1 px-2 py-0.5 bg-gray-100 text-gray-600 text-xs font-medium rounded-full">
                  <AlertCircle className="h-3 w-3" />
                  Not Connected
                </span>
              )}
            </div>
            {connector.description && (
              <p className="text-sm text-gray-500">{connector.description}</p>
            )}
            {connector.is_connected && (
              <div className="flex items-center gap-3 mt-1 text-xs text-gray-400">
                {connector.connected_at && (
                  <span className="flex items-center gap-1">
                    <Clock className="h-3 w-3" />
                    {new Date(connector.connected_at).toLocaleDateString()}
                  </span>
                )}
                {connector.connected_by && (
                  <span className="flex items-center gap-1">
                    <User className="h-3 w-3" />
                    {connector.connected_by}
                  </span>
                )}
              </div>
            )}
          </div>
        </div>
      </div>

      {/* Not Connected Banner */}
      {!connector.is_connected && (
        <NotConnectedBanner 
          connector={connector} 
          providerId={providerId}
          isAdmin={isAdmin}
          onConnected={onConnected}
        />
      )}

      {/* Tools Section */}
      <div className="flex-1 overflow-auto p-4">
        <div className="flex items-center gap-2 mb-4">
          <Zap className="h-4 w-4 text-gray-500" />
          <h3 className="font-medium text-gray-900">Available Tools</h3>
          <span className="text-xs text-gray-400 bg-gray-100 px-2 py-0.5 rounded-full">
            {connector.tools.length}
          </span>
        </div>

        {connector.tools.length > 0 ? (
          <div className="bg-white rounded-lg border border-gray-200 divide-y divide-gray-100">
            {connector.tools.map(tool => (
              <div key={tool.id} className="p-3">
                <div className="flex items-start gap-3">
                  <code className="flex-shrink-0 text-xs bg-gray-100 px-2 py-1 rounded font-mono text-gray-700">
                    {tool.id}
                  </code>
                  <div className="flex-1 min-w-0">
                    <p className="text-sm text-gray-600">{tool.description || tool.name}</p>
                    {tool.parameters && Object.keys(tool.parameters).length > 0 && (
                      <div className="mt-2 flex flex-wrap gap-1.5">
                        {Object.entries(tool.parameters).slice(0, 5).map(([name, param]) => (
                          <span 
                            key={name}
                            className={`text-[10px] px-1.5 py-0.5 rounded ${
                              param.required 
                                ? 'bg-blue-50 text-blue-600' 
                                : 'bg-gray-50 text-gray-500'
                            }`}
                          >
                            {name}
                          </span>
                        ))}
                        {Object.keys(tool.parameters).length > 5 && (
                          <span className="text-[10px] text-gray-400">
                            +{Object.keys(tool.parameters).length - 5} more
                          </span>
                        )}
                      </div>
                    )}
                  </div>
                </div>
              </div>
            ))}
          </div>
        ) : (
          <div className="text-center py-8 text-sm text-gray-500">
            No tools available for this connector.
          </div>
        )}

        {/* Usage Instructions */}
        {connector.is_connected && (
          <div className="mt-6 p-4 bg-gray-50 rounded-lg border border-gray-200">
            <h4 className="font-medium text-gray-900 mb-2 flex items-center gap-2">
              <RefreshCw className="h-4 w-4" />
              Using in Your App
            </h4>
            <p className="text-sm text-gray-600 mb-3">
              This connector is ready to use. Import the connectors client and call tools:
            </p>
            <pre className="text-xs bg-gray-900 text-gray-100 p-3 rounded-md overflow-x-auto">
{`import { connectors } from './lib/connectors';

const result = await connectors.execute(
  '${connector.connector_id}',
  'tool_name',
  { /* params */ }
);`}
            </pre>
          </div>
        )}
      </div>
    </div>
  )
}

// ============================================================================
// Utilities
// ============================================================================

function formatCategory(category: string): string {
  return category
    .split('_')
    .map(word => word.charAt(0).toUpperCase() + word.slice(1))
    .join(' ')
}

export default AppConnectorsPanel
