/**
 * IntegrationsPanel - Organization settings panel for managing external integrations.
 * 
 * Organization-level integration model:
 * - The org is registered as a single "user" with Merge
 * - Any org member can connect integrations for the whole organization
 * - All org members share the connected integrations
 * 
 * Flow:
 * 1. Provider is auto-created when the page is accessed
 * 2. Connectors are auto-synced from the tool pack on first access
 * 3. Users can connect any integration using Merge Agent Handler Link
 * 4. Connected integrations are available to all org members in apps
 */
import { useState, useMemo, useEffect, useCallback } from 'react'
import {
  RefreshCw,
  Check,
  Loader2,
  Link,
  ChevronRight,
  AlertCircle,
  Plug,
  User,
  Settings,
  Search,
  ExternalLink,
  Grid3X3,
  List,
  X,
  Zap,
} from 'lucide-react'
import { useAgentHandlerLink } from '@mergeapi/react-agent-handler-link'
import {
  useIntegrationProviders,
  useConnectors,
  useSyncConnectors,
  useGenerateLinkToken,
  useHandleLinkCallback,
} from '../../hooks/useIntegrations'
import type { Connector, IntegrationProvider } from '../../types/models'
import { fuzzySearch } from '../../utils/fuzzySearch'

interface IntegrationsPanelProps {
  orgId: string
}

export function IntegrationsPanel({ orgId }: IntegrationsPanelProps) {
  const { data: providers, isLoading: loadingProviders } = useIntegrationProviders(orgId)
  
  const provider = providers?.[0] // Currently only one provider per org
  
  if (loadingProviders) {
    return (
      <div className="flex flex-col items-center justify-center py-16">
        <Loader2 className="h-6 w-6 animate-spin text-gray-400" />
        <p className="mt-3 text-sm text-gray-500">Loading integrations...</p>
      </div>
    )
  }
  
  // If provider exists but Merge is not configured
  if (provider && !provider.is_configured) {
    return <NotConfiguredState />
  }
  
  // If no provider and nothing was created (edge case - shouldn't happen if Merge is configured)
  if (!provider) {
    return <NotConfiguredState />
  }
  
  return (
    <ProviderDashboard provider={provider} />
  )
}

// ============================================================================
// Not Configured State
// ============================================================================

function NotConfiguredState() {
  return (
    <div className="flex flex-col items-center justify-center py-16">
      <div className="h-16 w-16 rounded-xl bg-gray-100 flex items-center justify-center mb-4">
        <Settings className="h-8 w-8 text-gray-400" />
      </div>
      <h2 className="text-lg font-medium text-gray-900 mb-1">
        Integrations Not Available
      </h2>
      <p className="text-sm text-gray-500 mb-6 max-w-sm text-center">
        External integrations are not configured for this platform.
        Contact your administrator to enable integrations.
      </p>
      <div className="flex items-center gap-2 px-3 py-2 bg-gray-100 rounded-md text-xs text-gray-600">
        <AlertCircle className="h-3.5 w-3.5" />
        Missing: MERGE_TOOL_PACK_ID, MERGE_ACCESS_KEY
      </div>
    </div>
  )
}

// ============================================================================
// Provider Dashboard
// ============================================================================

interface ProviderDashboardProps {
  provider: IntegrationProvider
}

type ViewMode = 'grid' | 'list'
type FilterMode = 'all' | 'connected' | 'available'

function ProviderDashboard({ provider }: ProviderDashboardProps) {
  const { data: connectors, isLoading: loadingConnectors, refetch } = useConnectors(provider.id)
  const syncConnectors = useSyncConnectors()
  const [syncError, setSyncError] = useState<string | null>(null)
  const [searchQuery, setSearchQuery] = useState('')
  const [viewMode, setViewMode] = useState<ViewMode>('list')
  const [filterMode, setFilterMode] = useState<FilterMode>('all')
  
  const handleSync = async () => {
    setSyncError(null)
    try {
      await syncConnectors.mutateAsync(provider.id)
      refetch()
    } catch (err: any) {
      setSyncError(err.response?.data?.error || 'Failed to sync connectors')
    }
  }
  
  // Count connected
  const connectedCount = connectors?.filter(c => c.is_connected).length ?? 0
  const totalCount = connectors?.length ?? 0
  
  // Filter and search connectors
  const filteredConnectors = useMemo(() => {
    if (!connectors) return []
    
    let result = connectors
    
    // Apply filter
    if (filterMode === 'connected') {
      result = result.filter(c => c.is_connected)
    } else if (filterMode === 'available') {
      result = result.filter(c => !c.is_connected)
    }
    
    // Apply fuzzy search
    if (searchQuery.trim()) {
      result = fuzzySearch(result, searchQuery, [
        { getValue: (c) => c.name, weight: 2 },
        { getValue: (c) => c.category, weight: 1.5 },
        { getValue: (c) => c.description, weight: 1 },
      ])
    }
    
    return result
  }, [connectors, filterMode, searchQuery])
  
  // Sort connectors alphabetically by name
  const sortedConnectors = useMemo(() => 
    [...filteredConnectors].sort((a, b) => a.name.localeCompare(b.name)),
    [filteredConnectors]
  )
  
  return (
    <div className="space-y-6">
      {/* Toolbar */}
      <div className="flex flex-col sm:flex-row gap-3">
        {/* Search */}
        <div className="relative flex-1">
          <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-gray-400" />
          <input
            type="text"
            placeholder="Search integrations..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="w-full pl-9 pr-3 py-2 border border-gray-200 rounded-lg text-sm text-gray-900 placeholder:text-gray-500 focus:outline-none focus:border-gray-300 transition-all"
          />
          {searchQuery && (
            <button
              onClick={() => setSearchQuery('')}
              className="absolute right-3 top-1/2 -translate-y-1/2 text-gray-400 hover:text-gray-600"
            >
              <X className="h-4 w-4" />
            </button>
          )}
        </div>
        
        {/* Controls */}
        <div className="flex items-center gap-2">
          {/* Filter Tabs */}
          <div className="flex bg-gray-100 rounded-md p-0.5">
            {(['all', 'connected', 'available'] as FilterMode[]).map((mode) => (
              <button
                key={mode}
                onClick={() => setFilterMode(mode)}
                className={`px-3 py-1.5 text-xs font-medium rounded transition-all ${
                  filterMode === mode
                    ? 'bg-white text-gray-900 shadow-sm'
                    : 'text-gray-600 hover:text-gray-900'
                }`}
              >
                {mode === 'all' ? `All (${totalCount})` : mode === 'connected' ? `Connected (${connectedCount})` : `Available (${totalCount - connectedCount})`}
              </button>
            ))}
          </div>
          
          <div className="w-px h-6 bg-gray-200" />
          
          {/* View Toggle */}
          <div className="flex bg-gray-100 rounded-md p-0.5">
            <button
              onClick={() => setViewMode('list')}
              className={`p-1.5 rounded transition-all ${
                viewMode === 'list'
                  ? 'bg-white text-gray-900 shadow-sm'
                  : 'text-gray-400 hover:text-gray-600'
              }`}
              title="List view"
            >
              <List className="h-4 w-4" />
            </button>
            <button
              onClick={() => setViewMode('grid')}
              className={`p-1.5 rounded transition-all ${
                viewMode === 'grid'
                  ? 'bg-white text-gray-900 shadow-sm'
                  : 'text-gray-400 hover:text-gray-600'
              }`}
              title="Grid view"
            >
              <Grid3X3 className="h-4 w-4" />
            </button>
          </div>
          
          <div className="w-px h-6 bg-gray-200" />
          
          {/* Refresh */}
          <button
            onClick={handleSync}
            disabled={syncConnectors.isPending}
            className="flex items-center gap-2 px-3 py-1.5 text-sm text-gray-600 hover:text-gray-900 hover:bg-gray-100 rounded-md transition-colors disabled:opacity-50"
          >
            <RefreshCw className={`h-4 w-4 ${syncConnectors.isPending ? 'animate-spin' : ''}`} />
            Refresh
          </button>
        </div>
      </div>
      
      {syncError && (
        <div className="flex items-center gap-3 p-3 bg-red-50 border border-red-200 rounded-lg text-sm text-red-700">
          <AlertCircle className="h-4 w-4 flex-shrink-0" />
          <div className="flex-1">{syncError}</div>
          <button onClick={() => setSyncError(null)} className="text-red-400 hover:text-red-600">
            <X className="h-4 w-4" />
          </button>
        </div>
      )}
      
      {/* Connectors - Flat List */}
      {loadingConnectors ? (
        <div className="flex flex-col items-center justify-center py-12">
          <Loader2 className="h-5 w-5 animate-spin text-gray-400" />
          <p className="mt-3 text-sm text-gray-500">Loading integrations...</p>
        </div>
      ) : sortedConnectors.length > 0 ? (
        <div className={viewMode === 'grid' ? 'grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4' : 'space-y-3'}>
          {sortedConnectors.map(connector => (
            <ConnectorCard
              key={connector.id}
              connector={connector}
              providerId={provider.id}
              onConnected={() => refetch()}
              viewMode={viewMode}
            />
          ))}
        </div>
      ) : connectors && connectors.length > 0 ? (
        <div className="flex flex-col items-center justify-center py-12">
          <div className="h-12 w-12 rounded-lg bg-gray-100 flex items-center justify-center mb-4">
            <Search className="h-6 w-6 text-gray-400" />
          </div>
          <h3 className="text-sm font-medium text-gray-900 mb-1">
            No matching integrations
          </h3>
          <p className="text-sm text-gray-500 mb-4">
            Try adjusting your search or filter criteria
          </p>
          <button
            onClick={() => { setSearchQuery(''); setFilterMode('all') }}
            className="text-sm text-gray-700 hover:text-gray-900 font-medium underline underline-offset-2"
          >
            Clear filters
          </button>
        </div>
      ) : (
        <EmptyState onSync={handleSync} isSyncing={syncConnectors.isPending} />
      )}
    </div>
  )
}

// ============================================================================
// Empty State
// ============================================================================

interface EmptyStateProps {
  onSync: () => void
  isSyncing: boolean
}

function EmptyState({ onSync, isSyncing }: EmptyStateProps) {
  return (
    <div className="flex flex-col items-center justify-center py-16">
      <div className="h-16 w-16 rounded-xl bg-gray-100 flex items-center justify-center mb-4">
        <Plug className="h-8 w-8 text-gray-400" />
      </div>
      <h2 className="text-lg font-medium text-gray-900 mb-1">
        No Integrations Found
      </h2>
      <p className="text-sm text-gray-500 mb-6 max-w-sm text-center">
        Fetch available integrations from your tool pack to get started.
      </p>
      <button
        onClick={onSync}
        disabled={isSyncing}
        className="flex items-center gap-2 px-4 py-2 bg-gray-900 text-white rounded-md text-sm font-medium hover:bg-gray-800 transition-colors disabled:opacity-50"
      >
        {isSyncing ? (
          <Loader2 className="h-4 w-4 animate-spin" />
        ) : (
          <RefreshCw className="h-4 w-4" />
        )}
        Fetch Integrations
      </button>
    </div>
  )
}

// ============================================================================
// Connector Card
// ============================================================================

interface ConnectorCardProps {
  connector: Connector
  providerId: string
  onConnected: () => void
  viewMode: ViewMode
}

// ConnectorCard - Uses Merge Agent Handler Link React hook
// Per docs: https://docs.ah.merge.dev/get-started/setup-link
// React component: https://github.com/merge-api/react-agent-handler-link

function ConnectorCard({ connector, providerId, onConnected, viewMode }: ConnectorCardProps) {
  const [expanded, setExpanded] = useState(false)
  const [connecting, setConnecting] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [linkToken, setLinkToken] = useState<string | undefined>(undefined)
  const generateLinkToken = useGenerateLinkToken()
  const handleCallback = useHandleLinkCallback()
  
  // Use the Merge Agent Handler Link React hook
  const { open: openLink, isReady } = useAgentHandlerLink({
    linkToken: linkToken || '',
    onSuccess: async () => {
      // Verify connection with the backend (which checks Merge API)
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
    setConnecting(true)
    setError(null)
    try {
      // Generate link token from backend using registered user
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
  }, [connector.connector_id, generateLinkToken, providerId])
  
  // Get logo URL (prefer logo_url, fallback to icon_url)
  const logoUrl = connector.logo_url || connector.icon_url
  // Get categories (prefer categories array, fallback to single category)
  const categories = connector.categories?.length ? connector.categories : [connector.category]
  
  if (viewMode === 'grid') {
    return (
      <div className="bg-white border border-gray-200 rounded-lg p-4 transition-all hover:border-gray-300 hover:shadow-sm">
        <div className="flex items-start gap-3">
          <div className={`h-12 w-12 rounded-lg flex items-center justify-center flex-shrink-0 ${
            connector.is_connected ? 'bg-green-50' : 'bg-gray-100'
          }`}>
            {logoUrl ? (
              <img 
                src={logoUrl} 
                alt={connector.name} 
                className="h-8 w-8 object-contain"
              />
            ) : (
              <Plug className={`h-6 w-6 ${connector.is_connected ? 'text-green-600' : 'text-gray-400'}`} />
            )}
          </div>
          
          <div className="flex-1 min-w-0">
            <div className="flex items-center gap-2">
              <h4 className="text-sm font-medium text-gray-900 truncate">
                {connector.name}
              </h4>
              {connector.is_connected && (
                <Check className="h-4 w-4 text-green-600 flex-shrink-0" />
              )}
            </div>
            {connector.source_url && (
              <a 
                href={connector.source_url} 
                target="_blank" 
                rel="noopener noreferrer"
                onClick={(e) => e.stopPropagation()}
                className="inline-flex items-center gap-1 text-xs text-blue-600 hover:text-blue-700 hover:underline mt-0.5"
              >
                <ExternalLink className="h-3 w-3" />
                {new URL(connector.source_url).hostname.replace('www.', '')}
              </a>
            )}
          </div>
        </div>
        
        {/* Category tags */}
        <div className="mt-3 flex flex-wrap gap-1">
          {categories.slice(0, 3).map((cat) => (
            <span 
              key={cat}
              className="text-[10px] px-1.5 py-0.5 bg-gray-100 text-gray-600 rounded"
            >
              {cat}
            </span>
          ))}
        </div>
        
        {/* Tool count */}
        <div className="mt-2 flex items-center gap-1.5 text-xs text-gray-500">
          <Zap className="h-3 w-3" />
          <span>{connector.tool_count} tools</span>
        </div>
        
        {connector.is_connected ? (
          <div className="mt-3 flex items-center gap-1.5 text-xs text-gray-500">
            <User className="h-3 w-3" />
            <span>{connector.connected_by || 'Connected'}</span>
          </div>
        ) : (
          <button
            onClick={handleConnect}
            disabled={connecting}
            className="mt-3 w-full flex items-center justify-center gap-2 px-3 py-2 bg-gray-900 hover:bg-gray-800 text-white text-xs font-medium rounded-md transition-colors disabled:opacity-50"
          >
            {connecting ? (
              <Loader2 className="h-3.5 w-3.5 animate-spin" />
            ) : (
              <Link className="h-3.5 w-3.5" />
            )}
            {connecting ? 'Connecting...' : 'Connect'}
          </button>
        )}
        
        {error && (
          <p className="mt-2 text-xs text-red-600">{error}</p>
        )}
      </div>
    )
  }
  
  // List view
  return (
    <div className="bg-white border border-gray-200 rounded-lg overflow-hidden transition-all hover:border-gray-300 hover:shadow-sm">
      <div 
        className="flex items-center gap-4 p-4 cursor-pointer"
        onClick={() => setExpanded(!expanded)}
      >
        {/* Logo */}
        <div className={`h-12 w-12 rounded-lg flex items-center justify-center flex-shrink-0 ${
          connector.is_connected ? 'bg-green-50' : 'bg-gray-100'
        }`}>
          {logoUrl ? (
            <img 
              src={logoUrl} 
              alt={connector.name} 
              className="h-8 w-8 object-contain"
            />
          ) : (
            <Plug className={`h-6 w-6 ${connector.is_connected ? 'text-green-600' : 'text-gray-400'}`} />
          )}
        </div>
        
        {/* Info */}
        <div className="flex-1 min-w-0">
          <div className="flex items-center gap-2 flex-wrap">
            <span className="text-sm font-medium text-gray-900">
              {connector.name}
            </span>
            {connector.is_connected && (
              <span className="flex items-center gap-1 text-xs px-2 py-0.5 rounded-full bg-green-50 text-green-700 border border-green-200">
                <Check className="h-3 w-3" />
                Connected
              </span>
            )}
            {/* Category tags */}
            {categories.slice(0, 2).map((cat) => (
              <span 
                key={cat}
                className="text-[10px] px-1.5 py-0.5 bg-blue-50 text-blue-600 rounded"
              >
                {cat}
              </span>
            ))}
          </div>
          <div className="flex items-center gap-3 mt-1 text-xs text-gray-500">
            <span className="flex items-center gap-1">
              <Zap className="h-3 w-3" />
              {connector.tool_count} tools
            </span>
            {connector.source_url && (
              <a 
                href={connector.source_url} 
                target="_blank" 
                rel="noopener noreferrer"
                onClick={(e) => e.stopPropagation()}
                className="flex items-center gap-1 text-blue-600 hover:text-blue-700 hover:underline"
              >
                <ExternalLink className="h-3 w-3" />
                {new URL(connector.source_url).hostname.replace('www.', '')}
              </a>
            )}
            {connector.is_connected && connector.connected_by && (
              <span className="flex items-center gap-1">
                <User className="h-3 w-3" />
                by {connector.connected_by}
              </span>
            )}
          </div>
          {connector.description && (
            <p className="mt-1 text-xs text-gray-500 line-clamp-1">
              {connector.description}
            </p>
          )}
        </div>
        
        {/* Actions */}
        <div className="flex items-center gap-2">
          {!connector.is_connected && (
            <button
              onClick={(e) => {
                e.stopPropagation()
                handleConnect()
              }}
              disabled={connecting}
              className="flex items-center gap-1.5 px-3 py-1.5 bg-gray-900 text-white text-xs font-medium rounded-md hover:bg-gray-800 transition-colors disabled:opacity-50"
            >
              {connecting ? (
                <Loader2 className="h-3.5 w-3.5 animate-spin" />
              ) : (
                <Link className="h-3.5 w-3.5" />
              )}
              {connecting ? 'Connecting...' : 'Connect'}
            </button>
          )}
          {connector.is_connected && connector.source_url && (
            <a
              href={connector.source_url}
              target="_blank"
              rel="noopener noreferrer"
              onClick={(e) => e.stopPropagation()}
              className="flex items-center gap-1.5 px-3 py-1.5 text-xs text-gray-600 hover:text-gray-900 hover:bg-gray-100 rounded-md transition-colors"
            >
              <ExternalLink className="h-3.5 w-3.5" />
              Open
            </a>
          )}
          <ChevronRight 
            className={`h-4 w-4 text-gray-400 transition-transform ${expanded ? 'rotate-90' : ''}`} 
          />
        </div>
      </div>
      
      {error && (
        <div className="px-4 pb-3 -mt-1">
          <p className="text-xs text-red-600">{error}</p>
        </div>
      )}
      
      {/* Expanded Tools */}
      {expanded && connector.tools && connector.tools.length > 0 && (
        <div className="border-t border-gray-200 bg-gray-50 p-4">
          <h5 className="text-xs font-medium text-gray-500 uppercase tracking-wide mb-3">
            Available Tools ({connector.tools.length})
          </h5>
          <div className="space-y-2 max-h-48 overflow-y-auto">
            {connector.tools.map(tool => (
              <div 
                key={tool.id} 
                className="flex items-start gap-3 p-2 bg-white rounded-md border border-gray-100"
              >
                <code className="flex-shrink-0 text-xs bg-gray-100 text-gray-700 px-1.5 py-0.5 rounded font-mono">
                  {tool.id}
                </code>
                {tool.description && (
                  <span className="text-xs text-gray-600">
                    {tool.description}
                  </span>
                )}
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  )
}

export default IntegrationsPanel
