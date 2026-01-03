/**
 * Apps list page
 * 
 * Clean, light-themed enterprise dashboard showing all internal apps.
 * Follows the established design system: white backgrounds, gray borders,
 * subtle shadows, and minimal design.
 */
import { Link } from 'react-router-dom'
import { useAppSelector } from '../store/hooks'
import { useApps, useCreateApp } from '../hooks/useApps'
import { Button } from '../components/ui/button'
import { useDialog } from '../components/ui/dialog-provider'
import { 
  Plus, 
  Layers, 
  ArrowRight, 
  Clock,
  CheckCircle,
  Code2,
  FolderOpen
} from 'lucide-react'
import { formatDistanceToNow } from 'date-fns'

export function AppsPage() {
  const selectedOrgId = useAppSelector((state) => state.ui.selectedOrgId)
  const { data: apps, isLoading } = useApps(selectedOrgId || null)
  const createApp = useCreateApp()
  const { prompt } = useDialog()

  const handleCreateApp = async () => {
    const name = await prompt({
      title: 'Create New App',
      description: 'Enter a name for your new internal application.',
      placeholder: 'e.g., Customer Dashboard, Order Manager',
      confirmText: 'Create App',
      required: true,
    })
    
    if (name) {
      createApp.mutate({
        orgId: selectedOrgId!,
        data: { name, description: '', backend_connection: null },
      })
    }
  }

  if (!selectedOrgId) {
    return (
      <div className="flex items-center justify-center h-full bg-gray-50">
        <div className="text-center">
          <Layers className="h-12 w-12 text-gray-300 mx-auto mb-4" />
          <p className="text-gray-500">Please select an organization</p>
        </div>
      </div>
    )
  }

  if (isLoading) {
    return (
      <div className="flex items-center justify-center h-full bg-gray-50">
        <div className="text-gray-500">Loading apps...</div>
      </div>
    )
  }

  return (
    <div className="min-h-full bg-gray-50">
      {/* Header */}
      <div className="bg-white border-b border-gray-200">
        <div className="max-w-6xl mx-auto px-6 py-6">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-xl font-semibold text-gray-900">Apps</h1>
              <p className="text-sm text-gray-500 mt-0.5">
                Build and manage your internal applications
              </p>
            </div>
            <Button onClick={handleCreateApp}>
              <Plus className="h-4 w-4 mr-2" />
              New App
            </Button>
          </div>
        </div>
      </div>

      {/* Content */}
      <div className="max-w-6xl mx-auto px-6 py-6">
        {apps && apps.length > 0 ? (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
            {apps.map((app) => (
              <Link
                key={app.id}
                to={`/apps/${app.id}`}
                className="group block bg-white rounded-lg border border-gray-200 p-5 
                         hover:border-gray-300 hover:shadow-sm transition-all"
              >
                <div className="flex items-start justify-between mb-3">
                  <div className="h-10 w-10 rounded-lg bg-gray-100 flex items-center justify-center">
                    <Layers className="h-5 w-5 text-gray-500" />
                  </div>
                  <ArrowRight className="h-4 w-4 text-gray-300 group-hover:text-gray-500 
                                        group-hover:translate-x-0.5 transition-all" />
                </div>

                <h3 className="font-medium text-gray-900 mb-1 group-hover:text-gray-700">
                  {app.name}
                </h3>
                <p className="text-sm text-gray-500 mb-4 line-clamp-2">
                  {app.description || 'No description'}
                </p>

                <div className="flex items-center gap-3">
                  <span
                    className={`flex items-center gap-1.5 text-xs px-2 py-1 rounded-full ${
                      app.status === 'published'
                        ? 'bg-green-50 text-green-700 border border-green-200'
                        : 'bg-gray-100 text-gray-600 border border-gray-200'
                    }`}
                  >
                    {app.status === 'published' ? (
                      <CheckCircle className="h-3 w-3" />
                    ) : (
                      <Code2 className="h-3 w-3" />
                    )}
                    {app.status_display}
                  </span>
                  
                  <span className="flex items-center gap-1 text-xs text-gray-400">
                    <Clock className="h-3 w-3" />
                    {formatDistanceToNow(new Date(app.updated_at), { addSuffix: true })}
                  </span>
                </div>
              </Link>
            ))}
          </div>
        ) : (
          <div className="flex flex-col items-center justify-center py-16">
            <div className="h-16 w-16 rounded-xl bg-gray-100 flex items-center justify-center mb-4">
              <FolderOpen className="h-8 w-8 text-gray-400" />
            </div>
            <h2 className="text-lg font-medium text-gray-900 mb-1">
              No apps yet
            </h2>
            <p className="text-sm text-gray-500 mb-6 max-w-sm text-center">
              Create your first internal application to get started with building powerful tools.
            </p>
            <Button onClick={handleCreateApp}>
              <Plus className="h-4 w-4 mr-2" />
              Create Your First App
            </Button>
          </div>
        )}
      </div>
    </div>
  )
}
