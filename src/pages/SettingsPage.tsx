/**
 * Settings page - Organization and user settings
 */
import { useState } from 'react'
import { useAppSelector, useAppDispatch } from '../store/hooks'
import { logout } from '../store/slices/authSlice'
import { useOrganizations } from '../hooks/useOrganizations'
import { Button } from '../components/ui/button'
import { useDialog } from '../components/ui/dialog-provider'
import { User, Building2, LogOut, Shield, Bell } from 'lucide-react'
import { cn } from '../lib/utils'

type SettingsTab = 'profile' | 'organization' | 'security' | 'notifications'

const tabs: { id: SettingsTab; label: string; icon: React.ElementType }[] = [
  { id: 'profile', label: 'Profile', icon: User },
  { id: 'organization', label: 'Organization', icon: Building2 },
  { id: 'security', label: 'Security', icon: Shield },
  { id: 'notifications', label: 'Notifications', icon: Bell },
]

export function SettingsPage() {
  const dispatch = useAppDispatch()
  const { user } = useAppSelector((state) => state.auth)
  const selectedOrgId = useAppSelector((state) => state.ui.selectedOrgId)
  const { data: organizations } = useOrganizations()
  const { confirm } = useDialog()
  
  const [activeTab, setActiveTab] = useState<SettingsTab>('profile')

  const currentOrg = organizations?.find((org) => org.id === selectedOrgId)

  const handleLogout = async () => {
    const confirmed = await confirm({
      title: 'Sign Out',
      description: 'Are you sure you want to sign out of your account?',
      confirmText: 'Sign Out',
      cancelText: 'Cancel',
    })
    
    if (confirmed) {
      dispatch(logout())
      window.location.href = '/login'
    }
  }

  return (
    <div className="flex h-full">
      {/* Left panel: Tabs */}
      <div className="w-56 border-r border-gray-200 bg-white p-4">
        <nav className="space-y-1">
          {tabs.map((tab) => {
            const Icon = tab.icon
            const isActive = activeTab === tab.id
            return (
              <button
                key={tab.id}
                onClick={() => setActiveTab(tab.id)}
                className={cn(
                  'w-full flex items-center gap-3 px-3 py-2 rounded-md text-sm font-medium transition-colors text-left',
                  isActive
                    ? 'bg-gray-100 text-gray-900'
                    : 'text-gray-600 hover:bg-gray-50 hover:text-gray-900'
                )}
              >
                <Icon className="h-4 w-4" />
                {tab.label}
              </button>
            )
          })}
        </nav>
        
        <div className="mt-8 pt-4 border-t border-gray-200">
          <button
            onClick={handleLogout}
            className="w-full flex items-center gap-3 px-3 py-2 rounded-md text-sm font-medium text-red-600 hover:bg-red-50 transition-colors"
          >
            <LogOut className="h-4 w-4" />
            Sign Out
          </button>
        </div>
      </div>

      {/* Right panel: Content */}
      <div className="flex-1 bg-gray-50 overflow-y-auto">
        <div className="max-w-2xl mx-auto p-8">
          {/* Profile Tab */}
          {activeTab === 'profile' && (
            <div>
              <h1 className="text-xl font-semibold text-gray-900 mb-6">Profile</h1>
              
              <div className="bg-white rounded-lg border border-gray-200 p-6">
                <div className="flex items-start gap-4 mb-6">
                  <div className="h-16 w-16 rounded-full bg-gray-200 flex items-center justify-center text-gray-500 text-xl font-medium">
                    {user?.first_name?.[0]?.toUpperCase() || user?.email?.[0]?.toUpperCase() || '?'}
                  </div>
                  <div className="flex-1">
                    <h2 className="font-medium text-gray-900">
                      {user?.first_name} {user?.last_name}
                    </h2>
                    <p className="text-sm text-gray-500">{user?.email}</p>
                  </div>
                </div>

                <div className="space-y-4">
                  <div>
                    <label className="block text-sm font-medium text-gray-700 mb-1">
                      First Name
                    </label>
                    <input
                      type="text"
                      value={user?.first_name || ''}
                      readOnly
                      className="w-full px-3 py-2 border border-gray-200 rounded-md bg-gray-50 text-sm"
                    />
                  </div>
                  <div>
                    <label className="block text-sm font-medium text-gray-700 mb-1">
                      Last Name
                    </label>
                    <input
                      type="text"
                      value={user?.last_name || ''}
                      readOnly
                      className="w-full px-3 py-2 border border-gray-200 rounded-md bg-gray-50 text-sm"
                    />
                  </div>
                  <div>
                    <label className="block text-sm font-medium text-gray-700 mb-1">
                      Email
                    </label>
                    <input
                      type="email"
                      value={user?.email || ''}
                      readOnly
                      className="w-full px-3 py-2 border border-gray-200 rounded-md bg-gray-50 text-sm"
                    />
                  </div>
                </div>

                <div className="mt-6 pt-4 border-t border-gray-100">
                  <p className="text-xs text-gray-400">
                    Member since {user?.date_joined ? new Date(user.date_joined).toLocaleDateString() : 'N/A'}
                  </p>
                </div>
              </div>
            </div>
          )}

          {/* Organization Tab */}
          {activeTab === 'organization' && (
            <div>
              <h1 className="text-xl font-semibold text-gray-900 mb-6">Organization</h1>
              
              {currentOrg ? (
                <div className="bg-white rounded-lg border border-gray-200 p-6">
                  <div className="flex items-center gap-3 mb-6">
                    <div className="h-12 w-12 rounded-lg bg-blue-100 flex items-center justify-center text-blue-600">
                      <Building2 className="h-6 w-6" />
                    </div>
                    <div>
                      <h2 className="font-medium text-gray-900">{currentOrg.name}</h2>
                      <p className="text-sm text-gray-500">/{currentOrg.slug}</p>
                    </div>
                  </div>

                  <div className="space-y-4">
                    <div>
                      <label className="block text-sm font-medium text-gray-700 mb-1">
                        Organization Name
                      </label>
                      <input
                        type="text"
                        value={currentOrg.name}
                        readOnly
                        className="w-full px-3 py-2 border border-gray-200 rounded-md bg-gray-50 text-sm"
                      />
                    </div>
                    <div>
                      <label className="block text-sm font-medium text-gray-700 mb-1">
                        Slug
                      </label>
                      <input
                        type="text"
                        value={currentOrg.slug}
                        readOnly
                        className="w-full px-3 py-2 border border-gray-200 rounded-md bg-gray-50 text-sm"
                      />
                    </div>
                  </div>

                  <div className="mt-6 pt-4 border-t border-gray-100">
                    <p className="text-xs text-gray-400">
                      Created {new Date(currentOrg.created_at).toLocaleDateString()}
                    </p>
                  </div>
                </div>
              ) : (
                <div className="bg-white rounded-lg border border-gray-200 p-6 text-center">
                  <Building2 className="h-12 w-12 text-gray-300 mx-auto mb-3" />
                  <p className="text-gray-500">No organization selected</p>
                </div>
              )}
            </div>
          )}

          {/* Security Tab */}
          {activeTab === 'security' && (
            <div>
              <h1 className="text-xl font-semibold text-gray-900 mb-6">Security</h1>
              
              <div className="bg-white rounded-lg border border-gray-200 p-6">
                <h2 className="font-medium text-gray-900 mb-4">Authentication</h2>
                
                <div className="space-y-4">
                  <div className="flex items-center justify-between p-4 bg-gray-50 rounded-lg">
                    <div>
                      <div className="font-medium text-sm text-gray-900">Email Authentication</div>
                      <div className="text-xs text-gray-500">Sign in with magic link</div>
                    </div>
                    <div className="text-xs px-2 py-1 bg-green-100 text-green-700 rounded">Active</div>
                  </div>
                  
                  {user?.google_id && (
                    <div className="flex items-center justify-between p-4 bg-gray-50 rounded-lg">
                      <div>
                        <div className="font-medium text-sm text-gray-900">Google Account</div>
                        <div className="text-xs text-gray-500">Connected via OAuth</div>
                      </div>
                      <div className="text-xs px-2 py-1 bg-green-100 text-green-700 rounded">Connected</div>
                    </div>
                  )}
                </div>

                <div className="mt-6 pt-4 border-t border-gray-100">
                  <Button variant="outline" onClick={handleLogout}>
                    <LogOut className="h-4 w-4 mr-2" />
                    Sign Out All Devices
                  </Button>
                </div>
              </div>
            </div>
          )}

          {/* Notifications Tab */}
          {activeTab === 'notifications' && (
            <div>
              <h1 className="text-xl font-semibold text-gray-900 mb-6">Notifications</h1>
              
              <div className="bg-white rounded-lg border border-gray-200 p-6">
                <p className="text-sm text-gray-500 text-center py-8">
                  Notification preferences coming soon.
                </p>
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
  )
}

