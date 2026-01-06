/**
 * Settings page - Organization and user settings
 */
import { useState, useRef, useEffect } from 'react'
import { useAppSelector, useAppDispatch } from '../store/hooks'
import { logout } from '../store/slices/authSlice'
import { useOrganizations, useUploadOrganizationLogo, useDeleteOrganizationLogo, useUpdateOrganization } from '../hooks/useOrganizations'
import { useOrgMembers } from '../hooks/useMembers'
import { Button } from '../components/ui/button'
import { useDialog } from '../components/ui/dialog-provider'
import { MembersPanel } from '../components/settings/MembersPanel'
import { IntegrationsPanel } from '../components/settings/IntegrationsPanel'
import { User, Building2, LogOut, Shield, Bell, Upload, Trash2, Loader2, Check, Users, Plug } from 'lucide-react'
import { cn } from '../lib/utils'

type SettingsTab = 'profile' | 'organization' | 'members' | 'integrations' | 'security' | 'notifications'

const tabs: { id: SettingsTab; label: string; icon: React.ElementType; adminOnly?: boolean }[] = [
  { id: 'profile', label: 'Profile', icon: User },
  { id: 'organization', label: 'Organization', icon: Building2 },
  { id: 'members', label: 'Members', icon: Users },
  { id: 'integrations', label: 'Integrations', icon: Plug, adminOnly: true },
  { id: 'security', label: 'Security', icon: Shield },
  { id: 'notifications', label: 'Notifications', icon: Bell },
]

export function SettingsPage() {
  const dispatch = useAppDispatch()
  const { user } = useAppSelector((state) => state.auth)
  const selectedOrgId = useAppSelector((state) => state.ui.selectedOrgId)
  const { data: organizations } = useOrganizations()
  const { data: membersData } = useOrgMembers(selectedOrgId)
  const uploadLogo = useUploadOrganizationLogo()
  const deleteLogo = useDeleteOrganizationLogo()
  const updateOrganization = useUpdateOrganization()
  const { confirm, alert } = useDialog()
  
  const currentUserRole = membersData?.current_user_role
  const canManageIntegrations = currentUserRole === 'admin'
  
  const [activeTab, setActiveTab] = useState<SettingsTab>('profile')
  const fileInputRef = useRef<HTMLInputElement>(null)
  const [orgName, setOrgName] = useState('')
  const [orgSlug, setOrgSlug] = useState('')
  const [isOrgSaving, setIsOrgSaving] = useState(false)
  const [orgSaveSuccess, setOrgSaveSuccess] = useState(false)

  const currentOrg = organizations?.find((org) => org.id === selectedOrgId)

  // Generate slug from name: lowercase, replace spaces with hyphens, remove special chars
  const generateSlug = (name: string): string => {
    return name
      .toLowerCase()
      .trim()
      .replace(/\s+/g, '-')
      .replace(/[^a-z0-9-]/g, '')
      .replace(/-+/g, '-')
      .replace(/^-|-$/g, '')
  }

  // Sync org name and slug with current org
  useEffect(() => {
    if (currentOrg) {
      setOrgName(currentOrg.name)
      setOrgSlug(currentOrg.slug)
    }
  }, [currentOrg?.id]) // Only sync when org changes, not on every name update

  // Track if name has changed to avoid unnecessary saves
  const orgNameDirty = useRef(false)

  const handleOrgNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const newName = e.target.value
    const newSlug = generateSlug(newName)
    setOrgName(newName)
    setOrgSlug(newSlug)
    orgNameDirty.current = true
  }

  const handleOrgNameBlur = async () => {
    if (selectedOrgId && orgName.trim() && orgNameDirty.current) {
      setIsOrgSaving(true)
      const startTime = Date.now()
      
      try {
        await updateOrganization.mutateAsync({ orgId: selectedOrgId, data: { name: orgName, slug: orgSlug } })
        orgNameDirty.current = false
        
        // Ensure loader shows for at least 1 second
        const elapsed = Date.now() - startTime
        const remainingTime = Math.max(0, 400 - elapsed)
        
        await new Promise(resolve => setTimeout(resolve, remainingTime))
        setIsOrgSaving(false)
        setOrgSaveSuccess(true)
        setTimeout(() => setOrgSaveSuccess(false), 1500)
      } catch {
        setIsOrgSaving(false)
        // Error handled by mutation
      }
    }
  }

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

  const handleLogoUpload = async (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0]
    if (!file || !selectedOrgId) return

    // Validate file type
    const allowedTypes = ['image/jpeg', 'image/png', 'image/gif', 'image/webp']
    if (!allowedTypes.includes(file.type)) {
      await alert({
        title: 'Invalid File Type',
        description: 'Please upload a JPEG, PNG, GIF, or WebP image.',
        variant: 'destructive',
      })
      return
    }

    // Validate file size (5MB max)
    if (file.size > 5 * 1024 * 1024) {
      await alert({
        title: 'File Too Large',
        description: 'Logo must be less than 5MB.',
        variant: 'destructive',
      })
      return
    }

    try {
      await uploadLogo.mutateAsync({ orgId: selectedOrgId, file })
      await alert({
        title: 'Logo Uploaded',
        description: 'Your organization logo has been updated.',
        variant: 'success',
      })
    } catch (error: any) {
      await alert({
        title: 'Upload Failed',
        description: error?.response?.data?.error || 'Failed to upload logo.',
        variant: 'destructive',
      })
    }

    // Reset file input
    if (fileInputRef.current) {
      fileInputRef.current.value = ''
    }
  }

  const handleDeleteLogo = async () => {
    if (!selectedOrgId) return

    const confirmed = await confirm({
      title: 'Remove Logo',
      description: 'Are you sure you want to remove the organization logo?',
      confirmText: 'Remove',
      cancelText: 'Cancel',
    })

    if (confirmed) {
      try {
        await deleteLogo.mutateAsync(selectedOrgId)
        await alert({
          title: 'Logo Removed',
          description: 'Your organization logo has been removed.',
          variant: 'success',
        })
      } catch (error: any) {
        await alert({
          title: 'Error',
          description: error?.response?.data?.error || 'Failed to remove logo.',
          variant: 'destructive',
        })
      }
    }
  }

  return (
    <div className="flex h-full">
      {/* Left panel: Tabs */}
      <div className="w-56 border-r border-gray-200 bg-white p-4">
        <nav className="space-y-1">
          {tabs.map((tab) => {
            // Hide admin-only tabs for non-admins
            if (tab.adminOnly && !canManageIntegrations) {
              return null
            }
            
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
        <div className={cn(
          "mx-auto p-8",
          activeTab === 'integrations' ? 'max-w-5xl' : 'max-w-2xl'
        )}>
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

                <div className="space-y-3">
                  <div>
                    <span className="text-sm text-gray-500">Name</span>
                    <p className="text-sm text-gray-900">{user?.first_name} {user?.last_name}</p>
                  </div>
                  <div>
                    <span className="text-sm text-gray-500">Email</span>
                    <p className="text-sm text-gray-900">{user?.email}</p>
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
                <div className="space-y-6">
                  {/* Logo Section */}
                  <div className="bg-white rounded-lg border border-gray-200 p-5">
                    <div className="flex items-center gap-4">
                      {/* Logo Preview */}
                      <div className="flex-shrink-0">
                        {currentOrg.logo_url ? (
                          <img
                            src={currentOrg.logo_url}
                            alt={`${currentOrg.name} logo`}
                            className="h-14 w-14 rounded-lg object-cover border border-gray-200"
                          />
                        ) : (
                          <div className="h-14 w-14 rounded-lg bg-gray-100 flex items-center justify-center border border-gray-200">
                            <Building2 className="h-6 w-6 text-gray-400" />
                          </div>
                        )}
                      </div>

                      {/* Upload Controls */}
                      <div className="flex-0 min-w-0">
                        <div className="flex items-center justify-between mb-2">
                          <h2 className="font-medium text-gray-900 text-sm">Logo</h2>
                        </div>
                        
                        <div className="flex items-center gap-2">
                          <input
                            ref={fileInputRef}
                            type="file"
                            accept="image/jpeg,image/png,image/gif,image/webp"
                            onChange={handleLogoUpload}
                            className="hidden"
                          />
                          <Button
                            variant="outline"
                            size="sm"
                            onClick={() => fileInputRef.current?.click()}
                            disabled={uploadLogo.isPending}
                          >
                            {uploadLogo.isPending ? (
                              <Loader2 className="h-4 w-4 mr-2 animate-spin" />
                            ) : (
                              <Upload className="h-4 w-4 mr-2" />
                            )}
                            {currentOrg.logo_url ? 'Change' : 'Upload'}
                          </Button>
                          
                          {currentOrg.logo_url && (
                            <Button
                              variant="ghost"
                              size="sm"
                              onClick={handleDeleteLogo}
                              disabled={deleteLogo.isPending}
                              className="text-red-600 hover:text-red-700 hover:bg-red-50"
                            >
                              {deleteLogo.isPending ? (
                                <Loader2 className="h-4 w-4 mr-2 animate-spin" />
                              ) : (
                                <Trash2 className="h-4 w-4 mr-2" />
                              )}
                              Remove
                            </Button>
                          )}
                        </div>
                      </div>
                    </div>
                  </div>

                  {/* Organization Details */}
                  <div className="bg-white rounded-lg border border-gray-200 p-6">
                    <h2 className="font-medium text-gray-900 mb-4">Details</h2>
                    
                    <div className="space-y-4">
                      <div>
                        <div className="flex items-center justify-between mb-2.5">
                          <label className="block text-sm font-medium text-gray-700">
                            Organization Name
                          </label>
                          {isOrgSaving && (
                            <span className="flex items-center gap-1 text-xs text-gray-500">
                              <Loader2 className="h-3 w-3 animate-spin" />
                              Saving...
                            </span>
                          )}
                          {orgSaveSuccess && !isOrgSaving && (
                            <span className="flex items-center gap-1 text-xs text-green-600">
                              <Check className="h-3 w-3" />
                              Saved
                            </span>
                          )}
                        </div>
                        <input
                          type="text"
                          value={orgName}
                          onChange={handleOrgNameChange}
                          onBlur={handleOrgNameBlur}
                          disabled={isOrgSaving}
                          className="w-full px-3 py-2 border border-gray-200 rounded-md bg-white text-sm text-gray-900 focus:outline-none disabled:opacity-50"
                          placeholder="Organization name"
                        />
                      </div>
                      <div>
                        <label className="block text-sm font-medium text-gray-700 mb-2.5">
                          Slug
                        </label>
                        <input
                          type="text"
                          value={orgSlug}
                          disabled
                          tabIndex={-1}
                          className="w-full px-3 py-2 border border-gray-200 rounded-md bg-gray-50 text-sm text-gray-900 cursor-not-allowed"
                        />
                      </div>
                    </div>

                    <div className="mt-6 pt-4 border-t border-gray-100">
                      <p className="text-xs text-gray-400">
                        Created {new Date(currentOrg.created_at).toLocaleDateString()}
                      </p>
                    </div>
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

          {/* Members Tab */}
          {activeTab === 'members' && selectedOrgId && (
            <div>
              <h1 className="text-xl font-semibold text-gray-900 mb-6">Members</h1>
              <MembersPanel orgId={selectedOrgId} />
            </div>
          )}

          {/* Integrations Tab (Admin only) */}
          {activeTab === 'integrations' && selectedOrgId && canManageIntegrations && (
            <div>
              <h1 className="text-xl font-semibold text-gray-900 mb-6">Integrations</h1>
              <IntegrationsPanel orgId={selectedOrgId} />
            </div>
          )}
        </div>
      </div>
    </div>
  )
}
