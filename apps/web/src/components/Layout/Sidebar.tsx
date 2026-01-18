/**
 * Sidebar navigation (left rail)
 * Clean light enterprise theme with organization switcher
 */
import { useState } from 'react'
import { Link, useLocation } from 'react-router-dom'
import { useAppSelector } from '../../store/hooks'
import { useOrganizations, useSwitchOrganization } from '../../hooks/useOrganizations'
import { Layers, Plug, Settings, ChevronDown, Check, Building2, Plus, ListTodo } from 'lucide-react'
import { cn } from '../../lib/utils'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
  DropdownMenuSeparator,
} from '../ui/dropdown-menu'
import { CreateOrganizationDialog } from './CreateOrganizationDialog'

const navItems = [
  { path: '/apps', label: 'Apps', icon: Layers },
  { path: '/tasklist', label: 'Tasklist', icon: ListTodo },
  { path: '/integrations', label: 'Integrations', icon: Plug },
  { path: '/settings', label: 'Settings', icon: Settings },
]

export function Sidebar() {
  const location = useLocation()
  const selectedOrgId = useAppSelector((state) => state.ui.selectedOrgId)
  const { data: organizations } = useOrganizations()
  const switchOrg = useSwitchOrganization()
  const selectedOrg = organizations?.find((org) => org.id === selectedOrgId)
  const [createOrgOpen, setCreateOrgOpen] = useState(false)

  if (!selectedOrgId) {
    return null
  }

  return (
    <aside className="w-56 border-r border-gray-200 bg-white flex flex-col">
      {/* Organization Switcher */}
      <div className="p-[11px] border-b border-gray-200">
        <DropdownMenu>
          <DropdownMenuTrigger className="w-full focus:outline-none">
            <div className="flex items-center gap-3 px-2 py-2 rounded-md hover:bg-gray-50 transition-colors">
              {/* Organization Logo or Fallback */}
              {selectedOrg?.logo_url ? (
                <img
                  src={selectedOrg.logo_url}
                  alt={`${selectedOrg.name} logo`}
                  className="h-8 w-8 rounded-md object-cover flex-shrink-0"
                />
              ) : (
                <div className="h-8 w-8 rounded-md bg-gray-100 flex items-center justify-center flex-shrink-0">
                  <Building2 className="h-4 w-4 text-gray-500" />
                </div>
              )}
              <div className="flex-1 min-w-0 text-left">
                <div className="text-sm font-medium text-gray-900 truncate">
                  {selectedOrg?.name || 'Select Org'}
                </div>
                <div className="text-xs text-gray-500 truncate">
                  Organization
                </div>
              </div>
              <ChevronDown className="h-4 w-4 text-gray-400 flex-shrink-0" />
            </div>
          </DropdownMenuTrigger>
          <DropdownMenuContent 
            align="start" 
            className="w-52 bg-white border-gray-200 shadow-lg"
          >
            <div className="px-2 py-1.5 text-[11px] text-gray-400 font-medium uppercase tracking-wide">
              Organizations
            </div>
            {organizations?.map((org) => (
              <DropdownMenuItem
                key={org.id}
                onClick={() => switchOrg.mutate(org.id)}
                className="flex items-center gap-2 text-gray-700 hover:text-gray-900 
                         hover:bg-gray-50 focus:bg-gray-50 cursor-pointer"
              >
                {/* Org logo in dropdown */}
                {org.logo_url ? (
                  <img
                    src={org.logo_url}
                    alt={`${org.name} logo`}
                    className="h-5 w-5 rounded object-cover flex-shrink-0"
                  />
                ) : (
                  <div className="h-5 w-5 rounded bg-gray-100 flex items-center justify-center flex-shrink-0">
                    <Building2 className="h-3 w-3 text-gray-400" />
                  </div>
                )}
                <span className="truncate flex-1">{org.name}</span>
                {selectedOrgId === org.id && (
                  <Check className="h-4 w-4 text-gray-900 flex-shrink-0" />
                )}
              </DropdownMenuItem>
            ))}
            <DropdownMenuSeparator />
            <DropdownMenuItem
              onClick={() => setCreateOrgOpen(true)}
              className="flex items-center gap-2 text-gray-700 hover:text-gray-900 
                       hover:bg-gray-50 focus:bg-gray-50 cursor-pointer"
            >
              <div className="h-5 w-5 rounded bg-gray-100 flex items-center justify-center flex-shrink-0">
                <Plus className="h-3 w-3 text-gray-500" />
              </div>
              <span className="truncate flex-1">Create Organization</span>
            </DropdownMenuItem>
          </DropdownMenuContent>
        </DropdownMenu>
      </div>

      {/* Navigation */}
      <nav className="flex-1 p-3 space-y-1">
        {navItems.map((item) => {
          const Icon = item.icon
          const isActive = location.pathname.startsWith(item.path)
          return (
            <Link
              key={item.path}
              to={item.path}
              className={cn(
                'flex items-center gap-3 px-3 py-2 rounded-md text-sm font-medium transition-colors',
                isActive
                  ? 'bg-gray-100 text-gray-900'
                  : 'text-gray-600 hover:bg-gray-50 hover:text-gray-900'
              )}
            >
              <Icon className={cn('h-4 w-4', isActive ? 'text-gray-700' : 'text-gray-400')} />
              {item.label}
            </Link>
          )
        })}
      </nav>

      {/* Create Organization Dialog */}
      <CreateOrganizationDialog 
        open={createOrgOpen} 
        onOpenChange={setCreateOrgOpen} 
      />
    </aside>
  )
}
