/**
 * Top Navigation Bar (Dark theme)
 */
import { useAppSelector } from '../../store/hooks'
import { useOrganizations, useSwitchOrganization } from '../../hooks/useOrganizations'
import { useLogout } from '../../hooks/useAuth'
import { Button } from '../ui/button'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
  DropdownMenuSeparator,
} from '../ui/dropdown-menu'
import { ChevronDown, Check, User, LogOut, Building2 } from 'lucide-react'

export function TopNav() {
  const selectedOrgId = useAppSelector((state) => state.ui.selectedOrgId)
  const { data: organizations } = useOrganizations()
  const switchOrg = useSwitchOrganization()
  const logout = useLogout()
  const selectedOrg = organizations?.find((org) => org.id === selectedOrgId)

  const handleLogout = () => {
    logout.mutate()
  }

  return (
    <nav className="border-b border-zinc-800/50 bg-zinc-900/50 backdrop-blur-xl h-14 flex items-center justify-between px-4">
      <div className="flex items-center gap-3">
        {/* Org Switcher */}
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button 
              variant="ghost" 
              className="text-sm font-medium text-zinc-300 hover:text-zinc-100 hover:bg-zinc-800/50 gap-2"
            >
              <Building2 className="h-4 w-4 text-zinc-500" />
              {selectedOrg?.name || 'Select Organization'}
              <ChevronDown className="h-3.5 w-3.5 text-zinc-500" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent 
            align="start" 
            className="w-56 bg-zinc-900 border-zinc-800 shadow-xl"
          >
            <div className="px-2 py-1.5 text-xs text-zinc-500 font-medium uppercase tracking-wider">
              Organizations
            </div>
            {organizations?.map((org) => (
              <DropdownMenuItem
                key={org.id}
                onClick={() => switchOrg.mutate(org.id)}
                className="flex items-center justify-between text-zinc-300 hover:text-zinc-100 
                         hover:bg-zinc-800 focus:bg-zinc-800 cursor-pointer"
              >
                <span>{org.name}</span>
                {selectedOrgId === org.id && (
                  <Check className="h-4 w-4 text-violet-400" />
                )}
              </DropdownMenuItem>
            ))}
          </DropdownMenuContent>
        </DropdownMenu>
      </div>

      <div className="flex items-center gap-2">
        {/* User Menu */}
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button 
              variant="ghost" 
              size="sm"
              className="h-8 w-8 p-0 rounded-full bg-zinc-800 hover:bg-zinc-700"
            >
              <User className="h-4 w-4 text-zinc-400" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent 
            align="end" 
            className="w-48 bg-zinc-900 border-zinc-800 shadow-xl"
          >
            <DropdownMenuItem 
              className="text-zinc-300 hover:text-zinc-100 hover:bg-zinc-800 focus:bg-zinc-800 cursor-pointer"
            >
              <User className="h-4 w-4 mr-2" />
              Profile
            </DropdownMenuItem>
            <DropdownMenuSeparator className="bg-zinc-800" />
            <DropdownMenuItem 
              onClick={handleLogout}
              className="text-red-400 hover:text-red-300 hover:bg-red-500/10 focus:bg-red-500/10 cursor-pointer"
            >
              <LogOut className="h-4 w-4 mr-2" />
              Log out
            </DropdownMenuItem>
          </DropdownMenuContent>
        </DropdownMenu>
      </div>
    </nav>
  )
}
