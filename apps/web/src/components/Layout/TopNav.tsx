/**
 * Top Navigation Bar
 * Clean light enterprise theme with Vector branding
 */
import { Link } from 'react-router-dom'
import { useLogout } from '../../hooks/useAuth'
import { Button } from '../ui/button'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
  DropdownMenuSeparator,
} from '../ui/dropdown-menu'
import { User, LogOut } from 'lucide-react'
import { Logo } from '../Logo'

export function TopNav() {
  const logout = useLogout()

  const handleLogout = () => {
    logout.mutate()
  }

  return (
    <nav className="border-b border-gray-200 bg-white h-14 flex items-center justify-between px-4">
      {/* Logo / Brand */}
      <Link to="/apps" className="flex items-center gap-2">
        <Logo size="sm" showText />
      </Link>

      <div className="flex items-center gap-2">
        {/* User Menu */}
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button 
              variant="ghost" 
              size="sm"
              className="h-8 w-8 p-0 rounded-full bg-gray-100 hover:bg-gray-200"
            >
              <User className="h-4 w-4 text-gray-500" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent 
            align="end" 
            className="w-48 bg-white border-gray-200 shadow-lg"
          >
            <DropdownMenuItem 
              className="text-gray-700 hover:text-gray-900 hover:bg-gray-50 focus:bg-gray-50 cursor-pointer"
            >
              <User className="h-4 w-4 mr-2" />
              Profile
            </DropdownMenuItem>
            <DropdownMenuSeparator className="bg-gray-100" />
            <DropdownMenuItem 
              onClick={handleLogout}
              className="text-red-600 hover:text-red-700 hover:bg-red-50 focus:bg-red-50 cursor-pointer"
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
