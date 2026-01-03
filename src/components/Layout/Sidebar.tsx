/**
 * Sidebar navigation (left rail)
 * Dark theme with gradient accents
 */
import { Link, useLocation } from 'react-router-dom'
import { useAppSelector } from '../../store/hooks'
import { Layers, Database, Settings, Sparkles } from 'lucide-react'
import { cn } from '../../lib/utils'
import { motion } from 'framer-motion'

const navItems = [
  { path: '/apps', label: 'Apps', icon: Layers },
  { path: '/resources', label: 'Resources', icon: Database },
  { path: '/settings', label: 'Settings', icon: Settings },
]

export function Sidebar() {
  const location = useLocation()
  const selectedOrgId = useAppSelector((state) => state.ui.selectedOrgId)

  if (!selectedOrgId) {
    return null
  }

  return (
    <aside className="w-64 border-r border-zinc-800/50 bg-zinc-900/50 backdrop-blur-xl">
      {/* Logo / Brand */}
      <div className="p-4 border-b border-zinc-800/50">
        <Link to="/apps" className="flex items-center gap-2.5">
          <div className="w-8 h-8 rounded-lg bg-gradient-to-br from-violet-500 to-fuchsia-500 
                        flex items-center justify-center">
            <Sparkles className="h-4 w-4 text-white" />
          </div>
          <span className="font-bold text-zinc-100">Relay</span>
        </Link>
      </div>

      <nav className="p-3 space-y-1">
        {navItems.map((item) => {
          const Icon = item.icon
          const isActive = location.pathname.startsWith(item.path)
          return (
            <Link
              key={item.path}
              to={item.path}
              className={cn(
                'relative flex items-center gap-3 px-3 py-2.5 rounded-lg text-sm font-medium transition-all',
                isActive
                  ? 'text-zinc-100'
                  : 'text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800/50'
              )}
            >
              {isActive && (
                <motion.div
                  layoutId="sidebar-active"
                  className="absolute inset-0 bg-zinc-800/80 border border-zinc-700/50 rounded-lg"
                  transition={{ type: 'spring', duration: 0.3, bounce: 0.1 }}
                />
              )}
              <Icon className={cn('h-4 w-4 relative z-10', isActive && 'text-violet-400')} />
              <span className="relative z-10">{item.label}</span>
            </Link>
          )
        })}
      </nav>

      {/* Pro tip / help section */}
      <div className="absolute bottom-0 left-0 right-0 p-4 border-t border-zinc-800/50">
        <div className="p-3 rounded-lg bg-gradient-to-br from-violet-500/10 to-fuchsia-500/10 
                      border border-violet-500/20">
          <p className="text-xs text-zinc-400 leading-relaxed">
            <span className="text-violet-400 font-medium">Pro tip:</span> Use the AI builder 
            to create apps instantly. Just describe what you need!
          </p>
        </div>
      </div>
    </aside>
  )
}
