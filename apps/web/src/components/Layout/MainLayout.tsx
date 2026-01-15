/**
 * Main layout component (TopNav + Sidebar + Content)
 */
import type { ReactNode } from 'react'
import { TopNav } from './TopNav'
import { Sidebar } from './Sidebar'

export function MainLayout({ children }: { children: ReactNode }) {
  return (
    <div className="flex h-screen flex-col">
      <TopNav />
      <div className="flex flex-1 overflow-hidden">
        <Sidebar />
        <main className="flex-1 overflow-y-auto bg-gray-50">
          {children}
        </main>
      </div>
    </div>
  )
}

