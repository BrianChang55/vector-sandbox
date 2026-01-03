/**
 * Landing page - clean, minimal, Vercel/Retool style
 */
import { useNavigate } from 'react-router-dom'
import { Button } from '@/components/ui/button'

export function LandingPage() {
  const navigate = useNavigate()

  return (
    <div className="min-h-screen bg-white">
      {/* Header */}
      <header className="border-b border-gray-200 bg-white">
        <div className="mx-auto flex max-w-7xl items-center justify-between px-6 py-4">
          <div className="flex items-center gap-2">
            <div className="h-8 w-8 rounded bg-gray-900"></div>
            <span className="text-lg font-semibold text-gray-900">Internal Apps</span>
          </div>
          <div className="flex items-center gap-3">
            <Button
              variant="ghost"
              onClick={() => navigate('/login')}
            >
              Sign In
            </Button>
            <Button
              onClick={() => navigate('/signup')}
            >
              Get Started
            </Button>
          </div>
        </div>
      </header>

      {/* Hero Section */}
      <main className="mx-auto max-w-7xl px-6 py-20">
        <div className="mx-auto max-w-3xl text-center">
          <h1 className="text-5xl font-bold tracking-tight text-gray-900 sm:text-6xl">
            Build internal tools
            <br />
            <span className="text-gray-600">faster than ever</span>
          </h1>
          <p className="mt-6 text-lg leading-8 text-gray-600">
            Create powerful internal applications with AI. Connect your data sources,
            build custom interfaces, and deploy in minutes—not weeks.
          </p>
          <div className="mt-10 flex items-center justify-center gap-4">
            <Button
              size="lg"
              onClick={() => navigate('/signup')}
            >
              Get Started
            </Button>
            <Button
              variant="outline"
              size="lg"
              onClick={() => navigate('/login')}
            >
              Sign In
            </Button>
          </div>
        </div>

        {/* Feature Preview */}
        <div className="mt-24 rounded-lg border border-gray-200 bg-gray-50 p-8">
          <div className="mx-auto max-w-2xl text-center">
            <h2 className="text-2xl font-semibold text-gray-900">
              Everything you need to build internal tools
            </h2>
            <p className="mt-4 text-gray-600">
              AI-powered app generation, visual builder, version control, and more.
            </p>
          </div>
        </div>
      </main>

      {/* Footer */}
      <footer className="border-t border-gray-200 bg-white">
        <div className="mx-auto max-w-7xl px-6 py-12">
          <div className="flex items-center justify-between">
            <div className="flex items-center gap-2">
              <div className="h-6 w-6 rounded bg-gray-900"></div>
              <span className="text-sm font-medium text-gray-900">Internal Apps</span>
            </div>
            <p className="text-sm text-gray-500">
              © {new Date().getFullYear()} Internal Apps. All rights reserved.
            </p>
          </div>
        </div>
      </footer>
    </div>
  )
}

