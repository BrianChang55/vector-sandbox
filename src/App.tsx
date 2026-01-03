/**
 * Main App component with routing
 */
import { BrowserRouter, Routes, Route } from 'react-router-dom'
import { Provider } from 'react-redux'
import { QueryProvider } from './providers/QueryProvider'
import { DialogProvider } from './components/ui/dialog-provider'
import { store } from './store'
import { MainLayout } from './components/Layout/MainLayout'
import { AuthGuard } from './components/auth/AuthGuard'
import { LandingPage } from './pages/LandingPage'
import { LoginPage } from './pages/LoginPage'
import { SignUpPage } from './pages/SignUpPage'
import { GoogleOAuthCallbackPage } from './pages/GoogleOAuthCallbackPage'
import { MagicLinkVerifyPage } from './pages/MagicLinkVerifyPage'
import { AppsPage } from './pages/AppsPage'
import { AppBuilderPage } from './pages/AppBuilderPage'
import { ResourcesPage } from './pages/ResourcesPage'
import { SettingsPage } from './pages/SettingsPage'

function App() {
  return (
    <Provider store={store}>
      <QueryProvider>
        <DialogProvider>
          <BrowserRouter>
          <Routes>
            {/* Public routes */}
            <Route path="/" element={<LandingPage />} />
            <Route path="/login" element={<LoginPage />} />
            <Route path="/signup" element={<SignUpPage />} />
            <Route path="/auth/google/callback" element={<GoogleOAuthCallbackPage />} />
            <Route path="/auth/magic-link/verify" element={<MagicLinkVerifyPage />} />
            
            {/* Protected routes */}
            <Route
              path="/apps"
              element={
                <AuthGuard>
                  <MainLayout>
                    <AppsPage />
                  </MainLayout>
                </AuthGuard>
              }
            />
            {/* AppBuilder is fullscreen without MainLayout for immersive vibe coding */}
            <Route
              path="/apps/:appId"
              element={
                <AuthGuard>
                  <AppBuilderPage />
                </AuthGuard>
              }
            />
            <Route
              path="/resources"
              element={
                <AuthGuard>
                  <MainLayout>
                    <ResourcesPage />
                  </MainLayout>
                </AuthGuard>
              }
            />
            <Route
              path="/settings"
              element={
                <AuthGuard>
                  <MainLayout>
                    <SettingsPage />
                  </MainLayout>
                </AuthGuard>
              }
            />
          </Routes>
          </BrowserRouter>
        </DialogProvider>
      </QueryProvider>
    </Provider>
  )
}

export default App
