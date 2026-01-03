import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { analytics } from './services/analyticsService'
import { logger } from './services/loggingService'
import './index.css'
import App from './App.tsx'

// Initialize services
analytics.init()
logger.init()

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <App />
  </StrictMode>,
)
