/**
 * Sentry Error Tracking Service
 * 
 * Tracks errors, warnings, and important application logs using Sentry.
 * Only active in production to avoid polluting logs during development.
 */

import * as Sentry from '@sentry/react'

// Environment detection
const IS_PRODUCTION = import.meta.env.MODE === 'production'
const SENTRY_DSN = import.meta.env.VITE_SENTRY_DSN || ''
const SENTRY_ENVIRONMENT = import.meta.env.VITE_SENTRY_ENVIRONMENT || (IS_PRODUCTION ? 'production' : 'development')

// Context that will be attached to all logs
interface LogContext {
  service: 'internal-apps-web'
  environment: 'production' | 'development'
  url?: string
  userAgent?: string
  userId?: string
  orgId?: string
}

class LoggingService {
  private initialized = false
  private context: LogContext = {
    service: 'internal-apps-web',
    environment: IS_PRODUCTION ? 'production' : 'development',
  }

  /**
   * Initialize Sentry error tracking
   */
  init(): void {
    if (this.initialized) {
      return
    }

    if (!SENTRY_DSN) {
      if (IS_PRODUCTION) {
        console.warn('[Logging] Sentry DSN not configured, error tracking disabled')
      } else {
        console.log('[Logging] Sentry disabled in development mode')
      }
      this.initialized = true
      return
    }

    try {
      Sentry.init({
        dsn: SENTRY_DSN,
        environment: SENTRY_ENVIRONMENT,
        sendDefaultPii: true,
        tracesSampleRate: 0.1,
        profilesSampleRate: 0.1,
        replaysSessionSampleRate: 0.1,
        replaysOnErrorSampleRate: 1.0,
        integrations: [
          Sentry.replayIntegration({
            maskAllText: false,
            blockAllMedia: false,
          }),
          Sentry.browserTracingIntegration(),
        ],
        ignoreErrors: [
          'top.GLOBALS',
          'originalCreateNotification',
          'canvas.contentDocument',
          'NetworkError',
          'Network request failed',
          '404',
        ],
        beforeSend(event, _hint) {
          if (event.contexts) {
            event.contexts = {
              ...event.contexts,
              custom: {
                service: 'internal-apps-web',
              },
            }
          }
          return event
        },
      })

      this.context.url = window.location.href
      this.context.userAgent = navigator.userAgent
      this.initialized = true
      console.log('[Logging] Sentry initialized')
    } catch (error) {
      console.error('[Logging] Failed to initialize Sentry:', error)
      this.initialized = true
    }
  }

  setUser(userId: string, orgId?: string): void {
    this.context.userId = userId
    if (orgId) {
      this.context.orgId = orgId
    }

    if (this.initialized && SENTRY_DSN) {
      Sentry.setUser({
        id: userId,
        username: userId,
      })
      Sentry.setContext('organization', {
        id: orgId,
      })
    }
  }

  clearUser(): void {
    delete this.context.userId
    delete this.context.orgId

    if (this.initialized && SENTRY_DSN) {
      Sentry.setUser(null)
    }
  }

  error(message: string, data?: Record<string, unknown>): void {
    const logData = { ...this.context, ...data }
    console.error(`[Error] ${message}`, logData)

    if (this.initialized && SENTRY_DSN) {
      Sentry.captureException(new Error(message), {
        contexts: {
          custom: logData,
        },
        tags: {
          error_type: 'manual',
        },
      })
    }
  }

  warn(message: string, data?: Record<string, unknown>): void {
    const logData = { ...this.context, ...data }
    console.warn(`[Warn] ${message}`, logData)

    if (this.initialized && SENTRY_DSN) {
      Sentry.captureMessage(message, {
        level: 'warning',
        contexts: {
          custom: logData,
        },
      })
    }
  }

  info(message: string, data?: Record<string, unknown>): void {
    const logData = { ...this.context, ...data }

    if (!IS_PRODUCTION) {
      console.info(`[Info] ${message}`, logData)
    }

    if (this.initialized && SENTRY_DSN) {
      Sentry.addBreadcrumb({
        message,
        level: 'info',
        data: logData,
      })
    }
  }

  debug(message: string, data?: Record<string, unknown>): void {
    const logData = { ...this.context, ...data }

    if (!IS_PRODUCTION) {
      console.debug(`[Debug] ${message}`, logData)
    }

    if (this.initialized && SENTRY_DSN) {
      Sentry.addBreadcrumb({
        message,
        level: 'debug',
        data: logData,
      })
    }
  }

  async flush(): Promise<void> {
    if (this.initialized && SENTRY_DSN) {
      await Sentry.flush(2000)
    }
  }

  apiError(endpoint: string, statusCode: number, errorMessage: string, data?: Record<string, unknown>): void {
    const error = new Error(`API Error: ${endpoint}`)
    
    if (this.initialized && SENTRY_DSN) {
      Sentry.captureException(error, {
        contexts: {
          custom: {
            ...this.context,
            endpoint,
            statusCode,
            errorMessage,
            ...data,
          },
        },
        tags: {
          error_type: 'api_error',
          status_code: String(statusCode),
        },
      })
    }
    
    console.error(`[API Error] ${endpoint}`, { statusCode, errorMessage, ...data })
  }
}

export const logger = new LoggingService()

