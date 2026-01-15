/**
 * Mixpanel Analytics Service
 * 
 * Tracks user intent, feature usage, milestones, and outcomes.
 */

import mixpanel from 'mixpanel-browser'

// Environment detection
const IS_PRODUCTION = import.meta.env.MODE === 'production'
const ENVIRONMENT = IS_PRODUCTION ? 'prod' : 'staging'
const MIXPANEL_TOKEN = import.meta.env.VITE_MIXPANEL_TOKEN || ''

interface GlobalProperties {
  event_source: 'frontend'
  service: 'web'
  environment: 'prod' | 'staging'
  platform: 'web'
  user_id?: string
  org_id?: string
  plan?: string
}

interface UserProfile {
  $email?: string
  $first_name?: string
  $last_name?: string
  plan?: string
  org_id?: string
  org_name?: string
}

class AnalyticsService {
  private initialized = false
  private globalProperties: GlobalProperties = {
    event_source: 'frontend',
    service: 'web',
    environment: ENVIRONMENT as 'prod' | 'staging',
    platform: 'web',
  }

  init(): void {
    if (!IS_PRODUCTION) {
      console.log('[Analytics] Disabled in development mode')
      return
    }

    if (this.initialized || !MIXPANEL_TOKEN) {
      if (!MIXPANEL_TOKEN) {
        console.warn('[Analytics] Mixpanel token not configured, analytics disabled')
      }
      return
    }

    try {
      mixpanel.init(MIXPANEL_TOKEN, {
        debug: ENVIRONMENT !== 'prod',
        track_pageview: false,
        persistence: 'localStorage',
        ignore_dnt: false,
        batch_requests: true,
        batch_size: 10,
        batch_flush_interval_ms: 5000,
      })
      
      this.initialized = true
      console.log('[Analytics] Mixpanel initialized')
    } catch (error) {
      console.error('[Analytics] Failed to initialize Mixpanel:', error)
    }
  }

  identify(user: {
    id: string
    email: string
    first_name?: string
    last_name?: string
  }, organization?: {
    id: number
    name: string
    plan?: string
  }): void {
    if (!this.initialized) return

    try {
      mixpanel.identify(user.id)

      this.globalProperties.user_id = user.id
      if (organization) {
        this.globalProperties.org_id = String(organization.id)
        this.globalProperties.plan = organization.plan || 'free'
      }

      const profile: UserProfile = {
        $email: user.email,
        $first_name: user.first_name,
        $last_name: user.last_name,
      }

      if (organization) {
        profile.org_id = String(organization.id)
        profile.org_name = organization.name
        profile.plan = organization.plan || 'free'
      }

      mixpanel.people.set(profile)
      console.log('[Analytics] User identified:', user.id)
    } catch (error) {
      console.error('[Analytics] Failed to identify user:', error)
    }
  }

  reset(): void {
    if (!this.initialized) return

    try {
      mixpanel.reset()
      this.globalProperties = {
        event_source: 'frontend',
        service: 'web',
        environment: ENVIRONMENT as 'prod' | 'staging',
        platform: 'web',
      }
      console.log('[Analytics] Session reset')
    } catch (error) {
      console.error('[Analytics] Failed to reset session:', error)
    }
  }

  track(eventName: string, properties?: Record<string, unknown>): void {
    if (!this.initialized) return

    try {
      const eventProperties = {
        ...this.globalProperties,
        ...properties,
        timestamp: new Date().toISOString(),
      }

      mixpanel.track(eventName, eventProperties)
    } catch (error) {
      console.error('[Analytics] Failed to track event:', eventName, error)
    }
  }

  viewedPage(pageName: string, properties?: Record<string, unknown>): void {
    this.track(`ui.viewed_${pageName}`, properties)
  }
}

export const analytics = new AnalyticsService()

