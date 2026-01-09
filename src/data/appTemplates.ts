/**
 * App Templates Data
 * 
 * Each template contains:
 * - id: Unique identifier
 * - title: Short display title
 * - description: Short description for display
 * - prompt: Hidden prompt for the coding agent (NEVER shown to users)
 * - icon: Lucide icon name
 * - color: Background color class
 * - iconColor: Icon color class
 */

import {
  Scale,
  LineChart,
  RefreshCw,
  Rocket,
  MessageSquareMore,
  Target,
  Users,
  AlertTriangle,
  ShieldCheck,
  Wallet,
  BarChart3,
  Handshake,
  type LucideIcon,
} from 'lucide-react'

export interface AppTemplate {
  id: string
  title: string
  description: string
  prompt: string // Hidden - never displayed to users
  Icon: LucideIcon
  color: string
  iconColor: string
  borderHover: string
}

export const APP_TEMPLATES: AppTemplate[] = [
  {
    id: 'deal-desk',
    title: 'Deal Desk & Approvals',
    description: 'Control pricing, discounts, and non-standard terms with structured approvals',
    prompt: `Build an internal Deal Desk web application. The app should include a left-hand navigation menu with sections: Deals, Approval Queue, Pricing Rules, Reports, and Settings. The core data object is DealRequest with fields including account name, opportunity ID, ARR, discount percentage, term length, payment terms, non-standard clauses, deal owner, approving manager, current status, and timestamps. Create a deal intake form with validation and real-time warnings when approval thresholds are exceeded. Implement a rules engine that routes approvals based on configurable conditions (for example: discount > 15% routes to VP Sales; non-standard legal terms route to Legal). Approval UI should allow approve, reject, or request changes with required comments. Include a timeline view showing all actions and decisions. Add SLA timers per approver and escalation alerts. Admin users can configure pricing rules and approver groups. Reporting dashboards should show approval cycle time, discount distribution, bottlenecks, and win rates. Role-based access for reps, managers, finance, legal, and admins.`,
    Icon: Scale,
    color: 'bg-blue-50',
    iconColor: 'text-blue-600',
    borderHover: 'hover:border-blue-200',
  },
  {
    id: 'pipeline-hygiene',
    title: 'Pipeline Hygiene & Forecast',
    description: 'Force pipeline truth before forecast calls',
    prompt: `Build a pipeline inspection and forecasting app with a left sidebar containing My Deals, Team View, Forecast, Hygiene Rules, Reports, and Settings. Deal records include stage, amount, close date, last activity date, and required qualification fields (MEDDICC or similar). Implement a hygiene engine that flags deals missing required fields or recent activity. Create a weekly forecast workflow where reps must categorize each deal as Commit, Best Case, or Pipeline and provide rationale text. Managers receive a review queue with inline comments and override capability. Build dashboards showing rollups by rep, team, and stage, as well as forecast accuracy over time. Add alerts for deals that slip repeatedly or change close dates frequently. Enforce role-based permissions for reps, managers, RevOps, and executives.`,
    Icon: LineChart,
    color: 'bg-emerald-50',
    iconColor: 'text-emerald-600',
    borderHover: 'hover:border-emerald-200',
  },
  {
    id: 'renewals-expansion',
    title: 'Renewals & Expansion',
    description: 'Predict churn before it happens',
    prompt: `Build a renewals management app with navigation sections: Upcoming Renewals, At-Risk Accounts, Expansion Opportunities, Playbooks, Reports. The Account object should store renewal date, ARR, owner, health score components, and notes. Implement a risk workflow where users select a risk reason, define a mitigation plan, assign an executive sponsor, and set confidence levels. Add expansion signals triggered by usage thresholds, feature adoption, or contract flexibility. Include dashboards summarizing retained ARR, at-risk ARR, and expansion upside. Add task tracking tied to renewal milestones. Permissions should support CS, RevOps, Finance, and Exec roles.`,
    Icon: RefreshCw,
    color: 'bg-violet-50',
    iconColor: 'text-violet-600',
    borderHover: 'hover:border-violet-200',
  },
  {
    id: 'customer-onboarding',
    title: 'Customer Onboarding',
    description: 'Run onboarding like a product, not a checklist',
    prompt: `Build an onboarding tracker app with sidebar sections: Active Onboardings, Templates, Risks, Reports. Each onboarding record links to an account and includes milestones, tasks, owners, due dates, and status indicators. Create a sales handoff intake form capturing requirements and success criteria. Implement visual status states (on-track, blocked, at-risk). Allow optional external status page sharing. Add risk flagging with resolution owners. Include a completion checklist and internal retrospective notes once onboarding finishes.`,
    Icon: Rocket,
    color: 'bg-amber-50',
    iconColor: 'text-amber-600',
    borderHover: 'hover:border-amber-200',
  },
  {
    id: 'product-feedback',
    title: 'Product Feedback OS',
    description: 'Turn feedback into roadmap decisions',
    prompt: `Build a feedback intake and triage system with sections: Feedback Inbox, Scoring Board, Roadmap, Reports. Feedback objects include source, customer, ARR impact, summary, tags, and timestamps. Implement similarity detection flags for duplicates. Add scoring fields such as impact, urgency, revenue influence, and effort. Provide a board view (Now, Next, Later). Notify feedback submitters automatically when status changes.`,
    Icon: MessageSquareMore,
    color: 'bg-teal-50',
    iconColor: 'text-teal-600',
    borderHover: 'hover:border-teal-200',
  },
  {
    id: 'competitive-intel',
    title: 'Competitive Intel',
    description: 'Keep battlecards always current',
    prompt: `Build a competitive intelligence app with sections: Competitors, Field Intel, Battlecards, Reports. Competitor profiles should include positioning, pricing notes, strengths, weaknesses, and target segments. Field intel submission form includes deal context and confidence level. Generate battlecard views tailored by persona. Trigger alerts when competitor mentions spike.`,
    Icon: Target,
    color: 'bg-pink-50',
    iconColor: 'text-pink-600',
    borderHover: 'hover:border-pink-200',
  },
  {
    id: 'hiring-pipeline',
    title: 'Hiring Pipeline',
    description: 'Make hiring decisions faster and fairer',
    prompt: `Build a hiring workflow app with sections: Roles, Candidates, Interviews, Decisions. Role intake defines must-haves and scorecard criteria. Candidate pipeline tracks stage progression. Interview feedback forms enforce rubric completion. Decision workflow includes hire/no-hire outcomes, compensation band, and approval logging.`,
    Icon: Users,
    color: 'bg-indigo-50',
    iconColor: 'text-indigo-600',
    borderHover: 'hover:border-indigo-200',
  },
  {
    id: 'incident-tracker',
    title: 'Incident Tracker',
    description: 'Track incidents and prevent repeats',
    prompt: `Build an incident management app with sections: Active Incidents, History, Postmortems, Actions. Incident intake captures severity, impact, and timeline. Allow live updates. Postmortem templates include root cause and contributing factors. Action items are assigned owners and due dates with aging reports.`,
    Icon: AlertTriangle,
    color: 'bg-orange-50',
    iconColor: 'text-orange-600',
    borderHover: 'hover:border-orange-200',
  },
  {
    id: 'vendor-risk',
    title: 'Vendor Risk Reviews',
    description: 'Stay audit-ready without heavy GRC tools',
    prompt: `Build a vendor risk app with sections: Vendors, Reviews, Exceptions, Reports. Vendor inventory includes data types, risk tier, and owner. Review workflows include questionnaire responses and evidence uploads. Exception handling requires approval and expiration tracking. Reporting supports audit exports.`,
    Icon: ShieldCheck,
    color: 'bg-cyan-50',
    iconColor: 'text-cyan-600',
    borderHover: 'hover:border-cyan-200',
  },
  {
    id: 'spend-controls',
    title: 'Spend Controls',
    description: 'Control burn without slowing teams',
    prompt: `Build a spend approval app with sections: Requests, Budgets, Vendors, Reports. Purchase request form enforces policy rules. Approval routing based on thresholds. Budget vs actual dashboards by department. Renewal reminders for recurring spend.`,
    Icon: Wallet,
    color: 'bg-lime-50',
    iconColor: 'text-lime-600',
    borderHover: 'hover:border-lime-200',
  },
  {
    id: 'revops-metrics',
    title: 'RevOps Metrics',
    description: 'One source of GTM truth',
    prompt: `Build a metrics operating system with sections: Metrics, Scorecards, Annotations, Reports. Metric definitions include logic, owner, and source. Weekly and monthly scorecards with trend views. Annotation layer for explaining changes. Role-based metric visibility.`,
    Icon: BarChart3,
    color: 'bg-rose-50',
    iconColor: 'text-rose-600',
    borderHover: 'hover:border-rose-200',
  },
  {
    id: 'partner-pipeline',
    title: 'Partner Pipeline',
    description: 'Make partnerships measurable',
    prompt: `Build a partner management app with sections: Partners, Deals, Co-Marketing, Reports. Partner profiles include tier, goals, and owners. Joint pipeline tracking with attribution. Co-marketing calendar and asset checklist. Reporting on sourced and influenced pipeline.`,
    Icon: Handshake,
    color: 'bg-purple-50',
    iconColor: 'text-purple-600',
    borderHover: 'hover:border-purple-200',
  },
]

// Storage keys and expiration
export const TEMPLATE_STORAGE_KEY = 'pending_template_data'
export const TEMPLATE_EXPIRATION_MS = 5 * 60 * 1000 // 5 minutes

export interface StoredTemplateData {
  templateId: string
  title: string
  description: string
  hiddenPrompt: string
  timestamp: number
}

/**
 * Store template selection with expiration timestamp
 */
export function storeTemplateSelection(template: AppTemplate): void {
  const data: StoredTemplateData = {
    templateId: template.id,
    title: template.title,
    description: template.description,
    hiddenPrompt: template.prompt,
    timestamp: Date.now(),
  }
  localStorage.setItem(TEMPLATE_STORAGE_KEY, JSON.stringify(data))
}

/**
 * Retrieve stored template data if not expired
 */
export function getStoredTemplateData(): StoredTemplateData | null {
  const raw = localStorage.getItem(TEMPLATE_STORAGE_KEY)
  if (!raw) return null

  try {
    const data: StoredTemplateData = JSON.parse(raw)
    const age = Date.now() - data.timestamp
    
    if (age > TEMPLATE_EXPIRATION_MS) {
      localStorage.removeItem(TEMPLATE_STORAGE_KEY)
      return null
    }
    
    return data
  } catch {
    localStorage.removeItem(TEMPLATE_STORAGE_KEY)
    return null
  }
}

/**
 * Clear stored template data
 */
export function clearStoredTemplateData(): void {
  localStorage.removeItem(TEMPLATE_STORAGE_KEY)
}

/**
 * Get template by ID
 */
export function getTemplateById(id: string): AppTemplate | undefined {
  return APP_TEMPLATES.find(t => t.id === id)
}

