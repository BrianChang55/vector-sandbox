# Phase 3: Questioning Integration - Research

**Researched:** 2026-01-15
**Domain:** React frontend patterns for chat-based AI workflow in internal-apps-web-app
**Confidence:** HIGH

<research_summary>
## Summary

Researched the frontend architecture of internal-apps-web-app to understand how to integrate the questioning phase UI. The frontend uses React 19 with TypeScript, Redux Toolkit for UI state, React Query for server state, and a sophisticated SSE streaming pattern with custom fetch-based parsing.

The chat interface is implemented in `AgenticChatPanel.tsx` (2300+ lines), which handles message rendering, SSE event processing, and workflow phase indicators. Existing patterns for "thinking" and "planning" indicators can be extended to show a "questioning" phase.

**Primary recommendation:** Add new `questioning_*` event types to agent.ts, extend JobStatus to include `questioning` status, add `QuestioningIndicator` component following existing `ThinkingIndicator` pattern, and add "Skip to build" button in the input area next to ModelSelector.

</research_summary>

<standard_stack>
## Standard Stack

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| React | 19.2.0 | UI framework | Core framework |
| TypeScript | 5.9.3 | Type safety | All code is typed |
| Vite | 7.2.4 | Build tool | Dev server on port 5176 |
| Redux Toolkit | 2.11.0 | UI state | App-wide state (selected org/app/version) |
| React Query | 5.59.0 | Server state | Data fetching, caching, invalidation |

### UI Components
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| Radix UI | various | Primitives | Dialog, dropdown, select, tabs |
| CVA | 0.7.1 | Variants | Component variant styling |
| Lucide React | 0.556.0 | Icons | All icons |
| Framer Motion | 12.23.25 | Animations | Enter/exit, layout animations |
| Tailwind CSS | 3.4.18 | Styling | All styles |
| clsx + tailwind-merge | via cn() | Class merging | Conditional classes |

### Relevant to Questioning UI
| File | Purpose | Notes |
|------|---------|-------|
| `src/services/agentService.ts` | SSE streaming, event handling | Add questioning event types |
| `src/types/agent.ts` | Event types, state interfaces | Add QuestioningEventData types |
| `src/components/builder/AgenticChatPanel.tsx` | Chat UI | Add QuestioningIndicator, skip button |
| `src/services/apiService.ts` | API calls | Add /respond endpoint call |

</standard_stack>

<architecture_patterns>
## Architecture Patterns

### Project Structure (relevant paths)
```
src/
├── components/builder/
│   └── AgenticChatPanel.tsx   # Main chat UI (2300+ lines)
├── services/
│   ├── agentService.ts        # SSE streaming, event reducer
│   ├── apiService.ts          # Centralized API (axios)
│   └── api.ts                 # Axios instance
├── types/
│   └── agent.ts               # AgentEventType, AgentState, etc.
└── lib/
    └── utils.ts               # cn() class merger
```

### Pattern 1: SSE Event Handling (from agentService.ts)
**What:** Custom fetch-based SSE parsing (not EventSource) for auth support
**When to use:** All streaming from backend
**Example:**
```typescript
// src/services/agentService.ts
const reader = response.body?.getReader()
const decoder = new TextDecoder()
let buffer = ''

while (true) {
  const { done, value } = await reader.read()
  if (done) break

  buffer += decoder.decode(value, { stream: true })
  const lines = buffer.split('\n')
  buffer = lines.pop() || ''

  for (const line of lines) {
    if (line.startsWith('event: ')) eventType = line.slice(7).trim()
    if (line.startsWith('data: ')) eventData = line.slice(6)
    if (line === '' && eventType && eventData) {
      const event = { type: eventType, timestamp: new Date().toISOString(), data: JSON.parse(eventData) }
      options.onEvent(event)
    }
  }
}
```

### Pattern 2: Phase Indicator Component (from AgenticChatPanel.tsx)
**What:** Animated indicator with timer for long-running phases
**When to use:** Any workflow phase that takes >1s
**Example:**
```tsx
// ThinkingIndicator pattern - copy for QuestioningIndicator
function ThinkingIndicator({ startTime }: { startTime: number }) {
  const [elapsed, setElapsed] = useState(0)

  useEffect(() => {
    const interval = setInterval(() => {
      setElapsed(Math.floor((Date.now() - startTime) / 1000))
    }, 1000)
    return () => clearInterval(interval)
  }, [startTime])

  return (
    <div className="flex items-center gap-3 py-3">
      <motion.div
        className="w-8 h-8 rounded-lg bg-gray-100 border border-gray-200 flex items-center justify-center"
        animate={{ boxShadow: [...] }}
        transition={{ duration: 2, repeat: Infinity }}
      >
        <Lightbulb className="h-4 w-4 text-gray-500" />
      </motion.div>
      <div>
        <span className="text-sm text-gray-600">Thought for </span>
        <span className="text-sm font-medium text-gray-900">{elapsed}s</span>
      </div>
    </div>
  )
}
```

### Pattern 3: Event Type Union (from agent.ts)
**What:** Discriminated union for all SSE event types
**When to use:** Type safety for event handling
**Example:**
```typescript
export type AgentEventType =
  | 'agent_start'
  | 'phase_change'
  | 'thinking'
  | 'plan_created'
  // ... add new questioning events here
  | 'questioning_started'
  | 'question_asked'
  | 'questioning_complete'
```

### Pattern 4: Job Status Tracking (from agentService.ts)
**What:** JobStatus interface tracks generation state
**When to use:** Showing current job phase to user
**Example:**
```typescript
export interface JobStatus {
  job_id: string
  status: 'queued' | 'processing' | 'streaming' | 'complete' | 'failed' | 'cancelled'
  // Add 'questioning' status
}
```

### Anti-Patterns to Avoid
- **Don't use EventSource:** Doesn't support custom auth headers; use fetch-based SSE
- **Don't duplicate event parsing:** All SSE logic in agentService.ts, components just handle events
- **Don't modify messages array directly:** Always use setMessages with functional update
</architecture_patterns>

<dont_hand_roll>
## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| SSE parsing | Custom parser | Existing fetch loop in agentService.ts | Auth headers, error handling |
| Animated indicators | New animation system | Framer Motion like ThinkingIndicator | Consistent UX, tested |
| API calls | Direct fetch | apiService.ts / api.ts axios | Error handling, auth tokens |
| Class merging | String concat | cn() from lib/utils | Handles conflicts |
| Event state | Manual state | agentStateReducer pattern | Centralized, tested |

**Key insight:** The frontend already has robust patterns for all needed functionality. The questioning UI should extend existing patterns, not create new ones.
</dont_hand_roll>

<common_pitfalls>
## Common Pitfalls

### Pitfall 1: Stale Closure in Event Handlers
**What goes wrong:** Event handler captures old state, updates don't reflect
**Why it happens:** useCallback dependencies missing or callbacks stored in refs
**How to avoid:** Use functional setState updates: `setMessages(prev => [...prev, msg])`
**Warning signs:** State updates "lost" or out of order

### Pitfall 2: Message Array Mutation
**What goes wrong:** UI doesn't re-render after "updating" messages
**Why it happens:** Direct array mutation instead of creating new array
**How to avoid:** Always spread: `[...prev.slice(0, -1), updatedMsg]`
**Warning signs:** Console shows state changed but UI unchanged

### Pitfall 3: SSE Buffer Handling
**What goes wrong:** Events split across chunks parsed incorrectly
**Why it happens:** Chunks don't respect event boundaries
**How to avoid:** Keep buffer between reads, only parse complete lines
**Warning signs:** JSON parse errors, missing events

### Pitfall 4: Loading State Race Conditions
**What goes wrong:** Multiple jobs started, or UI stuck in loading
**Why it happens:** isLoading not checked before starting new request
**How to avoid:** Guard all request triggers with `if (isLoading) return`
**Warning signs:** Duplicate requests in network tab
</common_pitfalls>

<code_examples>
## Code Examples

### Adding New Event Type (agent.ts)
```typescript
// src/types/agent.ts
export type AgentEventType =
  | 'agent_start'
  // ... existing types
  // Add questioning events
  | 'questioning_started'
  | 'question_asked'
  | 'questioning_skipped'
  | 'questioning_complete'

// Add event data interfaces
export interface QuestioningStartedData {
  session_id: string
  initial_request: string
}

export interface QuestionAskedData {
  question: string
  question_number: number
}

export interface QuestioningCompleteData {
  facts: Record<string, any>
  question_count: number
  was_skipped: boolean
}
```

### Adding Skip Button (AgenticChatPanel.tsx input area)
```tsx
// In the input area div, between ModelSelector and send button
<div className="mt-3 flex items-center justify-between gap-3 px-4 py-3 bg-white rounded-b-xl">
  <ModelSelector ... />

  {/* Skip button - only show during questioning phase */}
  {isQuestioning && (
    <button
      onClick={handleSkipQuestioning}
      className="text-xs text-gray-600 hover:text-gray-900 px-3 py-1.5
                 rounded-lg border border-gray-200 hover:border-gray-300 transition-colors"
    >
      Skip to build
    </button>
  )}

  <button onClick={handleSendClick} ... >
    ...
  </button>
</div>
```

### Questioning Indicator Component
```tsx
// Similar to ThinkingIndicator, with MessageCircle icon
import { MessageCircle } from 'lucide-react'

function QuestioningIndicator({ questionNumber }: { questionNumber: number }) {
  return (
    <div className="flex items-center gap-3 py-3">
      <motion.div
        className="w-8 h-8 rounded-lg bg-blue-50 border border-blue-200 flex items-center justify-center"
        animate={{
          boxShadow: [
            '0 0 0 0 rgba(59, 130, 246, 0)',
            '0 0 0 4px rgba(59, 130, 246, 0.1)',
            '0 0 0 0 rgba(59, 130, 246, 0)',
          ]
        }}
        transition={{ duration: 2, repeat: Infinity, ease: 'easeInOut' }}
      >
        <MessageCircle className="h-4 w-4 text-blue-500" />
      </motion.div>
      <div>
        <span className="text-sm text-gray-600">Gathering requirements </span>
        <span className="text-sm font-medium text-gray-900">· Question {questionNumber}</span>
      </div>
    </div>
  )
}
```

### API Call for Skip/Respond
```typescript
// src/services/apiService.ts (add to existing exports)
export const questioningApi = {
  respond: (jobId: string, message: string) =>
    api.post<void>(`/jobs/${jobId}/respond/`, { message }),

  skip: (jobId: string) =>
    api.post<void>(`/jobs/${jobId}/skip/`),
}
```
</code_examples>

<sota_updates>
## State of the Art (2025-2026)

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| React 18 | React 19 | 2024 | New hooks, concurrent features |
| Separate state libs | Redux Toolkit + React Query | 2023+ | UI state vs server state separation |
| EventSource | Fetch-based SSE | 2023+ | Auth header support |

**New tools/patterns to consider:**
- React 19's use() hook could simplify async state (not yet used in codebase)
- Server components (not applicable - this is client-side)

**Deprecated/outdated:**
- Class components (codebase is fully functional)
- Direct Redux (codebase uses RTK)
</sota_updates>

<open_questions>
## Open Questions

1. **Question input vs regular input**
   - What we know: Same textarea used for initial request and question responses
   - What's unclear: Should questioning responses use the same input or a distinct UI?
   - Recommendation: Use same input, differentiate via job status check

2. **Multi-turn reconnection**
   - What we know: Existing reconnectToJob() replays all events
   - What's unclear: Will questioning events replay correctly after page refresh?
   - Recommendation: Ensure questioning events follow same pattern as generation events
</open_questions>

<sources>
## Sources

### Primary (HIGH confidence)
- `/Users/henry/Desktop/internal-apps/internal-apps-web-app/src/services/agentService.ts` - SSE implementation
- `/Users/henry/Desktop/internal-apps/internal-apps-web-app/src/types/agent.ts` - Event types
- `/Users/henry/Desktop/internal-apps/internal-apps-web-app/src/components/builder/AgenticChatPanel.tsx` - Chat UI

### Secondary (MEDIUM confidence)
- Explored codebase patterns via Task agent

### Tertiary (LOW confidence - needs validation)
- None - all findings from direct codebase analysis
</sources>

<metadata>
## Metadata

**Research scope:**
- Core technology: React 19 + TypeScript frontend
- Ecosystem: Redux Toolkit, React Query, Framer Motion, Tailwind
- Patterns: SSE streaming, phase indicators, event handling
- Pitfalls: State management, async race conditions

**Confidence breakdown:**
- Standard stack: HIGH - direct codebase analysis
- Architecture: HIGH - read source files
- Pitfalls: HIGH - observed patterns in existing code
- Code examples: HIGH - based on existing components

**Research date:** 2026-01-15
**Valid until:** 2026-02-15 (30 days - internal codebase, stable patterns)
</metadata>

---

*Phase: 03-questioning-integration*
*Research completed: 2026-01-15*
*Ready for planning: yes*
