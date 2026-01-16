---
phase: 02-main-agent-service
plan: 01
subsystem: services
tags: [llm, state-machine, orchestrator, questioning]

# Dependency graph
requires:
  - phase: 01-questioning-service
    provides: [QuestioningService, extract_facts, generate_question, detect_skip_request]
provides:
  - MainAgentService orchestrator
  - AgentState enum (idle, questioning, skip, extracted, ready)
  - AgentDecision dataclass
  - LLM-based sufficiency check
affects: [phase-3-templates, phase-4-complexity, phase-8-integration]

# Tech tracking
tech-stack:
  added: []
  patterns: [state-machine-pattern, orchestrator-pattern, decision-layer]

key-files:
  created:
    - vector_app/services/main_agent_service.py
    - vector_app/prompts/main_agent.py
  modified: []

key-decisions:
  - "Use Haiku for sufficiency decisions (fast control-flow)"
  - "Hard limit of 5 questions with graceful fallback"
  - "Singleton pattern matching IntentClassifier and QuestioningService"

patterns-established:
  - "Decision pattern: orchestrator decides, subservices distill"
  - "State machine: idle → questioning/skip → extracted → ready"

issues-created: []

# Metrics
duration: 1 min
completed: 2026-01-15
---

# Phase 2 Plan 1: MainAgentService Core Summary

**MainAgentService orchestrator with state machine pattern for questioning flow control-flow decisions**

## Performance

- **Duration:** 1 min
- **Started:** 2026-01-15T22:23:59Z
- **Completed:** 2026-01-15T22:25:44Z
- **Tasks:** 3
- **Files created:** 2

## Accomplishments

- Created MainAgentService as central orchestrator for questioning flow
- Implemented state machine: idle → questioning/skip → extracted → ready
- Added LLM-based sufficiency check using Haiku for fast decisions
- Established decision-layer pattern (orchestrator decides, subservices distill)

## Task Commits

Each task was committed atomically:

1. **Task 1: Create MainAgentService with state machine** - `71ad7cc` (feat)
2. **Task 2: Add sufficiency decision prompts** - `1cc3403` (feat)
3. **Task 3: Implement LLM-based sufficiency check** - `37e0d49` (feat)

## Files Created/Modified

- `vector_app/services/main_agent_service.py` - MainAgentService orchestrator with state machine and LLM decision logic
- `vector_app/prompts/main_agent.py` - Sufficiency decision prompts for control-flow decisions

## Decisions Made

- **Haiku for sufficiency decisions** — Fast model for control-flow decisions (15s timeout), not content decisions
- **Hard limit of 5 questions** — Prevents infinite questioning loops with graceful fallback on LLM errors
- **Singleton pattern** — Matches IntentClassifier and QuestioningService patterns

## Deviations from Plan

None — plan executed exactly as written.

## Issues Encountered

None — all tasks completed without issues.

## Next Phase Readiness

- Phase 2 Plan 1 complete: MainAgentService orchestrator ready
- Implements the "main agent decides" principle from Phase 1
- Ready for next plan (if any) or Phase 3 (Question Templates)

---
*Phase: 02-main-agent-service*
*Completed: 2026-01-15*
