# Roadmap: GSD Workflow Integration

## Overview

Transform the internal-apps code generation system by adding a multi-turn questioning phase before building, organizing work into structured build phases (scaffold → core → styling → polish), and delivering higher quality apps through better understanding of user intent.

## Domain Expertise

None

## Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Q&A Storage | New QuestioningSession model | Track Q&A pairs separately from chat messages |
| LLM Calls | Separate per phase | More control, compacted context propagation downstream |
| Phase Failures | Retry same phase | Re-run with error context before proceeding |
| Complexity Detection | LLM classification | Let AI assess if request is clear enough |
| Question Limit | LLM decides | AI determines when it has enough info |
| Hook Point | Before intent classification | Question first, then classify refined request |
| Streaming Events | Phase + file-level | Both granular and summary events |
| Context Compaction | File list + summary | Compact but informative downstream context |

## Phases

- [ ] **Phase 1: Questioning Service** - Core questioning logic for multi-turn requirement gathering
- [ ] **Phase 2: Question Templates** - App-type specific templates (dashboard, form, data viewer)
- [ ] **Phase 3: Complexity Detection** - LLM-based classification to skip questioning for simple requests
- [ ] **Phase 4: Build Phase System** - Phase organization (scaffold → core → styling → polish)
- [ ] **Phase 5: Phase-Aware Planning** - Generate separate plans per build phase with context compaction
- [ ] **Phase 6: Progress Streaming** - Real-time phase and file-level progress events via SSE
- [ ] **Phase 7: Integration** - Wire into existing agentic workflow before intent classification

## Phase Details

### Phase 1: Questioning Service
**Goal**: Build the core service that conducts multi-turn questioning to gather requirements
**Depends on**: Nothing (first phase)
**Research**: Unlikely (builds on existing chat/message infrastructure)
**Plans**: TBD

Key deliverables:
- QuestioningSession model for storing Q&A pairs
- QuestioningService with conversation management
- Prompts for requirement gathering
- Integration point in message handling flow

### Phase 2: Question Templates
**Goal**: Create app-type specific question templates that guide the questioning phase
**Depends on**: Phase 1
**Research**: Unlikely (internal data structures)
**Plans**: TBD

Key deliverables:
- Question template data structure
- Templates for: dashboard, form, data viewer, CRUD app, etc.
- Template selection logic based on initial request

### Phase 3: Complexity Detection
**Goal**: LLM-based classification to determine when questioning can be skipped
**Depends on**: Phase 1
**Research**: Unlikely (extends existing intent classifier)
**Plans**: TBD

Key deliverables:
- Complexity classifier prompt
- Integration with intent classification flow
- Skip logic for simple/clear requests

### Phase 4: Build Phase System
**Goal**: Implement the scaffold → core → styling → polish phase structure
**Depends on**: Phase 1
**Research**: Unlikely (internal workflow architecture)
**Plans**: TBD

Key deliverables:
- BuildPhase enum/model
- Phase execution orchestrator
- Phase state tracking
- Retry logic for failed phases

### Phase 5: Phase-Aware Planning
**Goal**: Generate separate plans for each build phase with compacted context
**Depends on**: Phase 4
**Research**: Unlikely (extends existing planning service)
**Plans**: TBD

Key deliverables:
- Per-phase plan generation
- Context compaction (file list + summary)
- Phase plan prompts
- Context propagation between phases

### Phase 6: Progress Streaming
**Goal**: Real-time SSE events for phase and file-level progress
**Depends on**: Phase 4
**Research**: Unlikely (uses existing SSE infrastructure)
**Plans**: TBD

Key deliverables:
- Phase start/end events
- File-level progress events
- Progress event types and handlers
- Frontend event consumption (if needed)

### Phase 7: Integration
**Goal**: Wire everything into the existing agentic workflow
**Depends on**: Phases 1-6
**Research**: Unlikely (wiring existing components)
**Plans**: TBD

Key deliverables:
- Hook questioning before intent classification
- Update agentic service to use build phases
- End-to-end flow testing
- Cleanup and documentation

## Progress

| Phase | Plans Complete | Status | Completed |
|-------|----------------|--------|-----------|
| 1. Questioning Service | 3/3 | Complete | 2026-01-15 |
| 2. Question Templates | 0/TBD | Not started | - |
| 3. Complexity Detection | 0/TBD | Not started | - |
| 4. Build Phase System | 0/TBD | Not started | - |
| 5. Phase-Aware Planning | 0/TBD | Not started | - |
| 6. Progress Streaming | 0/TBD | Not started | - |
| 7. Integration | 0/TBD | Not started | - |
