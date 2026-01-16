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

- [x] **Phase 1: Questioning Service** - Core questioning logic for multi-turn requirement gathering
- [x] **Phase 2: Main Agent Service** - Orchestrator that uses QuestioningService, decides sufficiency, controls flow
- [x] **Phase 3: Questioning Integration** - Hook up services via Celery, new task executes before code generation
- [ ] **Phase 4: Question Templates** - App-type specific templates (dashboard, form, data viewer)
- [ ] **Phase 5: Complexity Detection** - LLM-based classification to skip questioning for simple requests
- [ ] **Phase 6: Build Phase System** - Phase organization (scaffold → core → styling → polish)
- [ ] **Phase 7: Phase-Aware Planning** - Generate separate plans per build phase with context compaction
- [ ] **Phase 8: Progress Streaming** - Real-time phase and file-level progress events via SSE

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

### Phase 2: Main Agent Service
**Goal**: Create the orchestrator that uses QuestioningService and makes all decisions
**Depends on**: Phase 1
**Research**: Unlikely (builds on QuestioningService)
**Plans**: TBD

Key deliverables:
- MainAgentService that orchestrates the questioning flow
- Sufficiency decision logic (using extracted facts from QuestioningService)
- Flow control: when to ask more questions, when to stop
- Integration with existing agentic workflow

This phase implements the "main agent decides" principle established in Phase 1.

### Phase 3: Questioning Integration
**Goal**: Hook up QuestioningService and MainAgentService via Celery, including frontend UI
**Depends on**: Phases 1 & 2
**Research**: Yes (frontend patterns researched)
**Plans**: 5

Key deliverables:
- QUESTIONING status for CodeGenerationJob
- New `run_questioning_phase` Celery task
- /respond and /skip endpoints for user interaction
- Frontend event types and API methods (internal-apps-web-app)
- QuestioningIndicator component and "Skip to build" button

### Phase 4: Question Templates
**Goal**: Create app-type specific question templates that guide the questioning phase
**Depends on**: Phase 3
**Research**: Unlikely (internal data structures)
**Plans**: TBD

Key deliverables:
- Question template data structure
- Templates for: dashboard, form, data viewer, CRUD app, etc.
- Template selection logic based on initial request

### Phase 5: Complexity Detection
**Goal**: LLM-based classification to determine when questioning can be skipped
**Depends on**: Phase 3
**Research**: Unlikely (extends existing intent classifier)
**Plans**: TBD

Key deliverables:
- Complexity classifier prompt
- Integration with intent classification flow
- Skip logic for simple/clear requests

### Phase 5: Build Phase System
**Goal**: Implement the scaffold → core → styling → polish phase structure
**Depends on**: Phase 2
**Research**: Unlikely (internal workflow architecture)
**Plans**: TBD

Key deliverables:
- BuildPhase enum/model
- Phase execution orchestrator
- Phase state tracking
- Retry logic for failed phases

### Phase 6: Phase-Aware Planning
**Goal**: Generate separate plans for each build phase with compacted context
**Depends on**: Phase 5
**Research**: Unlikely (extends existing planning service)
**Plans**: TBD

Key deliverables:
- Per-phase plan generation
- Context compaction (file list + summary)
- Phase plan prompts
- Context propagation between phases

### Phase 7: Progress Streaming
**Goal**: Real-time SSE events for phase and file-level progress
**Depends on**: Phase 5
**Research**: Unlikely (uses existing SSE infrastructure)
**Plans**: TBD

Key deliverables:
- Phase start/end events
- File-level progress events
- Progress event types and handlers
- Frontend event consumption (if needed)

### Phase 8: Integration
**Goal**: Wire everything into the existing agentic workflow
**Depends on**: Phases 1-7
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
| 2. Main Agent Service | 1/1 | Complete | 2026-01-15 |
| 3. Questioning Integration | 5/5 | Complete | 2026-01-16 |
| 4. Question Templates | 0/TBD | Not started | - |
| 5. Complexity Detection | 0/TBD | Not started | - |
| 6. Build Phase System | 0/TBD | Not started | - |
| 7. Phase-Aware Planning | 0/TBD | Not started | - |
| 8. Progress Streaming | 0/TBD | Not started | - |
