# GSD Workflow Integration

## What This Is

Enhancement to the internal-apps code generation system that integrates Get Shit Done (GSD) workflow principles. Adds a multi-turn questioning phase before code generation, organizes work into build phases (scaffold → core logic → styling → polish), and produces higher quality generated apps through better planning.

## Core Value

Generated apps are higher quality because the AI truly understands what the user wants before building, and follows structured phases with clear plans.

## Requirements

### Validated (Existing in Codebase)
- Multi-phase agentic workflow (Research → Plan → Execute → Validate → Fix)
- Intent classification for code generation requests
- SSE streaming for real-time feedback
- Chat session and message persistence
- Planning service that creates step-by-step plans
- LLM integration via OpenRouter with multiple models
- Background job support via Celery

### Active (To Build)
- [ ] Multi-turn questioning phase before code generation
- [ ] Question templates for different app types (dashboard, form, data viewer, etc.)
- [ ] Build phase organization (scaffold → core logic → styling → polish)
- [ ] Phase-aware plan generation (each phase gets its own plan)
- [ ] Phase progress tracking and streaming events
- [ ] Ability to skip questioning for simple/clear requests (detect complexity)

## Technical Context

- **Interface**: Chat-based (existing chat UI)
- **Questioning UX**: Multi-turn conversation before building
- **Flow**: Question first, then build
- **Streaming**: Required (SSE already in place)

## Build Phases

1. **Scaffold** - File structure, imports, basic component shells
2. **Core Logic** - Business logic, data handling, state management
3. **Styling** - UI polish, responsive design, visual refinement
4. **Polish** - Edge cases, error handling, final touches

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
