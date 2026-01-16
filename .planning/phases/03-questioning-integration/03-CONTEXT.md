# Phase 3: Questioning Integration - Context

**Gathered:** 2026-01-15
**Status:** Ready for planning

<vision>
## How This Should Work

When a user requests to build an app, a new Celery task (`run_questioning_phase`) runs first, before code generation. The MainAgentService controls the flow — it decides whether to ask more questions based on chat history, using QuestioningService to generate questions and extract facts.

The task is robust: when a question is emitted, the task ends. The user's response triggers a new task invocation. All state lives in the database (QuestioningSession + ChatMessages), so it survives worker restarts.

**Frontend integration is part of this phase.** The questioning conversation renders in the chat UI with a distinct "questioning mode" indicator (similar to the existing "planning done" / "generating" phases). Users see the back-and-forth Q&A as assistant and user messages.

A "Skip to build" button appears near the message input, allowing users to bypass questioning at any time. Clicking it immediately extracts facts from whatever was discussed and starts generation.

The flow:
1. User sends initial request → job created with QUESTIONING status
2. `run_questioning_phase` runs → MainAgentService decides → emits question SSE event
3. Frontend shows question in chat UI (questioning mode indicator visible)
4. User responds (or clicks Skip) → new task triggered
5. Loop until MainAgentService says "proceed" → extracted facts stored → `run_agentic_generation` triggered
6. Existing generation flow continues with extracted facts as context

</vision>

<essential>
## What Must Be Nailed

- **Clean handoff to generation** — Extracted facts flow seamlessly to `run_agentic_generation` with no context loss
- **Reliable multi-turn loop** — The question → respond → question cycle works smoothly without losing state
- **SSE events for questions** — Frontend receives question events and displays them in chat UI
- **Frontend questioning mode** — Visual indicator that we're gathering requirements, with "Skip to build" button near input

</essential>

<boundaries>
## What's Out of Scope

- Question templates (app-type specific like dashboard, form) — that's Phase 4
- Complexity detection (auto-skip questioning for simple requests) — later phase
- Mobile optimization — desktop first
- Analytics on questioning effectiveness — not this phase

</boundaries>

<specifics>
## Specific Ideas

- Job status field + SSE events (belt and suspenders) — status tells frontend the state, events give real-time updates
- Default to questioning first, but API supports `skip_questioning=true` parameter for the frontend skip button
- Questioning mode indicator follows existing pattern (like "planning done", "generating" indicators)
- "Skip to build" button near message input — clicking it extracts whatever was discussed and starts generation immediately (no confirmation needed)
- Task ends after emitting question (more robust than long-polling)

</specifics>

<notes>
## Additional Context

This phase spans both backend (internal-apps-backend) and frontend (internal-apps-web-app).

The frontend work involves:
- Rendering questioning conversation in existing chat UI
- Adding questioning mode visual indicator
- Adding "Skip to build" button near input
- Handling new SSE event types for questions

Priority is robustness — the multi-turn loop must work reliably across worker restarts and browser refreshes.

</notes>

---

*Phase: 03-questioning-integration*
*Context gathered: 2026-01-15*
