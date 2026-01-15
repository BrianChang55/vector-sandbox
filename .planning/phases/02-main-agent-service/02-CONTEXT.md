# Phase 2: Main Agent Service - Context

**Gathered:** 2026-01-15
**Status:** Ready for planning

<vision>
## How This Should Work

The Main Agent Service is a full orchestrator — not just for QuestioningService, but extensible to make different decisions down the line. It's the central brain that routes user requests through the appropriate flows.

For the questioning flow specifically:
1. Agent ingests the user's request
2. Uses QuestioningSession model to hold chat history
3. Makes control-flow decisions based on that history:
   - Should I ask another question?
   - Which uncertainty is still unresolved?
   - Did the user already answer X?
   - Did the user ask to skip?
   - Is the last answer vague or clear?

These are **control-flow decisions, not content decisions**. The QuestioningService extracts facts; the Main Agent decides what to do with them.

State machine approach with states: `idle → (questioning OR skip) → extracted → ready`

The agent can skip questioning if:
- User explicitly uses skip keywords
- Agent determines enough info based on chat history and extracted facts

</vision>

<essential>
## What Must Be Nailed

- **Clean extensible architecture** — Easy to add new decision types and routes later (build pipelines, multiple intents, etc.)
- **Working questioning flow end-to-end** — User request → questions → extraction → ready to build
- **State machine pattern** — Explicit states make the flow clear and extensible
- **Control-flow intelligence** — Agent makes smart decisions about when to ask more vs proceed

</essential>

<boundaries>
## What's Out of Scope

- **Build/code generation** — This phase ends at "ready to build", actual generation is later phases
- **Complex routing logic** — Just questioning flow for now, other routes (modify app, explain, etc.) come in future phases
- **Complexity detection** — That's Phase 4; for now, agent uses chat history + skip keywords
- **Integration with existing agentic_service** — This runs parallel as a new flow, integration is Phase 8

</boundaries>

<specifics>
## Specific Ideas

- **State machine pattern** — Explicit states: idle, questioning, extracted, ready
- **Use QuestioningSession model** — Persist state to DB, agent loads/saves
- **Parallel to existing flow** — New endpoint/flow specifically for questioning-first builds
- **Future extensibility** — Design for routing to different build pipelines and handling multiple user intents

</specifics>

<notes>
## Additional Context

The key insight is separation of concerns:
- QuestioningService = distillation layer (extracts facts, generates questions)
- Main Agent = decision layer (decides sufficiency, controls flow)

This matches the architectural principle established in Phase 1: subservices distill, the main agent decides.

The agent's decisions are control-flow focused:
- "Do I have enough?" not "What should I build?"
- "Ask more or proceed?" not "What features to include?"

</notes>

---

*Phase: 02-main-agent-service*
*Context gathered: 2026-01-15*
