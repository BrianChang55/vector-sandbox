# Phase 1: Questioning Service - Context

**Gathered:** 2026-01-14
**Status:** Needs revision (architectural principle clarified)

<vision>
## How This Should Work

The QuestioningService is a **pure distillation layer**. It gathers information from the user and organizes it for the main agent — nothing more.

The service asks questions and extracts what the user actually said. It does NOT:
- Decide if we have "enough" information
- Make inferences about what the user probably wants
- Fill in gaps with guesses
- Judge the quality of answers

The **main agent** is the decision-maker. It receives the distilled output and decides:
- Whether there are too many unknowns to proceed
- Whether to ask more questions or start building
- What the user's answers actually mean for implementation

</vision>

<essential>
## What Must Be Nailed

- **Distillation only** — The service extracts and organizes facts, never infers
- **Unknowns are explicit** — Output clearly states what the user didn't answer or was unclear about
- **No sufficiency judgment** — The service reports facts; the main agent decides if they're enough
- **Skip detection** — If user says "just build it", pass that signal to main agent (who decides what to do)

</essential>

<boundaries>
## What's Out of Scope

- **Decision-making** — QuestioningService does NOT decide anything
- **Inference** — No "this probably means X" or "user likely wants Y"
- **Gap-filling** — If user didn't say it, it goes in unknowns, not inferred requirements
- **Sufficiency checking** — Main agent's responsibility, not this service

</boundaries>

<specifics>
## Output Format

The synthesis should output structured facts:
- **Goals** — What the user explicitly said they want to achieve
- **Explicit requirements** — Specific features/behaviors they mentioned
- **UI mentions** — Any UI/UX specifics they described
- **Unknowns** — Things not answered or unclear

No inferences. No guesses. The main agent interprets these facts and makes decisions.

</specifics>

<notes>
## Additional Context

This represents a critical architectural principle that applies to ALL subservices, not just QuestioningService:

**Subservices distill. The main agent decides.**

The questioning service was initially built with LLM-based sufficiency checking (`check_exit_condition`) which violates this principle. The service needs to be revised to:
1. Remove the sufficiency check (or make it report-only, not decision-making)
2. Update synthesis to output facts + unknowns, not inferred requirements

</notes>

---

*Phase: 01-questioning-service*
*Context gathered: 2026-01-14*
