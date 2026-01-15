from typing import Any, Dict


PLAN_PROMPT_TEMPLATE = """You are planning an internal app generation task.

User Request: {user_message}
App Name: {app_name}
Available Resources: {available_resources}
Has Existing Spec: {has_existing_spec}

🚨 **CRITICAL PLANNING RULES:**


## 1. App.tsx Requirement (MANDATORY!)

🚨 **EVERY PLAN MUST INCLUDE src/App.tsx IN target_files** 🚨

- **REQUIRED**: At least ONE step must have "src/App.tsx" in its target_files array
- This is the main entry point of the React application
- Typically this should be an "integration" type step (operation_type="generate" or "edit")
- If you have multiple components, App.tsx should import and orchestrate them
- **YOUR PLAN WILL BE REJECTED if src/App.tsx is not included!**

Examples:
- ✅ GOOD: Step with target_files: ["src/App.tsx"] and description explaining how it imports components
- ❌ BAD: No step mentions src/App.tsx - PLAN WILL BE REJECTED


## 2. Parallel Execution with step_order
Each step has a `step_order` (integer) that determines when it executes:
- **step_order=0**: Executes first
- **step_order=1, 2, 3...**: Higher numbers execute after lower numbers complete
- **Steps with the SAME step_order run IN PARALLEL** - they must be 100% independent!

Rules for parallel steps (same step_order):
- They MUST NOT create or modify the same files
- They MUST NOT depend on each other's output
- Each step's description MUST explicitly list the files it will create

Integration/styling steps that hook up components in App.tsx should have a HIGHER step_order \
than the component steps they depend on - they cannot run in parallel with \
steps that create the components they need to import.

## 3. Detailed Step by Step Descriptions. These are CRUCIAL

Each step description must be self-contained and specific enough that an AI executing \
ONLY that step can succeed without seeing other steps. Think of this as design document level \
detail and quality; you must trust that another engineer can execute this step with ONLY its description. \
This should be meticulously detailed, not a generic and underdeveloped description. This can should be up to 3 paragraphs \
long.

=== Example Plan: Kanban Board Feature ===

--- Step 1 of 3 ---
Title: Create Kanban Card and Form Components
step_order: 0
type: component
operation_type: generate
target_files: [src/components/KanbanForm.tsx, src/components/KanbanCard.tsx]

Description:
Build the foundational leaf components for the Kanban system: the task card and the add/edit form modal. These have no dependencies on other Kanban components and will be imported by higher-level components in subsequent steps.

KanbanCard Component (src/components/KanbanCard.tsx):
Accepts `task: Task` prop with fields {{id, title, description, status, assigneeId, assigneeName, assigneeAvatar, dueDate, priority}} and `onClick: (task: Task) => void` callback. 

Layout: Compact card with title at top (max 2 lines, truncate with ellipsis), followed by optional description preview (1 line max). Bottom row shows due date with Calendar icon on left (display in red if overdue) and assignee avatar on right (circular, show tooltip with name on hover). Priority indicated by colored left border: high=red, medium=yellow, low=green. Card is draggable with grab cursor. Hover state elevates the card slightly. Export Task type interface from this file for reuse.

KanbanForm Component (src/components/KanbanForm.tsx):
Modal dialog for creating or editing tasks. Props: `task?: Task` (if provided, form is in edit mode with pre-filled values), `defaultStatus?: 'todo' | 'in_progress' | 'done'` (for new tasks created from a specific column), `onSave: (taskData: Omit<Task, 'id'> | Task) => void`, `onClose: () => void`. 

Structure: Centered modal over dimmed backdrop with blur. Header shows "New Task" or "Edit Task" based on mode, with X close button. Form fields stacked vertically: title (required text input), description (multi-line textarea, optional), status (dropdown with three options), priority (radio buttons with color indicators matching card borders), dueDate (date picker, minimum today), assigneeId (dropdown, "Unassigned" placeholder). Footer with Cancel and Save buttons. Save disabled until title is filled, shows loading spinner during submission. Close on backdrop click or Escape key.

--- Step 2 of 3 ---
Title: Create Kanban Column Component
step_order: 1
type: component
operation_type: generate
target_files: [src/components/KanbanColumn.tsx]

Description:
Build the column container that displays a vertical list of KanbanCard components. This component imports KanbanCard from the previous step and will be imported by KanbanBoard in the next step.

KanbanColumn Component (src/components/KanbanColumn.tsx):
Props: `status: 'todo' | 'in_progress' | 'done'`, `title: string` (display name like "To Do", "In Progress", "Done"), `tasks: Task[]`, `onCardClick: (task: Task) => void`, `onAddTask: (status: string) => void`, `onDragOver: (e: DragEvent) => void`, `onDrop: (e: DragEvent, status: string) => void`, `isDropTarget?: boolean` (visual highlight when card is being dragged over). 

Structure: Fixed-width column with minimum height, scrollable when content overflows. Sticky header at top with column title and task count badge. Scrollable body area renders KanbanCard for each task. Empty state when no tasks: centered muted icon (Inbox for todo, Clock for in_progress, CheckCircle for done), "No tasks" message, and helper text "Drag here or add new". Footer contains full-width "Add Task" button with Plus icon. When isDropTarget is true, column shows highlighted border to indicate valid drop zone.

--- Step 3 of 3 ---
Title: Create Kanban Board Parent Component
step_order: 2
type: component
operation_type: generate
target_files: [src/components/KanbanBoard.tsx]

Description:
Build the parent orchestrator component that manages state and renders three KanbanColumn children. This component imports KanbanColumn and KanbanForm from previous steps and handles all drag-and-drop logic and task CRUD operations.

KanbanBoard Component (src/components/KanbanBoard.tsx):
Props: `tasks: Task[]`, `onTaskCreate: (task: Omit<Task, 'id'>) => Promise<void>`, `onTaskUpdate: (taskId: string, updates: Partial<Task>) => Promise<void>`, `onTaskDelete?: (taskId: string) => Promise<void>`, `loading?: boolean`. 

State: `draggedTaskId: string | null` (track which card is being dragged), `dropTargetStatus: string | null` (which column is hovered during drag), `formOpen: boolean`, `formMode: 'create' | 'edit'`, `editingTask: Task | null`, `formDefaultStatus: string | null`, `saving: boolean`.

Layout: Horizontal row of three KanbanColumn instances with status/title pairs: ('todo', 'To Do'), ('in_progress', 'In Progress'), ('done', 'Done'). Filter tasks array by status field for each column. Pass isDropTarget={{dropTargetStatus === status}} to highlight active drop zone.

Drag-and-drop handlers: onDragStart sets draggedTaskId from event.dataTransfer. onDragOver prevents default and sets dropTargetStatus. onDragLeave clears dropTargetStatus. onDrop extracts taskId, calls onTaskUpdate(taskId, {{status: newStatus}}), clears drag state. Escape key cancels drag operation.

Form integration: Clicking a card opens form in edit mode with that task. Clicking "Add Task" in a column opens form in create mode with that status pre-selected. Form onSave calls either onTaskCreate or onTaskUpdate based on mode, shows saving state, closes form on success. Form onClose resets all form state.

Loading state: When loading=true, render three skeleton columns with pulsing placeholder cards instead of real content.

Error handling: Wrap task operations in try/catch, show toast notification on error. Use optimistic updates where possible (update UI immediately, rollback on failure).

=== End Example Plan ===

## 4. Go Back and Double Check Your Steps
Read each existing steps and see if any forms, modals, etc. are missing. For any missing items \
edit the plan to include that as a previous step.

### Step Type Guidelines:

**"component" steps:**
- Specify EXACT file path(s) this step creates
- List key UI elements: tables, forms, modals, buttons, filters
- Specify user interactions: click handlers, form submissions, navigation
- ✅ "Create src/components/ProjectBoard.tsx: Displays a 3-column Kanban board (To Do, In Progress, Done). Each task card shows title, assignee avatar, and due date. Clicking a card opens edit modal. Include 'Add Task' button per column that opens TaskForm with pre-set status."
- ✅ "Create src/components/TaskForm.tsx: A modal form for creating/editing tasks. Fields: title (required, min 3 chars), description (optional textarea), status (select: todo/in_progress/done), priority (1-5 slider), due_date (date picker). Props: task?: Task for edit mode, onSave callback, onClose callback."
- ❌ "Build the task UI" (no file paths, no specifics)

**"integration" steps:**
- List ALL files being MODIFIED (not created)
- Describe routing structure if adding React Router
- Explain how components connect: shared state, callbacks, context
- ✅ "Modify src/App.tsx: Add React Router with routes: / (Dashboard), /projects (ProjectList), /projects/:id (ProjectBoard), /team (TeamMembers). Import Sidebar and render it persistently on the left. Pass navigation handlers to Sidebar. Add global state for currentUser using useState."
- ❌ "Wire up the app" (no specifics about what's being wired)

**"styling" steps:**
- Specify which files are being styled
- Describe the visual theme, color scheme, or design tokens
- ✅ "Style src/components/Dashboard.tsx and src/components/StatCard.tsx: Apply dark theme with slate-800 backgrounds, rounded-xl cards with subtle ring borders, gradient accent colors (blue-500 to purple-500) for CTAs, and consistent p-6 spacing."
- ❌ "Make it look nice" (no specifics)

**"code" steps (utilities/helpers):**
- Specify file path and exported functions
- Describe function signatures and purposes
- ✅ "Create src/utils/taskHelpers.ts: Export getTasksByStatus(tasks, status) to group tasks, calculateProjectProgress(tasks) returning percentage complete, getOverdueTasks(tasks) filtering by due_date < today, formatDueDate(date) returning 'Today', 'Tomorrow', or formatted date."
- ❌ "Add helper functions" (which helpers? what do they do?)

**"validation" steps:**
- Specify which forms/inputs are being validated
- List validation rules for each field
- Describe how errors are displayed
- ✅ "Add validation to src/components/TaskForm.tsx: title required (min 3, max 100 chars), due_date must be today or future, priority must be 1-5. Show inline error messages below each field in red-500 text. Disable submit button while form is invalid."

### Universal Requirements:
1. **Exact file paths** - Every step must list files it creates or modifies
2. **Self-contained** - Another AI should be able to execute this step with ONLY its description

## 5. Operation Types and Target Files

Each step must specify:
- **target_files**: Array of file paths this step will create or modify
- **operation_type**: The type of operation being performed

**Operation Types:**
- `generate` - Create new files from scratch (new components, new utilities)
- `edit` - Modify existing files (integration, styling changes)
- `add_feature` - Add new functionality to existing code
- `fix` - Fix issues or bugs
- `refactor` - Reorganize or restructure code

**Default operation_type by step type:**
- `component` steps → `generate` (new) or `edit` (modifying existing)
- `integration` steps → `edit`
- `styling` steps → `edit`
- `code` steps → `generate` or `add_feature`
- `validation` steps → `edit`

## 7. Form Dependencies and Foreign Key Ordering
Never assume data will come from a mock data store. Data will either need to be added via form or \
fetched via integration. Do not allocate any steps for creating the DB, that is done separately, instead \
allocate steps for Forms that allow the user to enter data.

When planning forms for data entry, **consider the dependency order based on foreign keys**:

- **Identify all forms needed**: Before assigning step_orders, list every form/component that lets users create or edit data
- **Foreign key dependencies determine order**: If Table A has a foreign key to Table B, the form for Table B must be created/usable BEFORE the form for Table A
- **Parent entities come first**: Users must be able to populate referenced tables before they can create records that reference them

**Example - Stock Trade Tracker:**
If you're building an app to track stock market trades:
- `users` table (trader profiles)
- `trades` table with `user_id` FK → references `users`

Form order:
- **Step 1 (step_order=1)**: Create UserForm component - users need to exist first
- **Step 2 (step_order=2)**: Create TradeForm component - can now select from existing users

**Exception - Integration Data:**
If the referenced data comes from an external integration/connector (not user-entered), you may not need a form for it. For example, if users are synced from an HR system, you don't need a UserForm - just the TradeForm with a user selector.

**Checklist for form planning:**
1. List all tables that need user input forms
2. Map foreign key relationships between them
3. Assign lower step_orders to forms for "parent" entities (referenced tables)
4. Assign higher step_orders to forms for "child" entities (tables with FKs)
5. Skip forms for tables populated by integrations

Generate a plan with 2-5 steps. Return JSON:
{{
    "steps": [
        {{
            "title": "<string>",
            "description": "<detailed description following guidelines above>",
            "target_files": ["<file_path>", ...],
            "operation_type": "<generate|edit|add_feature|fix|refactor>"
            "type": "<step_type>",
            "step_order": <int>,
        }}
    ]
}}

Step types: research, design, code, component, styling, integration, validation
**REMEMBER**: Steps with the same step_order run in parallel and must not conflict!"""


def build_plan_prompt(user_message: str, context: Dict[str, Any]) -> str:
    """Format the planning prompt with user intent and runtime context."""
    data_store_summary = context.get("data_store_summary", "")
    connectors_summary = context.get("connectors_summary", "")
    available_resources = context.get("available_resources", ["none"])

    # Include data store tables in available resources if present
    if data_store_summary:
        available_resources = list(available_resources) + [f"Data Store: {data_store_summary}"]

    # Include connectors in available resources if present
    if connectors_summary:
        available_resources = list(available_resources) + [f"Connectors: {connectors_summary}"]

    return PLAN_PROMPT_TEMPLATE.format(
        user_message=user_message,
        app_name=context.get("app_name", "App"),
        available_resources=", ".join(available_resources) if available_resources else "none",
        has_existing_spec=context.get("has_existing_spec", False),
    )
