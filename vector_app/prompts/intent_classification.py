"""
Intent Classification Prompts

Prompts for classifying user requests into actionable intents
for the intent-aware agent architecture.

Supports compound intent detection for multi-step requests.
"""

INTENT_CLASSIFICATION_SYSTEM_PROMPT = """You are an expert at understanding user intent for an AI app builder.
Your job is to analyze user requests and classify them into the appropriate action category.

You must consider:
1. What the user is asking for (new feature, edit, fix, etc.)
2. The current state of the app (existing files, tables, components)
3. The scope of changes needed (full rebuild vs surgical edit)
4. Whether the request requires MULTIPLE operations (compound request)

Be precise in your classification. A small styling change should be EDIT_CODE, not GENERATE_NEW.
Adding a new feature to an existing app should be ADD_FEATURE, not GENERATE_NEW.

IMPORTANT: Many requests require MULTIPLE intents executed in sequence. For example:
- "Add a due date field to the TODO inputs" needs BOTH schema modification AND UI code changes
- "Add user email to the form and save it" needs schema change + UI update + write logic
Always identify ALL required operations, not just the first keyword match."""


INTENT_CLASSIFICATION_PROMPT = """Analyze this user request in the context of an existing app and classify the intent.

## User Request
{user_message}

## Existing App Context
- Has existing files: {has_files}
- File count: {file_count}
- Existing components: {components}
- Existing tables: {tables}
- Has data store: {has_data_store}
- Table columns: {table_columns}

## Schema-Aware Classification

When the user mentions data fields, tables, or columns:
1. **Check if the field/table already exists** in the schema above
2. If it exists → likely EDIT_CODE (display change) or ADD_FEATURE (new UI for existing data)
3. If it doesn't exist → likely MODIFY_SCHEMA (need to create it first)
4. Words like "add a column", "new field", "add property" → definitely MODIFY_SCHEMA

Field Existence Check Examples:
- User says "add due date" and tables show "todos: id, title, status" → MODIFY_SCHEMA (due_date doesn't exist)
- User says "show status in a badge" and tables show "todos: id, title, status" → EDIT_CODE (status exists)

## Classification Instructions

First, determine if this is a COMPOUND request that needs multiple operations.

### Compound Request Detection
A request is COMPOUND when it requires changes in multiple areas:
- Adding a new field that needs UI display → MODIFY_SCHEMA + ADD_FEATURE
- Adding form input that needs to persist → ADD_FEATURE + MODIFY_SCHEMA  
- Adding a feature that needs new data storage → ADD_FEATURE + MODIFY_SCHEMA
- Fixing a bug that requires schema changes → FIX_BUG + MODIFY_SCHEMA

Example compound requests:
- "Add a due date field to the TODO inputs in the UI" → Schema change + UI update
- "Add user preferences that users can configure" → Schema + Feature + UI
- "Add email field and validate it on save" → Schema + Code changes

### Intent Types

1. **GENERATE_NEW** - Build from scratch, or user says "rebuild", "start over", "create new"
   - Examples: "Build me a task manager", "Create a dashboard", "Start fresh"

2. **EDIT_CODE** - Modify specific existing code (styling, behavior, text, layout)
   - Examples: "Change the button color", "Make the header smaller", "Update text"

3. **ADD_FEATURE** - Add new functionality (UI components, interactions, displays)
   - Examples: "Add a search feature", "Add pagination", "Add dark mode toggle"

4. **MODIFY_SCHEMA** - Change data structures (add fields, create tables, modify columns)
   - Examples: "Add an email field to users", "Create orders table", "Remove column"

5. **FIX_BUG** - Fix a problem or error
   - Examples: "The delete button doesn't work", "I get an error when saving"

6. **REFACTOR** - Reorganize code without changing functionality
   - Examples: "Split into components", "Extract to separate file", "Clean up code"

## Response Format

Return ONLY valid JSON (no markdown, no code blocks).

For SINGLE intent:
{{
  "intent": "EDIT_CODE",
  "confidence": 0.9,
  "affected_files": ["src/components/Button.tsx"],
  "affected_tables": [],
  "scope": "surgical",
  "reasoning": "User specifically mentions changing button color, which is a targeted CSS edit",
  "is_compound": false
}}

For COMPOUND requests (multiple operations needed):
{{
  "intent": "MODIFY_SCHEMA",
  "confidence": 0.9,
  "affected_files": [],
  "affected_tables": ["todos"],
  "scope": "partial",
  "reasoning": "Adding due date field requires schema change first, then UI updates",
  "is_compound": true,
  "secondary_intents": [
    {{
      "intent": "ADD_FEATURE",
      "confidence": 0.85,
      "affected_files": ["src/components/TodoForm.tsx"],
      "affected_tables": [],
      "scope": "partial",
      "reasoning": "UI needs new date input and form handling for due date"
    }}
  ]
}}

Scope values:
- "full" - Entire app rebuild
- "partial" - Multiple files/components affected
- "surgical" - Single file or very targeted change

CRITICAL: If the request mentions BOTH data/fields AND UI/form/input, it is almost certainly compound."""


AFFECTED_FILES_PROMPT = """Based on the user's request, identify which files are most likely to be affected.

User Request: {user_message}

Existing Files:
{file_list}

Return a JSON array of file paths that will need to be modified:
["src/App.tsx", "src/components/Header.tsx"]

If you're unsure or it could affect many files, return an empty array: []"""


def build_intent_classification_prompt(
    user_message: str,
    has_files: bool,
    file_count: int,
    components: str,
    tables: str,
    has_data_store: bool,
    table_columns: str = "",
) -> str:
    """Build the intent classification prompt with context.
    
    Args:
        user_message: The user's request
        has_files: Whether the app has existing files
        file_count: Number of existing files
        components: Comma-separated list of component names
        tables: Comma-separated list of table names
        has_data_store: Whether the app has a data store
        table_columns: Detailed table schema with columns (e.g., "todos: id, title, status")
    """
    return INTENT_CLASSIFICATION_PROMPT.format(
        user_message=user_message,
        has_files=has_files,
        file_count=file_count,
        components=components or "None",
        tables=tables or "None",
        has_data_store=has_data_store,
        table_columns=table_columns or "None",
    )

