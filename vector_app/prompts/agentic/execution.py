"""
Prompt templates used by the agentic execution workflow.
"""

from typing import Any, Dict, List, Optional, Sequence

from .connectors import CONNECTORS_PROMPT_TEMPLATE
from .data_store import DATA_STORE_PROMPT_TEMPLATE
from .design import DESIGN_STYLE_PROMPT
from .guards import OVER_EAGERNESS_GUARD


STEP_PROMPT_TEMPLATE = """Step {step_number}: {step_title}
Description: {step_description}

User's Original Request: {user_message}
App Name: {app_name}

Available Data Resources:
{resources_info}
{existing_code}

CRITICAL: Generate complete, working React code for this step.

{over_eagerness_guard}

## DATA STORAGE INSTRUCTIONS (CRITICAL - READ CAREFULLY)

### The dataStore Module is ALWAYS Pre-Generated
The file `src/lib/dataStore.ts` is AUTOMATICALLY provided by the runtime environment.
You do NOT need to create it. Simply import and use it:

```typescript
import {{ dataStore }} from '../lib/dataStore';  // For components in src/components/
import {{ dataStore }} from './lib/dataStore';   // For App.tsx in src/
```

### NEVER Create Custom Store Files (CRITICAL!)
The `dataStore` is the ONLY data API. NEVER create entity-specific stores:

❌ FORBIDDEN - DO NOT CREATE THESE FILES:
- `import {{ habitStore }} from './habitStore'` - WRONG!
- `import {{ taskStore }} from '../stores/taskStore'` - WRONG!
- `import {{ noteStore }} from './lib/noteStore'` - WRONG!
- `import {{ expenseStore }} from './expenseStore'` - WRONG!

✅ CORRECT - Use the SINGLE dataStore for ALL data:
```typescript
import {{ dataStore }} from '../lib/dataStore';

// For events: use the TABLE SLUG, not a custom store
await dataStore.query('events', {{}});
await dataStore.insert('events', {{ title: 'Team Meeting', start_time: '2024-01-15T10:00:00Z' }});
await dataStore.update('events', rowId, {{ attendee_count: 5 }});
await dataStore.delete('events', rowId);
```

### Creating Data Tables
If your app needs to store data, define table schemas using this EXACT format:

🚨 **CRITICAL: DO NOT define 'id', 'created_at', or 'updated_at' columns!**
These are AUTOMATICALLY added by the system to every table:
- `id` (uuid, primary key, auto-generated)
- `created_at` (datetime, auto-set on create)
- `updated_at` (datetime, auto-set on update)

If you need a custom identifier (like project code, ticket number, etc.), use a DIFFERENT name:
- ✅ `project_code`, `ticket_number`, `sku`, `product_id`, etc.
- ❌ NOT `id` (reserved by system)

```table:your-table-slug
name: Your Table Name
description: What this table stores
columns:
  - name: title, type: string, nullable: false
  - name: status, type: string, default: active
  - name: priority, type: integer, default: 0
```

Then use the table slug in dataStore calls: `dataStore.query('your-table-slug')`

### 🎯 CRITICAL: Import TypeScript Types for Type Safety

**🚨 DO NOT CREATE OR EDIT `src/lib/types.ts` - IT IS THE DATABASE SOURCE OF TRUTH!**

The file `src/lib/types.ts` reflects the ACTUAL database schema - the ONLY fields and attributes that exist.
**THIS IS READ-ONLY - YOU CAN ONLY IMPORT IT, NEVER CREATE OR MODIFY IT!**

✅ **ALWAYS import and use these types to ensure correct field names:**

```typescript
import {{ dataStore }} from '../lib/dataStore';
import type {{ Database }} from '../lib/types';

// Type-safe insert - TypeScript validates field names!
const newItem: Database['your-table-slug']['insert'] = {{
  title: 'Example',        // ✅ TypeScript ensures this field exists
  status: 'active'         // ✅ TypeScript ensures this field exists
  // text: 'wrong'         // ❌ TypeScript error if field doesn't exist
}};
await dataStore.insert('your-table-slug', newItem);

// Type-safe query results
const result = await dataStore.query('your-table-slug', {{}});
const items: Database['your-table-slug']['row'][] = result.rows;
```

**Why this matters:** The types file contains the EXACT field names from the database schema. If you try to use a field that doesn't exist, TypeScript will show an error at compile time, preventing runtime errors.

### Using dataStore in Components (DO NOT recreate dataStore.ts)

```typescript
import {{ dataStore }} from '../lib/dataStore';
import type {{ Database }} from '../lib/types';

// In a component:
const [items, setItems] = useState<Database['your-table-slug']['row'][]>([]);
useEffect(() => {{
  dataStore.query('your-table-slug', {{}}).then(r => setItems(r.rows || []));
}}, []);
```

## OUTPUT FORMAT

You MUST format each file exactly like this:

```src/App.tsx
import React from 'react';
import type {{ Database }} from ../lib/types';  // 🚨 REQUIRED if using dataStore!
import {{ dataStore }} from './lib/dataStore';

// ... complete code here
export default function App() {{ ... }}
```

```src/components/TaskList.tsx
import React from 'react';
import type {{ Database }} from '../lib/types';  // 🚨 REQUIRED if using dataStore!
import {{ dataStore }} from '../lib/dataStore';

// Component code here
```

Requirements:
- **🚨 DO NOT HALLUCINATE FIELDS 🚨**: Before using ANY field in a dataStore call, CHECK types.ts above to verify it exists
- **COMMON MISTAKE**: Do NOT invent fields like `is_active`, `isActive`, `active_status` - use ONLY the exact field names shown in types.ts
- **VERIFICATION STEP**: For each dataStore.query/insert/update call:
  1. Look at types.ts above
  2. Find the table you're accessing
  3. Check that EVERY field you use is listed in that table's type definition
  4. If a field doesn't exist, DON'T USE IT - change your approach or add it to the schema first
- Use TypeScript with proper types
- **🚨 CRITICAL - CHECK YOUR CODE FOR STRAY QUOTES 🚨**: Every line must end cleanly with semicolon `;` NOT `;'` or `;"` - these cause parse errors!
- **CRITICAL**: Ensure all TypeScript syntax is valid - no stray quotes, unclosed brackets, or malformed type declarations
- **MANDATORY**: If a file uses dataStore, it MUST import `type {{ Database }} from '../lib/types'`
- **MANDATORY**: Use `Database['table-slug']['insert']` types for ALL insert operations
- **MANDATORY**: Use `Database['table-slug']['row']` types for ALL query results
- **CRITICAL - AVOID SYNTAX ERRORS**:
  ```typescript
  // ❌ WRONG - Stray quotes cause parse errors
  owner: Database['projects']['row'];'  // Extra quote at end!
  status: string";  // Malformed!

  // ✅ CORRECT - Clean syntax, use string literal keys
  owner: Database['projects']['row'];
  status: string;
  interface Foo extends Database['projects']['row'] {{ extra: string; }}

  // ✅ ALSO CORRECT - Import and use TableSlug enum
  import type {{ Database }} from '../lib/types';
  import {{ TableSlug }} from '../lib/types';
  interface Foo extends Database[TableSlug.Projects]['row'] {{ extra: string; }}
  ```
- Use Tailwind CSS for all styling (available via CDN)
- **AVAILABLE NPM PACKAGES**: Only use `react`, `react-dom`, `lucide-react`, `framer-motion`, `react-router-dom`
- **DO NOT import**: Any other npm packages not listed above
- For navigation between views, use `react-router-dom` (BrowserRouter, Routes, Route, Link, useNavigate)
- For apps with data, ALWAYS create tables and use dataStore - NO hardcoded mock data
- Create a complete, functional UI that looks professional
- Include proper loading states and error handling
- Make sure the code is complete and runnable - no placeholders or TODOs

If building a table/data display:
- Create the data table first using TABLE_DEFINITION format above
- Use dataStore.query() to fetch real data
- Add edit/delete functionality using dataStore.update()/delete()
- Include search/filter if appropriate"""
FINAL_APP_PROMPT_TEMPLATE = """Generate the MAIN App.tsx file that creates a complete, polished application.

User's Request: {user_message}
App Name: {app_name}

Generated Components Available:
{components_info}

Other Files Generated:
{other_files}

CRITICAL REQUIREMENTS:
1. Create a COMPLETE App.tsx that is the main entry point
2. Import and USE all the generated components
3. Include proper state management for the app
4. Add a professional header/layout
5. Use dataStore to fetch and display real data from the database
6. Make it look polished and production-ready
7. Use Tailwind CSS for all styling
8. Handle loading and error states
9. Make it interactive (search, filters, etc. as appropriate)
10. **🚨 DO NOT HALLUCINATE FIELDS 🚨**: Before using ANY field in a dataStore call, CHECK types.ts to verify it exists - Do NOT invent fields like `is_active`, `isActive`, `active_status`
11. **FIELD VERIFICATION**: For each dataStore.query/insert/update call, verify EVERY field you use is listed in types.ts for that specific table
12. **🚨 DIFFERENT TABLES = DIFFERENT FIELDS 🚨**: Projects use `name`, Tasks use `title` - NEVER assume field names! Example: `projects.name` ✅ but `tasks.name` ❌ (use `tasks.title` instead)
13. **🚨 CRITICAL - CHECK YOUR CODE FOR STRAY QUOTES 🚨**: Every line must end cleanly with semicolon `;` NOT `;'` or `;"` - these cause parse errors!
14. **CRITICAL**: Ensure all TypeScript syntax is valid - no stray quotes, unclosed brackets, or malformed type declarations
15. **ONLY import from**: react, react-dom, lucide-react, framer-motion, react-router-dom
16. **DO NOT use**: Any other npm packages not listed above
17. For navigation, use react-router-dom (BrowserRouter, Routes, Route, Link, useNavigate)
18. **CRITICAL - AVOID SYNTAX ERRORS**:
    ```typescript
    // ❌ WRONG - Stray quotes cause parse errors
    owner: Database['projects']['row'];'  // Extra quote at end!
    status: string";  // Malformed!

    // ✅ CORRECT - Clean syntax, use string literal keys
    owner: Database['projects']['row'];
    status: string;
    interface Foo extends Database['projects']['row'] {{ extra: string; }}

    // ✅ ALSO CORRECT - Import and use TableSlug enum
    import type {{ Database }} from './lib/types';
    import {{ TableSlug }} from './lib/types';
    interface Foo extends Database[TableSlug.Projects]['row'] {{ extra: string; }}
    ```

OUTPUT FORMAT - Generate ONLY the App.tsx file:
```src/App.tsx
import React, {{ useState }} from 'react';
// ... imports for all components
// ... your complete app code
export default function App() {{
  // ... complete implementation
}}
```

DO NOT generate placeholder code or TODOs. Generate the COMPLETE working app."""

# Dedicated system prompt for the final App.tsx synthesis step.
FINAL_APP_SYSTEM_PROMPT = (
    "You are an expert React developer. Generate complete, production-ready code. Never use placeholders."
)
def apply_design_style_prompt(
    message: str,
    data_store_context: Optional[str] = None,
    connectors_context: Optional[str] = None,
) -> str:
    """Append the shared design/style guide, data store context, and connectors context to a user message."""
    base_message = (message or "").strip()
    design_prompt = f"------ Design & Style Requirements ------\n\n{DESIGN_STYLE_PROMPT}"

    parts = []
    if base_message:
        parts.append(base_message)

    # Add data store context if provided
    if data_store_context:
        data_store_prompt = DATA_STORE_PROMPT_TEMPLATE.format(data_store_context=data_store_context)
        parts.append(data_store_prompt)

    # Add connectors context if provided
    if connectors_context:
        connectors_prompt = CONNECTORS_PROMPT_TEMPLATE.format(connectors_context=connectors_context)
        parts.append(connectors_prompt)

    parts.append(design_prompt)

    return "\n\n".join(parts)
def build_step_prompt(
    step: Any,
    step_index: int,
    user_message: str,
    context: Dict[str, Any],
    existing_files: Sequence[Any],
    registry_surface: Dict[str, Any],
    data_store_context: Optional[str] = None,
    connectors_context: Optional[str] = None,
) -> str:
    """Build the per-step execution prompt for streaming code generation."""
    existing_code = ""
    typescript_types_section = ""

    # Check if types.ts exists in existing files - show it in full (not truncated)
    types_ts_file = None
    other_files = []

    if existing_files:
        for f in existing_files:
            path = getattr(f, "path", "")
            if path == "src/lib/types.ts":
                types_ts_file = f
            else:
                other_files.append(f)

    # If types.ts exists, show it prominently with full content
    if types_ts_file:
        content = getattr(types_ts_file, "content", "")

        # Extract table names from the Database type definition
        import re
        table_slugs = re.findall(r"'([^']+)':\s*\{", content)
        table_list = ", ".join(f"'{t}'" for t in table_slugs) if table_slugs else "none"

        # Generate concrete examples using the first table if available
        concrete_examples = ""
        if table_slugs:
            first_table = table_slugs[0]
            # Try to extract field names for the first table
            # Look for the insert type fields
            insert_match = re.search(rf"'{first_table}':\s*\{{\s*insert:\s*\{{([^}}]+)\}}", content, re.DOTALL)
            if insert_match:
                fields_str = insert_match.group(1)
                # Extract field names (look for patterns like "field_name:")
                field_names = re.findall(r'(\w+):', fields_str)
                if field_names:
                    # Show first 2-3 fields as example
                    example_fields = field_names[:3]
                    fields_example = "\n  ".join(f"{fname}: 'value'," for fname in example_fields)
                    concrete_examples = f"""

**CONCRETE EXAMPLE using YOUR actual table '{first_table}':**

```typescript
import {{ dataStore }} from '../lib/dataStore';
import type {{ Database }} from '../lib/types';

// ✅ Type-safe insert with ACTUAL field names from YOUR schema
const newItem: Database['{first_table}']['insert'] = {{
  {fields_example}
}};
await dataStore.insert('{first_table}', newItem);

// ✅ Type-safe query results
const result = await dataStore.query('{first_table}', {{}});
const items: Database['{first_table}']['row'][] = result.rows;
```

**DO THIS FOR ALL YOUR TABLES**: Replace '{first_table}' with the actual table slug you're working with.
"""

        typescript_types_section = f"""
{'='*80}
🚨🚨🚨 STOP - READ THESE CONSTRAINTS FIRST 🚨🚨🚨
{'='*80}

AVAILABLE TABLES: {table_list}
{f"⚠️  MISSING/UNAVAILABLE: (any tables not listed above)" if table_slugs else "No tables available yet"}

{'='*80}
CRITICAL RULES - YOU MUST FOLLOW THESE
{'='*80}

1. ONLY USE TABLES LISTED ABOVE
   ❌ If step mentions 'team_members' but it's not in the list → Skip that feature
   ❌ If step mentions 'users' but it's not in the list → Skip that feature
   ✅ Only generate code for tables that EXIST in the list above

2. ONLY USE FIELDS SHOWN IN types.ts BELOW
   ❌ Do NOT invent fields like 'is_active', 'isActive', 'active'
   ❌ Do NOT assume common fields exist
   ✅ CHECK the type definition for EXACT field names

3. DO NOT USE INLINE IMPORTS IN TYPES
   ❌ WRONG: Database[import('./types').TableSlug.Projects]
   ✅ CORRECT: Import TableSlug first, then use Database[TableSlug.Projects]

4. DO NOT USE EXTENDS WITH INDEXED ACCESS (TypeScript Error ts(2499))
   ❌ WRONG: interface Foo extends Database[TableSlug.Projects]['row']
   ✅ CORRECT: type Foo = Database[TableSlug.Projects]['row'] & {{ extra: string }}

5. IF STEP CANNOT BE COMPLETED → SKIP IT
   If the step requires missing tables, DON'T generate broken code
   Instead, focus only on features that CAN work with available tables

{'='*80}

Full types.ts content (THIS IS THE SOURCE OF TRUTH):
```typescript
{content}
```

{concrete_examples}
"""

    # Show other existing files (last 3, truncated)
    if other_files:
        existing_code = "\n\nOther existing files generated so far:\n"
        for f in list(other_files)[-3:]:
            path = getattr(f, "path", "")
            content = getattr(f, "content", "")
            existing_code += f"\n--- {path} ---\n{content[:1000]}...\n"

    resources_info = ""
    for r in registry_surface.get("resources", []):
        resources_info += f"- {r['resource_id']}: fields={r.get('exposed_fields', [])}\n"

    # Add data store context to resources info
    if data_store_context:
        resources_info += f"\n\n{data_store_context}"

    # Add connectors/MCP tools context to resources info
    if connectors_context:
        # Check if this is already a complete MCP context (from mcp_context.py)
        # or if it needs to be wrapped in the template
        if "## Available MCP" in connectors_context or "mcpTools" in connectors_context:
            # Already formatted MCP context - use directly
            resources_info += f"\n\n{connectors_context}"
        else:
            # Legacy connectors context - wrap in template
            connectors_prompt = CONNECTORS_PROMPT_TEMPLATE.format(connectors_context=connectors_context)
            resources_info += f"\n\n{connectors_prompt}"

    if not resources_info.strip():
        resources_info = "No data resources available - you can create tables using TABLE_DEFINITION blocks"

    # Check if this is a "data" step (table definition step)
    step_type = getattr(step, "type", "code")
    step_title_lower = getattr(step, "title", "").lower()
    is_data_step = (
        step_type == "data"
        or "table" in step_title_lower
        or "schema" in step_title_lower
        or "data model" in step_title_lower
    )

    # Add specific instructions for data steps
    data_step_instruction = ""
    if is_data_step:
        data_step_instruction = """

🚨 **THIS IS A DATA DEFINITION STEP - CRITICAL INSTRUCTIONS:**

1. **READ THE STEP DESCRIPTION CAREFULLY** - It lists EVERY table you must create!
   - The step description above contains the COMPLETE list of tables needed
   - Count how many tables are mentioned in the description
   - Create a TABLE_DEFINITION block for EACH AND EVERY table listed
   - **DO NOT SKIP ANY TABLES** - if the description mentions 4 tables, create 4 TABLE_DEFINITION blocks!
   - **YOU CANNOT CREATE TABLES LATER** - this is your ONLY chance to define tables!

2. **VERIFY YOU CREATED ALL TABLES** before finishing:
   - Count your TABLE_DEFINITION blocks
   - Compare against the tables mentioned in the step description
   - If the description mentions "projects, tasks, team_members, comments", you MUST create 4 blocks
   - Missing even ONE table will break the entire application!

3. **ONLY generate TABLE_DEFINITION blocks** - DO NOT generate any TypeScript/React code
4. **DO NOT use dataStore** API in this step - that will be done in the next step
5. **DO NOT define 'id', 'created_at', 'updated_at'** - these are AUTO-GENERATED by the system!
6. **Your output should ONLY contain TABLE_DEFINITION blocks** like this:

```table:users
name: Users
description: User accounts
columns:
  - name: email, type: string, nullable: false
  - name: name, type: string
  - name: role, type: string, default: member
```

```table:user-activities
name: User Activities
description: Track user actions for history
columns:
  - name: user_id, type: uuid, nullable: false
  - name: activity_type, type: string, nullable: false
  - name: metadata, type: json
```

**System automatically adds to every table:** `id` (UUID), `created_at`, `updated_at`

7. **DO NOT generate App.tsx, components, or any UI code** - this step is ONLY for defining the data schema
8. **The next step will generate code that uses these tables** - make sure you create ALL tables they'll need!
9. **FINAL CHECK**: Before submitting, count your TABLE_DEFINITION blocks and verify it matches the number of tables in the step description!

"""

    return (
        typescript_types_section +
        STEP_PROMPT_TEMPLATE.format(
            step_number=step_index + 1,
            step_title=getattr(step, "title", ""),
            step_description=getattr(step, "description", ""),
            user_message=user_message,
            app_name=context.get("app_name", "App"),
            resources_info=resources_info.strip(),
            existing_code=existing_code.strip(),
            over_eagerness_guard=OVER_EAGERNESS_GUARD,
        ) + data_step_instruction
    )
SINGLE_FILE_PROMPT_TEMPLATE = """## Generate: {file_path}

You are generating a SINGLE FILE: `{file_path}`

### Context
- **User's Request**: {user_message}
- **App Name**: {app_name}
- **Step**: {step_title}
- **Step Description**: {step_description}

### Other Files Being Generated (by other agents)
{other_files_info}

### Existing Files (for imports/dependencies)
{existing_files_info}

### Available Data Resources
{resources_info}

{over_eagerness_guard}

## CRITICAL INSTRUCTIONS

1. **GENERATE ONLY ONE FILE**: `{file_path}`
   - Output exactly ONE code block with the complete file content
   - DO NOT generate any other files

2. **IMPORT FROM EXISTING FILES**: Reference the existing files above for correct import paths

3. **COORDINATE WITH OTHER FILES**: The other agents are generating the files listed above
   - Make sure your imports match what they will export
   - Use consistent naming and types

4. **OUTPUT FORMAT**:
```{file_path}
// Your complete file content here
```

Requirements:
- Use TypeScript with proper types
- Use Tailwind CSS for styling
- Handle loading and error states
- Make the code complete and runnable - no placeholders or TODOs
- Import dataStore from '../lib/dataStore' or './lib/dataStore' as appropriate
- Import types from '../lib/types' or './lib/types' as appropriate
"""
def build_file_prompt(
    file_path: str,
    step: Any,
    step_index: int,
    user_message: str,
    context: Dict[str, Any],
    existing_files: Sequence[Any],
    other_target_files: List[str],
    registry_surface: Dict[str, Any],
    data_store_context: Optional[str] = None,
    connectors_context: Optional[str] = None,
) -> str:
    """
    Build a focused prompt for generating a single file.
    
    Used by the sub-agent execution strategy where each file is generated
    by a separate LLM call.
    
    Args:
        file_path: The specific file path to generate (e.g., 'src/components/TaskList.tsx')
        step: The PlanStep being executed
        step_index: Index of the step in the plan
        user_message: The original user request
        context: Planning context dictionary
        existing_files: Files already generated (for import context)
        other_target_files: Other files being generated by parallel agents
        registry_surface: Registry surface with resources
        data_store_context: Optional data store context
        connectors_context: Optional MCP tools context
    
    Returns:
        Formatted prompt string for single-file generation
    """
    import re
    
    # Build existing files info
    existing_files_info = ""
    typescript_types_content = ""
    
    if existing_files:
        for f in existing_files:
            path = getattr(f, "path", "")
            content = getattr(f, "content", "")
            
            if path == "src/lib/types.ts":
                # Show types.ts in full - it's the source of truth
                typescript_types_content = f"""
### TypeScript Types (src/lib/types.ts) - SOURCE OF TRUTH
```typescript
{content}
```
"""
            else:
                # Show other files truncated
                truncated = content[:800] + "..." if len(content) > 800 else content
                existing_files_info += f"\n--- {path} ---\n```\n{truncated}\n```\n"
    
    if not existing_files_info:
        existing_files_info = "No existing files yet."
    
    # Build other files info
    other_files_info = ""
    if other_target_files:
        other_files_info = "The following files are being generated by other agents (do NOT generate these):\n"
        for other_path in other_target_files:
            other_files_info += f"  - {other_path}\n"
    else:
        other_files_info = "No other files being generated in parallel."
    
    # Build resources info
    resources_info = ""
    for r in registry_surface.get("resources", []):
        resources_info += f"- {r['resource_id']}: fields={r.get('exposed_fields', [])}\n"
    
    if data_store_context:
        resources_info += f"\n\n{data_store_context}"
    
    if connectors_context:
        if "## Available MCP" in connectors_context or "mcpTools" in connectors_context:
            resources_info += f"\n\n{connectors_context}"
        else:
            connectors_prompt = CONNECTORS_PROMPT_TEMPLATE.format(connectors_context=connectors_context)
            resources_info += f"\n\n{connectors_prompt}"
    
    if not resources_info.strip():
        resources_info = "No data resources available."
    
    # Determine file extension for code block
    file_ext = file_path.split('.')[-1] if '.' in file_path else 'tsx'
    
    prompt = typescript_types_content + SINGLE_FILE_PROMPT_TEMPLATE.format(
        file_path=file_path,
        user_message=user_message,
        app_name=context.get("app_name", "App"),
        step_title=getattr(step, "title", ""),
        step_description=getattr(step, "description", ""),
        other_files_info=other_files_info,
        existing_files_info=existing_files_info,
        resources_info=resources_info.strip(),
        over_eagerness_guard=OVER_EAGERNESS_GUARD,
    )
    
    return prompt
def build_final_app_prompt(
    user_message: str,
    context: Dict[str, Any],
    components_info: str,
    other_files: List[Any],
) -> str:
    """Format the final App.tsx synthesis prompt."""
    return FINAL_APP_PROMPT_TEMPLATE.format(
        user_message=user_message,
        app_name=context.get("app_name", "App"),
        components_info=components_info,
        other_files=[getattr(f, "path", f) for f in other_files],
    )
