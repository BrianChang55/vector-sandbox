"""
Prompt templates used by the agentic code generation workflow.

These helpers keep every model call aligned on tone, structure, and required
runtime constraints so changes can be made in one place.
"""

from typing import Any, Dict, List, Optional, Sequence

# Shared style guide injected into all design-aware prompts.
DESIGN_STYLE_PROMPT = """This base style guide (enterprise, light theme, minimal):
- Backgrounds: bg-gray-50 for canvas, bg-white surfaces; borders: border-gray-200 (hover border-gray-300); subtle shadow-sm only when needed.
- Typography: text-gray-900 primary, text-gray-700 body, placeholders text-gray-500; system font stack.
- Spacing: p-4/p-6 sections, gap-2/3/4; clean layouts, clear hierarchy.
- Buttons: primary bg-gray-900 hover:bg-gray-800, secondary bg-gray-100 hover:bg-gray-200, outline border-gray-200 bg-white hover:bg-gray-50; rounded-md.
- Corners: cards rounded-md, inputs/buttons rounded-md; minimal accents; avoid dark/gradient themes."""

# Base system prompt for codegen; design style is injected dynamically.
CODEGEN_SYSTEM_PROMPT_TEMPLATE = """You are an expert React/TypeScript developer building internal business applications.

CRITICAL: For all data operations, you MUST use the Runtime API:

```typescript
// In src/lib/runtime.ts
export async function runtimeQuery(params: {
  resourceId: string;
  querySpec: {
    select?: string[];
    filters?: Array<{ field: string; op: string; value: any }>;
    orderBy?: Array<{ field: string; dir: 'asc' | 'desc' }>;
    limit?: number;
    offset?: number;
  };
}): Promise<{ data: any[]; count: number }> {
  const config = window.__RELAY_CONFIG__;
  const response = await fetch(`${config.apiBaseUrl}/runtime/query/`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      appId: config.appId,
      versionId: config.versionId,
      ...params,
    }),
  });
  return response.json();
}

export async function runtimeAction(params: {
  actionId: string;
  args: Record<string, any>;
}): Promise<{ success: boolean; data: any }> {
  const config = window.__RELAY_CONFIG__;
  const response = await fetch(`${config.apiBaseUrl}/runtime/action/`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      appId: config.appId,
      versionId: config.versionId,
      ...params,
    }),
  });
  return response.json();
}
```

Stack:
- React 18+ with TypeScript
- TailwindCSS (utility-first)
- Lucide React for icons
- No external state libraries (use React hooks)

Guidelines:
1. Generate complete, runnable code
2. Handle loading and error states elegantly
3. Use professional, modern UI patterns
4. Include proper TypeScript types
5. Make responsive designs
6. Use semantic HTML
7. Add helpful comments

Base design/style requirements (always follow):
{design_style}

NEVER import from node_modules that aren't available. Use inline implementations."""

PLAN_PROMPT_TEMPLATE = """You are planning an internal app generation task.

User Request: {user_message}
App Name: {app_name}
Available Resources: {available_resources}
Has Existing Spec: {has_existing_spec}

IMPORTANT: If the app needs to store/manage data (tasks, customers, products, etc.), your plan MUST include:
1. A step to define data table schemas (using TABLE_DEFINITION blocks)
2. A step to generate the dataStore client
3. Steps to build UI components that use the dataStore API

Generate a plan with 2-4 steps (fewer for simple requests). Return JSON:
{{
    "reasoning": "Your analysis of what needs to be built, including what data tables are needed",
    "steps": [
        {{"type": "data", "title": "Define Data Tables", "description": "Create table schemas for X, Y, Z"}},
        {{"type": "code", "title": "Build Core Components", "description": "..."}}
    ]
}}

Step types: research, design, data, code, component, styling, integration, validation
Keep it concise but comprehensive. ALWAYS include a 'data' step if the app manages any kind of data."""

STEP_PROMPT_TEMPLATE = """Step {step_number}: {step_title}
Description: {step_description}

User's Original Request: {user_message}
App Name: {app_name}

Available Data Resources:
{resources_info}
{existing_code}

CRITICAL: Generate complete, working React code for this step.

## DATA STORAGE INSTRUCTIONS

If your app needs to store/manage data (e.g., tasks, customers, products, orders):

1. FIRST, create the required table(s) using this EXACT format:

```table:your-table-slug
name: Your Table Name
description: What this table stores
columns:
  - name: id, type: uuid, primary_key: true, auto_generate: true
  - name: title, type: string, nullable: false
  - name: status, type: string, default: active
  - name: created_at, type: datetime, auto_now_add: true
```

2. THEN, use the dataStore client from lib/dataStore (already provided):

IMPORTANT: The dataStore client is ALREADY available at `./lib/dataStore.ts`. 
DO NOT create your own dataStore.ts - just import and use the existing one.
The config is automatically injected via window.__RELAY_CONFIG__.

If you must regenerate it (only if specifically needed), use this exact template:

```src/lib/dataStore.ts
// Runtime config is injected by the preview environment
const getConfig = () => {{
  const config = (window as any).__RELAY_CONFIG__;
  if (!config?.apiBaseUrl) {{
    console.error('RELAY_CONFIG not found - using fallback API URL');
  }}
  return {{
    apiBaseUrl: config?.apiBaseUrl || 'http://localhost:8001/api/v1',
    appId: config?.appId || '',
    versionId: config?.versionId || '',
  }};
}};

async function apiCall<T>(operation: string, tableSlug: string | null, params: any = {{}}): Promise<T> {{
  const config = getConfig();
  const response = await fetch(`${{config.apiBaseUrl}}/runtime/data/`, {{
    method: 'POST',
    headers: {{ 'Content-Type': 'application/json' }},
    body: JSON.stringify({{ 
      appId: config.appId, 
      versionId: config.versionId,
      operation, 
      tableSlug, 
      params 
    }}),
  }});
  if (!response.ok) {{
    const error = await response.json().catch(() => ({{}}));
    throw new Error(error.error || 'API request failed');
  }}
  return response.json();
}}

export const dataStore = {{
  listTables: () => apiCall<{{tables: any[]}}>('listTables', null).then(r => r.tables || []),
  query: (tableSlug: string, params: any = {{}}) => apiCall<any>('query', tableSlug, params),
  insert: (tableSlug: string, data: any) => apiCall<any>('insert', tableSlug, {{ data }}),
  update: (tableSlug: string, rowId: string, data: any) => apiCall<any>('update', tableSlug, {{ rowId, data }}),
  delete: (tableSlug: string, rowId: string) => apiCall<any>('delete', tableSlug, {{ rowId }}),
  bulkInsert: (tableSlug: string, rows: any[]) => apiCall<any>('bulkInsert', tableSlug, {{ rows }}),
  bulkDelete: (tableSlug: string, rowIds: string[]) => apiCall<any>('bulkDelete', tableSlug, {{ rowIds }}),
}};
```

3. Use the dataStore in components:

```typescript
import {{ dataStore }} from '../lib/dataStore';

// In a component:
const [items, setItems] = useState([]);
useEffect(() => {{
  dataStore.query('your-table-slug', {{}}).then(r => setItems(r.rows || []));
}}, []);
```

## OUTPUT FORMAT

You MUST format each file exactly like this:
```src/App.tsx
import React from 'react';
// ... complete code here
export default function App() {{ ... }}
```

Another example:
```src/components/DataTable.tsx
// Component code here
```

Requirements:
- Use TypeScript with proper types
- Use Tailwind CSS for all styling (available via CDN)
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
5. Include realistic mock data for demonstration
6. Make it look polished and production-ready
7. Use Tailwind CSS for all styling
8. Handle loading and error states
9. Make it interactive (search, filters, etc. as appropriate)

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

# Data Store prompt template - injected when app has data tables or needs to create them
DATA_STORE_PROMPT_TEMPLATE = """
## App Data Store

You can create and use data tables for this app. The data store provides persistent storage for your application data.

{data_store_context}

### Creating New Tables

To create a new data table, include a TABLE_DEFINITION block in your response:

```table:table-slug
name: Display Name
description: Optional description
columns:
  - name: id, type: uuid, primary_key: true, auto_generate: true
  - name: title, type: string, nullable: false
  - name: description, type: text
  - name: status, type: enum, enum_values: [draft, active, archived]
  - name: count, type: integer, default: 0
  - name: price, type: float
  - name: is_featured, type: boolean, default: false
  - name: metadata, type: json
  - name: created_at, type: datetime, auto_now_add: true
  - name: updated_at, type: datetime, auto_now: true
```

**Supported Column Types:**
- `uuid` - UUID identifier (use for primary keys)
- `string` - Short text (max 255 chars)
- `text` - Long text (unlimited)
- `integer` - Whole numbers
- `float` - Decimal numbers
- `boolean` - True/false
- `datetime` - Date and time
- `date` - Date only
- `enum` - Fixed values (requires `enum_values`)
- `json` - Arbitrary JSON data

### Using Tables in Code

Always use the dataStore API - it's available at `./lib/dataStore`:

```typescript
import {{ dataStore }} from './lib/dataStore';

// Query rows with filtering and sorting
const result = await dataStore.query('customers', {{
  filters: [{{ field: 'status', op: 'eq', value: 'active' }}],
  orderBy: [{{ field: 'name', dir: 'asc' }}],
  limit: 50
}});
// Returns: {{ rows: [...], total_count: number, has_more: boolean }}

// Insert a new row
const newCustomer = await dataStore.insert('customers', {{
  name: 'John Doe',
  email: 'john@example.com',
  status: 'active'
}});
// Returns: {{ id: 'uuid', data: {{...}}, row_index: number }}

// Update an existing row
await dataStore.update('customers', 'row-uuid', {{
  status: 'inactive'
}});

// Delete a row
await dataStore.delete('customers', 'row-uuid');

// Bulk operations
await dataStore.bulkInsert('customers', [
  {{ name: 'User 1', email: 'user1@example.com' }},
  {{ name: 'User 2', email: 'user2@example.com' }}
]);
await dataStore.bulkDelete('customers', ['uuid-1', 'uuid-2']);
```

**Filter Operators:** `eq`, `neq`, `gt`, `gte`, `lt`, `lte`, `in`, `not_in`, `contains`, `icontains`, `is_null`

IMPORTANT: When creating apps that need persistent data:
1. First define the table(s) using TABLE_DEFINITION blocks
2. Generate the dataStore.ts file in src/lib/
3. Use the dataStore API in your components - never use hardcoded mock data for the main functionality
"""

# System prompt for code generation with data store support
CODEGEN_SYSTEM_PROMPT_WITH_DATASTORE = """You are an expert React/TypeScript developer building internal business applications with persistent data storage.

CRITICAL: For all data operations, you MUST use the Data Store API:

```typescript
// In src/lib/dataStore.ts - this file should be generated for apps using data
import {{ dataStore }} from './lib/dataStore';

// Query data
const {{ rows, total_count }} = await dataStore.query('table-slug', {{
  filters: [{{ field: 'status', op: 'eq', value: 'active' }}],
  orderBy: [{{ field: 'created_at', dir: 'desc' }}],
  limit: 50
}});

// Insert
const newRow = await dataStore.insert('table-slug', {{ field: 'value' }});

// Update
await dataStore.update('table-slug', 'row-id', {{ field: 'new-value' }});

// Delete
await dataStore.delete('table-slug', 'row-id');
```

Stack:
- React 18+ with TypeScript
- TailwindCSS (utility-first)
- Lucide React for icons
- No external state libraries (use React hooks)
- Data Store API for persistent data

Guidelines:
1. Generate complete, runnable code
2. Handle loading and error states elegantly
3. Use professional, modern UI patterns
4. Include proper TypeScript types
5. Make responsive designs
6. Use semantic HTML
7. Add helpful comments
8. Use dataStore API for all data operations - no mock data for main functionality

Base design/style requirements (always follow):
{design_style}

NEVER import from node_modules that aren't available. Use inline implementations."""


def apply_design_style_prompt(message: str, data_store_context: Optional[str] = None) -> str:
    """Append the shared design/style guide and data store context to a user message."""
    base_message = (message or "").strip()
    design_prompt = f"------ Design & Style Requirements ------\n\n{DESIGN_STYLE_PROMPT}"
    
    parts = []
    if base_message:
        parts.append(base_message)
    
    # Add data store context if provided
    if data_store_context:
        data_store_prompt = DATA_STORE_PROMPT_TEMPLATE.format(
            data_store_context=data_store_context
        )
        parts.append(data_store_prompt)
    
    parts.append(design_prompt)
    
    return "\n\n".join(parts)


def build_plan_prompt(user_message: str, context: Dict[str, Any]) -> str:
    """Format the planning prompt with user intent and runtime context."""
    data_store_summary = context.get("data_store_summary", "")
    available_resources = context.get("available_resources", ["none"])
    
    # Include data store tables in available resources if present
    if data_store_summary:
        available_resources = list(available_resources) + [f"Data Store: {data_store_summary}"]
    
    return PLAN_PROMPT_TEMPLATE.format(
        user_message=user_message,
        app_name=context.get("app_name", "App"),
        available_resources=", ".join(available_resources) if available_resources else "none",
        has_existing_spec=context.get("has_existing_spec", False),
    )




def build_step_prompt(
    step: Any,
    step_index: int,
    user_message: str,
    context: Dict[str, Any],
    existing_files: Sequence[Any],
    registry_surface: Dict[str, Any],
    data_store_context: Optional[str] = None,
) -> str:
    """Build the per-step execution prompt for streaming code generation."""
    existing_code = ""
    if existing_files:
        existing_code = "\n\nExisting files generated so far:\n"
        for f in list(existing_files)[-3:]:
            path = getattr(f, "path", "")
            content = getattr(f, "content", "")
            existing_code += f"\n--- {path} ---\n{content[:1000]}...\n"

    resources_info = ""
    for r in registry_surface.get("resources", []):
        resources_info += f"- {r['resource_id']}: fields={r.get('exposed_fields', [])}\n"
    
    # Add data store context to resources info
    if data_store_context:
        resources_info += f"\n\n{data_store_context}"
    
    if not resources_info.strip():
        resources_info = "No data resources available - you can create tables using TABLE_DEFINITION blocks"

    return STEP_PROMPT_TEMPLATE.format(
        step_number=step_index + 1,
        step_title=getattr(step, "title", ""),
        step_description=getattr(step, "description", ""),
        user_message=user_message,
        app_name=context.get("app_name", "App"),
        resources_info=resources_info.strip(),
        existing_code=existing_code.strip(),
    )




def build_codegen_system_prompt(
    registry_surface: Dict[str, Any],
    has_data_store: bool = False
) -> str:
    """Return the system prompt for code generation with injected design style."""
    if has_data_store:
        return CODEGEN_SYSTEM_PROMPT_WITH_DATASTORE.replace("{design_style}", DESIGN_STYLE_PROMPT)
    return CODEGEN_SYSTEM_PROMPT_TEMPLATE.replace("{design_style}", DESIGN_STYLE_PROMPT)


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



