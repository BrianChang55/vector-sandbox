# Data Store prompt template - injected when app has data tables or needs to create them
DATA_STORE_PROMPT_TEMPLATE = """
## App Data Store

The data store provides persistent storage for your application data.

{data_store_context}

### Creating New Tables (DATA STEPS ONLY)

⚠️ **Tables can ONLY be created in "data" type steps (step_order=0).**
In component/code steps, you can ONLY use existing tables shown in types.ts.

To create a table in a DATA STEP, include a TABLE_DEFINITION block:

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

Import types from `src/lib/types.ts` for type safety:

```typescript
import {{ dataStore }} from './lib/dataStore';
import type {{ Database }} from './lib/types';

// Type your operations - Example: articles table
const article: Database['articles']['insert'] = {{ title: 'Getting Started', content: 'Lorem ipsum...', author_id: 'user123' }};
await dataStore.insert('articles', article);

const result = await dataStore.query('articles', {{
  filters: [{{ field: 'published', op: 'eq', value: true }}]
}});
const articles: Database['articles']['row'][] = result.rows;

// Access fields via row.data:
result.rows.map(row => <div key={{row.id}}>{{row.data.title}}</div>);

// Update/delete uses row.id (NOT row.data.id):
await dataStore.update('articles', row.id, {{ published: false }});
await dataStore.delete('articles', row.id);
```

**Filter Operators:** `eq`, `neq`, `gt`, `gte`, `lt`, `lte`, `in`, `not_in`, `contains`, `icontains`, `is_null`

🚨 **CRITICAL: Different Tables Have Different Field Names** 🚨

**DO NOT assume all tables use the same field names!** Check types.ts for EXACT field names:

```typescript
// ✅ CORRECT - Projects use 'name'
await dataStore.insert('projects', {{ name: 'Q1 Launch' }});
result.rows.map(row => <div>{{row.data.name}}</div>);

// ✅ CORRECT - Tasks use 'title' (NOT 'name')
await dataStore.insert('tasks', {{ title: 'Design homepage', project_id: 'xyz' }});
result.rows.map(row => <div>{{row.data.title}}</div>);

// ❌ WRONG - Tasks don't have 'name' field
await dataStore.insert('tasks', {{ name: 'Design homepage' }}); // ERROR!
result.rows.map(row => <div>{{row.data.name}}</div>); // ERROR!
```

**Common mistake:** Assuming 'name' exists on all tables. Always verify field names in types.ts.

**CRITICAL: Row Data Structure**
- Use `row.id` for the row UUID (for update/delete operations)
- Use `row.data.fieldName` to access your data fields (e.g., `row.data.title`, `row.data.email`)
- The `data` object contains all your table columns

🚨 **CRITICAL: Understanding Row Structure and ID Handling** 🚨

Every row returned by dataStore has this structure:
```typescript
{{
  id: "uuid-string",           // ← Use THIS for update/delete
  data: {{                      // ← Your actual field data
    title: "Task 1",
    status: "active",
    // ... all your table fields
  }},
  row_index: 0,
  created_at: "2024-01-01T00:00:00Z"
}}
```

**🚨 COMMON MISTAKES THAT CAUSE "Row not found" ERRORS:**

**Mistake #1: ID Overwrite Pattern**
```typescript
// ❌ WRONG - data.id will overwrite row.id!
const task = {{ id: row.id, ...row.data }}
// If row.data has an 'id' field, it overwrites the real row.id

// ✅ CORRECT - row.id overwrites any data.id
const task = {{ ...row.data, id: row.id }}
```

**Mistake #2: Using row.data.id for Operations**
```typescript
// ❌ WRONG - row.data.id is your data, not the row ID
await dataStore.update('tasks', row.data.id, {{ status: 'done' }})
await dataStore.delete('tasks', row.data.id)

// ✅ CORRECT - use row.id for operations
await dataStore.update('tasks', row.id, {{ status: 'done' }})
await dataStore.delete('tasks', row.id)
```

**Summary:**
- `row.id` = UUID for the row (use for update/delete/identifying the row)
- `row.data.fieldName` = Access your actual data fields
- NEVER use `row.data.id` for operations
- When spreading, put `...row.data` FIRST, then `id: row.id`

🚨 **CRITICAL RULES - MUST FOLLOW:**

1. **ONLY USE TABLES THAT EXIST OR YOU CREATE** - You MUST NOT reference tables in your code that don't exist
   - If current schema shows: projects, tasks
   - You can ONLY use: 'projects', 'tasks'
   - ❌ WRONG: dataStore.query('burndown_snapshots', ...) // This table doesn't exist!
   - ✅ CORRECT: First create the table with TABLE_DEFINITION, THEN use it in code

2. **CREATE BEFORE USE** - If you need a new table, you MUST:
   - Step 1: Define it with ```table:slug``` block
   - Step 2: Then use it in your code
   - Never reference a table that hasn't been defined yet

3. **CHECK THE CURRENT SCHEMA** - Look at the "Available Tables" section above
   - These are the ONLY tables you can use right now
   - If the table you need isn't listed, you MUST create it first

IMPORTANT: When creating apps that need persistent data:
1. First define the table(s) using TABLE_DEFINITION blocks
2. Generate the dataStore.ts file in src/lib/
3. Use the dataStore API in your components - never use hardcoded mock data for the main functionality
"""

