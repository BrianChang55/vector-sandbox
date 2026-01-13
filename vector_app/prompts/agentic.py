"""
Prompt templates used by the agentic code generation workflow.

These helpers keep every model call aligned on tone, structure, and required
runtime constraints so changes can be made in one place.
"""

from typing import Any, Dict, List, Optional, Sequence


# =============================================================================
# ANTI-OVER-ENGINEERING GUARDS
# =============================================================================
# Prevents the LLM from adding unnecessary complexity, features, or abstractions
# that weren't explicitly requested by the user.

# =============================================================================
# TYPESCRIPT COMPILE SAFETY RULES
# =============================================================================
# Prevents common TypeScript compilation errors that occur in generated code.

TYPESCRIPT_SAFETY_RULES = """
## TypeScript Compilation Safety (CRITICAL - Follow These EXACTLY)

These rules prevent TypeScript compilation errors. Violating them WILL cause build failures.

### 1. React Imports - ALWAYS Include React Default Import
When using React types like `React.ChangeEvent` or `React.FormEvent`, you MUST import React:
```typescript
// ✅ CORRECT - Includes React import for namespace types
import React, { useState, useEffect } from 'react';
const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => { ... };

// ❌ WRONG - Missing React import will cause "Cannot find namespace 'React'" error
import { useState, useEffect } from 'react';
const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => { ... };
```

Alternative: Use the inline type instead of React namespace:
```typescript
// ✅ ALSO CORRECT - Using generic event handler
import { useState, ChangeEvent } from 'react';
const handleChange = (e: ChangeEvent<HTMLInputElement>) => { ... };
```

### 2. Timer Types - Use `number` Instead of `NodeJS.Timeout`
For setTimeout/setInterval return values, use `number` or `ReturnType<typeof setTimeout>`:
```typescript
// ✅ CORRECT
const timerRef = useRef<number | null>(null);
timerRef.current = window.setTimeout(() => {}, 1000);

// ❌ WRONG - NodeJS.Timeout may not be available
const timerRef = useRef<NodeJS.Timeout | null>(null);
```

### 3. Lucide Icons - ONLY Use These Verified Icons
ONLY import icons from this verified list. Using icons NOT on this list WILL cause compilation errors:

**Navigation & Arrows:**
- Plus, Minus, X, Check, Save, Edit, Edit2, Pencil, Trash, Trash2
- ArrowUp, ArrowDown, ArrowLeft, ArrowRight, ArrowUpRight, ArrowDownRight
- ChevronUp, ChevronDown, ChevronLeft, ChevronRight
- RotateCcw, RotateCw, RefreshCw, RefreshCcw, Repeat, Repeat2, Shuffle

**Status & Feedback:**
- CheckCircle, CheckCircle2, AlertCircle, AlertTriangle, Info, Loader2
- XCircle, HelpCircle, Ban, ShieldCheck, ShieldAlert

**User & Communication:**
- User, Users, UserPlus, UserMinus, Mail, Phone, MessageCircle, MessageSquare
- Bell, BellOff, Send, Share, Share2

**Time & Calendar:**
- Calendar, CalendarDays, Clock, Timer, History, AlarmClock

**UI Controls:**
- Search, Eye, EyeOff, Settings, Filter, SlidersHorizontal
- Menu, MoreHorizontal, MoreVertical, Grip, GripVertical

**Data & Finance:**
- DollarSign, TrendingUp, TrendingDown, BarChart, BarChart2, BarChart3
- Percent, Calculator, PieChart, LineChart, Activity

**Files & Content:**
- File, FileText, FileCode, Folder, FolderOpen, FolderPlus
- Image, Camera, Video, Music, Headphones
- Link, Link2, ExternalLink, Download, Upload

**General Objects:**
- Home, Building, Building2, MapPin, Map, Globe, Navigation
- Book, BookOpen, BookMarked, Bookmark, Library, Notebook, FileText
- Copy, Clipboard, ClipboardCheck, ClipboardList, ClipboardCopy
- Heart, Star, Sparkles, Zap, Award, Trophy, Target, Flag
- Lock, Unlock, Key, Shield, Fingerprint
- Tag, Tags, Hash, AtSign
- Package, Box, Archive, Inbox, Layers
- Code, Terminal, Database, Server, Cloud, Wifi
- Palette, Paintbrush, Droplet, Sun, Moon, CloudSun

**NEVER use these (common mistakes that cause errors):**
- ArrowLeftRight ❌ (use ArrowLeft + ArrowRight separately, or RefreshCw for swap)
- ArrowRightLeft ❌ (doesn't exist)
- Swap ❌ (use RefreshCw or Repeat instead)
- Exchange ❌ (doesn't exist)

```typescript
// ✅ CORRECT - Verified icons
import { Plus, Trash2, Loader2, Check, RefreshCw, ArrowRight } from 'lucide-react';

// ❌ WRONG - These icons DO NOT exist and will cause compilation errors
import { ArrowLeftRight, Swap, Exchange } from 'lucide-react'; // ERROR: TS2724
```

### 4. Event Handler Typing
Use explicit typing or inline handlers to avoid type errors:
```typescript
// ✅ CORRECT - Event handler with explicit type  
<input onChange={(e) => setValue(e.target.value)} />

// ✅ ALSO CORRECT - With React import and type
import React from 'react';
const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
  setValue(e.target.value);
};
```

### 5. Avoid Problematic Patterns
- Don't use `as any` excessively - use proper types
- Don't use optional chaining on required properties without null checks
- Always provide default values for optional state

### 6. Component Props - MUST Match Exactly
When defining and using component props, ensure perfect alignment:
```typescript
// ✅ CORRECT - Props interface matches exactly what component receives
interface ButtonProps {
  onClick: () => void;
  label: string;
  disabled?: boolean;
}
const Button = ({ onClick, label, disabled = false }: ButtonProps) => {...}

// When using: Pass ONLY the props defined in the interface
<Button onClick={handleClick} label="Submit" />
```

When rendering a list of components, React's `key` prop is INTERNAL and should NOT be in your interface:
```typescript
// ✅ CORRECT - 'key' is NOT in the interface (it's a React internal)
interface ListItemProps { id: string; text: string; }
{items.map(item => <ListItem key={item.id} id={item.id} text={item.text} />)}
```

### 7. Avoid Naming Collisions
When importing and defining, use UNIQUE names:
```typescript
// ❌ WRONG - Same name for type and variable causes "Duplicate identifier" error
type Result = { value: number };
const Result = calculateSomething();

// ✅ CORRECT - Different names
type ResultType = { value: number };
const result = calculateSomething();
```

When importing components that share names with interfaces/types, rename one:
```typescript
// ❌ WRONG - Both import and local type have same name
import { ConversionResult } from './components/ConversionResult';
type ConversionResult = { from: string; to: string; value: number };

// ✅ CORRECT - Rename one
import { ConversionResult } from './components/ConversionResult';
type ConversionResultData = { from: string; to: string; value: number };
```

### 8. MouseEvent - Use Correct Access Pattern
When handling mouse events for modals/overlays:
```typescript
// ✅ CORRECT - Use inline comparison
<div onClick={(e) => { if (e.target === e.currentTarget) onClose(); }}>

// ✅ ALSO CORRECT - With proper typing
const handleBackdropClick = (e: React.MouseEvent<HTMLDivElement>) => {
  if (e.target === e.currentTarget) onClose();
};
```

### 9. Type Consistency - MUST Match Across Files
When defining a type/interface and using it elsewhere, properties MUST match exactly:
```typescript
// ✅ CORRECT - Type definition matches usage
// In types/item.ts:
interface Item { id: string; name: string; price: number; }

// In components/ItemList.tsx:
const items: Item[] = [...];
items.map(item => <div key={item.id}>{item.name}: ${item.price}</div>)

// ❌ WRONG - Using properties not in the type definition
items.map(item => <div>{item.quantity}</div>) // ERROR: 'quantity' doesn't exist on Item
```

If you need additional properties, ADD THEM to the type definition first, then use them.

### 10. Module Imports - CRITICAL Rules for dataStore
The dataStore module is ALWAYS available at `../lib/dataStore` or `./lib/dataStore`. NEVER create custom store files.

```typescript
// ✅ CORRECT - dataStore is ALWAYS available at this path
import {{ dataStore }} from '../lib/dataStore';
import {{ dataStore }} from './lib/dataStore';  // From App.tsx

// ❌ WRONG - NEVER create or import custom store files
import {{ habitStore }} from './habitStore';        // ERROR: doesn't exist
import {{ taskStore }} from '../stores/taskStore';  // ERROR: doesn't exist
import {{ noteStore }} from './lib/noteStore';      // ERROR: doesn't exist
```

IMPORTANT: All data operations go through the single `dataStore` object:
- `dataStore.query('table-slug', {...})`
- `dataStore.insert('table-slug', data)`
- `dataStore.update('table-slug', rowId, data)`
- `dataStore.delete('table-slug', rowId)`

NEVER create separate store files for different entities - use the table slug parameter instead.

### 11. Component Props Passing - Match Interfaces Exactly
When one component passes props to another, the props MUST match the child's interface EXACTLY:

```typescript
// Child component defines its interface
interface ToastProps {
  id: string;
  message: string;
  type: 'success' | 'error';
  onClose: () => void;  // Takes no arguments
}
const Toast = ({ id, message, type, onClose }: ToastProps) => {...}

// ✅ CORRECT - Props match exactly
<Toast id={t.id} message={t.message} type={t.type} onClose={() => removeToast(t.id)} />

// ❌ WRONG - onClose signature doesn't match (takes id but interface says no args)
<Toast id={t.id} message={t.message} type={t.type} onClose={removeToast} />
// ERROR: Type '(id: string) => void' is not assignable to type '() => void'
```

### 12. Self-Contained Components - Define Types in Same File
To avoid cross-file type mismatches, prefer defining types in the same file as the component that uses them:

```typescript
// ✅ CORRECT - Type defined in same file as component
// In Toast.tsx:
export interface ToastData {{
  id: string;
  message: string;
  type: 'success' | 'error';
}}

export function Toast({{ id, message, type, onClose }}: ToastData & {{ onClose: () => void }}) {{...}}

// In parent component:
import {{ Toast, ToastData }} from './Toast';
```

### 13. Type Syntax - Avoid Malformed Type Declarations & Prefer TableSlug
Always ensure TypeScript syntax is clean and well-formed. **Prefer TableSlug enum for type safety:**

```typescript
// ❌ WRONG - Stray quotes or malformed syntax
owner: Database['projects']['row'];'  // Extra quote at end!
status: string";  // Extra quote!

// ✅ CORRECT - Clean, well-formed syntax
owner: Database['projects']['row'];
status: string;

// ✅✅ PREFERRED - Import and use TableSlug enum (algorithmically generated, zero typos)
import type {{ Database, TableSlug }} from './lib/types';
interface ExtendedTask extends Database[TableSlug.Tasks]['row'] {{
  extraField: string;
}}
const tasks: Database[TableSlug.Tasks]['row'][] = result.rows;

// ⚠️ ALSO VALID - String literal keys work but prone to typos
interface ExtendedTask extends Database['tasks']['row'] {{
  extraField: string;
}}
```

**Why prefer TableSlug?** It's auto-generated from your schema, so it's always correct and provides IDE autocomplete.

### 14. ALWAYS Export Types That Other Files Need (CRITICAL)
If you define a type/interface that ANY other file will import, you MUST use `export`:

```typescript
// ❌ WRONG - Type defined but NOT exported, other files CAN'T import it
interface TaskData {{
  id: string;
  title: string;
}}

// ✅ CORRECT - ALWAYS export types that other files need
export interface TaskData {{
  id: string;
  title: string;
}}
```

NEVER import something that isn't exported. Before importing, verify the source file exports it:
- If a file has `interface Foo` (no export), you CANNOT `import {{ Foo }} from './file'`
- Only `export interface Foo` or `export type Foo` can be imported

### 14. Only Import What Actually Exists (CRITICAL)
NEVER import types/interfaces that don't exist in the source file. Common mistakes:

```typescript
// In types/task.ts - only Task is defined
export interface Task {{
  id: string;
  title: string;
  completed: boolean;
}}

// ❌ WRONG - CreateTaskData doesn't exist in types/task.ts!
import {{ Task, CreateTaskData }} from '../types/task';  // ERROR: TS2305

// ✅ CORRECT - Only import what exists, define what you need locally
import {{ Task }} from '../types/task';
type CreateTaskData = Omit<Task, 'id'>;  // Define locally if needed
```

### 15. Consistent Props Between Parent and Child Components
When a parent component passes props to a child, the props MUST EXACTLY match what the child expects:

```typescript
// Child component
interface ItemProps {{
  task: Task;
  onComplete: (id: string) => void;
}}
function TaskItem({{ task, onComplete }}: ItemProps) {{...}}

// ✅ CORRECT - Props match exactly
<TaskItem task={{item}} onComplete={{handleComplete}} />

// ❌ WRONG - Extra props not in interface
<TaskItem task={{item}} onComplete={{handleComplete}} onDelete={{handleDelete}} />
// ERROR: 'onDelete' does not exist on type 'ItemProps'
```

### 16. DataStore Query Results - Transform Before Using in State (CRITICAL)
The dataStore.query() returns rows in a wrapped structure. You MUST transform them before setting state:

```typescript
// dataStore.query returns: {{ rows: [{{ id: string, data: YourType, ... }}], total_count, has_more }}

// ❌ WRONG - Setting raw query result to state typed as Note[]
const [notes, setNotes] = useState<Note[]>([]);
const result = await dataStore.query('notes', {{}});
setNotes(result.rows);  // ERROR: rows are {{ id, data }} objects, not Note

// ✅ CORRECT - Transform rows by extracting data and adding id
interface Note {{
  id: string;
  title: string;
  content: string;
}}
const [notes, setNotes] = useState<Note[]>([]);
const result = await dataStore.query('notes', {{}});
// CORRECT: spread data first, then id
setNotes(result.rows.map(row => ({{ ...row.data, id: row.id }})));
// Or if your Note type matches row.data:
setNotes(result.rows.map(row => ({{ id: row.id, title: row.data.title, content: row.data.content }})));
```

### 16b. DataStore Update/Delete - Use row.id NOT row.data.id (CRITICAL!)
When updating or deleting rows, you MUST use `row.id` (the system row UUID), NOT `row.data.id`:

```typescript
// ❌ WRONG - Using row.data.id causes "Row not found" error!
await dataStore.update('notes', row.data.id, {{ title: 'Updated' }});  // FAILS!
await dataStore.delete('notes', row.data.id);  // FAILS!

// ✅ CORRECT - Use row.id (the system-generated row UUID)
await dataStore.update('notes', row.id, {{ title: 'Updated' }});  // WORKS!
await dataStore.delete('notes', row.id);  // WORKS!

// When transforming rows for state, preserve row.id for later CRUD:
interface NoteWithId {{
  _rowId: string;  // Store row.id here for update/delete
  id: string;      // This is row.data.id (your schema's id)
  title: string;
}}
setNotes(result.rows.map(row => ({{
  _rowId: row.id,  // SAVE THIS for update/delete operations!
  ...row.data
}})));

// Later, when updating:
await dataStore.update('notes', note._rowId, {{ title: 'New Title' }});
```

### 17. Toast Component Pattern - Use Consistent Interface
When creating Toast components, use this exact pattern to avoid props mismatches:

```typescript
// Toast.tsx - Define props WITHOUT 'key' (key is a React internal prop)
export interface ToastProps {{
  id: string;
  message: string;
  type: 'success' | 'error';
  onClose: () => void;  // Takes no arguments, parent handles id
}}

// Destructure EXACTLY what's in ToastProps
export function Toast({{ id, message, type, onClose }}: ToastProps) {{
  return (
    <div className={{type === 'success' ? 'bg-green-500' : 'bg-red-500'}}>
      {{message}}
      <button onClick={{onClose}}>×</button>
    </div>
  );
}}

// ToastContainer.tsx - Props passed MUST match ToastProps exactly
import {{ Toast, ToastProps }} from './Toast';

// NOTE: 'key' is passed to React, NOT to the component's props
function ToastContainer({{ toasts, removeToast }}: {{
  toasts: Array<{{ id: string; message: string; type: 'success' | 'error' }}>;
  removeToast: (id: string) => void;
}}) {{
  return (
    <>
      {{toasts.map(t => (
        <Toast
          key={{t.id}}  // key goes to React, NOT to ToastProps
          id={{t.id}}   // These four match ToastProps exactly
          message={{t.message}}
          type={{t.type}}
          onClose={{() => removeToast(t.id)}}
        />
      ))}}
    </>
  );
}}
```

### 18. React 'key' Prop is NOT Part of Component Props
The `key` prop is a React internal mechanism. NEVER include it in component interfaces:

```typescript
// ❌ WRONG - Including 'key' in props interface
interface ItemProps {{
  key: string;  // ERROR: Don't include key!
  title: string;
}}

// ✅ CORRECT - Only include props your component actually uses
interface ItemProps {{
  title: string;
}}
// key is passed separately: <Item key={{id}} title={{title}} />
```

### 19. When Using Existing Components - Match Their Interface EXACTLY (CRITICAL)
When a component already exists with defined props, you MUST pass props that match EXACTLY:

```typescript
// If TaskItem was defined with this interface:
interface TaskItemProps {{
  task: Task;
  onComplete: (id: string) => void;  // Note: returns void
  onDelete: (id: string) => void;    // Note: returns void
}}

// ❌ WRONG - Different prop names or types
<TaskItem 
  task={{task}}
  onToggleComplete={{handleToggle}}  // ERROR: should be 'onComplete'
  onDelete={{handleDelete}}
/>

// ❌ WRONG - Wrong return type (Promise<void> vs void)
onComplete={{async (id) => await toggleTask(id)}}  // ERROR if interface says void

// ❌ WRONG - Passing extra props not in interface
<TaskItem 
  task={{task}}
  onComplete={{handleComplete}}
  onDelete={{handleDelete}}
  updating={{true}}  // ERROR: 'updating' not in TaskItemProps
/>

// ✅ CORRECT - Props match interface exactly
<TaskItem 
  task={{task}}
  onComplete={{handleComplete}}
  onDelete={{handleDelete}}
/>
```

CRITICAL: Before using any component:
1. Check its interface definition
2. Use EXACTLY the prop names defined
3. Match types exactly (including Promise<void> vs void)
4. Don't pass extra props not in the interface

### 20. Toast Pattern - Define Hook in Same File (CRITICAL)
If you create a Toast component and need a useToast hook, define BOTH in the same file:

```typescript
// Toast.tsx - Include BOTH the component AND the hook
export interface ToastData {{
  id: string;
  message: string;
  type: 'success' | 'error';
}}

// The hook - exported from the SAME file
export function useToast() {{
  const [toasts, setToasts] = useState<ToastData[]>([]);
  
  const showToast = (message: string, type: 'success' | 'error') => {{
    const id = Date.now().toString();
    setToasts(prev => [...prev, {{ id, message, type }}]);
  }};
  
  const removeToast = (id: string) => {{
    setToasts(prev => prev.filter(t => t.id !== id));
  }};
  
  return {{ toasts, showToast, removeToast }};
}}

// The component
export function Toast({{ id, message, type, onClose }}: ToastData & {{ onClose: () => void }}) {{
  return <div>{{message}}</div>;
}}
```

Then import from the SAME file:
```typescript
import {{ useToast, Toast }} from './Toast';  // Both from same file
```

NEVER import a hook from a file that only contains a component. If you need useToast, it MUST be exported from the same file you're importing from.

### 21. useState Type Consistency - Maintain Exact Types
When updating state with setState, the new value MUST match the state type exactly:

```typescript
interface Todo {{
  id: string;
  title: string;
  completed: boolean;
}}

const [todos, setTodos] = useState<Todo[]>([]);

// ✅ CORRECT - Returns Todo[] (same type as state)
setTodos(prev => prev.map(todo => 
  todo.id === id ? {{ ...todo, completed: !todo.completed }} : todo
));

// ❌ WRONG - Returns (Todo | NewType)[] which doesn't match Todo[]
setTodos(prev => [...prev, {{ title: 'New', completed: false }}]);  // Missing id!

// ✅ CORRECT - Include all required properties
setTodos(prev => [...prev, {{ id: Date.now().toString(), title: 'New', completed: false }}]);
```

When spreading objects, ensure ALL required properties of the interface are present.
"""


OVER_EAGERNESS_GUARD = """
## Anti-Over-Engineering Rules (CRITICAL)

You MUST follow these rules to avoid over-engineering:

1. **Only implement what is explicitly requested**
   - A bug fix does NOT need surrounding code "cleaned up"
   - A simple feature does NOT need extra configurability
   - If user asks for a button, add a button - not a button factory

2. **Do NOT add unrequested complexity**
   - No error handling for scenarios that can't happen
   - No helpers/utilities for one-time operations
   - No abstractions for hypothetical future requirements
   - No "just in case" validation or fallbacks

3. **Minimize changes**
   - The right amount of complexity is the MINIMUM needed
   - Reuse existing code and patterns - follow DRY principle
   - Don't refactor adjacent code unless explicitly asked
   - Don't add features "while you're in there"

4. **Stay focused**
   - Complete only the requested task
   - Don't suggest or implement related features
   - Don't add documentation unless requested
   - Don't add tests unless requested
"""


# =============================================================================
# ANTI-AI-SLOP GUIDELINES
# =============================================================================
# Prevents generic, overused AI aesthetic patterns in UI generation.

ANTI_AI_SLOP_GUIDE = """
## Avoid Generic AI Aesthetics (CRITICAL)

DO NOT USE these overused AI patterns:
- Inter, Roboto, Arial, or generic system fonts
- Purple/violet gradients on white backgrounds (the "AI cliché")
- Predictable 3-column card layouts with equal spacing
- Generic hero sections with centered text and a single CTA
- Rounded corners on everything with no visual hierarchy
- Blue (#3B82F6) as the only accent color
- Generic placeholder text like "Lorem ipsum" or "Your description here"

INSTEAD, use these distinctive approaches:
- Commit to a cohesive color scheme: dominant + 1-2 sharp accents
- Add micro-interactions: hover states with subtle transforms, transitions
- Use CSS animations for polish: staggered fade-ins, subtle parallax
- Create visual hierarchy through size, weight, and spacing contrast
- Match the app's purpose with appropriate aesthetics:
  - Business/enterprise: Clean grays, minimal color, professional
  - Creative tools: Bold colors, expressive typography
  - Data-heavy apps: Dense but organized, clear information hierarchy

Remember: Generic = forgettable. Make intentional design choices.
"""


# Shared style guide injected into all design-aware prompts.
DESIGN_STYLE_PROMPT = """This base style guide (enterprise, light theme, minimal):

## Typography
- CRITICAL: Avoid excessive bolding. Use font-medium for most text, font-semibold sparingly and only for truly important headings.
- Page titles: text-lg or text-xl font-medium text-gray-900 (NOT font-bold)
- Section headings: text-sm font-medium text-gray-900
- Body text: text-sm text-gray-700, subtext: text-xs text-gray-500
- Labels: text-xs font-medium text-gray-600 uppercase tracking-wide (for form labels)
- Use system font stack. Never use font-bold unless absolutely critical.

## Spacing & Layout
- Generous spacing creates visual breathing room: p-4 for compact areas, p-6 for main sections, p-8 for page containers
- Use gap-3 or gap-4 between related items, gap-6 between sections
- Cards: p-4 or p-5 internal padding with space-y-4 for stacked content
- Form fields: space-y-4 between fields, mt-1 or mt-1.5 between label and input
- Section dividers: border-t border-gray-100 with py-4 above and below
- Maintain consistent vertical rhythm throughout

## Colors & Surfaces
- Canvas: bg-gray-50; Cards/surfaces: bg-white
- Borders: border-gray-200 default, hover:border-gray-300
- Subtle shadows: shadow-sm for cards, shadow-md for elevated elements like dropdowns/popovers
- Status colors (soft backgrounds): bg-green-50 border-green-200 text-green-700, bg-red-50 border-red-200 text-red-700, bg-yellow-50 border-yellow-200 text-yellow-700, bg-blue-50 border-blue-200 text-blue-700

## Buttons
- Primary: bg-gray-900 hover:bg-gray-800 text-white rounded-md px-4 py-2 text-sm font-medium
- Secondary: bg-gray-100 hover:bg-gray-200 text-gray-700 rounded-md px-3 py-1.5 text-sm font-medium
- Outline: border border-gray-200 bg-white hover:bg-gray-50 text-gray-700 rounded-md
- Ghost: bg-transparent hover:bg-gray-100 text-gray-600 rounded-md
- Small buttons: px-3 py-1.5 text-xs, icon buttons: p-2 rounded-md
- Disabled: opacity-50 cursor-not-allowed

## Form Inputs & Select Dropdowns
- NEVER use browser default selects. Always style custom selects:
- Input/Select base: w-full bg-white border border-gray-200 rounded-md px-3 py-2 text-sm text-gray-900 placeholder:text-gray-500 focus:outline-none focus:ring-2 focus:ring-gray-900 focus:ring-offset-1 focus:border-gray-300
- Custom select trigger: flex items-center justify-between, add ChevronDown icon (h-4 w-4 text-gray-400)
- Dropdown content: bg-white border border-gray-200 rounded-md shadow-lg p-1 z-50
- Dropdown item: px-3 py-2 text-sm text-gray-700 rounded-sm cursor-pointer hover:bg-gray-100 focus:bg-gray-100

## Dropdown Menus (Action menus)
- Container: min-w-[8rem] bg-white border border-gray-200 rounded-md shadow-md p-1 z-50
- Menu item: flex items-center gap-2 px-2 py-1.5 text-sm text-gray-700 rounded-sm cursor-pointer hover:bg-gray-100 transition-colors
- Separator: my-1 h-px bg-gray-200
- Icons in menus: h-4 w-4 text-gray-500

## Popovers
- Container: bg-white border border-gray-200 rounded-lg shadow-lg z-50 overflow-hidden
- Header: px-4 py-3 border-b border-gray-100 bg-gray-50
- Content area: p-4 space-y-4
- Use subtle entrance animation: opacity 0→1, slight y-offset (-4px→0), scale 0.98→1
- Close on outside click

## Modals/Dialogs
- Backdrop: fixed inset-0 bg-black/40 backdrop-blur-sm z-50
- Modal container: bg-white rounded-xl shadow-2xl max-w-md w-full mx-4 max-h-[85vh] overflow-hidden flex flex-col
- Header: flex items-center justify-between px-6 py-4 border-b border-gray-200
- Header title: text-lg font-medium text-gray-900 (NOT font-bold or font-semibold)
- Content: flex-1 overflow-y-auto px-6 py-4
- Footer: flex items-center justify-end gap-3 px-6 py-4 border-t border-gray-200 bg-gray-50 rounded-b-xl
- Close button: p-2 text-gray-400 hover:text-gray-600 hover:bg-gray-100 rounded-lg

## Toast Notifications
- ALWAYS implement custom toasts, never use browser alerts:
- Position: fixed bottom-4 right-4 z-[100]
- Container: flex items-start gap-3 p-4 rounded-lg border shadow-lg min-w-[320px] max-w-[400px]
- Success: bg-green-50 border-green-200, icon CheckCircle text-green-600
- Error: bg-red-50 border-red-200, icon AlertCircle text-red-600
- Warning: bg-yellow-50 border-yellow-200, icon AlertTriangle text-yellow-600
- Info: bg-blue-50 border-blue-200, icon Info text-blue-600
- Title: text-sm font-medium text-gray-900
- Description: text-sm text-gray-600 mt-1
- Close button: p-1 text-gray-400 hover:text-gray-600 rounded
- Animation: slide in from right, fade out on dismiss

## Tables
- Container: border border-gray-200 rounded-lg overflow-hidden
- Header row: bg-gray-50 border-b border-gray-200
- Header cells: px-4 py-3 text-xs font-medium text-gray-500 uppercase tracking-wide text-left
- Body rows: hover:bg-gray-50 border-b border-gray-100 last:border-0
- Body cells: px-4 py-3 text-sm text-gray-700
- Actions column: text-right, use icon buttons or dropdown menu

## Cards
- Container: bg-white border border-gray-200 rounded-lg shadow-sm overflow-hidden
- Clickable cards: hover:border-gray-300 hover:shadow transition-all cursor-pointer
- Card header: px-4 py-3 border-b border-gray-100 (optional)
- Card content: p-4 or p-5
- Card footer: px-4 py-3 border-t border-gray-100 bg-gray-50

## Corners & Shapes
- Buttons/inputs: rounded-md
- Cards/modals: rounded-lg or rounded-xl
- Small elements (badges, tags): rounded-full or rounded-md
- Avoid sharp corners

## Icons
- Use Lucide React icons consistently
- Size: h-4 w-4 for inline, h-5 w-5 for buttons, h-6 w-6 for larger UI elements
- Colors: text-gray-400 for decorative, text-gray-500 for secondary, text-gray-700 for primary

## Loading States
- Spinner: Loader2 icon with animate-spin
- Skeleton: animate-pulse with bg-gray-200 rounded blocks
- Disabled during loading: opacity-60 pointer-events-none

""" + ANTI_AI_SLOP_GUIDE

# Base system prompt for codegen; design style is injected dynamically.
CODEGEN_SYSTEM_PROMPT_TEMPLATE = """You are an expert React/TypeScript developer building internal business applications.

{over_eagerness_guard}

{typescript_safety_rules}

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
  const config = window.__VECTOR_CONFIG__;
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
  const config = window.__VECTOR_CONFIG__;
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

{template_context}

NEVER import from node_modules that aren't available. Use inline implementations."""


# =============================================================================
# TEMPLATE CONTEXT
# =============================================================================
# Injects available pre-built components and hooks into the prompt.

TEMPLATE_CONTEXT = """
## Pre-built Components Available

You have access to a component library in `/components/ui/`. ALWAYS prefer using these over writing custom implementations.

### UI Components (import from './components/ui')

| Component | Props | Description |
|-----------|-------|-------------|
| `Button` | variant: 'default'|'destructive'|'outline'|'secondary'|'ghost', size: 'default'|'sm'|'lg'|'icon', loading: boolean | Primary interactive button |
| `Input` | label?: string, error?: string | Text input with label and error state |
| `Card` | title?: string, interactive?: boolean | Content container with optional title |
| `Badge` | variant: 'default'|'success'|'warning'|'error'|'info' | Status indicator |
| `PageHeader` | title: string, subtitle?: string, actions?: ReactNode | Page title with actions |
| `EmptyState` | icon?: ReactNode, title: string, description?: string, action?: ReactNode | Empty data placeholder |
| `StatCard` | title: string, value: string|number, change?: number, changeLabel?: string | Metric display with trend |

### Utility (import from './components/ui')
- `cn()` - Utility to merge Tailwind CSS classes

### Hooks (import from './hooks')

| Hook | Returns | Description |
|------|---------|-------------|
| `useDataQuery(tableSlug, options?, deps?)` | { data, totalCount, loading, error, refetch } | Fetch rows from a data table |
| `useMutation(tableSlug)` | { insert, update, remove, loading, error } | Insert, update, delete rows |

### Usage Examples

```typescript
// Import components
import { Button, Card, Badge, PageHeader, EmptyState, StatCard, cn } from './components/ui';
import { useDataQuery, useMutation } from './hooks';

// Fetch data
const { data: customers, loading, refetch } = useDataQuery('customers', {
  filters: [{ field: 'status', op: 'eq', value: 'active' }],
  orderBy: [{ field: 'created_at', dir: 'desc' }],
  limit: 50,
});

// Mutations
const { insert, update, remove, loading: saving } = useMutation('customers');
await insert({ name: 'John Doe', email: 'john@example.com' });
await update(rowId, { status: 'inactive' });
await remove(rowId);

// UI Components
<PageHeader title="Customers" subtitle="Manage your customer base" actions={<Button>Add New</Button>} />
<Card title="Customer Details">...</Card>
<Badge variant="success">Active</Badge>
<StatCard title="Revenue" value="$12,450" change={12.5} changeLabel="vs last month" />
```

IMPORTANT: Use these pre-built components instead of writing custom implementations for tables, cards, buttons, forms, and data fetching.
"""

PLAN_PROMPT_TEMPLATE = """You are planning an internal app generation task.

User Request: {user_message}
App Name: {app_name}
Available Resources: {available_resources}
Has Existing Spec: {has_existing_spec}

🚨 **CRITICAL PLANNING RULES:**

## 1. Data Storage Rule

### For NEW apps (no existing tables):
If the app needs to store/manage ANY data, you MUST:
- **First Step MUST be type="data" with step_order=0**: Define ALL table schemas needed for the ENTIRE application
- Think through EVERY entity that needs to be stored upfront
- **YOU CANNOT CREATE TABLES LATER** - all tables must be defined in this first step!

## 2. Parallel Execution with step_order
Each step has a `step_order` (integer) that determines when it executes:
- **step_order=0**: Executes first (always use for "data" type steps)
- **step_order=1, 2, 3...**: Higher numbers execute after lower numbers complete
- **Steps with the SAME step_order run IN PARALLEL** - they must be 100% independent!

Rules for parallel steps (same step_order):
- They MUST NOT create or modify the same files
- They MUST NOT depend on each other's output
- Each step's description MUST explicitly list the files it will create

## 3. Explicit File Ownership in Descriptions
Each step description MUST clearly state which files it will create or modify:
- ❌ BAD: "Build UI components"
- ✅ GOOD: "Create src/components/Header.tsx and src/components/Sidebar.tsx"
- ❌ BAD: "Create table schemas for core data"
- ✅ GOOD: "Create table schemas for: projects, tasks, sprints, team_members"

## 4. Detailed Step Descriptions

Each step description must be self-contained and specific enough that an AI executing ONLY that step can succeed without seeing other steps.

### Step Type Guidelines:

**"data" steps (ALWAYS step_order=0):**
- List EVERY table that the entire app needs - you cannot add tables later!
- For each table, specify key columns with their purpose
- Think through relationships and foreign keys upfront
- ✅ "Create table schemas for: projects (name, description, status, owner_id, due_date), tasks (title, project_id FK, assignee_id, status, priority, due_date, completed_at), team_members (name, email, role, avatar_url), comments (task_id FK, author_id, content, created_at)"
- ❌ "Create the database tables" (missing table names and columns)

**"component" steps:**
- Specify EXACT file path(s) this step creates
- **ONLY reference tables that YOU DEFINED in the data step** - if you didn't create a table in step_order=0, don't reference it here!
- Describe what data the component fetches (which table slug, what filters)
- List key UI elements: tables, forms, modals, buttons, filters
- Specify user interactions: click handlers, form submissions, navigation
- ⚠️ If a feature needs a table you forgot to create, either add it to the data step OR remove that feature from this step description
- ✅ "Create src/components/ProjectBoard.tsx: Fetches tasks from 'tasks' table filtered by project_id. Displays a 3-column Kanban board (To Do, In Progress, Done). Each task card shows title, assignee avatar, and due date. Clicking a card opens edit modal. Include 'Add Task' button per column that opens TaskForm with pre-set status."
- ✅ "Create src/components/TaskForm.tsx: A modal form for creating/editing tasks. Fields: title (required, min 3 chars), description (optional textarea), status (select: todo/in_progress/done), priority (1-5 slider), due_date (date picker), assignee_id (select from team_members). Props: task?: Task for edit mode, onSave callback, onClose callback."
- ❌ "Build the task UI" (no file paths, no specifics)
- ❌ "Create TeamManagement.tsx: Fetches from 'team_members'" (if 'team_members' is not in Available Resources)

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
2. **Data dependencies** - Reference table slugs for any data access
3. **Self-contained** - Another AI should be able to execute this step with ONLY its description

Generate a plan with 2-5 steps. Return JSON:
{{
    "reasoning": "<string: Your analysis of what needs to be built. LIST ALL DATA TABLES NEEDED.>",
    "steps": [
        {{"type": "<step_type>", "step_order": <int>, "title": "<string>", "description": "<detailed description following guidelines above>"}}
    ]
}}

Step types: research, design, data, code, component, styling, integration, validation
**REMEMBER**: Data steps are always step_order=0. Steps with the same step_order run in parallel and must not conflict!"""

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

// For habits: use the TABLE SLUG, not a custom store
await dataStore.query('habits', {{}});
await dataStore.insert('habits', {{ name: 'Exercise' }});
await dataStore.update('habits', rowId, {{ completed: true }});
await dataStore.delete('habits', rowId);
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
  import type {{ Database, TableSlug }} from '../lib/types';
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
12. **🚨 CRITICAL - CHECK YOUR CODE FOR STRAY QUOTES 🚨**: Every line must end cleanly with semicolon `;` NOT `;'` or `;"` - these cause parse errors!
13. **CRITICAL**: Ensure all TypeScript syntax is valid - no stray quotes, unclosed brackets, or malformed type declarations
14. **ONLY import from**: react, react-dom, lucide-react, framer-motion, react-router-dom
15. **DO NOT use**: Any other npm packages not listed above
16. For navigation, use react-router-dom (BrowserRouter, Routes, Route, Link, useNavigate)
17. **CRITICAL - AVOID SYNTAX ERRORS**:
    ```typescript
    // ❌ WRONG - Stray quotes cause parse errors
    owner: Database['projects']['row'];'  // Extra quote at end!
    status: string";  // Malformed!

    // ✅ CORRECT - Clean syntax, use string literal keys
    owner: Database['projects']['row'];
    status: string;
    interface Foo extends Database['projects']['row'] {{ extra: string; }}

    // ✅ ALSO CORRECT - Import and use TableSlug enum
    import type {{ Database, TableSlug }} from './lib/types';
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

// Type your operations
const item: Database['customers']['insert'] = {{ name: 'John', email: 'john@example.com' }};
await dataStore.insert('customers', item);

const result = await dataStore.query('customers', {{
  filters: [{{ field: 'status', op: 'eq', value: 'active' }}]
}});
const customers: Database['customers']['row'][] = result.rows;

// Access fields via row.data:
result.rows.map(row => <div key={{row.id}}>{{row.data.name}}</div>);

// Update/delete uses row.id (NOT row.data.id):
await dataStore.update('customers', row.id, {{ status: 'inactive' }});
await dataStore.delete('customers', row.id);
```

**Filter Operators:** `eq`, `neq`, `gt`, `gte`, `lt`, `lte`, `in`, `not_in`, `contains`, `icontains`, `is_null`

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

# System prompt for code generation with data store support
CODEGEN_SYSTEM_PROMPT_WITH_DATASTORE = """You are an expert React/TypeScript developer building internal business applications with persistent data storage.

{over_eagerness_guard}

{typescript_safety_rules}

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

{template_context}

NEVER import from node_modules that aren't available. Use inline implementations."""


# Connectors/Integrations prompt template - injected when external integrations are available
CONNECTORS_PROMPT_TEMPLATE = """
## External Integrations

Your app can interact with external services through connectors. These integrations allow your app to create issues in Jira, send Slack messages, manage tasks in Linear, and more.

{connectors_context}

### Using Connectors in Code

The connectors client is available at `./lib/connectors.ts`:

```typescript
import {{ connectors }} from './lib/connectors';

// List available connectors
const available = await connectors.list();
// Returns: {{ connectors: [{{ id: 'jira', name: 'Jira', category: 'project_management', tool_count: 5 }}, ...] }}

// Get tools available for a specific connector
const jiraTools = await connectors.getTools('jira');
// Returns: {{ tools: [{{ id: 'create_issue', name: 'Create Issue', description: '...', parameters: {{...}} }}, ...] }}

// Execute a tool on a connector
const result = await connectors.execute('jira', 'create_issue', {{
  project: 'PROJ',
  summary: 'New task from app',
  description: 'Created via internal app',
  issue_type: 'Task'
}});
// Returns: {{ success: true, data: {{...}} }} or {{ success: false, error: '...' }}
```

### Connectors Client Implementation

If you need to generate the connectors client, use this template for `src/lib/connectors.ts`:

```typescript
const getConfig = () => {{
  const config = (window as any).__VECTOR_CONFIG__;
  return {{
    apiBaseUrl: config?.apiBaseUrl || 'http://localhost:8001/api/v1',
    appId: config?.appId || '',
  }};
}};

async function apiCall<T>(
  connectorId: string,
  toolId: string,
  params: Record<string, any>
): Promise<T> {{
  const config = getConfig();
  const response = await fetch(`${{config.apiBaseUrl}}/runtime/connectors/`, {{
    method: 'POST',
    headers: {{ 'Content-Type': 'application/json' }},
    body: JSON.stringify({{
      appId: config.appId,
      connectorId,
      toolId,
      params,
    }}),
  }});
  if (!response.ok) {{
    const error = await response.json().catch(() => ({{}}));
    throw new Error(error.error || 'Connector request failed');
  }}
  return response.json();
}}

interface Connector {{
  id: string;
  name: string;
  category: string;
  icon_url?: string;
  tool_count: number;
}}

interface Tool {{
  id: string;
  name: string;
  description: string;
  parameters: Record<string, any>;
}}

export const connectors = {{
  list: () => apiCall<{{connectors: Connector[]}}>('_meta', 'list', {{}}),
  getTools: (connectorId: string) => apiCall<{{tools: Tool[]}}>('_meta', 'tools', {{ connectorId }}),
  execute: <T = any>(connectorId: string, toolId: string, params: Record<string, any>) =>
    apiCall<{{success: boolean; data?: T; error?: string}}>(connectorId, toolId, params),
}};
```

### Common Patterns

1. **Creating items** - Use `create_*` tools (e.g., `connectors.execute('jira', 'create_issue', {{...}})`)
2. **Fetching data** - Use `get_*` or `list_*` tools
3. **Updating items** - Use `update_*` tools
4. **Sending notifications** - Use `send_message` or similar tools

### Error Handling

```typescript
const handleCreateTask = async () => {{
  setLoading(true);
  try {{
    const result = await connectors.execute('jira', 'create_issue', {{
      project: selectedProject,
      summary: taskTitle,
      description: taskDescription,
    }});
    
    if (result.success) {{
      showToast('Task created successfully!');
      // result.data contains the created issue details
    }} else {{
      showError(result.error || 'Failed to create task');
    }}
  }} catch (error) {{
    showError('Network error - please try again');
  }} finally {{
    setLoading(false);
  }}
}};
```
"""


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
        data_store_prompt = DATA_STORE_PROMPT_TEMPLATE.format(
            data_store_context=data_store_context
        )
        parts.append(data_store_prompt)
    
    # Add connectors context if provided
    if connectors_context:
        connectors_prompt = CONNECTORS_PROMPT_TEMPLATE.format(
            connectors_context=connectors_context
        )
        parts.append(connectors_prompt)
    
    parts.append(design_prompt)
    
    return "\n\n".join(parts)


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
            connectors_prompt = CONNECTORS_PROMPT_TEMPLATE.format(
                connectors_context=connectors_context
            )
            resources_info += f"\n\n{connectors_prompt}"
    
    if not resources_info.strip():
        resources_info = "No data resources available - you can create tables using TABLE_DEFINITION blocks"

    # Check if this is a "data" step (table definition step)
    step_type = getattr(step, "type", "code")
    step_title_lower = getattr(step, "title", "").lower()
    is_data_step = (
        step_type == "data" or
        "table" in step_title_lower or
        "schema" in step_title_lower or
        "data model" in step_title_lower
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




def build_codegen_system_prompt(
    registry_surface: Dict[str, Any],
    has_data_store: bool = False
) -> str:
    """Return the system prompt for code generation with injected design style and guards."""
    if has_data_store:
        prompt = CODEGEN_SYSTEM_PROMPT_WITH_DATASTORE.replace("{design_style}", DESIGN_STYLE_PROMPT)
    else:
        prompt = CODEGEN_SYSTEM_PROMPT_TEMPLATE.replace("{design_style}", DESIGN_STYLE_PROMPT)
    
    # Inject over-eagerness guard
    prompt = prompt.replace("{over_eagerness_guard}", OVER_EAGERNESS_GUARD)
    
    # Inject TypeScript safety rules to prevent common compilation errors
    prompt = prompt.replace("{typescript_safety_rules}", TYPESCRIPT_SAFETY_RULES)
    
    # Inject template context (pre-built components and hooks)
    prompt = prompt.replace("{template_context}", TEMPLATE_CONTEXT)
    
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



