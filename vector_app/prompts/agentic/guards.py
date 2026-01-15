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
// In types/product.ts:
interface Product { id: string; title: string; price: number; inStock: boolean; }

// In components/ProductList.tsx:
const products: Product[] = [...];
products.map(product => <div key={product.id}>{product.title}: ${product.price}</div>)

// ❌ WRONG - Using properties not in the type definition
products.map(product => <div>{product.name}</div>) // ERROR: 'name' doesn't exist on Product (use 'title')
products.map(product => <div>{product.quantity}</div>) // ERROR: 'quantity' doesn't exist on Product
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
import type {{ Database }} from './lib/types';
import {{ TableSlug }} from './lib/types';
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
