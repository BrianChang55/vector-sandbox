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

// Fetch data - Example: orders table
const { data: orders, loading, refetch } = useDataQuery('orders', {
  filters: [{ field: 'status', op: 'eq', value: 'pending' }],
  orderBy: [{ field: 'created_at', dir: 'desc' }],
  limit: 50,
});

// Mutations - Example: orders table
const { insert, update, remove, loading: saving } = useMutation('orders');
await insert({ total: 99.99, customer_email: 'john@example.com', status: 'pending' });
await update(rowId, { status: 'shipped' });
await remove(rowId);

// UI Components
<PageHeader title="Customers" subtitle="Manage your customer base" actions={<Button>Add New</Button>} />
<Card title="Customer Details">...</Card>
<Badge variant="success">Active</Badge>
<StatCard title="Revenue" value="$12,450" change={12.5} changeLabel="vs last month" />
```

IMPORTANT: Use these pre-built components instead of writing custom implementations for tables, cards, buttons, forms, and data fetching.
"""
