/**
 * Template UI Components for Sandpack
 * 
 * These pre-built components are injected into the Sandpack environment
 * to provide a starting point for AI-generated apps.
 */

export const TEMPLATE_UI_COMPONENTS: Record<string, string> = {
  '/components/ui/utils.ts': `type ClassValue = string | number | boolean | undefined | null | ClassValue[];
function clsx(...inputs: ClassValue[]): string {
  return inputs.flat().filter((x) => typeof x === 'string' && x.length > 0).join(' ');
}
export function cn(...inputs: ClassValue[]): string { return clsx(...inputs); }
`,

  '/components/ui/Button.tsx': `import React from 'react';
import { cn } from './utils';

interface ButtonProps extends React.ButtonHTMLAttributes<HTMLButtonElement> {
  variant?: 'default' | 'destructive' | 'outline' | 'secondary' | 'ghost';
  size?: 'default' | 'sm' | 'lg' | 'icon';
  loading?: boolean;
}

const variants = {
  default: 'bg-gray-900 text-white hover:bg-gray-800',
  destructive: 'bg-red-600 text-white hover:bg-red-700',
  outline: 'border border-gray-200 bg-white text-gray-900 hover:bg-gray-50',
  secondary: 'bg-gray-100 text-gray-900 hover:bg-gray-200',
  ghost: 'text-gray-900 hover:bg-gray-100',
};
const sizes = { default: 'h-9 px-4 py-2', sm: 'h-8 px-3 text-xs', lg: 'h-11 px-8', icon: 'h-9 w-9' };

export function Button({ className, variant = 'default', size = 'default', loading, disabled, children, ...props }: ButtonProps) {
  return (
    <button className={cn('inline-flex items-center justify-center rounded-md text-sm font-medium transition-colors disabled:opacity-50', variants[variant], sizes[size], className)} disabled={disabled || loading} {...props}>
      {loading && <svg className="mr-2 h-4 w-4 animate-spin" fill="none" viewBox="0 0 24 24"><circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4"/></svg>}
      {children}
    </button>
  );
}
export default Button;
`,

  '/components/ui/Input.tsx': `import React from 'react';
import { cn } from './utils';

interface InputProps extends React.InputHTMLAttributes<HTMLInputElement> { label?: string; error?: string; }

export const Input = React.forwardRef<HTMLInputElement, InputProps>(({ className, label, error, ...props }, ref) => (
  <div className="w-full">
    {label && <label className="block text-sm font-medium text-gray-700 mb-1.5">{label}</label>}
    <input className={cn('flex h-10 w-full rounded border border-gray-200 bg-white px-3 py-2 text-sm text-gray-900 placeholder:text-gray-400 focus:outline-none focus:border-gray-400 disabled:opacity-50', error && 'border-red-300', className)} ref={ref} {...props}/>
    {error && <p className="mt-1 text-sm text-red-600">{error}</p>}
  </div>
));
Input.displayName = 'Input';
export default Input;
`,

  '/components/ui/Card.tsx': `import React from 'react';
import { cn } from './utils';

interface CardProps extends React.HTMLAttributes<HTMLDivElement> { title?: string; interactive?: boolean; }

export function Card({ className, children, title, interactive, ...props }: CardProps) {
  return (
    <div className={cn('bg-white rounded-lg border border-gray-200 p-6', interactive && 'hover:border-gray-300 hover:shadow-sm transition-all cursor-pointer', className)} {...props}>
      {title && <h3 className="font-medium text-gray-900 mb-4">{title}</h3>}
      {children}
    </div>
  );
}
export default Card;
`,

  '/components/ui/Badge.tsx': `import React from 'react';
import { cn } from './utils';

type BadgeVariant = 'default' | 'success' | 'warning' | 'error' | 'info';
const styles: Record<BadgeVariant, string> = {
  default: 'bg-gray-100 text-gray-600 border-gray-200',
  success: 'bg-green-50 text-green-700 border-green-200',
  warning: 'bg-yellow-50 text-yellow-700 border-yellow-200',
  error: 'bg-red-50 text-red-700 border-red-200',
  info: 'bg-blue-50 text-blue-700 border-blue-200',
};

export function Badge({ className, variant = 'default', children, ...props }: React.HTMLAttributes<HTMLSpanElement> & { variant?: BadgeVariant }) {
  return <span className={cn('inline-flex items-center text-xs px-2 py-1 rounded-full border font-medium', styles[variant], className)} {...props}>{children}</span>;
}
export default Badge;
`,

  '/components/ui/PageHeader.tsx': `import React from 'react';
import { cn } from './utils';

interface PageHeaderProps { title: string; subtitle?: string; actions?: React.ReactNode; className?: string; }

export function PageHeader({ title, subtitle, actions, className }: PageHeaderProps) {
  return (
    <div className={cn('bg-white border-b border-gray-200', className)}>
      <div className="max-w-6xl mx-auto px-6 py-6 flex items-center justify-between">
        <div>
          <h1 className="text-xl font-semibold text-gray-900">{title}</h1>
          {subtitle && <p className="text-sm text-gray-500 mt-0.5">{subtitle}</p>}
        </div>
        {actions && <div className="flex items-center gap-3">{actions}</div>}
      </div>
    </div>
  );
}
export default PageHeader;
`,

  '/components/ui/EmptyState.tsx': `import React from 'react';
import { cn } from './utils';

interface EmptyStateProps { icon?: React.ReactNode; title: string; description?: string; action?: React.ReactNode; className?: string; }

export function EmptyState({ icon, title, description, action, className }: EmptyStateProps) {
  return (
    <div className={cn('flex flex-col items-center justify-center py-16', className)}>
      {icon && <div className="h-16 w-16 rounded-xl bg-gray-100 flex items-center justify-center mb-4"><span className="text-gray-400">{icon}</span></div>}
      <h2 className="text-lg font-medium text-gray-900 mb-1">{title}</h2>
      {description && <p className="text-sm text-gray-500 mb-6 max-w-sm text-center">{description}</p>}
      {action}
    </div>
  );
}
export default EmptyState;
`,

  '/components/ui/StatCard.tsx': `import React from 'react';
import { cn } from './utils';

interface StatCardProps { title: string; value: string | number; change?: number; changeLabel?: string; }

export function StatCard({ title, value, change, changeLabel }: StatCardProps) {
  const isPositive = change !== undefined && change >= 0;
  return (
    <div className="bg-white rounded-lg border border-gray-200 p-6">
      <p className="text-sm font-medium text-gray-500 mb-2">{title}</p>
      <p className="text-2xl font-semibold text-gray-900">{value}</p>
      {change !== undefined && <div className="mt-2 flex items-center gap-1"><span className={cn('text-sm font-medium', isPositive ? 'text-green-600' : 'text-red-600')}>{isPositive ? '+' : ''}{change}%</span>{changeLabel && <span className="text-sm text-gray-500">{changeLabel}</span>}</div>}
    </div>
  );
}
export default StatCard;
`,

  '/components/ui/index.ts': `export { Button } from './Button';
export { Input } from './Input';
export { Card } from './Card';
export { Badge } from './Badge';
export { PageHeader } from './PageHeader';
export { EmptyState } from './EmptyState';
export { StatCard } from './StatCard';
export { cn } from './utils';
`,

  '/hooks/useDataQuery.ts': `import { useState, useEffect, useCallback } from 'react';
import { dataStore } from '../lib/dataStore';

export function useDataQuery<T = any>(tableSlug: string, options: any = {}, deps: any[] = []) {
  const [data, setData] = useState<T[]>([]);
  const [totalCount, setTotalCount] = useState(0);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const refetch = useCallback(async () => {
    setLoading(true); setError(null);
    try {
      const result = await dataStore.query(tableSlug, options);
      setData(result.rows.map((r: any) => ({ _id: r.id, ...r.data })) as T[]);
      setTotalCount(result.total_count);
    } catch (e) { setError(String(e)); } finally { setLoading(false); }
  }, [tableSlug, JSON.stringify(options)]);

  useEffect(() => { refetch(); }, [refetch, ...deps]);
  return { data, totalCount, loading, error, refetch };
}
export default useDataQuery;
`,

  '/hooks/useMutation.ts': `import { useState, useCallback } from 'react';
import { dataStore } from '../lib/dataStore';

export function useMutation(tableSlug: string) {
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const withLoading = useCallback(async <T,>(op: () => Promise<T>): Promise<T> => {
    setLoading(true); setError(null);
    try { const r = await op(); setLoading(false); return r; }
    catch (e) { setError(String(e)); setLoading(false); throw e; }
  }, []);

  const insert = useCallback((d: Record<string, any>) => withLoading(() => dataStore.insert(tableSlug, d)), [tableSlug, withLoading]);
  const update = useCallback((rowId: string, d: Record<string, any>) => withLoading(() => dataStore.update(tableSlug, rowId, d)), [tableSlug, withLoading]);
  const remove = useCallback((rowId: string) => withLoading(() => dataStore.delete(tableSlug, rowId)), [tableSlug, withLoading]);

  return { insert, update, remove, loading, error };
}
export default useMutation;
`,

  '/hooks/index.ts': `export { useDataQuery } from './useDataQuery';
export { useMutation } from './useMutation';
`,
}

