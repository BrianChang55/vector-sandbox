/**
 * Table List Component
 * 
 * Displays list of data tables with selection and actions.
 */
import { useState } from 'react'
import {
  Table2,
  MoreVertical,
  Trash2,
  Hash,
} from 'lucide-react'
import { useDeleteTable } from '@/hooks/useDataStore'
import { useDialog } from '@/components/ui/dialog-provider'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu'
import { cn } from '@/lib/utils'
import type { DataTable } from '@/types/dataStore'

interface TableListProps {
  tables: DataTable[]
  selectedTable: DataTable | null
  onSelectTable: (table: DataTable) => void
  appId: string
}

export function TableList({
  tables,
  selectedTable,
  onSelectTable,
  appId,
}: TableListProps) {
  const deleteTable = useDeleteTable()
  const { confirm } = useDialog()
  const [deletingId, setDeletingId] = useState<string | null>(null)

  const handleDelete = async (table: DataTable, e: Event) => {
    e.preventDefault()
    e.stopPropagation()

    const confirmed = await confirm({
      title: 'Delete table?',
      description: `This will permanently delete "${table.name}" and all its data (${table.row_count} rows). This action cannot be undone.`,
      variant: 'destructive',
      confirmText: 'Delete',
    })

    if (!confirmed) return

    setDeletingId(table.id)
    try {
      await deleteTable.mutateAsync({ appId, tableSlug: table.slug })
      if (selectedTable?.id === table.id) {
        onSelectTable(tables.find((t) => t.id !== table.id) || null as any)
      }
    } catch (error) {
      console.error('Failed to delete table:', error)
    } finally {
      setDeletingId(null)
    }
  }

  return (
    <div className="p-2 space-y-0.5">
      {tables.map((table) => (
        <div
          key={table.id}
          className={cn(
            'group flex items-center gap-2 px-2.5 py-2 rounded-md cursor-pointer transition-colors',
            selectedTable?.id === table.id
              ? 'bg-gray-100 text-gray-900'
              : 'text-gray-700 hover:bg-gray-50 hover:text-gray-900'
          )}
          onClick={() => onSelectTable(table)}
        >
          <Table2
            className={cn(
              'h-4 w-4 flex-shrink-0',
              selectedTable?.id === table.id ? 'text-gray-700' : 'text-gray-400'
            )}
          />
          
          <div className="flex-1 min-w-0">
            <div className="text-sm font-medium truncate">{table.name}</div>
            <div className="flex items-center gap-2 text-[10px] text-gray-500">
              <span className="flex items-center gap-0.5">
                <Hash className="h-2.5 w-2.5" />
                {table.row_count} rows
              </span>
              <span>•</span>
              <span>{table.column_count} cols</span>
            </div>
          </div>

          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <button
                className={cn(
                  'p-1 rounded-md transition-colors',
                  'opacity-0 group-hover:opacity-100',
                  'text-gray-400 hover:text-gray-600 hover:bg-gray-200'
                )}
                onClick={(e) => e.stopPropagation()}
              >
                <MoreVertical className="h-3.5 w-3.5" />
              </button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end" sideOffset={4}>
              <DropdownMenuItem
                className="gap-2 text-red-600 focus:bg-red-50"
                onSelect={(e) => handleDelete(table, e)}
              >
                <Trash2 className="h-4 w-4" />
                Delete Table
              </DropdownMenuItem>
            </DropdownMenuContent>
          </DropdownMenu>

          {deletingId === table.id && (
            <span className="text-[10px] text-red-500">Deleting...</span>
          )}
        </div>
      ))}
    </div>
  )
}

