/**
 * Data Grid Component
 * 
 * Displays table data in a grid format with pagination,
 * sorting, filtering, and row operations.
 */
import { useState, useMemo } from 'react'
import {
  Plus,
  Trash2,
  RefreshCw,
  Search,
  ChevronLeft,
  ChevronRight,
  ArrowUpDown,
  ArrowUp,
  ArrowDown,
  Loader2,
  MoreHorizontal,
  PencilLine,
  Copy,
  Settings2,
  Filter,
  X,
} from 'lucide-react'
import {
  useQueryRows,
  useDeleteRow,
  useBulkDeleteRows,
} from '@/hooks/useDataStore'
import { useDialog } from '@/components/ui/dialog-provider'
import { Button } from '@/components/ui/button'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu'
import { RowFormDialog } from './RowFormDialog'
import { SchemaViewer } from './SchemaViewer'
import { ImportExportMenu } from './ImportExportMenu'
import { cn } from '@/lib/utils'
import type { DataTable, QuerySpec, QueryResultRow, ColumnDef, FilterDef, FilterOperator } from '@/types/dataStore'

interface DataGridProps {
  appId: string
  table: DataTable
}

const PAGE_SIZE = 25

export function DataGrid({ appId, table }: DataGridProps) {
  const [page, setPage] = useState(0)
  const [sortColumn, setSortColumn] = useState<string | null>(null)
  const [sortDir, setSortDir] = useState<'asc' | 'desc'>('asc')
  const [selectedRows, setSelectedRows] = useState<Set<string>>(new Set())
  const [rowFormOpen, setRowFormOpen] = useState(false)
  const [editingRow, setEditingRow] = useState<QueryResultRow | null>(null)
  const [schemaViewerOpen, setSchemaViewerOpen] = useState(false)
  const [showFilters, setShowFilters] = useState(false)
  const [filters, setFilters] = useState<FilterDef[]>([])
  
  const deleteRow = useDeleteRow()
  const bulkDelete = useBulkDeleteRows()
  const { confirm } = useDialog()

  // Build query spec
  const querySpec: QuerySpec = useMemo(() => {
    const spec: QuerySpec = {
      limit: PAGE_SIZE,
      offset: page * PAGE_SIZE,
    }
    
    if (sortColumn) {
      spec.order_by = [{ field: sortColumn, dir: sortDir }]
    }
    
    if (filters.length > 0) {
      spec.filters = filters
    }
    
    return spec
  }, [page, sortColumn, sortDir, filters])

  const { data: queryResult, isLoading, refetch } = useQueryRows(
    appId,
    table.slug,
    querySpec
  )

  const columns = table.schema_json.columns || []
  const rows = queryResult?.rows || []
  const totalCount = queryResult?.total_count || 0
  const totalPages = Math.ceil(totalCount / PAGE_SIZE)

  const handleSort = (columnName: string) => {
    if (sortColumn === columnName) {
      if (sortDir === 'asc') {
        setSortDir('desc')
      } else {
        setSortColumn(null)
        setSortDir('asc')
      }
    } else {
      setSortColumn(columnName)
      setSortDir('asc')
    }
    setPage(0)
  }

  const handleSelectAll = () => {
    if (selectedRows.size === rows.length) {
      setSelectedRows(new Set())
    } else {
      setSelectedRows(new Set(rows.map((r) => r.id)))
    }
  }

  const handleSelectRow = (rowId: string) => {
    const newSelected = new Set(selectedRows)
    if (newSelected.has(rowId)) {
      newSelected.delete(rowId)
    } else {
      newSelected.add(rowId)
    }
    setSelectedRows(newSelected)
  }

  const handleDeleteRow = async (row: QueryResultRow) => {
    const confirmed = await confirm({
      title: 'Delete row?',
      description: 'This action cannot be undone.',
      variant: 'destructive',
      confirmText: 'Delete',
    })

    if (!confirmed) return

    try {
      await deleteRow.mutateAsync({
        appId,
        tableSlug: table.slug,
        rowId: row.id,
      })
      setSelectedRows((prev) => {
        const next = new Set(prev)
        next.delete(row.id)
        return next
      })
    } catch (error) {
      console.error('Failed to delete row:', error)
    }
  }

  const handleBulkDelete = async () => {
    if (selectedRows.size === 0) return

    const confirmed = await confirm({
      title: `Delete ${selectedRows.size} rows?`,
      description: 'This action cannot be undone.',
      variant: 'destructive',
      confirmText: 'Delete',
    })

    if (!confirmed) return

    try {
      await bulkDelete.mutateAsync({
        appId,
        tableSlug: table.slug,
        data: { row_ids: Array.from(selectedRows) },
      })
      setSelectedRows(new Set())
    } catch (error) {
      console.error('Failed to delete rows:', error)
    }
  }

  const formatCellValue = (value: unknown, column: ColumnDef): string => {
    if (value === null || value === undefined) return '—'
    
    if (column.type === 'boolean') {
      return value ? 'true' : 'false'
    }
    
    if (column.type === 'json') {
      return JSON.stringify(value)
    }
    
    if (column.type === 'datetime' && typeof value === 'string') {
      try {
        return new Date(value).toLocaleString()
      } catch {
        return value
      }
    }
    
    if (column.type === 'date' && typeof value === 'string') {
      try {
        return new Date(value).toLocaleDateString()
      } catch {
        return value
      }
    }
    
    return String(value)
  }

  return (
    <div className="flex flex-col h-full">
      {/* Toolbar */}
      <div className="flex items-center justify-between px-4 py-3 border-b border-gray-200 bg-white">
        <div className="flex items-center gap-3">
          <h3 className="font-medium text-gray-900">{table.name}</h3>
          <span className="text-xs text-gray-500 bg-gray-100 px-2 py-0.5 rounded-full">
            {totalCount} rows
          </span>
          <button
            onClick={() => setSchemaViewerOpen(true)}
            className="p-1 text-gray-400 hover:text-gray-600 hover:bg-gray-100 rounded transition-colors"
            title="View schema"
          >
            <Settings2 className="h-4 w-4" />
          </button>
        </div>

        <div className="flex items-center gap-2">
          {selectedRows.size > 0 && (
            <>
              <span className="text-xs text-gray-500">
                {selectedRows.size} selected
              </span>
              <Button
                variant="outline"
                size="sm"
                className="text-red-600 hover:text-red-700 hover:bg-red-50"
                onClick={handleBulkDelete}
              >
                <Trash2 className="h-3.5 w-3.5 mr-1.5" />
                Delete
              </Button>
              <div className="w-px h-6 bg-gray-200" />
            </>
          )}

          <Button
            variant={showFilters || filters.length > 0 ? 'secondary' : 'outline'}
            size="sm"
            onClick={() => setShowFilters(!showFilters)}
          >
            <Filter className="h-3.5 w-3.5 mr-1.5" />
            Filter
            {filters.length > 0 && (
              <span className="ml-1.5 bg-gray-900 text-white text-[10px] rounded-full px-1.5 py-0.5">
                {filters.length}
              </span>
            )}
          </Button>

          <ImportExportMenu appId={appId} table={table} />

          <Button
            variant="ghost"
            size="sm"
            onClick={() => refetch()}
            disabled={isLoading}
          >
            <RefreshCw className={cn('h-3.5 w-3.5', isLoading && 'animate-spin')} />
          </Button>

          <Button
            size="sm"
            onClick={() => {
              setEditingRow(null)
              setRowFormOpen(true)
            }}
          >
            <Plus className="h-3.5 w-3.5 mr-1.5" />
            Add Row
          </Button>
        </div>
      </div>

      {/* Filter Panel */}
      {showFilters && (
        <div className="px-4 py-3 border-b border-gray-200 bg-gray-50">
          <div className="flex items-center gap-2 flex-wrap">
            {filters.map((filter, index) => (
              <div
                key={index}
                className="flex items-center gap-1.5 bg-white border border-gray-200 rounded-md px-2 py-1 text-xs"
              >
                <span className="font-medium text-gray-900">{filter.field}</span>
                <span className="text-gray-500">{filter.op}</span>
                <span className="text-gray-700">
                  {filter.value === null ? 'null' : String(filter.value)}
                </span>
                <button
                  onClick={() => {
                    setFilters((prev) => prev.filter((_, i) => i !== index))
                    setPage(0)
                  }}
                  className="ml-1 p-0.5 text-gray-400 hover:text-gray-600 hover:bg-gray-100 rounded"
                >
                  <X className="h-3 w-3" />
                </button>
              </div>
            ))}

            <DropdownMenu>
              <DropdownMenuTrigger asChild>
                <Button variant="ghost" size="sm" className="h-7 text-xs">
                  <Plus className="h-3 w-3 mr-1" />
                  Add filter
                </Button>
              </DropdownMenuTrigger>
              <DropdownMenuContent align="start" sideOffset={4}>
                {columns.map((col) => (
                  <DropdownMenuItem
                    key={col.name}
                    onSelect={() => {
                      const defaultOp: FilterOperator = col.type === 'string' || col.type === 'text' 
                        ? 'icontains' 
                        : 'eq'
                      setFilters((prev) => [...prev, { field: col.name, op: defaultOp, value: '' }])
                    }}
                  >
                    {col.name}
                    <span className="ml-2 text-xs text-gray-400">{col.type}</span>
                  </DropdownMenuItem>
                ))}
              </DropdownMenuContent>
            </DropdownMenu>

            {filters.length > 0 && (
              <Button
                variant="ghost"
                size="sm"
                className="h-7 text-xs text-red-600 hover:text-red-700 hover:bg-red-50"
                onClick={() => {
                  setFilters([])
                  setPage(0)
                }}
              >
                Clear all
              </Button>
            )}
          </div>

          {/* Active filter editor */}
          {filters.length > 0 && (
            <div className="mt-3 space-y-2">
              {filters.map((filter, index) => (
                <div key={index} className="flex items-center gap-2">
                  <span className="text-xs font-medium text-gray-700 w-24 truncate">
                    {filter.field}
                  </span>
                  <select
                    value={filter.op}
                    onChange={(e) => {
                      setFilters((prev) =>
                        prev.map((f, i) =>
                          i === index ? { ...f, op: e.target.value as FilterOperator } : f
                        )
                      )
                    }}
                    className="h-7 px-2 text-xs border border-gray-200 rounded-md bg-white"
                  >
                    <option value="eq">equals</option>
                    <option value="neq">not equals</option>
                    <option value="gt">greater than</option>
                    <option value="gte">greater or equal</option>
                    <option value="lt">less than</option>
                    <option value="lte">less or equal</option>
                    <option value="contains">contains</option>
                    <option value="icontains">contains (ignore case)</option>
                    <option value="is_null">is null</option>
                  </select>
                  {filter.op !== 'is_null' && (
                    <input
                      type="text"
                      value={filter.value as string || ''}
                      onChange={(e) => {
                        setFilters((prev) =>
                          prev.map((f, i) =>
                            i === index ? { ...f, value: e.target.value } : f
                          )
                        )
                      }}
                      placeholder="Value"
                      className="flex-1 h-7 px-2 text-xs border border-gray-200 rounded-md bg-white"
                    />
                  )}
                  <Button
                    variant="ghost"
                    size="sm"
                    className="h-7 w-7 p-0"
                    onClick={() => {
                      setFilters((prev) => prev.filter((_, i) => i !== index))
                      setPage(0)
                    }}
                  >
                    <X className="h-3 w-3" />
                  </Button>
                </div>
              ))}
              <Button
                size="sm"
                className="mt-2"
                onClick={() => {
                  setPage(0)
                  refetch()
                }}
              >
                Apply Filters
              </Button>
            </div>
          )}
        </div>
      )}

      {/* Table */}
      <div className="flex-1 overflow-auto">
        {isLoading && rows.length === 0 ? (
          <div className="flex items-center justify-center h-full">
            <Loader2 className="h-8 w-8 text-gray-400 animate-spin" />
          </div>
        ) : rows.length === 0 ? (
          <div className="flex flex-col items-center justify-center h-full py-12">
            <div className="h-12 w-12 rounded-lg bg-gray-100 flex items-center justify-center mb-3">
              <Search className="h-6 w-6 text-gray-400" />
            </div>
            <p className="text-sm font-medium text-gray-900 mb-1">No data yet</p>
            <p className="text-xs text-gray-500 text-center mb-4">
              Add your first row to get started
            </p>
            <Button
              size="sm"
              onClick={() => {
                setEditingRow(null)
                setRowFormOpen(true)
              }}
            >
              <Plus className="h-3.5 w-3.5 mr-1.5" />
              Add Row
            </Button>
          </div>
        ) : (
          <table className="w-full text-sm">
            <thead className="sticky top-0 bg-gray-50 border-b border-gray-200 z-10">
              <tr>
                <th className="w-10 px-3 py-2.5 text-left">
                  <input
                    type="checkbox"
                    checked={selectedRows.size === rows.length && rows.length > 0}
                    onChange={handleSelectAll}
                    className="rounded border-gray-300 text-gray-900 focus:ring-gray-900"
                  />
                </th>
                {columns.map((col) => (
                  <th
                    key={col.name}
                    className="px-3 py-2.5 text-left text-xs font-medium text-gray-500 uppercase tracking-wider cursor-pointer hover:bg-gray-100 transition-colors"
                    onClick={() => handleSort(col.name)}
                  >
                    <div className="flex items-center gap-1.5">
                      <span>{col.name}</span>
                      <span className="text-[10px] font-normal text-gray-400 normal-case">
                        {col.type}
                      </span>
                      {sortColumn === col.name ? (
                        sortDir === 'asc' ? (
                          <ArrowUp className="h-3 w-3 text-gray-700" />
                        ) : (
                          <ArrowDown className="h-3 w-3 text-gray-700" />
                        )
                      ) : (
                        <ArrowUpDown className="h-3 w-3 text-gray-300" />
                      )}
                    </div>
                  </th>
                ))}
                <th className="w-12 px-3 py-2.5"></th>
              </tr>
            </thead>
            <tbody className="divide-y divide-gray-100 bg-white">
              {rows.map((row) => (
                <tr
                  key={row.id}
                  className={cn(
                    'hover:bg-gray-50 transition-colors',
                    selectedRows.has(row.id) && 'bg-blue-50 hover:bg-blue-50'
                  )}
                >
                  <td className="px-3 py-2">
                    <input
                      type="checkbox"
                      checked={selectedRows.has(row.id)}
                      onChange={() => handleSelectRow(row.id)}
                      className="rounded border-gray-300 text-gray-900 focus:ring-gray-900"
                    />
                  </td>
                  {columns.map((col) => (
                    <td
                      key={col.name}
                      className="px-3 py-2 text-gray-900 max-w-xs truncate"
                      title={formatCellValue(row.data[col.name], col)}
                    >
                      {col.primary_key ? (
                        <code className="text-xs bg-gray-100 px-1.5 py-0.5 rounded text-gray-600">
                          {formatCellValue(row.data[col.name], col).slice(0, 8)}...
                        </code>
                      ) : col.type === 'boolean' ? (
                        <span
                          className={cn(
                            'text-xs px-1.5 py-0.5 rounded',
                            row.data[col.name]
                              ? 'bg-green-50 text-green-700'
                              : 'bg-gray-100 text-gray-600'
                          )}
                        >
                          {formatCellValue(row.data[col.name], col)}
                        </span>
                      ) : col.type === 'enum' ? (
                        <span className="text-xs px-1.5 py-0.5 rounded bg-blue-50 text-blue-700">
                          {formatCellValue(row.data[col.name], col)}
                        </span>
                      ) : (
                        formatCellValue(row.data[col.name], col)
                      )}
                    </td>
                  ))}
                  <td className="px-3 py-2">
                    <DropdownMenu>
                      <DropdownMenuTrigger asChild>
                        <button className="p-1 rounded-md text-gray-400 hover:text-gray-600 hover:bg-gray-100 transition-colors">
                          <MoreHorizontal className="h-4 w-4" />
                        </button>
                      </DropdownMenuTrigger>
                      <DropdownMenuContent align="end" sideOffset={4}>
                        <DropdownMenuItem
                          className="gap-2"
                          onSelect={() => {
                            setEditingRow(row)
                            setRowFormOpen(true)
                          }}
                        >
                          <PencilLine className="h-4 w-4 text-gray-500" />
                          Edit
                        </DropdownMenuItem>
                        <DropdownMenuItem
                          className="gap-2"
                          onSelect={() => {
                            navigator.clipboard.writeText(row.id)
                          }}
                        >
                          <Copy className="h-4 w-4 text-gray-500" />
                          Copy ID
                        </DropdownMenuItem>
                        <DropdownMenuSeparator />
                        <DropdownMenuItem
                          className="gap-2 text-red-600 focus:bg-red-50"
                          onSelect={() => handleDeleteRow(row)}
                        >
                          <Trash2 className="h-4 w-4" />
                          Delete
                        </DropdownMenuItem>
                      </DropdownMenuContent>
                    </DropdownMenu>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        )}
      </div>

      {/* Pagination */}
      {totalPages > 1 && (
        <div className="flex items-center justify-between px-4 py-3 border-t border-gray-200 bg-white">
          <div className="text-xs text-gray-500">
            Showing {page * PAGE_SIZE + 1} - {Math.min((page + 1) * PAGE_SIZE, totalCount)} of{' '}
            {totalCount}
          </div>
          <div className="flex items-center gap-1">
            <Button
              variant="outline"
              size="sm"
              disabled={page === 0}
              onClick={() => setPage((p) => Math.max(0, p - 1))}
            >
              <ChevronLeft className="h-4 w-4" />
            </Button>
            <span className="px-3 text-sm text-gray-700">
              {page + 1} / {totalPages}
            </span>
            <Button
              variant="outline"
              size="sm"
              disabled={page >= totalPages - 1}
              onClick={() => setPage((p) => p + 1)}
            >
              <ChevronRight className="h-4 w-4" />
            </Button>
          </div>
        </div>
      )}

      {/* Row Form Dialog */}
      <RowFormDialog
        open={rowFormOpen}
        onOpenChange={setRowFormOpen}
        appId={appId}
        table={table}
        editingRow={editingRow}
        onSuccess={() => {
          setRowFormOpen(false)
          setEditingRow(null)
        }}
      />

      {/* Schema Viewer Dialog */}
      <SchemaViewer
        open={schemaViewerOpen}
        onOpenChange={setSchemaViewerOpen}
        appId={appId}
        table={table}
      />
    </div>
  )
}

