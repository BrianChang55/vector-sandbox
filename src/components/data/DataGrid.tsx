/**
 * Data Grid Component
 * 
 * Displays table data in a grid format with pagination,
 * sorting, filtering, and row operations.
 */
import { useState, useMemo, useEffect, useCallback } from 'react'
import {
  Plus,
  Trash2,
  RefreshCw,
  Search,
  ChevronLeft,
  ChevronRight,
  ChevronDown,
  ArrowUpDown,
  ArrowUp,
  ArrowDown,
  Loader2,
  MoreHorizontal,
  PencilLine,
  Filter,
  X,
  Calendar,
  Hash,
  Type,
  ToggleLeft,
  List,
  Braces,
  Check,
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
import { RowFormDrawer } from './RowFormDrawer'
import { ImportExportMenu } from './ImportExportMenu'
import { cn } from '@/lib/utils'
import type { DataTable, QuerySpec, QueryResultRow, ColumnDef, FilterDef, FilterOperator } from '@/types/dataStore'

// Type-specific operator configurations
type OperatorConfig = {
  value: FilterOperator
  label: string
  needsValue: boolean
}

const getOperatorsForType = (type: string): OperatorConfig[] => {
  switch (type) {
    case 'string':
    case 'text':
      return [
        { value: 'icontains', label: 'contains', needsValue: true },
        { value: 'eq', label: 'equals', needsValue: true },
        { value: 'neq', label: 'not equals', needsValue: true },
        { value: 'is_null', label: 'is empty', needsValue: false },
      ]
    case 'integer':
    case 'float':
      return [
        { value: 'eq', label: 'equals', needsValue: true },
        { value: 'neq', label: 'not equals', needsValue: true },
        { value: 'gt', label: 'greater than', needsValue: true },
        { value: 'gte', label: 'at least', needsValue: true },
        { value: 'lt', label: 'less than', needsValue: true },
        { value: 'lte', label: 'at most', needsValue: true },
        { value: 'is_null', label: 'is empty', needsValue: false },
      ]
    case 'boolean':
      return [
        { value: 'eq', label: 'is true', needsValue: false },
        { value: 'neq', label: 'is false', needsValue: false },
        { value: 'is_null', label: 'is empty', needsValue: false },
      ]
    case 'date':
    case 'datetime':
      return [
        { value: 'eq', label: 'is', needsValue: true },
        { value: 'gt', label: 'after', needsValue: true },
        { value: 'lt', label: 'before', needsValue: true },
        { value: 'gte', label: 'on or after', needsValue: true },
        { value: 'lte', label: 'on or before', needsValue: true },
        { value: 'is_null', label: 'is empty', needsValue: false },
      ]
    case 'enum':
      return [
        { value: 'eq', label: 'is', needsValue: true },
        { value: 'neq', label: 'is not', needsValue: true },
        { value: 'is_null', label: 'is empty', needsValue: false },
      ]
    case 'uuid':
      return [
        { value: 'eq', label: 'equals', needsValue: true },
        { value: 'is_null', label: 'is empty', needsValue: false },
      ]
    case 'json':
      return [
        { value: 'is_null', label: 'is empty', needsValue: false },
      ]
    default:
      return [
        { value: 'eq', label: 'equals', needsValue: true },
        { value: 'neq', label: 'not equals', needsValue: true },
        { value: 'is_null', label: 'is empty', needsValue: false },
      ]
  }
}

const getDefaultOperator = (type: string): FilterOperator => {
  switch (type) {
    case 'string':
    case 'text':
      return 'icontains'
    case 'boolean':
      return 'eq'
    default:
      return 'eq'
  }
}

const getTypeIcon = (type: string) => {
  switch (type) {
    case 'string':
    case 'text':
      return Type
    case 'integer':
    case 'float':
      return Hash
    case 'boolean':
      return ToggleLeft
    case 'date':
    case 'datetime':
      return Calendar
    case 'enum':
      return List
    case 'json':
      return Braces
    default:
      return Type
  }
}

// Extended filter with column info for UI
interface FilterWithColumn extends FilterDef {
  column: ColumnDef
}

// Filter row component for inline editing
function FilterRow({
  filter,
  onUpdate,
  onRemove,
}: {
  filter: FilterWithColumn
  onUpdate: (updates: Partial<FilterDef>) => void
  onRemove: () => void
}) {
  const operators = getOperatorsForType(filter.column.type)
  const currentOp = operators.find(o => o.value === filter.op)
  const needsValue = currentOp?.needsValue !== false
  const TypeIcon = getTypeIcon(filter.column.type)

  // Handle boolean value - set based on operator
  useEffect(() => {
    if (filter.column.type === 'boolean') {
      if (filter.op === 'eq') {
        onUpdate({ value: true })
      } else if (filter.op === 'neq') {
        onUpdate({ value: false })
      }
    }
  }, [filter.op, filter.column.type])

  const renderValueInput = () => {
    if (!needsValue) return null

    const { type, enum_values } = filter.column

    const selectClass = "h-8 px-4 text-sm border border-gray-200 rounded bg-white text-gray-900 focus:outline-none"
    const inputClass = "h-8 px-4 text-sm border border-gray-200 rounded bg-white text-gray-900 placeholder:text-gray-400 focus:outline-none"

    // Enum dropdown
    if (type === 'enum' && enum_values && enum_values.length > 0) {
      return (
        <select
          value={filter.value as string || ''}
          onChange={(e) => onUpdate({ value: e.target.value })}
          className={selectClass}
        >
          <option value="">Select value...</option>
          {enum_values.map((val) => (
            <option key={val} value={val}>{val}</option>
          ))}
        </select>
      )
    }

    // Date picker
    if (type === 'date') {
      return (
        <input
          type="date"
          value={filter.value as string || ''}
          onChange={(e) => onUpdate({ value: e.target.value })}
          className={inputClass}
        />
      )
    }

    // Datetime picker
    if (type === 'datetime') {
      return (
        <input
          type="datetime-local"
          value={filter.value as string || ''}
          onChange={(e) => onUpdate({ value: e.target.value })}
          className={inputClass}
        />
      )
    }

    // Number input
    if (type === 'integer' || type === 'float') {
      return (
        <input
          type="number"
          step={type === 'float' ? 'any' : '1'}
          value={filter.value as string || ''}
          onChange={(e) => onUpdate({ value: e.target.value ? Number(e.target.value) : '' })}
          placeholder="Enter number..."
          className={`${inputClass} min-w-[120px]`}
        />
      )
    }

    // Text input (default)
    return (
      <input
        type="text"
        value={filter.value as string || ''}
        onChange={(e) => onUpdate({ value: e.target.value })}
        placeholder="Enter value..."
        className={`${inputClass} min-w-[140px] flex-1`}
      />
    )
  }

  return (
    <div className="flex items-center gap-4 bg-white rounded-lg border border-gray-200 px-4 py-2.5 shadow-sm">
      {/* Field name with icon */}
      <div className="flex items-center gap-2 px-3 py-1.5 bg-gray-50 rounded border border-gray-100">
        <TypeIcon className="h-3.5 w-3.5 text-gray-400" />
        <span className="text-sm font-medium text-gray-700">{filter.field}</span>
      </div>

      {/* Operator dropdown */}
      <DropdownMenu>
        <DropdownMenuTrigger asChild>
          <button className="h-8 pl-3 pr-2 text-sm border border-gray-200 rounded bg-white text-gray-700 
                             hover:bg-gray-50 transition-colors flex items-center gap-1.5 min-w-[110px] justify-between">
            <span>{currentOp?.label || filter.op}</span>
            <ChevronDown className="h-4 w-4 text-gray-400" />
          </button>
        </DropdownMenuTrigger>
        <DropdownMenuContent align="start" sideOffset={4} className="min-w-[140px]">
          {operators.map((op) => (
            <DropdownMenuItem
              key={op.value}
              onSelect={() => onUpdate({ op: op.value })}
              className="flex items-center justify-between gap-2"
            >
              <span>{op.label}</span>
              {filter.op === op.value && (
                <Check className="h-4 w-4 text-gray-900" />
              )}
            </DropdownMenuItem>
          ))}
        </DropdownMenuContent>
      </DropdownMenu>

      {/* Value input */}
      <div className="flex-1">
        {renderValueInput()}
      </div>

      {/* Remove button - always at far right */}
      <button
        onClick={onRemove}
        className="p-1.5 text-gray-400 hover:text-gray-600 hover:bg-gray-100 rounded transition-colors flex-shrink-0"
      >
        <X className="h-4 w-4" />
      </button>
    </div>
  )
}

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
  const [showFilters, setShowFilters] = useState(false)
  const [filtersWithColumns, setFiltersWithColumns] = useState<FilterWithColumn[]>([])
  
  const deleteRow = useDeleteRow()
  const bulkDelete = useBulkDeleteRows()
  const { confirm } = useDialog()

  const columns = table.schema_json.columns || []

  // Convert filters with columns to plain filters for the API
  const filters: FilterDef[] = useMemo(() => {
    return filtersWithColumns
      .filter(f => {
        const operators = getOperatorsForType(f.column.type)
        const currentOp = operators.find(o => o.value === f.op)
        // Include filter if it doesn't need a value, or if it has a value
        return currentOp?.needsValue === false || (f.value !== '' && f.value !== null && f.value !== undefined)
      })
      .map(({ field, op, value }) => ({ field, op, value }))
  }, [filtersWithColumns])

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

  // Add a filter for a column
  const addFilter = useCallback((column: ColumnDef) => {
    const defaultOp = getDefaultOperator(column.type)
    const defaultValue = column.type === 'boolean' ? true : ''
    setFiltersWithColumns(prev => [...prev, { 
      field: column.name, 
      op: defaultOp, 
      value: defaultValue,
      column 
    }])
    setShowFilters(true)
  }, [])

  // Update a filter
  const updateFilter = useCallback((index: number, updates: Partial<FilterDef>) => {
    setFiltersWithColumns(prev => prev.map((f, i) => 
      i === index ? { ...f, ...updates } : f
    ))
    setPage(0)
  }, [])

  // Remove a filter
  const removeFilter = useCallback((index: number) => {
    setFiltersWithColumns(prev => prev.filter((_, i) => i !== index))
    setPage(0)
  }, [])

  // Clear all filters
  const clearFilters = useCallback(() => {
    setFiltersWithColumns([])
    setPage(0)
  }, [])

  const { data: queryResult, isLoading, refetch } = useQueryRows(
    appId,
    table.slug,
    querySpec
  )

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
            variant={showFilters || filtersWithColumns.length > 0 ? 'secondary' : 'outline'}
            size="sm"
            onClick={() => setShowFilters(!showFilters)}
          >
            <Filter className="h-3.5 w-3.5 mr-1.5" />
            Filter
            {filtersWithColumns.length > 0 && (
              <span className="ml-1.5 bg-gray-900 text-white text-[10px] rounded-full h-4 w-4 flex items-center justify-center">
                {filtersWithColumns.length}
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
          {/* Filter header */}
          <div className="flex items-center justify-between mb-3">
            <div className="flex items-center gap-2">
              <Filter className="h-4 w-4 text-gray-500" />
              <span className="text-sm font-medium text-gray-700">Filters</span>
              {filters.length > 0 && (
                <span className="text-xs text-gray-500">
                  ({filters.length} active)
                </span>
              )}
            </div>
            <div className="flex items-center gap-2">
              {filtersWithColumns.length > 0 && (
                <Button
                  variant="ghost"
                  size="sm"
                  className="h-7 text-xs text-red-600 hover:text-red-700 hover:bg-red-50"
                  onClick={clearFilters}
                >
                  <X className="h-3 w-3 mr-1" />
                  Clear all
                </Button>
              )}
            </div>
          </div>

          {/* Active filters */}
          <div className="space-y-2">
            {filtersWithColumns.map((filter, index) => (
              <FilterRow
                key={`${filter.field}-${index}`}
                filter={filter}
                onUpdate={(updates) => updateFilter(index, updates)}
                onRemove={() => removeFilter(index)}
              />
            ))}
          </div>

          {/* Add filter button */}
          <div className="mt-3">
            <DropdownMenu>
              <DropdownMenuTrigger asChild>
                <Button variant="outline" size="sm" className="h-8">
                  <Plus className="h-3.5 w-3.5 mr-1.5" />
                  Add filter
                </Button>
              </DropdownMenuTrigger>
              <DropdownMenuContent align="start" sideOffset={4} className="w-56">
                <div className="px-2 py-1.5 text-xs font-medium text-gray-500 uppercase tracking-wider">
                  Filter by column
                </div>
                <DropdownMenuSeparator />
                {columns.filter(col => !col.primary_key).map((col) => {
                  const TypeIcon = getTypeIcon(col.type)
                  return (
                    <DropdownMenuItem
                      key={col.name}
                      onSelect={() => addFilter(col)}
                      className="gap-2"
                    >
                      <TypeIcon className="h-4 w-4 text-gray-400" />
                      <span className="flex-1 text-gray-900">{col.name}</span>
                      <span className="text-xs text-gray-400">{col.type}</span>
                    </DropdownMenuItem>
                  )
                })}
              </DropdownMenuContent>
            </DropdownMenu>
          </div>
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
            {filters.length > 0 ? (
              // Empty state when filters are applied but no results match
              <>
                <div className="h-12 w-12 rounded-lg bg-gray-100 flex items-center justify-center mb-3">
                  <Filter className="h-6 w-6 text-gray-400" />
                </div>
                <p className="text-sm font-medium text-gray-900 mb-1">No matching results</p>
                <p className="text-xs text-gray-500 text-center mb-4">
                  Try adjusting your filters to find what you're looking for
                </p>
                <Button
                  variant="outline"
                  size="sm"
                  onClick={clearFilters}
                >
                  <X className="h-3.5 w-3.5 mr-1.5" />
                  Clear filters
                </Button>
              </>
            ) : (
              // Empty state when table has no data
              <>
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
              </>
            )}
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
                    className="h-4 w-4 rounded border-gray-300 text-gray-900 focus:ring-0 focus:ring-offset-0"
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
                  onClick={() => {
                    setEditingRow(row)
                    setRowFormOpen(true)
                  }}
                  className={cn(
                    'hover:bg-gray-50 transition-colors cursor-pointer',
                    selectedRows.has(row.id) && 'bg-blue-50 hover:bg-blue-50'
                  )}
                >
                  <td className="px-3 py-2" onClick={(e) => e.stopPropagation()}>
                    <input
                      type="checkbox"
                      checked={selectedRows.has(row.id)}
                      onChange={() => handleSelectRow(row.id)}
                      className="h-4 w-4 rounded border-gray-300 text-gray-900 focus:ring-0 focus:ring-offset-0"
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
                  <td className="px-3 py-2" onClick={(e) => e.stopPropagation()}>
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

      {/* Row Form Drawer */}
      <RowFormDrawer
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
    </div>
  )
}

