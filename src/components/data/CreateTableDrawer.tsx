/**
 * Create Table Drawer
 * 
 * Drawer for creating a new data table with schema definition.
 * Slides in from the left side.
 */
import { useState } from 'react'
import { Plus, Trash2, AlertCircle, ChevronDown, Check } from 'lucide-react'
import { useCreateTable } from '@/hooks/useDataStore'
import { Button } from '@/components/ui/button'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu'
import {
  Drawer,
  DrawerBody,
  DrawerContent,
  DrawerDescription,
  DrawerFooter,
  DrawerHeader,
  DrawerTitle,
} from '@/components/ui/drawer'
import type { DataTable, ColumnDef, ColumnType } from '@/types/dataStore'

interface CreateTableDrawerProps {
  open: boolean
  onOpenChange: (open: boolean) => void
  appId: string
  onTableCreated?: (table: DataTable) => void
}

const COLUMN_TYPES: { value: ColumnType; label: string }[] = [
  { value: 'string', label: 'String' },
  { value: 'text', label: 'Text' },
  { value: 'integer', label: 'Integer' },
  { value: 'float', label: 'Float' },
  { value: 'boolean', label: 'Boolean' },
  { value: 'datetime', label: 'DateTime' },
  { value: 'date', label: 'Date' },
  { value: 'uuid', label: 'UUID' },
  { value: 'enum', label: 'Enum' },
  { value: 'json', label: 'JSON' },
]

interface ColumnFormData {
  id: string
  name: string
  type: ColumnType
  nullable: boolean
  unique: boolean
  default_value: string
  enum_values: string
}

const DEFAULT_COLUMNS: ColumnFormData[] = [
  {
    id: 'col_id',
    name: 'id',
    type: 'uuid',
    nullable: false,
    unique: false,
    default_value: '',
    enum_values: '',
  },
]

// Input styling: no outline focus borders, slightly less rounded, clean
const inputClasses = 
  'w-full h-9 px-3 rounded text-sm border border-gray-200 bg-white text-gray-900 placeholder:text-gray-400 focus:border-gray-400 focus:outline-none disabled:bg-gray-50 disabled:text-gray-500 transition-colors'

const inputSmClasses = 
  'w-full h-8 px-2.5 rounded text-sm border border-gray-200 bg-white text-gray-900 placeholder:text-gray-400 focus:border-gray-400 focus:outline-none disabled:bg-gray-50 disabled:text-gray-500 transition-colors'

export function CreateTableDrawer({
  open,
  onOpenChange,
  appId,
  onTableCreated,
}: CreateTableDrawerProps) {
  const createTable = useCreateTable()
  
  const [tableName, setTableName] = useState('')
  const [description, setDescription] = useState('')
  const [columns, setColumns] = useState<ColumnFormData[]>(DEFAULT_COLUMNS)
  const [error, setError] = useState<string | null>(null)
  const [saving, setSaving] = useState(false)

  const resetForm = () => {
    setTableName('')
    setDescription('')
    setColumns(DEFAULT_COLUMNS)
    setError(null)
  }

  const handleOpenChange = (newOpen: boolean) => {
    if (!newOpen && !saving) {
      resetForm()
    }
    onOpenChange(newOpen)
  }

  const addColumn = () => {
    setColumns((prev) => [
      ...prev,
      {
        id: `col_${Date.now()}`,
        name: '',
        type: 'string',
        nullable: true,
        unique: false,
        default_value: '',
        enum_values: '',
      },
    ])
  }

  const removeColumn = (id: string) => {
    if (columns.length <= 1) return
    setColumns((prev) => prev.filter((c) => c.id !== id))
  }

  const updateColumn = (id: string, updates: Partial<ColumnFormData>) => {
    setColumns((prev) =>
      prev.map((c) => (c.id === id ? { ...c, ...updates } : c))
    )
  }

  const handleSubmit = async () => {
    setError(null)

    // Validate
    if (!tableName.trim()) {
      setError('Table name is required')
      return
    }

    const columnDefs: ColumnDef[] = []
    let hasPrimaryKey = false

    for (const col of columns) {
      if (!col.name.trim()) {
        setError('All columns must have a name')
        return
      }

      if (!/^[a-zA-Z][a-zA-Z0-9_]*$/.test(col.name)) {
        setError(`Column "${col.name}" must start with a letter and contain only letters, numbers, and underscores`)
        return
      }

      const def: ColumnDef = {
        name: col.name.trim(),
        type: col.type,
        nullable: col.nullable,
      }

      // First UUID column is primary key
      if (col.type === 'uuid' && !hasPrimaryKey && col.name === 'id') {
        def.primary_key = true
        def.auto_generate = true
        def.nullable = false
        hasPrimaryKey = true
      }

      if (col.unique) {
        def.unique = true
      }

      if (col.type === 'enum' && col.enum_values.trim()) {
        def.enum_values = col.enum_values.split(',').map((v) => v.trim()).filter(Boolean)
        if (def.enum_values.length === 0) {
          setError(`Enum column "${col.name}" must have at least one value`)
          return
        }
      }

      if (col.default_value.trim()) {
        try {
          if (col.type === 'integer') {
            def.default = parseInt(col.default_value, 10)
          } else if (col.type === 'float') {
            def.default = parseFloat(col.default_value)
          } else if (col.type === 'boolean') {
            def.default = col.default_value.toLowerCase() === 'true'
          } else {
            def.default = col.default_value
          }
        } catch {
          setError(`Invalid default value for column "${col.name}"`)
          return
        }
      }

      columnDefs.push(def)
    }

    // Ensure primary key exists
    if (!hasPrimaryKey) {
      // Add auto-generated id column at the beginning
      columnDefs.unshift({
        name: 'id',
        type: 'uuid',
        primary_key: true,
        auto_generate: true,
        nullable: false,
      })
    }

    setSaving(true)
    try {
      const table = await createTable.mutateAsync({
        appId,
        data: {
          name: tableName.trim(),
          description: description.trim(),
          schema_json: { columns: columnDefs },
        },
      })
      resetForm()
      onTableCreated?.(table)
    } catch (err: any) {
      const message = err?.response?.data?.errors?.[0] || err?.message || 'Failed to create table'
      setError(message)
    } finally {
      setSaving(false)
    }
  }

  return (
    <Drawer open={open} onOpenChange={handleOpenChange}>
      <DrawerContent side="left" className="max-w-lg">
        <DrawerHeader>
          <DrawerTitle>Create Table</DrawerTitle>
          <DrawerDescription>
            Define the schema for your new data table.
          </DrawerDescription>
        </DrawerHeader>

        <DrawerBody>
          <div className="space-y-6">
            {/* Table name and description */}
            <div className="space-y-4">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1.5">
                  Table name
                </label>
                <input
                  type="text"
                  value={tableName}
                  onChange={(e) => setTableName(e.target.value)}
                  placeholder="e.g., customers"
                  className={inputClasses}
                />
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1.5">
                  Description
                  <span className="font-normal text-gray-400 ml-1">(optional)</span>
                </label>
                <input
                  type="text"
                  value={description}
                  onChange={(e) => setDescription(e.target.value)}
                  placeholder="What data does this table store?"
                  className={inputClasses}
                />
              </div>
            </div>

            {/* Columns */}
            <div>
              <div className="flex items-center justify-between mb-3">
                <label className="block text-sm font-medium text-gray-700">
                  Columns
                </label>
                <button
                  type="button"
                  onClick={addColumn}
                  className="flex items-center gap-1 text-xs font-medium text-gray-600 hover:text-gray-900 transition-colors"
                >
                  <Plus className="h-3.5 w-3.5" />
                  Add column
                </button>
              </div>

              <div className="space-y-3">
                {columns.map((col, index) => (
                  <div
                    key={col.id}
                    className="p-3 bg-gray-50 rounded border border-gray-100"
                  >
                    <div className="flex items-start gap-3">
                      <span className="flex-shrink-0 w-5 h-5 flex items-center justify-center text-[10px] font-medium text-gray-400 bg-white border border-gray-200 rounded mt-1">
                        {index + 1}
                      </span>
                      
                      <div className="flex-1 space-y-2.5">
                        {/* Row 1: Name and Type */}
                        <div className="grid grid-cols-2 gap-2">
                          <input
                            type="text"
                            value={col.name}
                            onChange={(e) => updateColumn(col.id, { name: e.target.value })}
                            placeholder="Column name"
                            className={inputSmClasses}
                          />
                          <DropdownMenu>
                            <DropdownMenuTrigger asChild>
                              <button
                                type="button"
                                className="w-full h-8 px-2.5 rounded text-sm border border-gray-200 bg-white text-gray-900 hover:border-gray-300 focus:border-gray-400 focus:outline-none transition-colors flex items-center justify-between"
                              >
                                <span>{COLUMN_TYPES.find(t => t.value === col.type)?.label || col.type}</span>
                                <ChevronDown className="h-4 w-4 text-gray-400" />
                              </button>
                            </DropdownMenuTrigger>
                            <DropdownMenuContent align="start" sideOffset={4} className="min-w-[140px]">
                              {COLUMN_TYPES.map((t) => (
                                <DropdownMenuItem
                                  key={t.value}
                                  onSelect={() => updateColumn(col.id, { type: t.value })}
                                  className="flex items-center justify-between gap-2"
                                >
                                  <span>{t.label}</span>
                                  {col.type === t.value && (
                                    <Check className="h-4 w-4 text-gray-900" />
                                  )}
                                </DropdownMenuItem>
                              ))}
                            </DropdownMenuContent>
                          </DropdownMenu>
                        </div>

                        {/* Row 2: Options */}
                        <div className="flex items-center gap-4">
                          <label className="flex items-center gap-1.5 text-xs text-gray-600 cursor-pointer">
                            <input
                              type="checkbox"
                              checked={col.nullable}
                              onChange={(e) => updateColumn(col.id, { nullable: e.target.checked })}
                              className="h-3.5 w-3.5 rounded border-gray-300 text-gray-900 focus:ring-0 focus:ring-offset-0"
                            />
                            Nullable
                          </label>
                          <label className="flex items-center gap-1.5 text-xs text-gray-600 cursor-pointer">
                            <input
                              type="checkbox"
                              checked={col.unique}
                              onChange={(e) => updateColumn(col.id, { unique: e.target.checked })}
                              className="h-3.5 w-3.5 rounded border-gray-300 text-gray-900 focus:ring-0 focus:ring-offset-0"
                            />
                            Unique
                          </label>
                        </div>

                        {/* Row 3: Enum values or Default */}
                        {col.type === 'enum' ? (
                          <input
                            type="text"
                            value={col.enum_values}
                            onChange={(e) => updateColumn(col.id, { enum_values: e.target.value })}
                            placeholder="Values (comma separated): active, pending, archived"
                            className={inputSmClasses}
                          />
                        ) : (
                          <input
                            type="text"
                            value={col.default_value}
                            onChange={(e) => updateColumn(col.id, { default_value: e.target.value })}
                            placeholder="Default value (optional)"
                            className={inputSmClasses}
                          />
                        )}
                      </div>

                      <button
                        type="button"
                        onClick={() => removeColumn(col.id)}
                        disabled={columns.length <= 1}
                        className="flex-shrink-0 p-1 rounded text-gray-400 hover:text-red-600 hover:bg-red-50 disabled:opacity-30 disabled:cursor-not-allowed transition-colors mt-1"
                      >
                        <Trash2 className="h-3.5 w-3.5" />
                      </button>
                    </div>
                  </div>
                ))}
              </div>

              <p className="mt-3 text-xs text-gray-500">
                A UUID primary key column named "id" will be auto-generated if not specified.
              </p>
            </div>

            {/* Error */}
            {error && (
              <div className="flex items-start gap-2 p-3 bg-red-50 border border-red-100 rounded text-sm text-red-700">
                <AlertCircle className="h-4 w-4 flex-shrink-0 mt-0.5" />
                <span>{error}</span>
              </div>
            )}
          </div>
        </DrawerBody>

        <DrawerFooter>
          <Button
            variant="ghost"
            onClick={() => handleOpenChange(false)}
            disabled={saving}
          >
            Cancel
          </Button>
          <Button onClick={handleSubmit} disabled={saving}>
            {saving ? 'Creating...' : 'Create Table'}
          </Button>
        </DrawerFooter>
      </DrawerContent>
    </Drawer>
  )
}

