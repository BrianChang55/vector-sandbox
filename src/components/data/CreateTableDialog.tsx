/**
 * Create Table Dialog
 * 
 * Dialog for creating a new data table with schema definition.
 */
import { useState } from 'react'
import {
  Plus,
  Trash2,
  AlertCircle,
} from 'lucide-react'
import { useCreateTable } from '@/hooks/useDataStore'
import { Button } from '@/components/ui/button'
import {
  Dialog,
  DialogBody,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import type { DataTable, ColumnDef, ColumnType } from '@/types/dataStore'

interface CreateTableDialogProps {
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

export function CreateTableDialog({
  open,
  onOpenChange,
  appId,
  onTableCreated,
}: CreateTableDialogProps) {
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
    <Dialog open={open} onOpenChange={handleOpenChange}>
      <DialogContent showCloseButton className="max-w-2xl max-h-[85vh] overflow-hidden flex flex-col">
        <DialogHeader>
          <DialogTitle>Create New Table</DialogTitle>
          <DialogDescription>
            Define the schema for your new data table.
          </DialogDescription>
        </DialogHeader>

        <DialogBody className="flex-1 overflow-y-auto">
          <div className="space-y-6">
            {/* Table name and description */}
            <div className="grid grid-cols-2 gap-4">
              <div>
                <label className="block text-sm font-medium text-gray-900 mb-1">
                  Table name *
                </label>
                <input
                  type="text"
                  value={tableName}
                  onChange={(e) => setTableName(e.target.value)}
                  placeholder="e.g., customers"
                  className="w-full h-9 px-3 rounded-md text-sm border border-gray-200 bg-white text-gray-900 placeholder:text-gray-400 focus:outline-none focus:ring-2 focus:ring-gray-900 focus:border-transparent"
                />
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-900 mb-1">
                  Description
                </label>
                <input
                  type="text"
                  value={description}
                  onChange={(e) => setDescription(e.target.value)}
                  placeholder="Optional description"
                  className="w-full h-9 px-3 rounded-md text-sm border border-gray-200 bg-white text-gray-900 placeholder:text-gray-400 focus:outline-none focus:ring-2 focus:ring-gray-900 focus:border-transparent"
                />
              </div>
            </div>

            {/* Columns */}
            <div>
              <div className="flex items-center justify-between mb-3">
                <label className="block text-sm font-medium text-gray-900">
                  Columns
                </label>
                <Button variant="outline" size="sm" onClick={addColumn}>
                  <Plus className="h-3.5 w-3.5 mr-1.5" />
                  Add Column
                </Button>
              </div>

              <div className="space-y-2">
                {columns.map((col) => (
                  <div
                    key={col.id}
                    className="flex items-start gap-2 p-3 bg-gray-50 rounded-lg border border-gray-200"
                  >
                    <div className="flex-1 grid grid-cols-4 gap-2">
                      <div>
                        <input
                          type="text"
                          value={col.name}
                          onChange={(e) => updateColumn(col.id, { name: e.target.value })}
                          placeholder="Column name"
                          className="w-full h-8 px-2 rounded-md text-sm border border-gray-200 bg-white text-gray-900 placeholder:text-gray-400 focus:outline-none focus:ring-2 focus:ring-gray-900"
                        />
                      </div>
                      <div>
                        <select
                          value={col.type}
                          onChange={(e) => updateColumn(col.id, { type: e.target.value as ColumnType })}
                          className="w-full h-8 px-2 rounded-md text-sm border border-gray-200 bg-white text-gray-900 focus:outline-none focus:ring-2 focus:ring-gray-900"
                        >
                          {COLUMN_TYPES.map((t) => (
                            <option key={t.value} value={t.value}>
                              {t.label}
                            </option>
                          ))}
                        </select>
                      </div>
                      <div className="flex items-center gap-3">
                        <label className="flex items-center gap-1.5 text-xs text-gray-600">
                          <input
                            type="checkbox"
                            checked={col.nullable}
                            onChange={(e) => updateColumn(col.id, { nullable: e.target.checked })}
                            className="rounded border-gray-300 text-gray-900 focus:ring-gray-900"
                          />
                          Nullable
                        </label>
                        <label className="flex items-center gap-1.5 text-xs text-gray-600">
                          <input
                            type="checkbox"
                            checked={col.unique}
                            onChange={(e) => updateColumn(col.id, { unique: e.target.checked })}
                            className="rounded border-gray-300 text-gray-900 focus:ring-gray-900"
                          />
                          Unique
                        </label>
                      </div>
                      <div>
                        {col.type === 'enum' ? (
                          <input
                            type="text"
                            value={col.enum_values}
                            onChange={(e) => updateColumn(col.id, { enum_values: e.target.value })}
                            placeholder="a, b, c"
                            className="w-full h-8 px-2 rounded-md text-sm border border-gray-200 bg-white text-gray-900 placeholder:text-gray-400 focus:outline-none focus:ring-2 focus:ring-gray-900"
                          />
                        ) : (
                          <input
                            type="text"
                            value={col.default_value}
                            onChange={(e) => updateColumn(col.id, { default_value: e.target.value })}
                            placeholder="Default"
                            className="w-full h-8 px-2 rounded-md text-sm border border-gray-200 bg-white text-gray-900 placeholder:text-gray-400 focus:outline-none focus:ring-2 focus:ring-gray-900"
                          />
                        )}
                      </div>
                    </div>
                    <button
                      type="button"
                      onClick={() => removeColumn(col.id)}
                      disabled={columns.length <= 1}
                      className="p-1.5 rounded-md text-gray-400 hover:text-red-600 hover:bg-red-50 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
                    >
                      <Trash2 className="h-4 w-4" />
                    </button>
                  </div>
                ))}
              </div>

              <p className="mt-2 text-xs text-gray-500">
                A UUID primary key column named "id" will be auto-generated if not specified.
              </p>
            </div>

            {/* Error */}
            {error && (
              <div className="flex items-start gap-2 p-3 bg-red-50 border border-red-200 rounded-lg text-sm text-red-700">
                <AlertCircle className="h-4 w-4 flex-shrink-0 mt-0.5" />
                {error}
              </div>
            )}
          </div>
        </DialogBody>

        <DialogFooter>
          <Button
            variant="outline"
            onClick={() => handleOpenChange(false)}
            disabled={saving}
            disableFocusRing
          >
            Cancel
          </Button>
          <Button onClick={handleSubmit} disabled={saving}>
            {saving ? 'Creating...' : 'Create Table'}
          </Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  )
}

