/**
 * Row Form Drawer
 * 
 * Drawer for creating or editing a data row.
 * Slides in from the right side.
 */
import { useState, useEffect, useMemo } from 'react'
import { AlertCircle } from 'lucide-react'
import { useInsertRow, useUpdateRow } from '@/hooks/useDataStore'
import { Button } from '@/components/ui/button'
import {
  Drawer,
  DrawerBody,
  DrawerContent,
  DrawerDescription,
  DrawerFooter,
  DrawerHeader,
  DrawerTitle,
} from '@/components/ui/drawer'
import type { DataTable, QueryResultRow, ColumnDef } from '@/types/dataStore'

interface RowFormDrawerProps {
  open: boolean
  onOpenChange: (open: boolean) => void
  appId: string
  table: DataTable
  editingRow: QueryResultRow | null
  onSuccess?: () => void
}

// Input styling: no outline focus borders, slightly less rounded, clean
const inputClasses = 
  'w-full h-9 px-3 rounded text-sm border border-gray-200 bg-white text-gray-900 placeholder:text-gray-400 focus:border-gray-400 focus:outline-none disabled:bg-gray-50 disabled:text-gray-500 transition-colors'

const selectClasses = 
  'w-full h-9 px-3 rounded text-sm border border-gray-200 bg-white text-gray-900 focus:border-gray-400 focus:outline-none disabled:bg-gray-50 disabled:text-gray-500 transition-colors'

const textareaClasses = 
  'w-full px-3 py-2 rounded text-sm border border-gray-200 bg-white text-gray-900 placeholder:text-gray-400 focus:border-gray-400 focus:outline-none disabled:bg-gray-50 disabled:text-gray-500 transition-colors resize-none'

export function RowFormDrawer({
  open,
  onOpenChange,
  appId,
  table,
  editingRow,
  onSuccess,
}: RowFormDrawerProps) {
  const insertRow = useInsertRow()
  const updateRow = useUpdateRow()
  
  const [formData, setFormData] = useState<Record<string, string>>({})
  const [error, setError] = useState<string | null>(null)
  const [saving, setSaving] = useState(false)

  // Memoize columns to prevent infinite re-renders
  const columns = useMemo(() => {
    const allColumns = table.schema_json.columns || []
    // Filter out auto-generated timestamp fields from the form
    return allColumns.filter(
      (col) => !['created_at', 'updated_at', 'createdAt', 'updatedAt'].includes(col.name)
    )
  }, [table.schema_json.columns])
  
  const isEditing = !!editingRow

  // Initialize form data
  useEffect(() => {
    if (open) {
      const initial: Record<string, string> = {}
      columns.forEach((col) => {
        if (editingRow) {
          const value = editingRow.data[col.name]
          if (value === null || value === undefined) {
            initial[col.name] = ''
          } else if (typeof value === 'object') {
            initial[col.name] = JSON.stringify(value)
          } else {
            initial[col.name] = String(value)
          }
        } else {
          initial[col.name] = ''
        }
      })
      setFormData(initial)
      setError(null)
    }
  }, [open, editingRow, columns])

  const handleOpenChange = (newOpen: boolean) => {
    if (!newOpen && !saving) {
      setFormData({})
      setError(null)
    }
    onOpenChange(newOpen)
  }

  const parseValue = (value: string, column: ColumnDef): unknown => {
    if (value === '' || value === null || value === undefined) {
      return null
    }

    switch (column.type) {
      case 'integer':
        const intVal = parseInt(value, 10)
        if (isNaN(intVal)) throw new Error(`Invalid integer value for ${column.name}`)
        return intVal
      case 'float':
        const floatVal = parseFloat(value)
        if (isNaN(floatVal)) throw new Error(`Invalid number value for ${column.name}`)
        return floatVal
      case 'boolean':
        return value.toLowerCase() === 'true' || value === '1'
      case 'json':
        try {
          return JSON.parse(value)
        } catch {
          throw new Error(`Invalid JSON value for ${column.name}`)
        }
      default:
        return value
    }
  }

  const handleSubmit = async () => {
    setError(null)

    // Parse and validate values
    const data: Record<string, unknown> = {}
    
    for (const col of columns) {
      // Skip auto-generated columns on create
      if (!isEditing && (col.auto_generate || col.auto_now_add)) {
        continue
      }
      
      // Skip auto_now columns always
      if (col.auto_now) {
        continue
      }

      const rawValue = formData[col.name]?.trim() || ''

      // Check required fields
      if (!col.nullable && !col.default && !col.auto_generate && !rawValue) {
        if (!isEditing || rawValue === '') {
          setError(`${col.name} is required`)
          return
        }
      }

      // Skip empty optional fields on create
      if (!isEditing && !rawValue && col.nullable) {
        continue
      }

      // Parse value
      try {
        const parsedValue = parseValue(rawValue, col)
        if (parsedValue !== null || isEditing) {
          data[col.name] = parsedValue
        }
      } catch (err: any) {
        setError(err.message)
        return
      }
    }

    setSaving(true)
    try {
      if (isEditing) {
        await updateRow.mutateAsync({
          appId,
          tableSlug: table.slug,
          rowId: editingRow.id,
          data: { data },
        })
      } else {
        await insertRow.mutateAsync({
          appId,
          tableSlug: table.slug,
          data: { data },
        })
      }
      onSuccess?.()
    } catch (err: any) {
      const message = 
        err?.response?.data?.errors?.[0] || 
        err?.response?.data?.error ||
        err?.message || 
        'Failed to save row'
      setError(message)
    } finally {
      setSaving(false)
    }
  }

  const renderInput = (col: ColumnDef) => {
    const value = formData[col.name] || ''
    const isAutoGenerated = col.auto_generate || col.auto_now_add || col.auto_now
    const isDisabled = saving || (isAutoGenerated && !isEditing) || col.primary_key

    switch (col.type) {
      case 'boolean':
        return (
          <select
            value={value}
            onChange={(e) => setFormData((prev) => ({ ...prev, [col.name]: e.target.value }))}
            disabled={isDisabled}
            className={selectClasses}
          >
            <option value="">Select...</option>
            <option value="true">true</option>
            <option value="false">false</option>
          </select>
        )

      case 'enum':
        return (
          <select
            value={value}
            onChange={(e) => setFormData((prev) => ({ ...prev, [col.name]: e.target.value }))}
            disabled={isDisabled}
            className={selectClasses}
          >
            <option value="">Select...</option>
            {col.enum_values?.map((v) => (
              <option key={v} value={v}>
                {v}
              </option>
            ))}
          </select>
        )

      case 'text':
      case 'json':
        return (
          <textarea
            value={value}
            onChange={(e) => setFormData((prev) => ({ ...prev, [col.name]: e.target.value }))}
            disabled={isDisabled}
            rows={3}
            placeholder={col.type === 'json' ? '{"key": "value"}' : ''}
            className={textareaClasses}
          />
        )

      case 'datetime':
        return (
          <input
            type="datetime-local"
            value={value ? value.slice(0, 16) : ''}
            onChange={(e) => {
              const val = e.target.value ? new Date(e.target.value).toISOString() : ''
              setFormData((prev) => ({ ...prev, [col.name]: val }))
            }}
            disabled={isDisabled}
            className={inputClasses}
          />
        )

      case 'date':
        return (
          <input
            type="date"
            value={value ? value.slice(0, 10) : ''}
            onChange={(e) => setFormData((prev) => ({ ...prev, [col.name]: e.target.value }))}
            disabled={isDisabled}
            className={inputClasses}
          />
        )

      case 'integer':
        return (
          <input
            type="number"
            value={value}
            onChange={(e) => setFormData((prev) => ({ ...prev, [col.name]: e.target.value }))}
            disabled={isDisabled}
            step="1"
            className={inputClasses}
          />
        )

      case 'float':
        return (
          <input
            type="number"
            value={value}
            onChange={(e) => setFormData((prev) => ({ ...prev, [col.name]: e.target.value }))}
            disabled={isDisabled}
            step="any"
            className={inputClasses}
          />
        )

      default:
        return (
          <input
            type="text"
            value={value}
            onChange={(e) => setFormData((prev) => ({ ...prev, [col.name]: e.target.value }))}
            disabled={isDisabled}
            placeholder={col.primary_key ? '(auto-generated)' : ''}
            className={inputClasses}
          />
        )
    }
  }

  return (
    <Drawer open={open} onOpenChange={handleOpenChange}>
      <DrawerContent side="right" className="max-w-md">
        <DrawerHeader>
          <DrawerTitle>{isEditing ? 'Edit Row' : 'Add Row'}</DrawerTitle>
          <DrawerDescription>
            {isEditing
              ? 'Update the values for this row.'
              : `Add a new row to ${table.name}.`}
          </DrawerDescription>
        </DrawerHeader>

        <DrawerBody>
          <div className="space-y-5">
            {columns.map((col) => (
              <div key={col.name}>
                <div className="flex items-baseline justify-between mb-1.5">
                  <label className="text-sm font-medium text-gray-700">
                    {col.name}
                  </label>
                  <span className="text-[11px] text-gray-400">
                    {col.type}
                    {col.primary_key && ' · pk'}
                    {col.auto_generate && ' · auto'}
                    {!col.nullable && !col.primary_key && ' · required'}
                  </span>
                </div>
                {renderInput(col)}
              </div>
            ))}

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
            {saving ? 'Saving...' : isEditing ? 'Save Changes' : 'Add Row'}
          </Button>
        </DrawerFooter>
      </DrawerContent>
    </Drawer>
  )
}

