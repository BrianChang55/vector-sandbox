/**
 * Schema Viewer Component
 * 
 * Displays the schema of a data table with column details.
 * Supports viewing and editing schema properties.
 */
import { useState } from 'react'
import {
  Table2,
  Key,
  Hash,
  Type,
  Calendar,
  ToggleLeft,
  List,
  Braces,
  Link,
  PencilLine,
  Save,
  AlertCircle,
} from 'lucide-react'
import { useUpdateTable } from '@/hooks/useDataStore'
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
import { cn } from '@/lib/utils'
import type { DataTable, ColumnType } from '@/types/dataStore'

interface SchemaViewerProps {
  open: boolean
  onOpenChange: (open: boolean) => void
  appId: string
  table: DataTable
}

const TYPE_ICONS: Record<ColumnType, React.ComponentType<{ className?: string }>> = {
  uuid: Key,
  string: Type,
  text: Type,
  integer: Hash,
  float: Hash,
  boolean: ToggleLeft,
  datetime: Calendar,
  date: Calendar,
  enum: List,
  json: Braces,
  reference: Link,
}

const TYPE_COLORS: Record<ColumnType, string> = {
  uuid: 'text-purple-600 bg-purple-50',
  string: 'text-blue-600 bg-blue-50',
  text: 'text-blue-600 bg-blue-50',
  integer: 'text-green-600 bg-green-50',
  float: 'text-green-600 bg-green-50',
  boolean: 'text-orange-600 bg-orange-50',
  datetime: 'text-pink-600 bg-pink-50',
  date: 'text-pink-600 bg-pink-50',
  enum: 'text-cyan-600 bg-cyan-50',
  json: 'text-indigo-600 bg-indigo-50',
  reference: 'text-amber-600 bg-amber-50',
}

export function SchemaViewer({
  open,
  onOpenChange,
  appId,
  table,
}: SchemaViewerProps) {
  const updateTable = useUpdateTable()
  const [isEditing, setIsEditing] = useState(false)
  const [editedName, setEditedName] = useState(table.name)
  const [editedDescription, setEditedDescription] = useState(table.description)
  const [saving, setSaving] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const columns = table.schema_json.columns || []

  const handleSave = async () => {
    if (!editedName.trim()) {
      setError('Table name is required')
      return
    }

    setSaving(true)
    setError(null)
    try {
      await updateTable.mutateAsync({
        appId,
        tableSlug: table.slug,
        data: {
          name: editedName.trim(),
          description: editedDescription.trim(),
        },
      })
      setIsEditing(false)
    } catch (err: any) {
      setError(err?.message || 'Failed to update table')
    } finally {
      setSaving(false)
    }
  }

  const handleOpenChange = (newOpen: boolean) => {
    if (!newOpen) {
      setIsEditing(false)
      setEditedName(table.name)
      setEditedDescription(table.description)
      setError(null)
    }
    onOpenChange(newOpen)
  }

  return (
    <Dialog open={open} onOpenChange={handleOpenChange}>
      <DialogContent showCloseButton className="max-w-2xl max-h-[85vh] overflow-hidden flex flex-col">
        <DialogHeader>
          <div className="flex items-center justify-between">
            <div className="flex items-center gap-3">
              <div className="h-10 w-10 rounded-lg bg-gray-100 flex items-center justify-center">
                <Table2 className="h-5 w-5 text-gray-500" />
              </div>
              <div>
                {isEditing ? (
                  <input
                    type="text"
                    value={editedName}
                    onChange={(e) => setEditedName(e.target.value)}
                    className="text-lg font-semibold text-gray-900 bg-transparent border-b border-gray-300 focus:outline-none focus:border-gray-900 pb-0.5"
                    autoFocus
                  />
                ) : (
                  <DialogTitle className="text-lg">{table.name}</DialogTitle>
                )}
                {isEditing ? (
                  <input
                    type="text"
                    value={editedDescription}
                    onChange={(e) => setEditedDescription(e.target.value)}
                    placeholder="Add a description..."
                    className="text-sm text-gray-500 bg-transparent border-b border-gray-200 focus:outline-none focus:border-gray-400 w-full mt-1"
                  />
                ) : (
                  <DialogDescription>
                    {table.description || 'No description'}
                  </DialogDescription>
                )}
              </div>
            </div>
            {!isEditing && (
              <Button
                variant="ghost"
                size="sm"
                onClick={() => setIsEditing(true)}
              >
                <PencilLine className="h-4 w-4 mr-1.5" />
                Edit
              </Button>
            )}
          </div>
        </DialogHeader>

        <DialogBody className="flex-1 overflow-y-auto">
          {/* Table Info */}
          <div className="flex items-center gap-4 mb-6 text-sm text-gray-500">
            <span className="flex items-center gap-1.5">
              <Hash className="h-4 w-4" />
              {table.row_count} rows
            </span>
            <span className="flex items-center gap-1.5">
              <Table2 className="h-4 w-4" />
              {columns.length} columns
            </span>
            <span>
              Slug: <code className="bg-gray-100 px-1.5 py-0.5 rounded text-xs">{table.slug}</code>
            </span>
          </div>

          {/* Columns */}
          <div className="space-y-2">
            <h3 className="text-sm font-medium text-gray-900 mb-3">Columns</h3>
            
            <div className="bg-gray-50 rounded-lg border border-gray-200 divide-y divide-gray-200">
              {columns.map((col) => {
                const IconComponent = TYPE_ICONS[col.type] || Type
                const colorClasses = TYPE_COLORS[col.type] || 'text-gray-600 bg-gray-50'
                
                return (
                  <div
                    key={col.name}
                    className="flex items-center gap-4 px-4 py-3"
                  >
                    <div className={cn('p-1.5 rounded', colorClasses)}>
                      <IconComponent className="h-4 w-4" />
                    </div>
                    
                    <div className="flex-1 min-w-0">
                      <div className="flex items-center gap-2">
                        <span className="font-medium text-gray-900">
                          {col.name}
                        </span>
                        {col.primary_key && (
                          <span className="text-[10px] px-1.5 py-0.5 rounded bg-purple-100 text-purple-700 font-medium">
                            PRIMARY KEY
                          </span>
                        )}
                        {col.unique && !col.primary_key && (
                          <span className="text-[10px] px-1.5 py-0.5 rounded bg-blue-100 text-blue-700 font-medium">
                            UNIQUE
                          </span>
                        )}
                        {col.auto_generate && (
                          <span className="text-[10px] px-1.5 py-0.5 rounded bg-gray-100 text-gray-600 font-medium">
                            AUTO
                          </span>
                        )}
                      </div>
                      <div className="flex items-center gap-3 text-xs text-gray-500 mt-0.5">
                        <span>{col.type}</span>
                        {col.nullable && <span>nullable</span>}
                        {col.default !== undefined && (
                          <span>default: {JSON.stringify(col.default)}</span>
                        )}
                        {col.enum_values && col.enum_values.length > 0 && (
                          <span>values: {col.enum_values.join(', ')}</span>
                        )}
                        {col.max_length && <span>max: {col.max_length}</span>}
                      </div>
                    </div>
                  </div>
                )
              })}
            </div>
          </div>

          {/* Error */}
          {error && (
            <div className="flex items-start gap-2 p-3 mt-4 bg-red-50 border border-red-200 rounded-lg text-sm text-red-700">
              <AlertCircle className="h-4 w-4 flex-shrink-0 mt-0.5" />
              {error}
            </div>
          )}
        </DialogBody>

        <DialogFooter>
          {isEditing ? (
            <>
              <Button
                variant="outline"
                onClick={() => {
                  setIsEditing(false)
                  setEditedName(table.name)
                  setEditedDescription(table.description)
                  setError(null)
                }}
                disabled={saving}
                disableFocusRing
              >
                Cancel
              </Button>
              <Button onClick={handleSave} disabled={saving}>
                <Save className="h-4 w-4 mr-1.5" />
                {saving ? 'Saving...' : 'Save Changes'}
              </Button>
            </>
          ) : (
            <Button variant="outline" onClick={() => handleOpenChange(false)}>
              Close
            </Button>
          )}
        </DialogFooter>
      </DialogContent>
    </Dialog>
  )
}

