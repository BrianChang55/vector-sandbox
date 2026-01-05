/**
 * Data Panel
 * 
 * Main container for viewing and managing app data tables.
 * Provides a master-detail layout with table list and data grid.
 */
import { useState } from 'react'
import {
  Database,
  Plus,
  Table2,
  Loader2,
  FolderOpen,
} from 'lucide-react'
import { useTables } from '@/hooks/useDataStore'
import { Button } from '@/components/ui/button'
import { TableList } from './TableList'
import { DataGrid } from './DataGrid'
import { CreateTableDialog } from './CreateTableDialog'
import type { DataTable } from '@/types/dataStore'

interface DataPanelProps {
  appId: string
  className?: string
}

export function DataPanel({ appId, className = '' }: DataPanelProps) {
  const { data: tables, isLoading } = useTables(appId)
  const [selectedTable, setSelectedTable] = useState<DataTable | null>(null)
  const [createDialogOpen, setCreateDialogOpen] = useState(false)

  // Auto-select first table
  if (tables && tables.length > 0 && !selectedTable) {
    setSelectedTable(tables[0])
  }

  // Update selected table reference when tables refresh
  if (selectedTable && tables) {
    const updated = tables.find((t) => t.id === selectedTable.id)
    if (updated && updated !== selectedTable) {
      setSelectedTable(updated)
    }
  }

  if (isLoading) {
    return (
      <div className={`flex items-center justify-center h-full bg-gray-50 ${className}`}>
        <Loader2 className="h-8 w-8 text-gray-400 animate-spin" />
      </div>
    )
  }

  return (
    <div className={`flex h-full bg-gray-50 ${className}`}>
      {/* Left Panel - Table List */}
      <div className="w-64 flex-shrink-0 border-r border-gray-200 bg-white flex flex-col">
        <div className="p-3 border-b border-gray-200 flex items-center justify-between">
          <div className="flex items-center gap-2">
            <Database className="h-4 w-4 text-gray-500" />
            <h2 className="font-medium text-sm text-gray-900">Tables</h2>
          </div>
          <Button
            size="sm"
            variant="ghost"
            className="h-7 w-7 p-0"
            onClick={() => setCreateDialogOpen(true)}
          >
            <Plus className="h-4 w-4" />
          </Button>
        </div>

        <div className="flex-1 overflow-y-auto">
          {tables && tables.length > 0 ? (
            <TableList
              tables={tables}
              selectedTable={selectedTable}
              onSelectTable={setSelectedTable}
              appId={appId}
            />
          ) : (
            <div className="flex flex-col items-center justify-center py-12 px-4">
              <div className="h-12 w-12 rounded-lg bg-gray-100 flex items-center justify-center mb-3">
                <Table2 className="h-6 w-6 text-gray-400" />
              </div>
              <p className="text-sm font-medium text-gray-900 mb-1">No tables</p>
              <p className="text-xs text-gray-500 text-center mb-4">
                Create your first data table
              </p>
              <Button size="sm" onClick={() => setCreateDialogOpen(true)}>
                <Plus className="h-3.5 w-3.5 mr-1.5" />
                New Table
              </Button>
            </div>
          )}
        </div>
      </div>

      {/* Right Panel - Data Grid */}
      <div className="flex-1 min-w-0 flex flex-col">
        {selectedTable ? (
          <DataGrid appId={appId} table={selectedTable} />
        ) : (
          <div className="flex flex-col items-center justify-center h-full">
            <div className="h-16 w-16 rounded-xl bg-gray-100 flex items-center justify-center mb-4">
              <FolderOpen className="h-8 w-8 text-gray-400" />
            </div>
            <h3 className="text-lg font-medium text-gray-900 mb-1">
              Select a table
            </h3>
            <p className="text-sm text-gray-500 mb-6 max-w-sm text-center">
              Choose a table from the left panel to view and manage its data.
            </p>
            {(!tables || tables.length === 0) && (
              <Button onClick={() => setCreateDialogOpen(true)}>
                <Plus className="h-4 w-4 mr-2" />
                Create Your First Table
              </Button>
            )}
          </div>
        )}
      </div>

      {/* Create Table Dialog */}
      <CreateTableDialog
        open={createDialogOpen}
        onOpenChange={setCreateDialogOpen}
        appId={appId}
        onTableCreated={(table) => {
          setSelectedTable(table)
          setCreateDialogOpen(false)
        }}
      />
    </div>
  )
}

