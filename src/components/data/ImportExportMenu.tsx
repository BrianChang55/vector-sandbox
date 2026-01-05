/**
 * Import/Export Menu Component
 * 
 * Provides CSV import and export functionality for table data.
 */
import { useState, useRef } from 'react'
import {
  Download,
  Upload,
  FileText,
  Loader2,
  AlertCircle,
  CheckCircle,
} from 'lucide-react'
import { useBulkInsertRows } from '@/hooks/useDataStore'
import { Button } from '@/components/ui/button'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu'
import {
  Dialog,
  DialogBody,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import type { DataTable, ColumnDef } from '@/types/dataStore'

interface ImportExportMenuProps {
  appId: string
  table: DataTable
}

export function ImportExportMenu({ appId, table }: ImportExportMenuProps) {
  const fileInputRef = useRef<HTMLInputElement>(null)
  const bulkInsert = useBulkInsertRows()
  
  const [importDialogOpen, setImportDialogOpen] = useState(false)
  const [importData, setImportData] = useState<Record<string, unknown>[]>([])
  const [importError, setImportError] = useState<string | null>(null)
  const [importing, setImporting] = useState(false)
  const [importResult, setImportResult] = useState<{
    success: number
    errors: string[]
  } | null>(null)
  const [exporting, setExporting] = useState(false)

  const columns = table.schema_json.columns || []

  const parseCSV = (text: string): Record<string, unknown>[] => {
    const lines = text.split('\n').filter((l) => l.trim())
    if (lines.length < 2) {
      throw new Error('CSV must have a header row and at least one data row')
    }

    const headers = lines[0].split(',').map((h) => h.trim().replace(/^"|"$/g, ''))
    const rows: Record<string, unknown>[] = []

    for (let i = 1; i < lines.length; i++) {
      const values = lines[i].split(',').map((v) => v.trim().replace(/^"|"$/g, ''))
      const row: Record<string, unknown> = {}
      
      headers.forEach((header, index) => {
        const column = columns.find((c) => c.name === header)
        if (column) {
          const value = values[index] || ''
          row[header] = parseValue(value, column)
        }
      })
      
      rows.push(row)
    }

    return rows
  }

  const parseValue = (value: string, column: ColumnDef): unknown => {
    if (value === '' || value === 'null') return null
    
    switch (column.type) {
      case 'integer':
        const intVal = parseInt(value, 10)
        return isNaN(intVal) ? null : intVal
      case 'float':
        const floatVal = parseFloat(value)
        return isNaN(floatVal) ? null : floatVal
      case 'boolean':
        return value.toLowerCase() === 'true' || value === '1'
      case 'json':
        try {
          return JSON.parse(value)
        } catch {
          return null
        }
      default:
        return value
    }
  }

  const handleFileSelect = (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0]
    if (!file) return

    setImportError(null)
    setImportResult(null)

    const reader = new FileReader()
    reader.onload = (event) => {
      try {
        const text = event.target?.result as string
        const data = parseCSV(text)
        setImportData(data)
        setImportDialogOpen(true)
      } catch (err: any) {
        setImportError(err.message || 'Failed to parse CSV file')
        setImportDialogOpen(true)
      }
    }
    reader.onerror = () => {
      setImportError('Failed to read file')
      setImportDialogOpen(true)
    }
    reader.readAsText(file)

    // Reset file input
    if (fileInputRef.current) {
      fileInputRef.current.value = ''
    }
  }

  const handleImport = async () => {
    if (importData.length === 0) return

    setImporting(true)
    setImportError(null)
    try {
      const result = await bulkInsert.mutateAsync({
        appId,
        tableSlug: table.slug,
        data: { rows: importData },
      })
      setImportResult({
        success: result.created_count,
        errors: result.errors || [],
      })
      setImportData([])
    } catch (err: any) {
      setImportError(err?.response?.data?.error || err?.message || 'Import failed')
    } finally {
      setImporting(false)
    }
  }

  const handleExport = async () => {
    setExporting(true)
    try {
      // Fetch all rows (up to 10000)
      const response = await fetch(
        `${import.meta.env.VITE_API_BASE_URL || 'http://localhost:8001/api/v1'}/apps/${appId}/data/tables/${table.slug}/query/`,
        {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: `Bearer ${localStorage.getItem('access_token')}`,
          },
          body: JSON.stringify({ limit: 10000 }),
        }
      )
      
      if (!response.ok) {
        throw new Error('Failed to fetch data')
      }

      const result = await response.json()
      const rows = result.rows || []

      // Generate CSV
      const headers = columns.map((c) => c.name)
      const csvLines = [headers.join(',')]

      rows.forEach((row: any) => {
        const values = headers.map((h) => {
          const value = row.data[h]
          if (value === null || value === undefined) return ''
          if (typeof value === 'object') return `"${JSON.stringify(value).replace(/"/g, '""')}"`
          if (typeof value === 'string' && (value.includes(',') || value.includes('"'))) {
            return `"${value.replace(/"/g, '""')}"`
          }
          return String(value)
        })
        csvLines.push(values.join(','))
      })

      // Download
      const blob = new Blob([csvLines.join('\n')], { type: 'text/csv;charset=utf-8;' })
      const url = URL.createObjectURL(blob)
      const link = document.createElement('a')
      link.href = url
      link.download = `${table.slug}_export.csv`
      document.body.appendChild(link)
      link.click()
      document.body.removeChild(link)
      URL.revokeObjectURL(url)
    } catch (err: any) {
      console.error('Export failed:', err)
    } finally {
      setExporting(false)
    }
  }

  const handleCloseImportDialog = () => {
    setImportDialogOpen(false)
    setImportData([])
    setImportError(null)
    setImportResult(null)
  }

  return (
    <>
      <input
        ref={fileInputRef}
        type="file"
        accept=".csv"
        onChange={handleFileSelect}
        className="hidden"
      />

      <DropdownMenu>
        <DropdownMenuTrigger asChild>
          <Button variant="outline" size="sm">
            <FileText className="h-3.5 w-3.5 mr-1.5" />
            Import / Export
          </Button>
        </DropdownMenuTrigger>
        <DropdownMenuContent align="end" sideOffset={4}>
          <DropdownMenuItem
            className="gap-2"
            onSelect={() => fileInputRef.current?.click()}
          >
            <Upload className="h-4 w-4 text-gray-500" />
            Import from CSV
          </DropdownMenuItem>
          <DropdownMenuSeparator />
          <DropdownMenuItem
            className="gap-2"
            onSelect={handleExport}
            disabled={exporting}
          >
            {exporting ? (
              <Loader2 className="h-4 w-4 animate-spin" />
            ) : (
              <Download className="h-4 w-4 text-gray-500" />
            )}
            Export to CSV
          </DropdownMenuItem>
        </DropdownMenuContent>
      </DropdownMenu>

      {/* Import Dialog */}
      <Dialog open={importDialogOpen} onOpenChange={handleCloseImportDialog}>
        <DialogContent showCloseButton className="max-w-lg">
          <DialogHeader>
            <DialogTitle>Import CSV Data</DialogTitle>
            <DialogDescription>
              {importResult
                ? 'Import completed'
                : importError
                  ? 'Import error'
                  : `Preview of ${importData.length} rows to import into ${table.name}`}
            </DialogDescription>
          </DialogHeader>

          <DialogBody>
            {importResult ? (
              <div className="space-y-4">
                <div className="flex items-center gap-3 p-4 bg-green-50 border border-green-200 rounded-lg">
                  <CheckCircle className="h-5 w-5 text-green-600" />
                  <div>
                    <p className="font-medium text-green-800">
                      Successfully imported {importResult.success} rows
                    </p>
                    {importResult.errors.length > 0 && (
                      <p className="text-sm text-green-600 mt-1">
                        {importResult.errors.length} rows had errors
                      </p>
                    )}
                  </div>
                </div>

                {importResult.errors.length > 0 && (
                  <div className="space-y-2">
                    <p className="text-sm font-medium text-gray-900">Errors:</p>
                    <div className="max-h-32 overflow-y-auto bg-gray-50 rounded-lg p-3 text-xs text-red-600 space-y-1">
                      {importResult.errors.map((err, i) => (
                        <div key={i}>{err}</div>
                      ))}
                    </div>
                  </div>
                )}
              </div>
            ) : importError ? (
              <div className="flex items-start gap-2 p-4 bg-red-50 border border-red-200 rounded-lg text-sm text-red-700">
                <AlertCircle className="h-5 w-5 flex-shrink-0" />
                <div>{importError}</div>
              </div>
            ) : (
              <div className="space-y-4">
                <div className="bg-gray-50 rounded-lg border border-gray-200 overflow-hidden">
                  <div className="max-h-64 overflow-auto">
                    <table className="w-full text-xs">
                      <thead className="bg-gray-100 sticky top-0">
                        <tr>
                          <th className="px-2 py-1.5 text-left font-medium text-gray-500">
                            #
                          </th>
                          {columns
                            .filter((c) => !c.auto_generate && !c.primary_key)
                            .slice(0, 4)
                            .map((col) => (
                              <th
                                key={col.name}
                                className="px-2 py-1.5 text-left font-medium text-gray-500"
                              >
                                {col.name}
                              </th>
                            ))}
                          {columns.filter((c) => !c.auto_generate && !c.primary_key).length > 4 && (
                            <th className="px-2 py-1.5 text-left font-medium text-gray-400">
                              ...
                            </th>
                          )}
                        </tr>
                      </thead>
                      <tbody className="divide-y divide-gray-100">
                        {importData.slice(0, 10).map((row, index) => (
                          <tr key={index}>
                            <td className="px-2 py-1.5 text-gray-400">{index + 1}</td>
                            {columns
                              .filter((c) => !c.auto_generate && !c.primary_key)
                              .slice(0, 4)
                              .map((col) => (
                                <td
                                  key={col.name}
                                  className="px-2 py-1.5 text-gray-700 max-w-[120px] truncate"
                                >
                                  {row[col.name] === null
                                    ? '—'
                                    : String(row[col.name])}
                                </td>
                              ))}
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                </div>

                {importData.length > 10 && (
                  <p className="text-xs text-gray-500 text-center">
                    Showing first 10 of {importData.length} rows
                  </p>
                )}
              </div>
            )}
          </DialogBody>

          <DialogFooter>
            {importResult ? (
              <Button onClick={handleCloseImportDialog}>Done</Button>
            ) : (
              <>
                <Button
                  variant="outline"
                  onClick={handleCloseImportDialog}
                  disabled={importing}
                  disableFocusRing
                >
                  Cancel
                </Button>
                {!importError && importData.length > 0 && (
                  <Button onClick={handleImport} disabled={importing}>
                    {importing ? (
                      <>
                        <Loader2 className="h-4 w-4 mr-1.5 animate-spin" />
                        Importing...
                      </>
                    ) : (
                      <>
                        <Upload className="h-4 w-4 mr-1.5" />
                        Import {importData.length} Rows
                      </>
                    )}
                  </Button>
                )}
              </>
            )}
          </DialogFooter>
        </DialogContent>
      </Dialog>
    </>
  )
}

