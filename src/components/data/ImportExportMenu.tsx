/**
 * Export Menu Component
 * 
 * Provides CSV export functionality for table data.
 */
import { useState } from 'react'
import { Download, Loader2 } from 'lucide-react'
import { Button } from '@/components/ui/button'
import type { DataTable } from '@/types/dataStore'

interface ImportExportMenuProps {
  appId: string
  table: DataTable
}

export function ImportExportMenu({ appId, table }: ImportExportMenuProps) {
  const [exporting, setExporting] = useState(false)

  const columns = table.schema_json.columns || []

  // Export as CSV
  const handleExport = async () => {
    setExporting(true)
    try {
      // Fetch all rows with pagination (API limit is 1000 per request)
      const allRows: any[] = []
      let offset = 0
      const batchSize = 1000
      let hasMore = true

      while (hasMore) {
        const response = await fetch(
          `${import.meta.env.VITE_API_BASE_URL || 'http://localhost:8001/api/v1'}/apps/${appId}/data/tables/${table.slug}/query/`,
          {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json',
              Authorization: `Bearer ${localStorage.getItem('access_token')}`,
            },
            body: JSON.stringify({ limit: batchSize, offset }),
          }
        )
        
        if (!response.ok) {
          throw new Error('Failed to fetch data')
        }

        const result = await response.json()
        const batchRows = result.rows || []
        allRows.push(...batchRows)
        
        // Check if there are more rows to fetch
        if (batchRows.length < batchSize) {
          hasMore = false
        } else {
          offset += batchSize
        }
      }

      const headers = columns.map((c) => c.name)
      
      // Build CSV content
      const csvRows: string[] = []
      
      // Header row
      csvRows.push(headers.map(escapeCsvField).join(','))
      
      // Data rows
      allRows.forEach((row: any) => {
        const rowValues = headers.map((h) => {
          const value = row.data[h]
          return escapeCsvField(formatCellValue(value))
        })
        csvRows.push(rowValues.join(','))
      })
      
      const csvContent = csvRows.join('\n')
      
      // Download as .csv (with BOM for Excel UTF-8 compatibility)
      const blob = new Blob(['\uFEFF' + csvContent], { type: 'text/csv;charset=utf-8;' })
      const url = URL.createObjectURL(blob)
      const link = document.createElement('a')
      link.href = url
      link.download = `${table.slug}_export.csv`
      document.body.appendChild(link)
      link.click()
      document.body.removeChild(link)
      URL.revokeObjectURL(url)
    } catch (err: any) {
      console.error('CSV export failed:', err)
    } finally {
      setExporting(false)
    }
  }

  return (
    <Button
      variant="outline"
      size="sm"
      onClick={handleExport}
      disabled={exporting}
    >
      {exporting ? (
        <Loader2 className="h-3.5 w-3.5 mr-1.5 animate-spin" />
      ) : (
        <Download className="h-3.5 w-3.5 mr-1.5" />
      )}
      Export
    </Button>
  )
}

// Helper to escape CSV field values
// Handles commas, quotes, and newlines in data
function escapeCsvField(value: string): string {
  // If the field contains comma, quote, or newline, wrap in quotes and escape internal quotes
  if (value.includes(',') || value.includes('"') || value.includes('\n') || value.includes('\r')) {
    return `"${value.replace(/"/g, '""')}"`
  }
  return value
}

// Format cell value for display
function formatCellValue(value: unknown): string {
  if (value === null || value === undefined) return ''
  if (typeof value === 'object') return JSON.stringify(value)
  return String(value)
}
