/**
 * CRM Dashboard Page
 *
 * Two-view page for managing customer groups:
 * - Management view: CRUD operations for tracked groups
 * - Dashboard view: Aggregated insights and visualizations
 */
import { useState } from 'react'
import { useAppSelector } from '../store/hooks'
import {
  useCustomerGroups,
  useCustomerGroupStats,
  useCreateCustomerGroup,
  useUpdateCustomerGroup,
  useDeleteCustomerGroup,
} from '../hooks/useCrm'
import type { CustomerGroup, HealthCategory, CustomerGroupInput } from '../types/models'
import { cn } from '../lib/utils'
import {
  LayoutDashboard,
  List,
  Plus,
  Pencil,
  Trash2,
  X,
  DollarSign,
  Users,
  TrendingUp,
  AlertTriangle,
} from 'lucide-react'
import { Button } from '../components/ui/button'

type ViewMode = 'management' | 'dashboard'

const HEALTH_OPTIONS: { value: HealthCategory; label: string; color: string }[] = [
  { value: 'prospect', label: 'Prospect', color: 'bg-blue-100 text-blue-700' },
  { value: 'good', label: 'Good', color: 'bg-green-100 text-green-700' },
  { value: 'average', label: 'Average', color: 'bg-yellow-100 text-yellow-700' },
  { value: 'at_risk', label: 'At Risk', color: 'bg-red-100 text-red-700' },
]

const HEALTH_COLORS: Record<HealthCategory, string> = {
  prospect: '#3b82f6',
  good: '#22c55e',
  average: '#eab308',
  at_risk: '#ef4444',
}

function formatCurrency(value: string | number): string {
  const num = typeof value === 'string' ? parseFloat(value) : value
  return new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD',
    minimumFractionDigits: 0,
    maximumFractionDigits: 0,
  }).format(num)
}

function getHealthBadgeClass(health: HealthCategory): string {
  const option = HEALTH_OPTIONS.find((o) => o.value === health)
  return option?.color || 'bg-gray-100 text-gray-700'
}

// =============================================================================
// Customer Group Form Modal
// =============================================================================

interface GroupFormProps {
  group?: CustomerGroup | null
  onClose: () => void
  onSubmit: (data: CustomerGroupInput) => void
  isLoading: boolean
}

function GroupForm({ group, onClose, onSubmit, isLoading }: GroupFormProps) {
  const [name, setName] = useState(group?.name || '')
  const [description, setDescription] = useState(group?.description || '')
  const [health, setHealth] = useState<HealthCategory>(group?.health || 'prospect')
  const [potentialValue, setPotentialValue] = useState(
    group?.potential_value ? parseFloat(group.potential_value).toString() : ''
  )
  const [error, setError] = useState('')

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault()
    setError('')

    if (!name.trim()) {
      setError('Name is required')
      return
    }

    const value = parseFloat(potentialValue) || 0
    if (value < 0) {
      setError('Potential value must be non-negative')
      return
    }

    onSubmit({
      name: name.trim(),
      description: description.trim(),
      health,
      potential_value: value,
    })
  }

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50">
      <div className="bg-white rounded-lg shadow-xl w-full max-w-md p-6">
        <div className="flex items-center justify-between mb-4">
          <h2 className="text-lg font-semibold text-gray-900">
            {group ? 'Edit Customer Group' : 'Add Customer Group'}
          </h2>
          <button
            onClick={onClose}
            className="p-1 hover:bg-gray-100 rounded transition-colors"
          >
            <X className="h-5 w-5 text-gray-500" />
          </button>
        </div>

        <form onSubmit={handleSubmit} className="space-y-4">
          {error && (
            <div className="p-3 bg-red-50 border border-red-200 rounded-md text-sm text-red-700">
              {error}
            </div>
          )}

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Name *
            </label>
            <input
              type="text"
              value={name}
              onChange={(e) => setName(e.target.value)}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-gray-900 focus:border-transparent"
              placeholder="e.g., Acme Corporation"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Description
            </label>
            <textarea
              value={description}
              onChange={(e) => setDescription(e.target.value)}
              rows={2}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-gray-900 focus:border-transparent"
              placeholder="Brief description..."
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Health Status
            </label>
            <select
              value={health}
              onChange={(e) => setHealth(e.target.value as HealthCategory)}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-gray-900 focus:border-transparent"
            >
              {HEALTH_OPTIONS.map((opt) => (
                <option key={opt.value} value={opt.value}>
                  {opt.label}
                </option>
              ))}
            </select>
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Potential Value ($)
            </label>
            <input
              type="number"
              value={potentialValue}
              onChange={(e) => setPotentialValue(e.target.value)}
              min="0"
              step="0.01"
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-gray-900 focus:border-transparent"
              placeholder="0.00"
            />
          </div>

          <div className="flex gap-3 pt-2">
            <Button
              type="button"
              variant="outline"
              onClick={onClose}
              className="flex-1"
            >
              Cancel
            </Button>
            <Button type="submit" className="flex-1" disabled={isLoading}>
              {isLoading ? 'Saving...' : group ? 'Update' : 'Add'}
            </Button>
          </div>
        </form>
      </div>
    </div>
  )
}

// =============================================================================
// Delete Confirmation Modal
// =============================================================================

interface DeleteConfirmProps {
  group: CustomerGroup
  onClose: () => void
  onConfirm: () => void
  isLoading: boolean
}

function DeleteConfirm({ group, onClose, onConfirm, isLoading }: DeleteConfirmProps) {
  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50">
      <div className="bg-white rounded-lg shadow-xl w-full max-w-sm p-6">
        <h2 className="text-lg font-semibold text-gray-900 mb-2">Delete Group</h2>
        <p className="text-sm text-gray-600 mb-4">
          Are you sure you want to delete "{group.name}"? This action cannot be undone.
        </p>
        <div className="flex gap-3">
          <Button variant="outline" onClick={onClose} className="flex-1">
            Cancel
          </Button>
          <Button
            variant="destructive"
            onClick={onConfirm}
            disabled={isLoading}
            className="flex-1"
          >
            {isLoading ? 'Deleting...' : 'Delete'}
          </Button>
        </div>
      </div>
    </div>
  )
}

// =============================================================================
// Management View
// =============================================================================

interface ManagementViewProps {
  groups: CustomerGroup[]
  orgId: string
}

function ManagementView({ groups, orgId }: ManagementViewProps) {
  const [showForm, setShowForm] = useState(false)
  const [editingGroup, setEditingGroup] = useState<CustomerGroup | null>(null)
  const [deletingGroup, setDeletingGroup] = useState<CustomerGroup | null>(null)

  const createMutation = useCreateCustomerGroup()
  const updateMutation = useUpdateCustomerGroup()
  const deleteMutation = useDeleteCustomerGroup()

  const handleCreate = (data: CustomerGroupInput) => {
    createMutation.mutate(
      { orgId, data },
      {
        onSuccess: () => setShowForm(false),
      }
    )
  }

  const handleUpdate = (data: CustomerGroupInput) => {
    if (!editingGroup) return
    updateMutation.mutate(
      { groupId: editingGroup.id, orgId, data },
      {
        onSuccess: () => setEditingGroup(null),
      }
    )
  }

  const handleDelete = () => {
    if (!deletingGroup) return
    deleteMutation.mutate(
      { groupId: deletingGroup.id, orgId },
      {
        onSuccess: () => setDeletingGroup(null),
      }
    )
  }

  return (
    <div>
      <div className="flex items-center justify-between mb-6">
        <div>
          <h2 className="text-lg font-semibold text-gray-900">Customer Groups</h2>
          <p className="text-sm text-gray-500 mt-1">
            Manage your tracked customer groups
          </p>
        </div>
        <Button onClick={() => setShowForm(true)}>
          <Plus className="h-4 w-4 mr-2" />
          Add Group
        </Button>
      </div>

      {groups.length === 0 ? (
        <div className="text-center py-12 bg-gray-50 rounded-lg border border-gray-200">
          <Users className="h-12 w-12 text-gray-400 mx-auto mb-3" />
          <h3 className="text-sm font-medium text-gray-900 mb-1">
            No customer groups yet
          </h3>
          <p className="text-sm text-gray-500 mb-4">
            Get started by adding your first customer group.
          </p>
          <Button onClick={() => setShowForm(true)}>
            <Plus className="h-4 w-4 mr-2" />
            Add Group
          </Button>
        </div>
      ) : (
        <div className="bg-white rounded-lg border border-gray-200 overflow-hidden">
          <table className="w-full">
            <thead className="bg-gray-50 border-b border-gray-200">
              <tr>
                <th className="text-left text-xs font-medium text-gray-500 uppercase px-4 py-3">
                  Name
                </th>
                <th className="text-left text-xs font-medium text-gray-500 uppercase px-4 py-3">
                  Health
                </th>
                <th className="text-right text-xs font-medium text-gray-500 uppercase px-4 py-3">
                  Potential Value
                </th>
                <th className="text-right text-xs font-medium text-gray-500 uppercase px-4 py-3 w-24">
                  Actions
                </th>
              </tr>
            </thead>
            <tbody className="divide-y divide-gray-200">
              {groups.map((group) => (
                <tr key={group.id} className="hover:bg-gray-50">
                  <td className="px-4 py-3">
                    <div className="font-medium text-gray-900">{group.name}</div>
                    {group.description && (
                      <div className="text-sm text-gray-500 truncate max-w-xs">
                        {group.description}
                      </div>
                    )}
                  </td>
                  <td className="px-4 py-3">
                    <span
                      className={cn(
                        'inline-flex px-2 py-1 text-xs font-medium rounded-full',
                        getHealthBadgeClass(group.health)
                      )}
                    >
                      {group.health_display}
                    </span>
                  </td>
                  <td className="px-4 py-3 text-right font-medium text-gray-900">
                    {formatCurrency(group.potential_value)}
                  </td>
                  <td className="px-4 py-3 text-right">
                    <div className="flex items-center justify-end gap-1">
                      <button
                        onClick={() => setEditingGroup(group)}
                        className="p-1.5 hover:bg-gray-100 rounded transition-colors"
                        title="Edit"
                      >
                        <Pencil className="h-4 w-4 text-gray-500" />
                      </button>
                      <button
                        onClick={() => setDeletingGroup(group)}
                        className="p-1.5 hover:bg-red-50 rounded transition-colors"
                        title="Delete"
                      >
                        <Trash2 className="h-4 w-4 text-red-500" />
                      </button>
                    </div>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      {showForm && (
        <GroupForm
          onClose={() => setShowForm(false)}
          onSubmit={handleCreate}
          isLoading={createMutation.isPending}
        />
      )}

      {editingGroup && (
        <GroupForm
          group={editingGroup}
          onClose={() => setEditingGroup(null)}
          onSubmit={handleUpdate}
          isLoading={updateMutation.isPending}
        />
      )}

      {deletingGroup && (
        <DeleteConfirm
          group={deletingGroup}
          onClose={() => setDeletingGroup(null)}
          onConfirm={handleDelete}
          isLoading={deleteMutation.isPending}
        />
      )}
    </div>
  )
}

// =============================================================================
// Dashboard View Components
// =============================================================================

function StatCard({
  icon: Icon,
  label,
  value,
  subtext,
}: {
  icon: React.ElementType
  label: string
  value: string | number
  subtext?: string
}) {
  return (
    <div className="bg-white rounded-lg border border-gray-200 p-5">
      <div className="flex items-center gap-3">
        <div className="p-2 bg-gray-100 rounded-lg">
          <Icon className="h-5 w-5 text-gray-600" />
        </div>
        <div>
          <p className="text-sm text-gray-500">{label}</p>
          <p className="text-xl font-semibold text-gray-900">{value}</p>
          {subtext && <p className="text-xs text-gray-400">{subtext}</p>}
        </div>
      </div>
    </div>
  )
}

function HealthDistributionChart({
  byHealth,
  total,
}: {
  byHealth: Record<HealthCategory, number>
  total: number
}) {
  if (total === 0) {
    return (
      <div className="flex items-center justify-center h-40 text-gray-400 text-sm">
        No data available
      </div>
    )
  }

  return (
    <div className="space-y-3">
      {HEALTH_OPTIONS.map((opt) => {
        const count = byHealth[opt.value] || 0
        const percentage = total > 0 ? (count / total) * 100 : 0
        return (
          <div key={opt.value}>
            <div className="flex items-center justify-between text-sm mb-1">
              <span className="text-gray-600">{opt.label}</span>
              <span className="font-medium text-gray-900">{count}</span>
            </div>
            <div className="h-2 bg-gray-100 rounded-full overflow-hidden">
              <div
                className="h-full rounded-full transition-all"
                style={{
                  width: `${percentage}%`,
                  backgroundColor: HEALTH_COLORS[opt.value],
                }}
              />
            </div>
          </div>
        )
      })}
    </div>
  )
}

function ValueDistributionChart({
  valueByHealth,
  total,
}: {
  valueByHealth: Record<HealthCategory, string>
  total: number
}) {
  if (total === 0) {
    return (
      <div className="flex items-center justify-center h-40 text-gray-400 text-sm">
        No data available
      </div>
    )
  }

  // Calculate percentages
  const values = HEALTH_OPTIONS.map((opt) => ({
    ...opt,
    value: parseFloat(valueByHealth[opt.value] || '0'),
  }))

  const totalValue = values.reduce((sum, v) => sum + v.value, 0)

  // Create pie chart segments
  let currentAngle = 0
  const segments = values
    .filter((v) => v.value > 0)
    .map((v) => {
      const percentage = totalValue > 0 ? (v.value / totalValue) * 100 : 0
      const angle = (percentage / 100) * 360
      const startAngle = currentAngle
      currentAngle += angle

      // Calculate SVG arc path
      const startRad = ((startAngle - 90) * Math.PI) / 180
      const endRad = ((startAngle + angle - 90) * Math.PI) / 180
      const x1 = 50 + 40 * Math.cos(startRad)
      const y1 = 50 + 40 * Math.sin(startRad)
      const x2 = 50 + 40 * Math.cos(endRad)
      const y2 = 50 + 40 * Math.sin(endRad)
      const largeArcFlag = angle > 180 ? 1 : 0

      return {
        ...v,
        percentage,
        path:
          angle >= 360
            ? `M 50 10 A 40 40 0 1 1 49.99 10 A 40 40 0 1 1 50 10`
            : `M 50 50 L ${x1} ${y1} A 40 40 0 ${largeArcFlag} 1 ${x2} ${y2} Z`,
        color: HEALTH_COLORS[v.value as unknown as HealthCategory],
      }
    })

  return (
    <div className="flex items-center gap-6">
      <svg viewBox="0 0 100 100" className="w-32 h-32 flex-shrink-0">
        {segments.map((seg, i) => (
          <path
            key={i}
            d={seg.path}
            fill={HEALTH_COLORS[seg.value as unknown as HealthCategory] || seg.color}
          />
        ))}
        {totalValue === 0 && (
          <circle cx="50" cy="50" r="40" fill="#e5e7eb" />
        )}
      </svg>
      <div className="flex-1 space-y-2">
        {values.map((v) => (
          <div key={v.value} className="flex items-center gap-2 text-sm">
            <div
              className="w-3 h-3 rounded-sm"
              style={{ backgroundColor: HEALTH_COLORS[v.value as unknown as HealthCategory] }}
            />
            <span className="text-gray-600 flex-1">{v.label}</span>
            <span className="font-medium text-gray-900">
              {formatCurrency(v.value)}
            </span>
          </div>
        ))}
      </div>
    </div>
  )
}

function TopGroupsChart({ groups }: { groups: CustomerGroup[] }) {
  if (groups.length === 0) {
    return (
      <div className="flex items-center justify-center h-40 text-gray-400 text-sm">
        No data available
      </div>
    )
  }

  const maxValue = Math.max(
    ...groups.map((g) => parseFloat(g.potential_value))
  )

  return (
    <div className="space-y-3">
      {groups.map((group) => {
        const value = parseFloat(group.potential_value)
        const percentage = maxValue > 0 ? (value / maxValue) * 100 : 0
        return (
          <div key={group.id}>
            <div className="flex items-center justify-between text-sm mb-1">
              <span className="text-gray-900 font-medium truncate flex-1 mr-2">
                {group.name}
              </span>
              <span
                className={cn(
                  'text-xs px-1.5 py-0.5 rounded',
                  getHealthBadgeClass(group.health)
                )}
              >
                {group.health_display}
              </span>
            </div>
            <div className="flex items-center gap-2">
              <div className="flex-1 h-4 bg-gray-100 rounded overflow-hidden">
                <div
                  className="h-full bg-gray-800 rounded transition-all"
                  style={{ width: `${percentage}%` }}
                />
              </div>
              <span className="text-sm font-medium text-gray-900 w-24 text-right">
                {formatCurrency(value)}
              </span>
            </div>
          </div>
        )
      })}
    </div>
  )
}

interface DashboardViewProps {
  orgId: string
}

function DashboardView({ orgId }: DashboardViewProps) {
  const { data: stats, isLoading, error } = useCustomerGroupStats(orgId)

  if (isLoading) {
    return (
      <div className="flex items-center justify-center py-12">
        <div className="h-8 w-8 animate-spin rounded-full border-2 border-gray-300 border-t-gray-900" />
      </div>
    )
  }

  if (error || !stats) {
    return (
      <div className="text-center py-12">
        <AlertTriangle className="h-12 w-12 text-gray-400 mx-auto mb-3" />
        <p className="text-sm text-gray-500">Failed to load dashboard data</p>
      </div>
    )
  }

  return (
    <div className="space-y-6">
      {/* Stats Row */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <StatCard
          icon={Users}
          label="Total Groups"
          value={stats.total_groups}
          subtext="Tracked customer groups"
        />
        <StatCard
          icon={DollarSign}
          label="Total Value"
          value={formatCurrency(stats.total_value)}
          subtext="Combined potential value"
        />
        <StatCard
          icon={TrendingUp}
          label="Avg Value"
          value={
            stats.total_groups > 0
              ? formatCurrency(
                  parseFloat(stats.total_value) / stats.total_groups
                )
              : '$0'
          }
          subtext="Per customer group"
        />
      </div>

      {/* Charts Row */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <div className="bg-white rounded-lg border border-gray-200 p-5">
          <h3 className="text-sm font-semibold text-gray-900 mb-4">
            Groups by Health Status
          </h3>
          <HealthDistributionChart
            byHealth={stats.by_health}
            total={stats.total_groups}
          />
        </div>

        <div className="bg-white rounded-lg border border-gray-200 p-5">
          <h3 className="text-sm font-semibold text-gray-900 mb-4">
            Value by Health Status
          </h3>
          <ValueDistributionChart
            valueByHealth={stats.value_by_health}
            total={stats.total_groups}
          />
        </div>
      </div>

      {/* Top Groups */}
      <div className="bg-white rounded-lg border border-gray-200 p-5">
        <h3 className="text-sm font-semibold text-gray-900 mb-4">
          Highest Value Groups
        </h3>
        <TopGroupsChart groups={stats.top_groups} />
      </div>
    </div>
  )
}

// =============================================================================
// Main CRM Page Component
// =============================================================================

export function CrmPage() {
  const [viewMode, setViewMode] = useState<ViewMode>('management')
  const selectedOrgId = useAppSelector((state) => state.ui.selectedOrgId)
  const { data: groups = [], isLoading, error } = useCustomerGroups(selectedOrgId)

  if (!selectedOrgId) {
    return (
      <div className="flex items-center justify-center h-64">
        <p className="text-gray-500">Please select an organization</p>
      </div>
    )
  }

  return (
    <div className="max-w-6xl mx-auto px-6 py-6">
      {/* Page Header */}
      <div className="flex items-center justify-between mb-6">
        <div>
          <h1 className="text-2xl font-semibold text-gray-900">CRM Dashboard</h1>
          <p className="text-sm text-gray-500 mt-1">
            Track customer groups and analyze pipeline health
          </p>
        </div>

        {/* View Toggle */}
        <div className="flex bg-gray-100 rounded-md p-0.5">
          <button
            onClick={() => setViewMode('management')}
            className={cn(
              'flex items-center gap-2 px-3 py-1.5 text-sm font-medium rounded transition-all',
              viewMode === 'management'
                ? 'bg-white text-gray-900 shadow-sm'
                : 'text-gray-600 hover:text-gray-900'
            )}
          >
            <List className="h-4 w-4" />
            Management
          </button>
          <button
            onClick={() => setViewMode('dashboard')}
            className={cn(
              'flex items-center gap-2 px-3 py-1.5 text-sm font-medium rounded transition-all',
              viewMode === 'dashboard'
                ? 'bg-white text-gray-900 shadow-sm'
                : 'text-gray-600 hover:text-gray-900'
            )}
          >
            <LayoutDashboard className="h-4 w-4" />
            Dashboard
          </button>
        </div>
      </div>

      {/* Content */}
      {isLoading ? (
        <div className="flex items-center justify-center py-12">
          <div className="h-8 w-8 animate-spin rounded-full border-2 border-gray-300 border-t-gray-900" />
        </div>
      ) : error ? (
        <div className="text-center py-12 bg-gray-50 rounded-lg border border-gray-200">
          <AlertTriangle className="h-12 w-12 text-gray-400 mx-auto mb-3" />
          <p className="text-sm text-gray-500">Failed to load customer groups</p>
        </div>
      ) : viewMode === 'management' ? (
        <ManagementView groups={groups} orgId={selectedOrgId} />
      ) : (
        <DashboardView orgId={selectedOrgId} />
      )}
    </div>
  )
}
