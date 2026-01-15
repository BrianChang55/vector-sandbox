from .guards import ANTI_AI_SLOP_GUIDE

# Shared style guide injected into all design-aware prompts.
DESIGN_STYLE_PROMPT = (
    """This base style guide (enterprise, light theme, minimal):

## Typography
- CRITICAL: Avoid excessive bolding. Use font-medium for most text, font-semibold sparingly and only for truly important headings.
- Page titles: text-lg or text-xl font-medium text-gray-900 (NOT font-bold)
- Section headings: text-sm font-medium text-gray-900
- Body text: text-sm text-gray-700, subtext: text-xs text-gray-500
- Labels: text-xs font-medium text-gray-600 uppercase tracking-wide (for form labels)
- Use system font stack. Never use font-bold unless absolutely critical.

## Spacing & Layout
- Generous spacing creates visual breathing room: p-4 for compact areas, p-6 for main sections, p-8 for page containers
- Use gap-3 or gap-4 between related items, gap-6 between sections
- Cards: p-4 or p-5 internal padding with space-y-4 for stacked content
- Form fields: space-y-4 between fields, mt-1 or mt-1.5 between label and input
- Section dividers: border-t border-gray-100 with py-4 above and below
- Maintain consistent vertical rhythm throughout

## Colors & Surfaces
- Canvas: bg-gray-50; Cards/surfaces: bg-white
- Borders: border-gray-200 default, hover:border-gray-300
- Subtle shadows: shadow-sm for cards, shadow-md for elevated elements like dropdowns/popovers
- Status colors (soft backgrounds): bg-green-50 border-green-200 text-green-700, bg-red-50 border-red-200 text-red-700, bg-yellow-50 border-yellow-200 text-yellow-700, bg-blue-50 border-blue-200 text-blue-700

## Buttons
- Primary: bg-gray-900 hover:bg-gray-800 text-white rounded-md px-4 py-2 text-sm font-medium
- Secondary: bg-gray-100 hover:bg-gray-200 text-gray-700 rounded-md px-3 py-1.5 text-sm font-medium
- Outline: border border-gray-200 bg-white hover:bg-gray-50 text-gray-700 rounded-md
- Ghost: bg-transparent hover:bg-gray-100 text-gray-600 rounded-md
- Small buttons: px-3 py-1.5 text-xs, icon buttons: p-2 rounded-md
- Disabled: opacity-50 cursor-not-allowed

## Form Inputs & Select Dropdowns
- NEVER use browser default selects. Always style custom selects:
- Input/Select base: w-full bg-white border border-gray-200 rounded-md px-3 py-2 text-sm text-gray-900 placeholder:text-gray-500 focus:outline-none focus:ring-2 focus:ring-gray-900 focus:ring-offset-1 focus:border-gray-300
- Custom select trigger: flex items-center justify-between, add ChevronDown icon (h-4 w-4 text-gray-400)
- Dropdown content: bg-white border border-gray-200 rounded-md shadow-lg p-1 z-50
- Dropdown item: px-3 py-2 text-sm text-gray-700 rounded-sm cursor-pointer hover:bg-gray-100 focus:bg-gray-100

## Dropdown Menus (Action menus)
- Container: min-w-[8rem] bg-white border border-gray-200 rounded-md shadow-md p-1 z-50
- Menu item: flex items-center gap-2 px-2 py-1.5 text-sm text-gray-700 rounded-sm cursor-pointer hover:bg-gray-100 transition-colors
- Separator: my-1 h-px bg-gray-200
- Icons in menus: h-4 w-4 text-gray-500

## Popovers
- Container: bg-white border border-gray-200 rounded-lg shadow-lg z-50 overflow-hidden
- Header: px-4 py-3 border-b border-gray-100 bg-gray-50
- Content area: p-4 space-y-4
- Use subtle entrance animation: opacity 0→1, slight y-offset (-4px→0), scale 0.98→1
- Close on outside click

## Modals/Dialogs
- Backdrop: fixed inset-0 bg-black/40 backdrop-blur-sm z-50
- Modal container: bg-white rounded-xl shadow-2xl max-w-md w-full mx-4 max-h-[85vh] overflow-hidden flex flex-col
- Header: flex items-center justify-between px-6 py-4 border-b border-gray-200
- Header title: text-lg font-medium text-gray-900 (NOT font-bold or font-semibold)
- Content: flex-1 overflow-y-auto px-6 py-4
- Footer: flex items-center justify-end gap-3 px-6 py-4 border-t border-gray-200 bg-gray-50 rounded-b-xl
- Close button: p-2 text-gray-400 hover:text-gray-600 hover:bg-gray-100 rounded-lg

## Toast Notifications
- ALWAYS implement custom toasts, never use browser alerts:
- Position: fixed bottom-4 right-4 z-[100]
- Container: flex items-start gap-3 p-4 rounded-lg border shadow-lg min-w-[320px] max-w-[400px]
- Success: bg-green-50 border-green-200, icon CheckCircle text-green-600
- Error: bg-red-50 border-red-200, icon AlertCircle text-red-600
- Warning: bg-yellow-50 border-yellow-200, icon AlertTriangle text-yellow-600
- Info: bg-blue-50 border-blue-200, icon Info text-blue-600
- Title: text-sm font-medium text-gray-900
- Description: text-sm text-gray-600 mt-1
- Close button: p-1 text-gray-400 hover:text-gray-600 rounded
- Animation: slide in from right, fade out on dismiss

## Tables
- Container: border border-gray-200 rounded-lg overflow-hidden
- Header row: bg-gray-50 border-b border-gray-200
- Header cells: px-4 py-3 text-xs font-medium text-gray-500 uppercase tracking-wide text-left
- Body rows: hover:bg-gray-50 border-b border-gray-100 last:border-0
- Body cells: px-4 py-3 text-sm text-gray-700
- Actions column: text-right, use icon buttons or dropdown menu

## Cards
- Container: bg-white border border-gray-200 rounded-lg shadow-sm overflow-hidden
- Clickable cards: hover:border-gray-300 hover:shadow transition-all cursor-pointer
- Card header: px-4 py-3 border-b border-gray-100 (optional)
- Card content: p-4 or p-5
- Card footer: px-4 py-3 border-t border-gray-100 bg-gray-50

## Corners & Shapes
- Buttons/inputs: rounded-md
- Cards/modals: rounded-lg or rounded-xl
- Small elements (badges, tags): rounded-full or rounded-md
- Avoid sharp corners

## Icons
- Use Lucide React icons consistently
- Size: h-4 w-4 for inline, h-5 w-5 for buttons, h-6 w-6 for larger UI elements
- Colors: text-gray-400 for decorative, text-gray-500 for secondary, text-gray-700 for primary

## Loading States
- Spinner: Loader2 icon with animate-spin
- Skeleton: animate-pulse with bg-gray-200 rounded blocks
- Disabled during loading: opacity-60 pointer-events-none

"""
    + ANTI_AI_SLOP_GUIDE
)
