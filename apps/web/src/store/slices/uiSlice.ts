/**
 * UI state slice (Redux Toolkit)
 * Manages: selected org/app/version, panel visibility, etc.
 */
import { createSlice } from '@reduxjs/toolkit'
import type { PayloadAction } from '@reduxjs/toolkit'

interface UIState {
  selectedOrgId: string | null
  selectedAppId: string | null
  selectedVersionId: string | null
  leftPanelOpen: boolean
  rightPanelOpen: boolean
  rightPanelView: 'code' | 'inspector' | null
}

const initialState: UIState = {
  selectedOrgId: null,
  selectedAppId: null,
  selectedVersionId: null,
  leftPanelOpen: true,
  rightPanelOpen: true,
  rightPanelView: 'code',
}

const uiSlice = createSlice({
  name: 'ui',
  initialState,
  reducers: {
    setSelectedOrg: (state, action: PayloadAction<string | null>) => {
      state.selectedOrgId = action.payload
      // Reset app/version when org changes
      state.selectedAppId = null
      state.selectedVersionId = null
    },
    setSelectedApp: (state, action: PayloadAction<string | null>) => {
      state.selectedAppId = action.payload
      // Reset version when app changes
      state.selectedVersionId = null
    },
    setSelectedVersion: (state, action: PayloadAction<string | null>) => {
      state.selectedVersionId = action.payload
    },
    setLeftPanelOpen: (state, action: PayloadAction<boolean>) => {
      state.leftPanelOpen = action.payload
    },
    setRightPanelOpen: (state, action: PayloadAction<boolean>) => {
      state.rightPanelOpen = action.payload
    },
    setRightPanelView: (state, action: PayloadAction<'code' | 'inspector' | null>) => {
      state.rightPanelView = action.payload
    },
  },
})

export const {
  setSelectedOrg,
  setSelectedApp,
  setSelectedVersion,
  setLeftPanelOpen,
  setRightPanelOpen,
  setRightPanelView,
} = uiSlice.actions

export default uiSlice.reducer

