import js from '@eslint/js'
import globals from 'globals'
import reactHooks from 'eslint-plugin-react-hooks'
import reactRefresh from 'eslint-plugin-react-refresh'
import tseslint from 'typescript-eslint'
import { defineConfig, globalIgnores } from 'eslint/config'

export default defineConfig([
  globalIgnores(['dist']),
  {
    files: ['**/*.{ts,tsx}'],
    extends: [
      js.configs.recommended,
      tseslint.configs.recommended,
      reactHooks.configs.flat.recommended,
      reactRefresh.configs.vite,
    ],
    languageOptions: {
      ecmaVersion: 2020,
      globals: globals.browser,
    },
    rules: {
      // Allow 'any' types for flexibility
      '@typescript-eslint/no-explicit-any': 'off',
      // Allow unused vars with underscore prefix
      '@typescript-eslint/no-unused-vars': ['warn', { argsIgnorePattern: '^_', varsIgnorePattern: '^_' }],
      // Warn instead of error for mixed exports (common in codebases)
      'react-refresh/only-export-components': 'warn',
      // Warn for exhaustive deps (common pattern to intentionally omit deps)
      'react-hooks/exhaustive-deps': 'warn',
      // Downgrade react-hooks errors to warnings for pre-existing issues
      'react-hooks/set-state-in-effect': 'warn',
      'react-hooks/immutability': 'warn',
      'react-hooks/purity': 'warn',
      'react-hooks/preserve-manual-memoization': 'warn',
      'react-hooks/static-components': 'warn',
      'react-hooks/rules-of-hooks': 'error',
      // Allow empty interfaces (useful for extending)
      '@typescript-eslint/no-empty-object-type': 'off',
      // Allow case block declarations
      'no-case-declarations': 'off',
      // Allow unused expressions (e.g., short-circuit evaluation)
      '@typescript-eslint/no-unused-expressions': 'off',
      // Downgrade constant binary expressions to warning
      'no-constant-binary-expression': 'warn',
    },
  },
])

