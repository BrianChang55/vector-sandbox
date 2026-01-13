import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import path from 'path'

// https://vite.dev/config/
export default defineConfig({
  plugins: [
    react({
      babel: {
        plugins: []  // Enable Babel mode for full TypeScript support
      }
    })
  ],
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './src'),
      // Enable React DevTools Profiler in production builds
      'react-dom/client': 'react-dom/profiling',
    },
  },
  server: {
    port: 5176,
  },
})

