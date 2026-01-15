/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        primary: {
          50: '#f0f9ff',
          100: '#e0f2fe',
          200: '#bae6fd',
          300: '#7dd3fc',
          400: '#38bdf8',
          500: '#0EA5E9',
          600: '#0284c7',
          700: '#0369a1',
          800: '#075985',
          900: '#0c4a6e',
        },
        background: '#FBFCFE',
        foreground: '#0F172A',
        muted: {
          DEFAULT: 'rgba(15, 23, 42, 0.6)',
          foreground: '#64748b',
        },
        success: '#10B981',
        warning: '#F59E0B',
        error: '#EF4444',
        info: '#3B82F6',
      },
      fontFamily: {
        serif: ['"Source Serif"', 'serif'],
        sans: ['Inter', 'system-ui', 'sans-serif'],
        mono: ['Fira Code', 'monospace'],
      },
      transitionDuration: {
        'instant': 'var(--internal-apps-duration-instant)',
        'fast': 'var(--internal-apps-duration-fast)',
        'standard': 'var(--internal-apps-duration-standard)',
        'slow': 'var(--internal-apps-duration-slow)',
        'slow-plus': 'var(--internal-apps-duration-slow-plus)',
      },
      transitionTimingFunction: {
        'out-soft': 'var(--internal-apps-ease-out-soft)',
        'in-soft': 'var(--internal-apps-ease-in-soft)',
        'in-out-soft': 'var(--internal-apps-ease-in-out-soft)',
      },
      keyframes: {
        'fade-in': {
          from: { opacity: '0' },
          to: { opacity: '1' },
        },
        'fade-out': {
          from: { opacity: '1' },
          to: { opacity: '0' },
        },
        'slide-in-from-left': {
          '0%': { transform: 'translateX(-100%)', opacity: '0.8' },
          '100%': { transform: 'translateX(0)', opacity: '1' },
        },
        'slide-out-to-left': {
          '0%': { transform: 'translateX(0)', opacity: '1' },
          '100%': { transform: 'translateX(-100%)', opacity: '0.8' },
        },
        'slide-in-from-right': {
          '0%': { transform: 'translateX(100%)', opacity: '0.8' },
          '100%': { transform: 'translateX(0)', opacity: '1' },
        },
        'slide-out-to-right': {
          '0%': { transform: 'translateX(0)', opacity: '1' },
          '100%': { transform: 'translateX(100%)', opacity: '0.8' },
        },
        'zoom-in': {
          from: { opacity: '0', transform: 'translate(-50%, -50%) scale(0.95)' },
          to: { opacity: '1', transform: 'translate(-50%, -50%) scale(1)' },
        },
        'zoom-out': {
          from: { opacity: '1', transform: 'translate(-50%, -50%) scale(1)' },
          to: { opacity: '0', transform: 'translate(-50%, -50%) scale(0.95)' },
        },
      },
      animation: {
        'fade-in': 'fade-in 0.25s cubic-bezier(0.4, 0, 0.2, 1)',
        'fade-out': 'fade-out 0.2s cubic-bezier(0.4, 0, 0.2, 1)',
        'slide-in-from-left': 'slide-in-from-left 0.35s cubic-bezier(0.32, 0.72, 0, 1)',
        'slide-out-to-left': 'slide-out-to-left 0.25s cubic-bezier(0.32, 0.72, 0, 1)',
        'slide-in-from-right': 'slide-in-from-right 0.35s cubic-bezier(0.32, 0.72, 0, 1)',
        'slide-out-to-right': 'slide-out-to-right 0.25s cubic-bezier(0.32, 0.72, 0, 1)',
        'zoom-in': 'zoom-in 0.2s ease-out',
        'zoom-out': 'zoom-out 0.15s ease-in',
      },
    },
  },
  plugins: [],
}

