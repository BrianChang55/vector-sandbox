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
    },
  },
  plugins: [],
}

