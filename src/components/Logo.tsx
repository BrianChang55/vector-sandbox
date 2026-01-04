/**
 * Logo component - Consistent branding throughout the app
 * Uses the logo.svg from the public directory
 */

interface LogoProps {
  size?: 'sm' | 'md' | 'lg'
  showText?: boolean
  className?: string
}

const sizeMap = {
  sm: 'h-6 w-6',
  md: 'h-8 w-8',
  lg: 'h-10 w-10',
}

export function Logo({ size = 'md', showText = false, className = '' }: LogoProps) {
  return (
    <div className={`flex items-center gap-2 ${className}`}>
      <img 
        src="/logo.png" 
        alt="Relay" 
        className={sizeMap[size]}
      />
      {showText && (
        <span className="font-semibold text-gray-900">Relay</span>
      )}
    </div>
  )
}

