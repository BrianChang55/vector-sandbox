/**
 * NeuralDotField - An AI-inspired animated canvas background
 * Creates a technical, neural network-like dot pattern with:
 * - Pulsing dots with phase-shifted animations
 * - Neural connection lines between nearby dots
 * - Flowing data streams/signals traveling between nodes
 * - Wave distortions rippling through the pattern
 * - Mouse interaction for cursor proximity effects
 */
import { useEffect, useRef, useCallback } from 'react'

interface Dot {
  x: number
  y: number
  baseRadius: number
  radius: number
  phase: number
  pulseSpeed: number
  opacity: number
  connections: number[]
  isActive: boolean
  activationTime: number
}

interface DataStream {
  fromDot: number
  toDot: number
  progress: number
  speed: number
  color: string
}

export function NeuralDotField() {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const animationRef = useRef<number | null>(null)
  const dotsRef = useRef<Dot[]>([])
  const streamsRef = useRef<DataStream[]>([])
  const mouseRef = useRef({ x: -1000, y: -1000 })
  const timeRef = useRef(0)
  const dimensionsRef = useRef({ width: 0, height: 0 })

  // Initialize dots grid
  const initializeDots = useCallback((width: number, height: number) => {
    const dots: Dot[] = []
    const spacing = 32
    const cols = Math.ceil(width / spacing) + 2
    const rows = Math.ceil(height / spacing) + 2
    const centerX = width / 2
    const centerY = height / 2
    const maxDist = Math.sqrt(centerX * centerX + centerY * centerY)

    for (let row = 0; row < rows; row++) {
      for (let col = 0; col < cols; col++) {
        const x = col * spacing - spacing
        const y = row * spacing - spacing
        
        // Distance from center
        const dx = x - centerX
        const dy = y - centerY
        const dist = Math.sqrt(dx * dx + dy * dy)
        const normalizedDist = dist / maxDist
        
        // Skip dots too close to center (content area)
        if (normalizedDist < 0.15) continue
        
        // Vary radius based on distance (smaller near center, larger at edges)
        const baseRadius = 0.6 + normalizedDist * 1.4
        
        dots.push({
          x,
          y,
          baseRadius,
          radius: baseRadius,
          phase: Math.random() * Math.PI * 2,
          pulseSpeed: 0.5 + Math.random() * 0.5,
          opacity: 0.15 + normalizedDist * 0.5,
          connections: [],
          isActive: false,
          activationTime: 0,
        })
      }
    }

    // Pre-compute connections (nearby dots)
    const connectionDist = spacing * 2.5
    for (let i = 0; i < dots.length; i++) {
      for (let j = i + 1; j < dots.length; j++) {
        const dx = dots[i].x - dots[j].x
        const dy = dots[i].y - dots[j].y
        const dist = Math.sqrt(dx * dx + dy * dy)
        if (dist < connectionDist) {
          dots[i].connections.push(j)
        }
      }
    }

    dotsRef.current = dots
    dimensionsRef.current = { width, height }
  }, [])

  // Create random data streams
  const spawnDataStream = useCallback(() => {
    const dots = dotsRef.current
    if (dots.length < 2) return
    
    // Pick random dot with connections
    const validDots = dots.filter(d => d.connections.length > 0)
    if (validDots.length === 0) return
    
    const fromIdx = dots.indexOf(validDots[Math.floor(Math.random() * validDots.length)])
    const fromDot = dots[fromIdx]
    if (fromDot.connections.length === 0) return
    
    const toIdx = fromDot.connections[Math.floor(Math.random() * fromDot.connections.length)]
    
    // AI-inspired colors - subtle blues, cyans, occasional purple
    const colors = [
      'rgba(59, 130, 246, 0.8)',   // Blue
      'rgba(6, 182, 212, 0.8)',    // Cyan
      'rgba(99, 102, 241, 0.7)',   // Indigo
      'rgba(139, 92, 246, 0.6)',   // Purple
      'rgba(20, 184, 166, 0.7)',   // Teal
    ]
    
    streamsRef.current.push({
      fromDot: fromIdx,
      toDot: toIdx,
      progress: 0,
      speed: 0.008 + Math.random() * 0.012,
      color: colors[Math.floor(Math.random() * colors.length)],
    })
    
    // Activate source dot
    dots[fromIdx].isActive = true
    dots[fromIdx].activationTime = timeRef.current
  }, [])

  // Animation loop
  const animate = useCallback((ctx: CanvasRenderingContext2D) => {
    const { width, height } = dimensionsRef.current
    const dots = dotsRef.current
    const streams = streamsRef.current
    const mouse = mouseRef.current
    const time = timeRef.current
    
    // Clear canvas
    ctx.clearRect(0, 0, width, height)
    
    const centerX = width / 2
    const centerY = height / 2
    
    // Update and draw connections first (behind dots)
    ctx.lineWidth = 0.5
    for (let i = 0; i < dots.length; i++) {
      const dot = dots[i]
      
      for (const j of dot.connections) {
        const other = dots[j]
        
        // Calculate connection opacity based on dot activity and wave
        const midX = (dot.x + other.x) / 2
        const midY = (dot.y + other.y) / 2
        const distFromCenter = Math.sqrt((midX - centerX) ** 2 + (midY - centerY) ** 2)
        const maxDist = Math.sqrt(centerX ** 2 + centerY ** 2)
        const normalizedDist = distFromCenter / maxDist
        
        // Wave effect
        const wavePhase = time * 0.5 - normalizedDist * 3
        const waveOpacity = (Math.sin(wavePhase) + 1) / 2 * 0.15
        
        // Base opacity
        let opacity = 0.03 + waveOpacity * normalizedDist
        
        // Boost if either dot is active
        if (dot.isActive || other.isActive) {
          opacity += 0.1
        }
        
        // Mouse proximity boost
        const mouseDistToMid = Math.sqrt((mouse.x - midX) ** 2 + (mouse.y - midY) ** 2)
        if (mouseDistToMid < 150) {
          opacity += (1 - mouseDistToMid / 150) * 0.15
        }
        
        if (opacity > 0.02) {
          ctx.strokeStyle = `rgba(156, 163, 175, ${opacity})`
          ctx.beginPath()
          ctx.moveTo(dot.x, dot.y)
          ctx.lineTo(other.x, other.y)
          ctx.stroke()
        }
      }
    }
    
    // Update and draw dots
    for (let i = 0; i < dots.length; i++) {
      const dot = dots[i]
      
      // Pulse animation
      const pulse = Math.sin(time * dot.pulseSpeed + dot.phase)
      dot.radius = dot.baseRadius * (1 + pulse * 0.3)
      
      // Distance from center for radial effects
      const distFromCenter = Math.sqrt((dot.x - centerX) ** 2 + (dot.y - centerY) ** 2)
      const maxDist = Math.sqrt(centerX ** 2 + centerY ** 2)
      const normalizedDist = distFromCenter / maxDist
      
      // Wave distortion
      const wavePhase = time * 0.8 - normalizedDist * 4
      const waveOffset = Math.sin(wavePhase) * 2
      const drawX = dot.x + waveOffset * (dot.x - centerX) / distFromCenter * 0.5
      const drawY = dot.y + waveOffset * (dot.y - centerY) / distFromCenter * 0.5
      
      // Calculate opacity with wave
      let opacity = dot.opacity * (0.7 + (Math.sin(wavePhase) + 1) / 2 * 0.3)
      
      // Activation glow fade
      if (dot.isActive) {
        const timeSinceActive = time - dot.activationTime
        if (timeSinceActive > 2) {
          dot.isActive = false
        } else {
          opacity = Math.min(1, opacity + (1 - timeSinceActive / 2) * 0.5)
        }
      }
      
      // Mouse proximity effect
      const mouseDist = Math.sqrt((mouse.x - dot.x) ** 2 + (mouse.y - dot.y) ** 2)
      if (mouseDist < 120) {
        const influence = 1 - mouseDist / 120
        dot.radius += influence * 2
        opacity = Math.min(1, opacity + influence * 0.3)
      }
      
      // Draw dot with subtle gradient for depth
      const gradient = ctx.createRadialGradient(
        drawX, drawY, 0,
        drawX, drawY, dot.radius * 2
      )
      
      // Color based on position and activity
      let baseColor = '156, 163, 175' // Gray
      if (dot.isActive) {
        baseColor = '59, 130, 246' // Blue when active
      }
      
      gradient.addColorStop(0, `rgba(${baseColor}, ${opacity})`)
      gradient.addColorStop(0.5, `rgba(${baseColor}, ${opacity * 0.5})`)
      gradient.addColorStop(1, `rgba(${baseColor}, 0)`)
      
      ctx.fillStyle = gradient
      ctx.beginPath()
      ctx.arc(drawX, drawY, dot.radius * 2, 0, Math.PI * 2)
      ctx.fill()
      
      // Core dot
      ctx.fillStyle = `rgba(${baseColor}, ${opacity})`
      ctx.beginPath()
      ctx.arc(drawX, drawY, dot.radius, 0, Math.PI * 2)
      ctx.fill()
    }
    
    // Update and draw data streams
    for (let i = streams.length - 1; i >= 0; i--) {
      const stream = streams[i]
      stream.progress += stream.speed
      
      if (stream.progress >= 1) {
        // Activate destination dot
        dots[stream.toDot].isActive = true
        dots[stream.toDot].activationTime = time
        
        // Sometimes chain to another stream
        if (Math.random() < 0.4) {
          const nextDot = dots[stream.toDot]
          if (nextDot.connections.length > 0) {
            const nextIdx = nextDot.connections[Math.floor(Math.random() * nextDot.connections.length)]
            streams.push({
              fromDot: stream.toDot,
              toDot: nextIdx,
              progress: 0,
              speed: stream.speed * (0.9 + Math.random() * 0.2),
              color: stream.color,
            })
          }
        }
        
        streams.splice(i, 1)
        continue
      }
      
      const from = dots[stream.fromDot]
      const to = dots[stream.toDot]
      
      // Calculate stream position with easing
      const eased = stream.progress * stream.progress * (3 - 2 * stream.progress)
      const x = from.x + (to.x - from.x) * eased
      const y = from.y + (to.y - from.y) * eased
      
      // Draw stream particle with trail
      const trailLength = 5
      for (let t = 0; t < trailLength; t++) {
        const trailProgress = Math.max(0, stream.progress - t * 0.03)
        const trailEased = trailProgress * trailProgress * (3 - 2 * trailProgress)
        const tx = from.x + (to.x - from.x) * trailEased
        const ty = from.y + (to.y - from.y) * trailEased
        const trailOpacity = (1 - t / trailLength) * 0.8
        
        ctx.fillStyle = stream.color.replace(/[\d.]+\)$/, `${trailOpacity})`)
        ctx.beginPath()
        ctx.arc(tx, ty, 2 - t * 0.3, 0, Math.PI * 2)
        ctx.fill()
      }
      
      // Glow effect
      const glow = ctx.createRadialGradient(x, y, 0, x, y, 8)
      glow.addColorStop(0, stream.color)
      glow.addColorStop(1, stream.color.replace(/[\d.]+\)$/, '0)'))
      ctx.fillStyle = glow
      ctx.beginPath()
      ctx.arc(x, y, 8, 0, Math.PI * 2)
      ctx.fill()
    }
    
    // Spawn new streams occasionally
    if (Math.random() < 0.015) {
      spawnDataStream()
    }
    
    // Increment time
    timeRef.current += 0.016
    
    // Continue animation
    animationRef.current = requestAnimationFrame(() => animate(ctx))
  }, [spawnDataStream])

  // Handle resize
  const handleResize = useCallback(() => {
    const canvas = canvasRef.current
    if (!canvas) return
    
    const rect = canvas.getBoundingClientRect()
    const dpr = window.devicePixelRatio || 1
    
    canvas.width = rect.width * dpr
    canvas.height = rect.height * dpr
    
    const ctx = canvas.getContext('2d')
    if (ctx) {
      ctx.scale(dpr, dpr)
    }
    
    initializeDots(rect.width, rect.height)
  }, [initializeDots])

  // Handle mouse move
  const handleMouseMove = useCallback((e: MouseEvent) => {
    const canvas = canvasRef.current
    if (!canvas) return
    
    const rect = canvas.getBoundingClientRect()
    mouseRef.current = {
      x: e.clientX - rect.left,
      y: e.clientY - rect.top,
    }
  }, [])

  const handleMouseLeave = useCallback(() => {
    mouseRef.current = { x: -1000, y: -1000 }
  }, [])

  // Setup effect
  useEffect(() => {
    const canvas = canvasRef.current
    if (!canvas) return

    const ctx = canvas.getContext('2d')
    if (!ctx) return

    handleResize()
    
    // Start animation
    animate(ctx)
    
    // Event listeners
    window.addEventListener('resize', handleResize)
    window.addEventListener('mousemove', handleMouseMove)
    canvas.addEventListener('mouseleave', handleMouseLeave)
    
    // Initial data streams
    setTimeout(() => {
      for (let i = 0; i < 5; i++) {
        setTimeout(() => spawnDataStream(), i * 500)
      }
    }, 1000)

    return () => {
      if (animationRef.current) {
        cancelAnimationFrame(animationRef.current)
      }
      window.removeEventListener('resize', handleResize)
      window.removeEventListener('mousemove', handleMouseMove)
      canvas.removeEventListener('mouseleave', handleMouseLeave)
    }
  }, [animate, handleResize, handleMouseMove, handleMouseLeave, spawnDataStream])

  return (
    <canvas
      ref={canvasRef}
      className="absolute inset-0 w-full h-full"
      style={{ 
        background: 'transparent',
      }}
    />
  )
}

