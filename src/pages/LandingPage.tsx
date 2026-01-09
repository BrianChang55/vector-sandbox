/**
 * Landing page - Slate-inspired layout with enterprise design system
 * Following STYLE_GUIDE.md: Light, minimal, professional
 * With framer-motion animations
 */
import { useState, useEffect, useRef, useCallback } from 'react'
import { useNavigate, Link } from 'react-router-dom'
import { motion } from 'framer-motion'
import { Button } from '@/components/ui/button'
import { Logo } from '@/components/Logo'
import { 
  ArrowRight, 
  Layers,
  Table2,
  Users,
  BarChart3,
  Settings,
  FileText,
  ShoppingCart,
  Ticket,
  Zap,
  Shield,
  Code2
} from 'lucide-react'
import { NeuralDotField } from '@/components/NeuralDotField'

// Animation variants
const staggerContainerVariants = {
  initial: {},
  animate: {
    transition: {
      staggerChildren: 0.1,
      delayChildren: 0.2,
    },
  },
}

const staggerItemVariants = {
  initial: { opacity: 0, y: 20 },
  animate: { 
    opacity: 1, 
    y: 0,
    transition: {
      duration: 0.5,
      ease: 'easeOut' as const,
    },
  },
}

// Animated placeholder phrases - the suffix after "Build a "
const PLACEHOLDER_PHRASES = [
  'dashboard to manage user subscriptions...',
  'customer support ticket system...',
  'inventory management tool...',
  'employee onboarding portal...',
  'order tracking dashboard...',
  'content moderation queue...',
  'analytics reporting tool...',
  'invoice management system...',
  'user feedback collector...',
  'team task tracker...',
]

const STATIC_PREFIX = 'Build a '

// Hero title for blur animation
const HERO_TITLE = 'Build Internal Apps in Seconds'

// Template app data
const APP_TEMPLATES = [
  {
    id: 'user-admin',
    title: 'User Admin Panel',
    description: 'Manage users, roles, and permissions',
    icon: Users,
    color: 'bg-blue-50',
    iconColor: 'text-blue-600',
    borderHover: 'hover:border-blue-200',
  },
  {
    id: 'data-table',
    title: 'Data Explorer',
    description: 'Browse and edit database records',
    icon: Table2,
    color: 'bg-emerald-50',
    iconColor: 'text-emerald-600',
    borderHover: 'hover:border-emerald-200',
  },
  {
    id: 'analytics',
    title: 'Analytics Dashboard',
    description: 'Visualize metrics and KPIs',
    icon: BarChart3,
    color: 'bg-violet-50',
    iconColor: 'text-violet-600',
    borderHover: 'hover:border-violet-200',
  },
  {
    id: 'settings',
    title: 'Settings Manager',
    description: 'Configure app settings and preferences',
    icon: Settings,
    color: 'bg-orange-50',
    iconColor: 'text-orange-600',
    borderHover: 'hover:border-orange-200',
  },
  {
    id: 'content-manager',
    title: 'Content Manager',
    description: 'Create and manage content entries',
    icon: FileText,
    color: 'bg-pink-50',
    iconColor: 'text-pink-600',
    borderHover: 'hover:border-pink-200',
  },
  {
    id: 'order-tracker',
    title: 'Order Tracker',
    description: 'Track and manage customer orders',
    icon: ShoppingCart,
    color: 'bg-teal-50',
    iconColor: 'text-teal-600',
    borderHover: 'hover:border-teal-200',
  },
  {
    id: 'ticket-system',
    title: 'Ticket System',
    description: 'Handle support tickets and requests',
    icon: Ticket,
    color: 'bg-amber-50',
    iconColor: 'text-amber-600',
    borderHover: 'hover:border-amber-200',
  },
  {
    id: 'workflow-builder',
    title: 'Workflow Builder',
    description: 'Design and automate workflows',
    icon: Layers,
    color: 'bg-indigo-50',
    iconColor: 'text-indigo-600',
    borderHover: 'hover:border-indigo-200',
  },
]

// Showcase apps - what people have built
const SHOWCASE_APPS = [
  { title: 'Customer Portal', category: 'Support', gradient: 'from-blue-50 to-indigo-50' },
  { title: 'Inventory System', category: 'Operations', gradient: 'from-emerald-50 to-teal-50' },
  { title: 'HR Dashboard', category: 'People', gradient: 'from-violet-50 to-purple-50' },
  { title: 'Sales Pipeline', category: 'Revenue', gradient: 'from-amber-50 to-orange-50' },
]

// Features data
const FEATURES = [
  {
    icon: Zap,
    title: 'AI-Powered',
    description: 'Describe what you need in plain English. Relay generates production-ready apps in seconds.',
  },
  {
    icon: Shield,
    title: 'Secure by Design',
    description: 'All queries respect your Row-Level Security policies. Your data rules are never bypassed.',
  },
  {
    icon: Code2,
    title: 'Full Code Access',
    description: "Not a black box. Export your code, customize freely, and sync with Git. It's yours.",
  },
]

export function LandingPage() {
  const [isScrolled, setIsScrolled] = useState(false)
  const [prompt, setPrompt] = useState('')
  const [animatedPlaceholder, setAnimatedPlaceholder] = useState('')
  const [phraseIndex, setPhraseIndex] = useState(0)
  const [isTyping, setIsTyping] = useState(true)
  const inputRef = useRef<HTMLTextAreaElement>(null)
  const navigate = useNavigate()

  // Typewriter animation for placeholder
  const animatePlaceholder = useCallback(() => {
    const currentPhrase = PLACEHOLDER_PHRASES[phraseIndex]
    const fullText = currentPhrase
    
    if (isTyping) {
      // Typing forward
      if (animatedPlaceholder.length < fullText.length) {
        const timeout = setTimeout(() => {
          setAnimatedPlaceholder(fullText.slice(0, animatedPlaceholder.length + 1))
        }, 50) // Speed of typing
        return () => clearTimeout(timeout)
      } else {
        // Pause at the end before deleting
        const timeout = setTimeout(() => {
          setIsTyping(false)
        }, 2000) // Pause duration
        return () => clearTimeout(timeout)
      }
    } else {
      // Deleting backward
      if (animatedPlaceholder.length > 0) {
        const timeout = setTimeout(() => {
          setAnimatedPlaceholder(animatedPlaceholder.slice(0, -1))
        }, 30) // Speed of deleting (faster)
        return () => clearTimeout(timeout)
      } else {
        // Move to next phrase
        setPhraseIndex((prev) => (prev + 1) % PLACEHOLDER_PHRASES.length)
        setIsTyping(true)
      }
    }
  }, [animatedPlaceholder, phraseIndex, isTyping])

  useEffect(() => {
    // Only animate when prompt is empty
    if (prompt) return
    return animatePlaceholder()
  }, [animatePlaceholder, prompt])

  // Set page title
  useEffect(() => {
    document.title = 'Relay | Build Internal Apps in Seconds'
  }, [])

  // Track scroll position for header background
  useEffect(() => {
    const handleScroll = () => {
      setIsScrolled(window.scrollY > 20)
    }
    window.addEventListener('scroll', handleScroll)
    return () => window.removeEventListener('scroll', handleScroll)
  }, [])

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault()
    if (prompt.trim()) {
      // Store prompt in localStorage for persistence through auth flow
      localStorage.setItem('pending_prompt', prompt)
      // Navigate to signup/app creation
      navigate('/signup')
    } else {
      // Just open signup if no prompt
      navigate('/signup')
    }
  }

  const handleTemplateClick = (templateId: string) => {
    // Store template selection
    localStorage.setItem('pending_template', templateId)
    navigate('/signup')
  }

  return (
    <div className="relative w-full overflow-hidden bg-white">
      {/* Hero Section with Background */}
      <div className="relative min-h-screen">
        {/* Clean light background */}
        <div 
          className="absolute inset-0 z-0"
          style={{
            background: 'linear-gradient(180deg, #FAFAFA 0%, #F5F5F5 50%, #FAFAFA 100%)'
          }}
        />
        
        {/* AI Neural Network Dot Field */}
        <div className="absolute inset-0 z-[1] overflow-hidden">
          <NeuralDotField />
          
          {/* Center clear zone gradient overlay */}
          <div 
            className="absolute inset-0 pointer-events-none"
            style={{
              background: 'radial-gradient(ellipse 50% 45% at 50% 48%, rgba(250,250,250,0.95) 0%, rgba(250,250,250,0.8) 30%, transparent 70%)'
            }}
          />
        </div>

        {/* Subtle animated glow effects */}
        <div className="absolute inset-0 z-[2] overflow-hidden pointer-events-none">
          {/* Ambient glow orbs */}
          <motion.div
            className="absolute w-[600px] h-[600px] rounded-full"
            style={{
              background: 'radial-gradient(circle, rgba(59, 130, 246, 0.03) 0%, transparent 70%)',
              left: '10%',
              top: '20%',
            }}
            animate={{
              x: [0, 50, 0],
              y: [0, 30, 0],
              scale: [1, 1.1, 1],
            }}
            transition={{
              duration: 15,
              repeat: Infinity,
              ease: 'easeInOut',
            }}
          />
          <motion.div
            className="absolute w-[500px] h-[500px] rounded-full"
            style={{
              background: 'radial-gradient(circle, rgba(6, 182, 212, 0.03) 0%, transparent 70%)',
              right: '5%',
              top: '40%',
            }}
            animate={{
              x: [0, -40, 0],
              y: [0, -20, 0],
              scale: [1, 1.15, 1],
            }}
            transition={{
              duration: 18,
              repeat: Infinity,
              ease: 'easeInOut',
              delay: 2,
            }}
          />
          <motion.div
            className="absolute w-[400px] h-[400px] rounded-full"
            style={{
              background: 'radial-gradient(circle, rgba(139, 92, 246, 0.02) 0%, transparent 70%)',
              left: '60%',
              bottom: '10%',
            }}
            animate={{
              x: [0, 30, 0],
              y: [0, -40, 0],
              scale: [1, 1.2, 1],
            }}
            transition={{
              duration: 20,
              repeat: Infinity,
              ease: 'easeInOut',
              delay: 5,
            }}
          />
        </div>

        {/* Bottom fade gradient - blends hero into white sections */}
        <div 
          className="absolute bottom-0 left-0 right-0 h-[200px] z-[3] pointer-events-none"
          style={{
            background: 'linear-gradient(to bottom, rgba(255,255,255,0) 0%, rgba(255,255,255,1) 100%)'
          }}
        />

        {/* Content Container */}
        <div className="relative z-10 flex flex-col">
          {/* Header - Fixed with scroll-based background */}
          <motion.header 
            initial={{ opacity: 0, y: -20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5 }}
            className={`fixed top-0 left-0 right-0 z-50 px-6 lg:px-10 py-3 transition-all duration-300 ${
              isScrolled 
                ? 'bg-white/80 backdrop-blur-lg border-b border-gray-200 shadow-sm' 
                : 'bg-transparent'
            }`}
          >
            <div className="relative flex items-center justify-between max-w-7xl mx-auto">
              {/* Logo */}
              <Link to="/" className="flex items-center gap-2.5 group">
                <Logo size="md" />
                <span className="text-xl font-semibold text-gray-900 group-hover:text-gray-700 transition-colors">
                  Relay
                </span>
              </Link>

              {/* Nav Links - Centered */}
              <nav className="hidden md:flex items-center gap-8 absolute left-1/2 -translate-x-1/2">
                <a 
                  href="#templates" 
                  className="text-sm font-medium text-gray-600 hover:text-gray-900 transition-colors"
                >
                  Templates
                </a>
                <a 
                  href="#showcase" 
                  className="text-sm font-medium text-gray-600 hover:text-gray-900 transition-colors"
                >
                  Showcase
                </a>
                <a 
                  href="#features" 
                  className="text-sm font-medium text-gray-600 hover:text-gray-900 transition-colors"
                >
                  Features
                </a>
              </nav>

              {/* Auth Buttons */}
          <div className="flex items-center gap-3">
            <Button
              variant="ghost"
              onClick={() => navigate('/login')}
                  className="text-gray-700 hover:text-gray-900"
            >
                  Log in
            </Button>
            <Button
              onClick={() => navigate('/signup')}
                  className="shadow-sm"
                >
                  Get started
                </Button>
              </div>
            </div>
          </motion.header>

          {/* Main Content - Centered Hero */}
          <main className="flex min-h-screen flex-col items-center justify-center px-4 pt-20 pb-32">
            <motion.div
              variants={staggerContainerVariants}
              initial="initial"
              animate="animate"
              className="flex flex-col items-center w-full max-w-[900px]"
            >
              {/* Hero Title with blur-in animation */}
              <motion.div variants={staggerItemVariants} className="mb-6 text-center">
                <h1 className="text-3xl sm:text-4xl lg:text-[48px] font-medium tracking-tight text-gray-900 leading-[1.1]">
                  {HERO_TITLE.split("").map((char, index) => (
                    <motion.span
                      key={index}
                      initial={{ opacity: 0, filter: "blur(12px)" }}
                      animate={{ opacity: 1, filter: "blur(0px)" }}
                      transition={{
                        duration: 0.4,
                        delay: 0.3 + index * 0.025,
                        ease: "easeOut",
                      }}
                      style={{ display: "inline-block" }}
                    >
                      {char === " " ? "\u00A0" : char}
                    </motion.span>
                  ))}
                </h1>
              </motion.div>

              {/* Subtitle - Fade in at midpoint of title */}
              <motion.p 
                className="mb-10 text-center text-md sm:text-lg text-gray-500 max-w-xl leading-relaxed"
                initial={{ opacity: 0, y: 10 }}
                animate={{ opacity: 1, y: 0 }}
                transition={{
                  duration: 0.6,
                  delay: 0.8,
                  ease: "easeOut",
                }}
              >
                Create custom internal apps by chatting with AI.
              </motion.p>

              {/* Create Input Card */}
              <motion.form 
                variants={staggerItemVariants}
                onSubmit={handleSubmit} 
                className="w-full max-w-[760px]"
              >
                {/* Card with enhanced styling */}
                <motion.div 
                  className="bg-white/95 backdrop-blur-sm border border-gray-200 rounded-2xl shadow-xl shadow-gray-200/50 p-3 flex flex-col"
                  whileHover={{ boxShadow: '0 25px 50px -12px rgba(0, 0, 0, 0.15)' }}
                  transition={{ duration: 0.3 }}
                >
                  {/* Textarea Container with Animated Placeholder */}
                  <div className="px-4 py-4 relative">
                    {/* Animated placeholder overlay - only visible when prompt is empty */}
                    {!prompt && (
                      <div className="absolute inset-0 px-4 py-4 pointer-events-none">
                        <span className="text-base sm:text-lg text-gray-400">
                          {STATIC_PREFIX}
                          <span>{animatedPlaceholder}</span>
                          <motion.span 
                            animate={{ opacity: [1, 0, 1] }}
                            transition={{ duration: 1, repeat: Infinity }}
                            className="text-gray-400"
                          >
                            |
                          </motion.span>
                        </span>
                      </div>
                    )}
                    <textarea
                      ref={inputRef}
                      value={prompt}
                      onChange={(e) => setPrompt(e.target.value)}
                      className="w-full h-[80px] text-base sm:text-lg bg-transparent border-0 text-gray-900 placeholder:text-gray-400 focus:ring-0 focus:outline-none resize-none relative z-10"
                    />
                  </div>

                  {/* Bottom Controls */}
                  <div className="flex items-center justify-end px-3 pb-2">
                    {/* Generate Button */}
                    <motion.div
                      whileHover={{ scale: 1.02 }}
                      whileTap={{ scale: 0.98 }}
                    >
                      <Button
                        type="submit"
                        className="px-5 shadow-sm bg-gray-900 hover:bg-gray-800 text-white"
                      >
                        Start Building
                        <ArrowRight className="ml-2 h-4 w-4" />
                      </Button>
                    </motion.div>
                  </div>
                </motion.div>
              </motion.form>

              {/* Trust signal - Three dots with checkmarks */}
              <motion.div 
                className="mt-10 flex items-center justify-center gap-6 sm:gap-10"
                initial={{ opacity: 0, y: 10 }}
                animate={{ opacity: 1, y: 0 }}
                transition={{ delay: 1.2, duration: 0.5 }}
              >
                {[
                  'Free to start',
                  'No credit card required',
                  'Ship in minutes'
                ].map((text, index) => (
                  <motion.span 
                    key={text}
                    className="flex items-center gap-2 text-sm text-gray-600"
                    initial={{ opacity: 0, y: 5 }}
                    animate={{ opacity: 1, y: 0 }}
                    transition={{ delay: 1.3 + index * 0.1, duration: 0.4 }}
                  >
                    <svg 
                      className="h-4 w-4 text-gray-500" 
                      fill="none" 
                      viewBox="0 0 24 24" 
                      stroke="currentColor" 
                      strokeWidth={2}
                    >
                      <path strokeLinecap="round" strokeLinejoin="round" d="M5 13l4 4L19 7" />
                    </svg>
                    {text}
                  </motion.span>
                ))}
              </motion.div>

            </motion.div>
          </main>
        </div>
      </div>

      {/* Built with Relay Section */}
      <section id="showcase" className="w-full bg-white py-24 px-6 lg:px-10">
        <div className="max-w-7xl mx-auto">
          {/* Section Header */}
          <motion.div 
            className="mb-10"
            initial={{ opacity: 0, y: 20 }}
            whileInView={{ opacity: 1, y: 0 }}
            viewport={{ once: true }}
            transition={{ duration: 0.5 }}
          >
            <h2 className="text-2xl sm:text-3xl font-semibold text-gray-900 mb-2">
              Built with Relay
            </h2>
            <p className="text-gray-600">
              Real apps made by real teams
            </p>
          </motion.div>

          {/* Showcase Grid */}
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4 sm:gap-6">
            {SHOWCASE_APPS.map((app, index) => (
              <motion.div 
                key={index}
                className={`group aspect-[4/3] rounded-xl overflow-hidden bg-gradient-to-br ${app.gradient} border border-gray-200 p-5 flex flex-col justify-between cursor-pointer`}
                initial={{ opacity: 0, y: 20 }}
                whileInView={{ opacity: 1, y: 0 }}
                viewport={{ once: true }}
                transition={{ duration: 0.5, delay: index * 0.1 }}
                whileHover={{ 
                  y: -4, 
                  boxShadow: '0 12px 24px -8px rgba(0, 0, 0, 0.1)',
                  borderColor: 'rgb(209, 213, 219)'
                }}
              >
                {/* Mock app UI */}
                <div className="space-y-2">
                  <div className="h-2 w-16 bg-gray-300/50 rounded" />
                  <div className="h-2 w-24 bg-gray-200/50 rounded" />
                </div>
                <div className="flex-1 my-3">
                  <div className="grid grid-cols-3 gap-1.5 h-full">
                    {[...Array(6)].map((_, i) => (
                      <motion.div 
                        key={i} 
                        className="bg-white/60 rounded-md border border-gray-100/50"
                        initial={{ opacity: 0.6 }}
                        whileHover={{ opacity: 1 }}
                      />
                    ))}
                  </div>
                </div>
                <div>
                  <p className="text-sm font-medium text-gray-900 group-hover:text-gray-700 transition-colors">{app.title}</p>
                  <p className="text-xs text-gray-500">{app.category}</p>
                </div>
              </motion.div>
            ))}
          </div>
        </div>
      </section>

      {/* App Templates Section */}
      <section id="templates" className="w-full bg-gray-50 border-y border-gray-200 py-24 px-6 lg:px-10">
        <div className="max-w-7xl mx-auto">
          {/* Section Header */}
          <motion.div 
            className="flex items-end justify-between mb-10"
            initial={{ opacity: 0, y: 20 }}
            whileInView={{ opacity: 1, y: 0 }}
            viewport={{ once: true }}
            transition={{ duration: 0.5 }}
          >
            <div>
              <h2 className="text-2xl sm:text-3xl font-semibold text-gray-900 mb-2">
                Start with a Template
              </h2>
              <p className="text-gray-600">
                Jump-start your next internal app
              </p>
            </div>
            <motion.button 
              type="button"
              onClick={() => navigate('/signup')}
              className="text-sm font-medium text-gray-900 hover:text-gray-700 transition-colors flex items-center gap-1.5 group"
              whileHover={{ x: 3 }}
            >
              View all
              <ArrowRight className="h-4 w-4 group-hover:translate-x-0.5 transition-transform" />
            </motion.button>
          </motion.div>

          {/* Templates Grid */}
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4 sm:gap-6">
            {APP_TEMPLATES.map((template, index) => (
              <motion.div 
                key={template.id}
                className="group cursor-pointer"
                onClick={() => handleTemplateClick(template.id)}
                initial={{ opacity: 0, y: 20 }}
                whileInView={{ opacity: 1, y: 0 }}
                viewport={{ once: true }}
                transition={{ duration: 0.5, delay: index * 0.05 }}
              >
                <motion.div 
                  className={`aspect-[4/3] rounded-xl overflow-hidden ${template.color} border border-gray-200 ${template.borderHover} mb-3 flex items-center justify-center transition-colors`}
                  whileHover={{ 
                    y: -4,
                    boxShadow: '0 12px 24px -8px rgba(0, 0, 0, 0.08)'
                  }}
                >
                  <motion.div
                    whileHover={{ scale: 1.1, rotate: 3 }}
                    transition={{ type: 'spring', stiffness: 300 }}
                  >
                    <template.icon className={`h-10 w-10 ${template.iconColor}`} />
                  </motion.div>
                </motion.div>
                <h3 className="text-sm font-medium text-gray-900 mb-0.5 group-hover:text-gray-700 transition-colors">
                  {template.title}
                </h3>
                <p className="text-xs text-gray-500">
                  {template.description}
                </p>
              </motion.div>
            ))}
          </div>
        </div>
      </section>

      {/* Features Section */}
      <section id="features" className="w-full bg-white py-24 px-6 lg:px-10">
        <div className="max-w-7xl mx-auto">
          <motion.div 
            className="text-center mb-14"
            initial={{ opacity: 0, y: 20 }}
            whileInView={{ opacity: 1, y: 0 }}
            viewport={{ once: true }}
            transition={{ duration: 0.5 }}
          >
            <h2 className="text-2xl sm:text-3xl font-semibold text-gray-900 mb-3">
              Why teams choose Relay
            </h2>
            <p className="text-gray-600 max-w-xl mx-auto">
              Build internal tools that are secure, fast, and easy to maintain.
            </p>
          </motion.div>

          <div className="grid md:grid-cols-3 gap-6 max-w-4xl mx-auto">
            {FEATURES.map((feature, index) => (
              <motion.div 
                key={feature.title}
                className="group p-6 bg-white rounded-xl border border-gray-200"
                initial={{ opacity: 0, y: 20 }}
                whileInView={{ opacity: 1, y: 0 }}
                viewport={{ once: true }}
                transition={{ duration: 0.5, delay: index * 0.1 }}
                whileHover={{ 
                  y: -4,
                  boxShadow: '0 12px 24px -8px rgba(0, 0, 0, 0.08)',
                  borderColor: 'rgb(209, 213, 219)'
                }}
              >
                <motion.div 
                  className="h-12 w-12 rounded-xl bg-gray-100 flex items-center justify-center mb-5"
                  whileHover={{ scale: 1.05, rotate: 3 }}
                  transition={{ type: 'spring', stiffness: 300 }}
                >
                  <feature.icon className="h-6 w-6 text-gray-700" />
                </motion.div>
                <h3 className="font-semibold text-gray-900 mb-2 group-hover:text-gray-700 transition-colors">
                  {feature.title}
                </h3>
                <p className="text-sm text-gray-600 leading-relaxed">
                  {feature.description}
                </p>
              </motion.div>
            ))}
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="w-full bg-gray-900 py-24 px-6 lg:px-10">
        <motion.div 
          className="max-w-3xl mx-auto text-center"
          initial={{ opacity: 0, y: 20 }}
          whileInView={{ opacity: 1, y: 0 }}
          viewport={{ once: true }}
          transition={{ duration: 0.5 }}
        >
          <h2 className="text-2xl sm:text-4xl font-semibold text-white mb-4">
            Ready to build?
          </h2>
          <p className="text-gray-400 mb-10 max-w-lg mx-auto text-lg">
            Start building your first internal app in minutes.
            No credit card required.
          </p>
          <div className="flex flex-col sm:flex-row items-center justify-center gap-4">
            <motion.div whileHover={{ scale: 1.02 }} whileTap={{ scale: 0.98 }}>
              <Button
                size="lg"
                onClick={() => navigate('/signup')}
                className="bg-white text-gray-900 hover:bg-gray-100 w-full sm:w-auto px-8 shadow-lg"
              >
                Start Building Free
                <ArrowRight className="ml-2 h-4 w-4" />
              </Button>
            </motion.div>
            <motion.div whileHover={{ scale: 1.02 }} whileTap={{ scale: 0.98 }}>
              <button
                onClick={() => window.location.href = 'mailto:hello@relay.app'}
                className="h-11 px-8 rounded-md text-sm font-medium border border-gray-600 bg-transparent text-white hover:bg-white/10 transition-colors w-full sm:w-auto"
              >
                Book a Demo
              </button>
            </motion.div>
          </div>
        </motion.div>
      </section>

      {/* Footer */}
      <footer className="w-full bg-gray-50 border-t border-gray-200 pt-16 pb-20 px-6 lg:px-10">
        <div className="max-w-7xl mx-auto">
          <div className="grid grid-cols-2 md:grid-cols-4 gap-10">
            {/* Logo */}
            <div className="col-span-2 md:col-span-1">
              <Link to="/" className="flex items-center gap-2.5 mb-4 group">
                <Logo size="md" />
                <span className="text-lg font-semibold text-gray-900 group-hover:text-gray-700 transition-colors">Relay</span>
              </Link>
              <p className="text-sm text-gray-600 max-w-xs leading-relaxed">
                AI-powered internal apps. Build faster, ship sooner.
              </p>
            </div>

            {/* Product */}
            <div>
              <h4 className="text-sm font-semibold text-gray-900 mb-4">Product</h4>
              <ul className="space-y-3">
                <li>
                  <a href="#templates" className="text-sm text-gray-600 hover:text-gray-900 transition-colors">
                    Templates
                  </a>
                </li>
                <li>
                  <a href="#showcase" className="text-sm text-gray-600 hover:text-gray-900 transition-colors">
                    Showcase
                  </a>
                </li>
                <li>
                  <a href="#features" className="text-sm text-gray-600 hover:text-gray-900 transition-colors">
                    Features
                  </a>
                </li>
                <li>
                  <a href="mailto:support@relay.app" className="text-sm text-gray-600 hover:text-gray-900 transition-colors">
                    Support
                  </a>
                </li>
              </ul>
            </div>

            {/* Legal */}
            <div>
              <h4 className="text-sm font-semibold text-gray-900 mb-4">Legal</h4>
              <ul className="space-y-3">
                <li>
                  <Link to="/privacy" className="text-sm text-gray-600 hover:text-gray-900 transition-colors">
                    Privacy Policy
                  </Link>
                </li>
                <li>
                  <Link to="/terms" className="text-sm text-gray-600 hover:text-gray-900 transition-colors">
                    Terms of Service
                  </Link>
                </li>
              </ul>
            </div>

            {/* Connect */}
            <div>
              <h4 className="text-sm font-semibold text-gray-900 mb-4">Connect</h4>
              <ul className="space-y-3">
                <li>
                  <a 
                    href="https://twitter.com/relay" 
                    target="_blank" 
                    rel="noopener noreferrer"
                    className="text-sm text-gray-600 hover:text-gray-900 transition-colors"
                  >
                    X / Twitter
                  </a>
                </li>
                <li>
                  <a 
                    href="https://github.com/relay" 
                    target="_blank" 
                    rel="noopener noreferrer"
                    className="text-sm text-gray-600 hover:text-gray-900 transition-colors"
                  >
                    GitHub
                  </a>
                </li>
                <li>
                  <a 
                    href="mailto:hello@relay.app"
                    className="text-sm text-gray-600 hover:text-gray-900 transition-colors"
                  >
                    Contact
                  </a>
                </li>
              </ul>
            </div>
          </div>

          <div className="mt-14 pt-8 border-t border-gray-200 flex flex-col sm:flex-row items-center justify-between gap-4">
            <p className="text-sm text-gray-500">
              © {new Date().getFullYear()} Relay. All rights reserved.
            </p>
            <div className="flex items-center gap-4">
              <a 
                href="https://twitter.com/relay" 
                target="_blank" 
                rel="noopener noreferrer"
                className="text-gray-400 hover:text-gray-600 transition-colors"
              >
                <svg className="h-5 w-5" fill="currentColor" viewBox="0 0 24 24">
                  <path d="M18.244 2.25h3.308l-7.227 8.26 8.502 11.24H16.17l-5.214-6.817L4.99 21.75H1.68l7.73-8.835L1.254 2.25H8.08l4.713 6.231zm-1.161 17.52h1.833L7.084 4.126H5.117z" />
                </svg>
              </a>
              <a 
                href="https://github.com/relay" 
                target="_blank" 
                rel="noopener noreferrer"
                className="text-gray-400 hover:text-gray-600 transition-colors"
              >
                <svg className="h-5 w-5" fill="currentColor" viewBox="0 0 24 24">
                  <path d="M12 2C6.477 2 2 6.484 2 12.017c0 4.425 2.865 8.18 6.839 9.504.5.092.682-.217.682-.483 0-.237-.008-.868-.013-1.703-2.782.605-3.369-1.343-3.369-1.343-.454-1.158-1.11-1.466-1.11-1.466-.908-.62.069-.608.069-.608 1.003.07 1.531 1.032 1.531 1.032.892 1.53 2.341 1.088 2.91.832.092-.647.35-1.088.636-1.338-2.22-.253-4.555-1.113-4.555-4.951 0-1.093.39-1.988 1.029-2.688-.103-.253-.446-1.272.098-2.65 0 0 .84-.27 2.75 1.026A9.564 9.564 0 0112 6.844c.85.004 1.705.115 2.504.337 1.909-1.296 2.747-1.027 2.747-1.027.546 1.379.202 2.398.1 2.651.64.7 1.028 1.595 1.028 2.688 0 3.848-2.339 4.695-4.566 4.943.359.309.678.92.678 1.855 0 1.338-.012 2.419-.012 2.747 0 .268.18.58.688.482A10.019 10.019 0 0022 12.017C22 6.484 17.522 2 12 2z" />
                </svg>
              </a>
              <a 
                href="https://linkedin.com/company/relay" 
                target="_blank" 
                rel="noopener noreferrer"
                className="text-gray-400 hover:text-gray-600 transition-colors"
              >
                <svg className="h-5 w-5" fill="currentColor" viewBox="0 0 24 24">
                  <path d="M20.447 20.452h-3.554v-5.569c0-1.328-.027-3.037-1.852-3.037-1.853 0-2.136 1.445-2.136 2.939v5.667H9.351V9h3.414v1.561h.046c.477-.9 1.637-1.85 3.37-1.85 3.601 0 4.267 2.37 4.267 5.455v6.286zM5.337 7.433c-1.144 0-2.063-.926-2.063-2.065 0-1.138.92-2.063 2.063-2.063 1.14 0 2.064.925 2.064 2.063 0 1.139-.925 2.065-2.064 2.065zm1.782 13.019H3.555V9h3.564v11.452zM22.225 0H1.771C.792 0 0 .774 0 1.729v20.542C0 23.227.792 24 1.771 24h20.451C23.2 24 24 23.227 24 22.271V1.729C24 .774 23.2 0 22.222 0h.003z" />
                </svg>
              </a>
            </div>
          </div>
        </div>
      </footer>
    </div>
  )
}
