/**
 * Terms of Service Page - Clean, enterprise-grade
 * Following STYLE_GUIDE.md: Light, minimal, professional
 */
import { useState, useEffect } from 'react'
import { Link, useNavigate } from 'react-router-dom'
import { Button } from '@/components/ui/button'
import { Logo } from '@/components/Logo'
import { ArrowRight } from 'lucide-react'

export function TermsOfServicePage() {
  const navigate = useNavigate()
  const [isScrolled, setIsScrolled] = useState(false)

  // Set page title
  useEffect(() => {
    document.title = 'Terms of Service | Relay'
  }, [])

  useEffect(() => {
    const handleScroll = () => {
      setIsScrolled(window.scrollY > 20)
    }
    window.addEventListener('scroll', handleScroll)
    return () => window.removeEventListener('scroll', handleScroll)
  }, [])

  return (
    <div className="min-h-screen bg-white">
      {/* Fixed Header */}
      <header
        className={`fixed top-0 left-0 right-0 z-50 transition-all duration-300 ${
          isScrolled
            ? 'bg-white/80 backdrop-blur-md border-b border-gray-200 shadow-sm'
            : 'bg-white border-b border-gray-200'
        }`}
      >
        <div className="mx-auto flex max-w-7xl items-center justify-between px-6 py-4">
          <Link to="/" className="flex items-center gap-2.5">
            <Logo size="md" />
            <span className="text-xl font-semibold text-gray-900">Relay</span>
          </Link>

          <nav className="hidden md:flex items-center gap-8">
            <Link to="/#features" className="text-sm font-medium text-gray-600 hover:text-gray-900 transition-colors">
              Features
            </Link>
            <Link to="/#integrations" className="text-sm font-medium text-gray-600 hover:text-gray-900 transition-colors">
              Integrations
            </Link>
          </nav>

          <div className="flex items-center gap-3">
            <Button
              variant="ghost"
              onClick={() => navigate('/login')}
              className="text-gray-700"
            >
              Sign In
            </Button>
            <Button onClick={() => navigate('/signup')}>
              Start Building
              <ArrowRight className="ml-2 h-4 w-4" />
            </Button>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <main className="pt-28 pb-20 px-6">
        <div className="max-w-3xl mx-auto">
          <h1 className="text-4xl font-bold text-gray-900 mb-4">
            Terms of Service
          </h1>
          <p className="text-gray-500 mb-12">
            Last updated: January 6, 2026
          </p>

          <div className="prose prose-gray max-w-none">
            <section className="mb-10">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">1. Acceptance of Terms</h2>
              <p className="text-gray-700 leading-relaxed mb-4">
                By accessing or using Relay's services ("Services"), you agree to be bound by these Terms of Service ("Terms"). If you do not agree to these Terms, you may not access or use the Services.
              </p>
              <p className="text-gray-700 leading-relaxed">
                We reserve the right to update these Terms at any time. Continued use of the Services after changes constitutes acceptance of the modified Terms.
              </p>
            </section>

            <section className="mb-10">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">2. Description of Services</h2>
              <p className="text-gray-700 leading-relaxed">
                Relay is an AI-powered internal application generation platform that enables users to create internal applications through natural language conversations. The platform connects to your data sources, discovers resources, and generates interactive applications that respect your security policies. Our Services include web applications, APIs, and related tools and features.
              </p>
            </section>

            <section className="mb-10">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">3. Account Registration</h2>
              <p className="text-gray-700 leading-relaxed mb-4">
                To use certain features of the Services, you must register for an account. You agree to:
              </p>
              <ul className="list-disc pl-6 text-gray-700 space-y-2">
                <li>Provide accurate and complete registration information</li>
                <li>Maintain the security of your account credentials</li>
                <li>Notify us immediately of any unauthorized access</li>
                <li>Accept responsibility for all activities under your account</li>
                <li>Use the Services only for lawful purposes</li>
              </ul>
            </section>

            <section className="mb-10">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">4. Organizations and Teams</h2>
              <p className="text-gray-700 leading-relaxed mb-4">
                Relay allows you to create and manage organizations. As an organization owner or administrator:
              </p>
              <ul className="list-disc pl-6 text-gray-700 space-y-2">
                <li>You are responsible for managing team member access and permissions</li>
                <li>You control which data sources and resources are connected to your organization</li>
                <li>You are responsible for ensuring team members comply with these Terms</li>
                <li>You may invite or remove team members at any time</li>
              </ul>
            </section>

            <section className="mb-10">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">5. Subscription and Payments</h2>
              <p className="text-gray-700 leading-relaxed mb-4">
                Some Services require payment of fees. By subscribing to a paid plan, you agree to:
              </p>
              <ul className="list-disc pl-6 text-gray-700 space-y-2 mb-4">
                <li>Pay all applicable fees as described at the time of purchase</li>
                <li>Provide valid payment information</li>
                <li>Authorize recurring charges for subscription plans</li>
              </ul>
              <p className="text-gray-700 leading-relaxed">
                Subscriptions automatically renew unless cancelled before the renewal date. Refunds are handled in accordance with our refund policy.
              </p>
            </section>

            <section className="mb-10">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">6. Data and Security</h2>
              <p className="text-gray-700 leading-relaxed mb-4">
                When connecting your data sources to Relay:
              </p>
              <ul className="list-disc pl-6 text-gray-700 space-y-2">
                <li>All connection credentials are encrypted at rest using industry-standard encryption</li>
                <li>All data queries use your user JWT tokens, ensuring your Row-Level Security (RLS) policies are enforced</li>
                <li>We never store your production data; all queries are proxied in real-time</li>
                <li>You maintain full control and ownership of your data at all times</li>
                <li>You are responsible for ensuring you have the right to connect and use the data sources</li>
              </ul>
            </section>

            <section className="mb-10">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">7. Acceptable Use</h2>
              <p className="text-gray-700 leading-relaxed mb-4">
                You agree not to use the Services to:
              </p>
              <ul className="list-disc pl-6 text-gray-700 space-y-2">
                <li>Build applications that violate any applicable laws or regulations</li>
                <li>Access or attempt to access data you are not authorized to use</li>
                <li>Distribute malware or engage in harmful activities</li>
                <li>Attempt to gain unauthorized access to our systems</li>
                <li>Resell or redistribute Services without authorization</li>
                <li>Interfere with or disrupt the Services or servers</li>
                <li>Use the Services to compete directly with Relay</li>
              </ul>
            </section>

            <section className="mb-10">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">8. Intellectual Property</h2>
              <p className="text-gray-700 leading-relaxed mb-4">
                <strong>Your Content:</strong> You retain ownership of content you upload to the Services (such as application configurations and connected data sources). You grant us a license to use this content solely to provide the Services.
              </p>
              <p className="text-gray-700 leading-relaxed mb-4">
                <strong>Generated Applications:</strong> Subject to these Terms and your subscription plan, you own the applications generated through our Services. You may use and deploy generated applications for your internal business purposes.
              </p>
              <p className="text-gray-700 leading-relaxed">
                <strong>Our Content:</strong> The Services, including software, design, and trademarks, are owned by Relay and protected by intellectual property laws.
              </p>
            </section>

            <section className="mb-10">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">9. Disclaimers</h2>
              <p className="text-gray-700 leading-relaxed mb-4">
                THE SERVICES ARE PROVIDED "AS IS" WITHOUT WARRANTIES OF ANY KIND. WE DISCLAIM ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING:
              </p>
              <ul className="list-disc pl-6 text-gray-700 space-y-2">
                <li>Merchantability and fitness for a particular purpose</li>
                <li>Non-infringement</li>
                <li>Accuracy or reliability of AI-generated content</li>
                <li>Uninterrupted or error-free service</li>
              </ul>
            </section>

            <section className="mb-10">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">10. Limitation of Liability</h2>
              <p className="text-gray-700 leading-relaxed">
                TO THE MAXIMUM EXTENT PERMITTED BY LAW, RELAY SHALL NOT BE LIABLE FOR ANY INDIRECT, INCIDENTAL, SPECIAL, CONSEQUENTIAL, OR PUNITIVE DAMAGES, OR ANY LOSS OF PROFITS OR REVENUES, WHETHER INCURRED DIRECTLY OR INDIRECTLY. OUR TOTAL LIABILITY SHALL NOT EXCEED THE AMOUNT PAID BY YOU FOR THE SERVICES IN THE TWELVE MONTHS PRECEDING THE CLAIM.
              </p>
            </section>

            <section className="mb-10">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">11. Termination</h2>
              <p className="text-gray-700 leading-relaxed">
                We may suspend or terminate your access to the Services at any time for violation of these Terms or for any other reason at our discretion. Upon termination, your right to use the Services will cease immediately. You may export your application configurations before termination. Provisions that should survive termination will remain in effect.
              </p>
            </section>

            <section className="mb-10">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">12. Governing Law</h2>
              <p className="text-gray-700 leading-relaxed">
                These Terms shall be governed by and construed in accordance with the laws of the State of Delaware, without regard to its conflict of law provisions. Any disputes arising from these Terms shall be resolved in the courts of Delaware.
              </p>
            </section>

            <section className="mb-10">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">13. Contact Us</h2>
              <p className="text-gray-700 leading-relaxed">
                If you have any questions about these Terms, please contact us at{' '}
                <a href="mailto:legal@relay.app" className="text-gray-900 font-medium hover:underline">
                  legal@relay.app
                </a>
              </p>
            </section>
          </div>
        </div>
      </main>

      {/* Footer */}
      <footer className="bg-white border-t border-gray-200">
        <div className="mx-auto max-w-7xl px-6 py-12">
          <div className="grid grid-cols-2 md:grid-cols-4 gap-8">
            {/* Logo & Description */}
            <div className="col-span-2 md:col-span-1">
              <Link to="/" className="flex items-center gap-2.5 mb-4">
                <Logo size="md" />
                <span className="text-lg font-semibold text-gray-900">Relay</span>
              </Link>
              <p className="text-sm text-gray-600 max-w-xs">
                AI-powered internal apps platform. Build faster, ship sooner.
              </p>
            </div>

            {/* Product */}
            <div>
              <h4 className="text-sm font-semibold text-gray-900 mb-4">Product</h4>
              <ul className="space-y-3">
                <li>
                  <Link to="/#features" className="text-sm text-gray-600 hover:text-gray-900 transition-colors">
                    Features
                  </Link>
                </li>
                <li>
                  <Link to="/#integrations" className="text-sm text-gray-600 hover:text-gray-900 transition-colors">
                    Integrations
                  </Link>
                </li>
              </ul>
            </div>

            {/* Company */}
            <div>
              <h4 className="text-sm font-semibold text-gray-900 mb-4">Company</h4>
              <ul className="space-y-3">
                <li>
                  <a href="mailto:hello@relay.app" className="text-sm text-gray-600 hover:text-gray-900 transition-colors">
                    Contact
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
                  <Link to="/terms" className="text-sm text-gray-900 font-medium">
                    Terms of Service
                  </Link>
                </li>
              </ul>
            </div>
          </div>

          <div className="mt-12 pt-8 border-t border-gray-200 flex flex-col sm:flex-row items-center justify-between gap-4">
            <p className="text-sm text-gray-500">
              © {new Date().getFullYear()} Relay. All rights reserved.
            </p>
          </div>
        </div>
      </footer>
    </div>
  )
}

