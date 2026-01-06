/**
 * Fuzzy search utility for approximate string matching.
 * 
 * Supports:
 * - Substring matching (highest priority)
 * - Sequential character matching (for typos/abbreviations)
 * - Case-insensitive matching
 * - Multi-field searching with weighted scoring
 */

/**
 * Calculate fuzzy match score between query and target string.
 * Returns a score where higher is better, or -1 if no match.
 */
export function fuzzyMatch(query: string, target: string): number {
  if (!query || !target) return -1
  
  const q = query.toLowerCase()
  const t = target.toLowerCase()
  
  // Exact match - highest score
  if (t === q) return 1000
  
  // Starts with query - very high score
  if (t.startsWith(q)) return 900 + (q.length / t.length) * 50
  
  // Contains query as substring - high score
  const substringIndex = t.indexOf(q)
  if (substringIndex !== -1) {
    // Score based on how early it appears and query length ratio
    return 800 - substringIndex + (q.length / t.length) * 50
  }
  
  // Word boundary match (query matches start of a word)
  const words = t.split(/[\s\-_]+/)
  for (let i = 0; i < words.length; i++) {
    if (words[i].startsWith(q)) {
      return 700 - i * 10 + (q.length / words[i].length) * 30
    }
  }
  
  // Sequential character match (for abbreviations/typos)
  let queryIndex = 0
  let score = 0
  let lastMatchIndex = -1
  let consecutiveMatches = 0
  
  for (let i = 0; i < t.length && queryIndex < q.length; i++) {
    if (t[i] === q[queryIndex]) {
      // Bonus for consecutive matches
      if (lastMatchIndex === i - 1) {
        consecutiveMatches++
        score += 10 + consecutiveMatches * 5
      } else {
        consecutiveMatches = 0
        score += 10
      }
      
      // Bonus for matching at word boundaries
      if (i === 0 || /[\s\-_]/.test(t[i - 1])) {
        score += 15
      }
      
      lastMatchIndex = i
      queryIndex++
    }
  }
  
  // If we matched all query characters, return the score
  if (queryIndex === q.length) {
    // Penalize if target is much longer than query
    const lengthPenalty = Math.max(0, (t.length - q.length) / t.length) * 50
    return Math.max(1, score - lengthPenalty)
  }
  
  // No match
  return -1
}

/**
 * Search configuration for multi-field fuzzy search
 */
export interface FuzzySearchField<T> {
  /** Function to get the field value from an item */
  getValue: (item: T) => string | null | undefined
  /** Weight multiplier for this field (default: 1) */
  weight?: number
}

/**
 * Perform fuzzy search on an array of items.
 * 
 * @param items - Array of items to search
 * @param query - Search query string
 * @param fields - Fields to search with optional weights
 * @returns Filtered and sorted array (best matches first)
 */
export function fuzzySearch<T>(
  items: T[],
  query: string,
  fields: FuzzySearchField<T>[]
): T[] {
  if (!query.trim()) return items
  
  const q = query.trim()
  
  // Score each item
  const scored = items
    .map(item => {
      let bestScore = -1
      
      for (const field of fields) {
        const value = field.getValue(item)
        if (!value) continue
        
        const weight = field.weight ?? 1
        const score = fuzzyMatch(q, value)
        
        if (score > 0) {
          const weightedScore = score * weight
          if (weightedScore > bestScore) {
            bestScore = weightedScore
          }
        }
      }
      
      return { item, score: bestScore }
    })
    .filter(({ score }) => score > 0)
  
  // Sort by score (descending)
  scored.sort((a, b) => b.score - a.score)
  
  return scored.map(({ item }) => item)
}

/**
 * Simple fuzzy filter that returns items matching the query.
 * Use this when you don't need sorting by relevance.
 */
export function fuzzyFilter<T>(
  items: T[],
  query: string,
  fields: FuzzySearchField<T>[]
): T[] {
  if (!query.trim()) return items
  
  const q = query.trim()
  
  return items.filter(item => {
    for (const field of fields) {
      const value = field.getValue(item)
      if (!value) continue
      
      if (fuzzyMatch(q, value) > 0) {
        return true
      }
    }
    return false
  })
}

