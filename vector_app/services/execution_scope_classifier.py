"""
Execution Scope Classifier Service

Classifies user requests into execution cost tiers (XS, S, M, L, XL)
for pricing and resource allocation purposes.

This service estimates the complexity and scope of AI agent runs
without actually solving the task.
"""
import logging
import re
from dataclasses import dataclass
from enum import Enum
from typing import Dict, List, Optional

from django.conf import settings
import httpx

from vector_app.ai.models import AIModel
from vector_app.prompts.execution_scope import (
    EXECUTION_SCOPE_SYSTEM_PROMPT,
    build_execution_scope_prompt,
)

logger = logging.getLogger(__name__)


class ExecutionScope(Enum):
    """Execution cost tier buckets."""
    XS = "XS"  # Explanation only, no code or changes
    S = "S"    # Small, localized change (1-2 files or components)
    M = "M"    # Feature addition touching multiple areas (UI + logic or data oe integration)
    L = "L"    # Large change or app generation touching many systems or components
    XL = "XL"  # Full app regeneration, major refactor, or ambiguous large scope


@dataclass
class ExecutionScopeResult:
    """Result of execution scope classification."""
    scope: ExecutionScope
    confidence: float
    raw_response: str
    
    def to_dict(self) -> Dict[str, any]:
        """Convert to dictionary for serialization."""
        return {
            "scope": self.scope.value,
            "confidence": self.confidence,
            "raw_response": self.raw_response,
        }


# Mapping of scope to estimated compute units (for pricing)
SCOPE_COMPUTE_UNITS: Dict[ExecutionScope, int] = {
    ExecutionScope.XS: 20,
    ExecutionScope.S: 50,
    ExecutionScope.M: 80,
    ExecutionScope.L: 140,
    ExecutionScope.XL: 200,
}


class ExecutionScopeClassifier:
    """
    Classifies user requests into execution cost tiers.
    
    Uses LLM classification with heuristic fallbacks to estimate
    the complexity and resource requirements of an AI agent run.
    
    Designed to be called before execution for pricing/billing purposes.
    """
    
    OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions"
    
    # Heuristic keywords for quick classification
    EXPLANATION_KEYWORDS = [
        "what is", "what are", "how does", "how do", "explain",
        "describe", "tell me about", "what does", "why is", "why does",
        "can you explain", "help me understand", "what's the difference",
    ]
    
    SMALL_CHANGE_KEYWORDS = [
        "change the color", "fix typo", "rename", "update text",
        "change the text", "adjust padding", "tweak", "small change",
        "fix the style", "change font", "update the label",
    ]
    
    MEDIUM_CHANGE_KEYWORDS = [
        "add a button", "add a feature", "add search", "add a search",
        "add filter", "add a filter", "add pagination", "implement",
        "create a component", "add validation", "add a modal",
        "add a form field", "search feature", "filter feature",
    ]
    
    LARGE_CHANGE_KEYWORDS = [
        "build me", "create an app", "generate", "new application",
        "build a", "create a dashboard", "make me an app",
        "build an api", "create a system",
    ]
    
    XL_CHANGE_KEYWORDS = [
        "rebuild", "start over", "completely rewrite", "major refactor",
        "redesign everything", "from scratch", "regenerate the entire",
        "rebuild the whole", "full rewrite",
    ]
    
    def __init__(self):
        self.api_key = getattr(settings, 'OPENROUTER_API_KEY', None) or \
                       getattr(settings, 'OPENAI_API_KEY', None)
        self.app_name = getattr(settings, 'OPENROUTER_APP_NAME', 'Internal Apps Builder')
        self.site_url = getattr(settings, 'BASE_URL', 'http://localhost:8001')
    
    def _build_headers(self) -> Dict[str, str]:
        """Build API headers for OpenRouter."""
        return {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json",
            "HTTP-Referer": self.site_url,
            "X-Title": self.app_name,
        }
    
    def classify(
        self,
        user_prompt: str,
        model: AIModel = AIModel.CLAUDE_HAIKU_4_5,
        use_heuristics_only: bool = False,
    ) -> ExecutionScopeResult:
        """
        Classify the user's prompt into an execution cost tier.
        
        Args:
            user_prompt: The user's request
            model: LLM model to use for classification (defaults to Haiku for cost efficiency)
            use_heuristics_only: If True, skip LLM and use only keyword heuristics
            
        Returns:
            ExecutionScopeResult with the classified scope
        """
        prompt_lower = user_prompt.lower().strip()
        
        # Try heuristic classification first (fast path)
        heuristic_result = self._heuristic_classify(prompt_lower)
        if heuristic_result and (heuristic_result.confidence >= 0.85 or use_heuristics_only):
            logger.info(f"Heuristic scope classification: {heuristic_result.scope.value} ({heuristic_result.confidence:.0%})")
            return heuristic_result
        
        # Use LLM for more nuanced classification
        try:
            llm_result = self._llm_classify(user_prompt, model)
            if llm_result:
                logger.info(f"LLM scope classification: {llm_result.scope.value}")
                return llm_result
        except Exception as e:
            logger.warning(f"LLM scope classification failed: {e}, falling back to heuristics")
        
        # Fallback to heuristic result or default to M (middle ground)
        if heuristic_result:
            return heuristic_result
        
        # Default: prefer larger bucket when uncertain (per spec)
        return ExecutionScopeResult(
            scope=ExecutionScope.M,
            confidence=0.5,
            raw_response="fallback",
        )
    
    def _check_keywords(self, message: str, keywords: List[str]) -> bool:
        """Check if message contains any of the keywords."""
        return any(kw in message for kw in keywords)
    
    def _count_keyword_matches(self, message: str, keywords: List[str]) -> int:
        """Count how many keywords match in the message."""
        return sum(1 for kw in keywords if kw in message)
    
    def _heuristic_classify(self, prompt_lower: str) -> Optional[ExecutionScopeResult]:
        """
        Use keyword heuristics for fast classification.
        
        Returns None if confidence is too low.
        """
        # Check for XL first (rebuild, rewrite)
        xl_count = self._count_keyword_matches(prompt_lower, self.XL_CHANGE_KEYWORDS)
        if xl_count >= 1:
            return ExecutionScopeResult(
                scope=ExecutionScope.XL,
                confidence=0.9,
                raw_response="heuristic:xl_keywords",
            )
        
        # Check for explanations (XS)
        if self._check_keywords(prompt_lower, self.EXPLANATION_KEYWORDS):
            # Make sure it's not also asking for changes
            if not self._check_keywords(prompt_lower, self.MEDIUM_CHANGE_KEYWORDS + self.LARGE_CHANGE_KEYWORDS):
                return ExecutionScopeResult(
                    scope=ExecutionScope.XS,
                    confidence=0.9,
                    raw_response="heuristic:explanation_keywords",
                )
        
        # Check for large changes (L)
        large_count = self._count_keyword_matches(prompt_lower, self.LARGE_CHANGE_KEYWORDS)
        if large_count >= 1:
            return ExecutionScopeResult(
                scope=ExecutionScope.L,
                confidence=0.85,
                raw_response="heuristic:large_keywords",
            )
        
        # Check for medium changes (M)
        medium_count = self._count_keyword_matches(prompt_lower, self.MEDIUM_CHANGE_KEYWORDS)
        if medium_count >= 1:
            return ExecutionScopeResult(
                scope=ExecutionScope.M,
                confidence=0.8,
                raw_response="heuristic:medium_keywords",
            )
        
        # Check for small changes (S)
        small_count = self._count_keyword_matches(prompt_lower, self.SMALL_CHANGE_KEYWORDS)
        if small_count >= 1:
            return ExecutionScopeResult(
                scope=ExecutionScope.S,
                confidence=0.85,
                raw_response="heuristic:small_keywords",
            )
        
        # Analyze prompt length and complexity as fallback
        word_count = len(prompt_lower.split())
        
        if word_count <= 5:
            # Very short prompts are usually simple
            return ExecutionScopeResult(
                scope=ExecutionScope.S,
                confidence=0.6,
                raw_response="heuristic:short_prompt",
            )
        elif word_count <= 15:
            return ExecutionScopeResult(
                scope=ExecutionScope.M,
                confidence=0.5,
                raw_response="heuristic:medium_prompt",
            )
        else:
            # Longer prompts suggest more complexity - prefer larger bucket
            return ExecutionScopeResult(
                scope=ExecutionScope.L,
                confidence=0.5,
                raw_response="heuristic:long_prompt",
            )
    
    def _llm_classify(
        self,
        user_prompt: str,
        model: AIModel,
    ) -> Optional[ExecutionScopeResult]:
        """
        Use LLM for nuanced scope classification.
        """
        if not self.api_key:
            logger.warning("No API key configured for LLM classification")
            return None
        
        prompt = build_execution_scope_prompt(user_prompt)
        
        try:
            with httpx.Client(timeout=15.0) as client:
                response = client.post(
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json={
                        "model": model,
                        "messages": [
                            {"role": "system", "content": EXECUTION_SCOPE_SYSTEM_PROMPT},
                            {"role": "user", "content": prompt},
                        ],
                        "temperature": 0.0,  # Deterministic for consistent classification
                        "max_tokens": 10,    # We only need a single label
                    },
                )
                response.raise_for_status()
                
                result = response.json()
                content = result["choices"][0]["message"]["content"].strip().upper()
                
                return self._parse_llm_response(content)
                
        except httpx.HTTPStatusError as e:
            logger.error(f"LLM API error: {e.response.status_code}")
            return None
        except Exception as e:
            logger.error(f"LLM classification error: {e}")
            return None
    
    def _parse_llm_response(self, content: str) -> Optional[ExecutionScopeResult]:
        """Parse the LLM's response into an ExecutionScopeResult."""
        # Clean up response - extract just the bucket label
        content = content.strip().upper()
        
        # Handle various response formats
        # Could be "XS", "S", "M", "L", "XL" or wrapped in quotes/punctuation
        clean_content = re.sub(r'[^A-Z]', '', content)
        
        # Map to enum
        scope_map = {
            "XS": ExecutionScope.XS,
            "S": ExecutionScope.S,
            "M": ExecutionScope.M,
            "L": ExecutionScope.L,
            "XL": ExecutionScope.XL,
        }
        
        scope = scope_map.get(clean_content)
        if scope:
            return ExecutionScopeResult(
                scope=scope,
                confidence=0.9,  # LLM classification is high confidence
                raw_response=content,
            )
        
        # Try to extract from longer response
        for label in ["XL", "XS", "L", "M", "S"]:  # XL before L, XS before S
            if label in content:
                return ExecutionScopeResult(
                    scope=scope_map[label],
                    confidence=0.8,
                    raw_response=content,
                )
        
        logger.warning(f"Could not parse LLM scope response: {content}")
        return None
    
    def get_compute_units(self, scope: ExecutionScope) -> int:
        """Get estimated compute units for a scope tier.
        
        Args:
            scope: The execution scope
            
        Returns:
            Number of compute units for pricing
        """
        return SCOPE_COMPUTE_UNITS.get(scope, 5)
    
    def estimate_cost(
        self,
        scope: ExecutionScope,
        base_rate_per_unit: float = 0.01,
    ) -> float:
        """Estimate the cost for an execution scope.
        
        Args:
            scope: The execution scope
            base_rate_per_unit: Cost per compute unit in dollars
            
        Returns:
            Estimated cost in dollars
        """
        units = self.get_compute_units(scope)
        return units * base_rate_per_unit


# Singleton instance
_execution_scope_classifier: Optional[ExecutionScopeClassifier] = None


def get_execution_scope_classifier() -> ExecutionScopeClassifier:
    """Get singleton execution scope classifier instance."""
    global _execution_scope_classifier
    if _execution_scope_classifier is None:
        _execution_scope_classifier = ExecutionScopeClassifier()
    return _execution_scope_classifier

