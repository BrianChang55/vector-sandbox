"""
Intent Classifier Service

Classifies user requests into actionable intents to route
to the appropriate handler (generate, edit, add feature, etc.).

Supports compound intents for requests that require multiple
operations (e.g., schema change + UI update).
"""
import json
import logging
import re
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Dict, List, Literal, Optional, TYPE_CHECKING

from django.conf import settings
import httpx

from relay_app.prompts.intent_classification import (
    INTENT_CLASSIFICATION_SYSTEM_PROMPT,
    build_intent_classification_prompt,
)

if TYPE_CHECKING:
    from relay_app.services.context_analyzer import AppContext

logger = logging.getLogger(__name__)


class UserIntent(Enum):
    """Types of user intents for the agent."""
    GENERATE_NEW = "generate_new"      # Build from scratch
    EDIT_CODE = "edit_code"            # Modify existing code
    ADD_FEATURE = "add_feature"        # Add new functionality
    MODIFY_SCHEMA = "modify_schema"    # Change data models
    FIX_BUG = "fix_bug"                # Fix reported issues
    REFACTOR = "refactor"              # Reorganize code


@dataclass
class IntentResult:
    """Result of intent classification.
    
    Supports compound intents via secondary_intents for requests
    that require multiple operations in sequence.
    """
    intent: UserIntent
    confidence: float
    affected_files: List[str] = field(default_factory=list)
    affected_tables: List[str] = field(default_factory=list)
    scope: Literal["full", "partial", "surgical"] = "full"
    reasoning: str = ""
    # For compound requests: additional intents to execute after primary
    secondary_intents: List['IntentResult'] = field(default_factory=list)
    # Whether this is part of a compound request
    is_compound: bool = False
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        result = {
            "intent": self.intent.value,
            "confidence": self.confidence,
            "affected_files": self.affected_files,
            "affected_tables": self.affected_tables,
            "scope": self.scope,
            "reasoning": self.reasoning,
            "is_compound": self.is_compound,
        }
        if self.secondary_intents:
            result["secondary_intents"] = [s.to_dict() for s in self.secondary_intents]
        return result
    
    def all_intents(self) -> List['IntentResult']:
        """Get all intents in execution order (primary + secondary)."""
        return [self] + self.secondary_intents


class IntentClassifier:
    """
    Classifies user messages into actionable intents.
    
    Uses a combination of keyword heuristics and LLM classification
    to determine what type of operation the user is requesting.
    
    Supports compound intent detection for requests that need
    multiple operations (e.g., add field + update UI).
    """
    
    OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions"
    
    # Keywords that strongly suggest specific intents
    # Order matters - more specific patterns first
    GENERATE_KEYWORDS = [
        "build me", "create me", "make me", "generate me",
        "build a", "create a", "make a", "generate a",
        "start fresh", "from scratch", "new app", "rebuild", "start over"
    ]
    
    EDIT_KEYWORDS = [
        "change the", "modify the", "update the", "edit the", "adjust the",
        "tweak the", "fix the color", "fix the style", "make it",
        "set the", "replace the text", "rename", "change color",
        "make the", "move the", "resize"
    ]
    
    # Schema keywords - pure data model changes (no UI mention)
    SCHEMA_KEYWORDS = [
        "add column", "add a column", "new column",
        "new table", "create table", "add table",
        "modify table", "remove column", "remove field", "delete field",
        "schema", "database", "data model",
    ]
    
    # Keywords that suggest field additions (often compound with UI)
    FIELD_KEYWORDS = [
        "add field", "add a field", "add another field", "new field",
        "add property", "due date field", "date field", "email field",
        "field called", "field to", "field for"
    ]
    
    ADD_FEATURE_KEYWORDS = [
        "add a button", "add button", "add search", "add a search", "add filter",
        "add pagination", "add sorting", "add dark mode",
        "include", "implement", "integrate",
        "new feature", "also need", "can you add a",
        "search feature", "filter feature", "add navigation",
    ]
    
    # UI-specific keywords (suggest code changes, not just schema)
    UI_KEYWORDS = [
        "in the ui", "in the form", "in the input", "to the form",
        "to the input", "to the ui", "input field", "form field",
        "display", "show", "render", "component", "button", "modal",
        "page", "screen", "inputs", "outputs"
    ]
    
    FIX_KEYWORDS = [
        "doesn't work", "not working", "broken", "error",
        "bug", "issue", "problem", "fix the", "crash", "fails",
        "wrong", "incorrect"
    ]
    
    REFACTOR_KEYWORDS = [
        "refactor", "reorganize", "split", "extract",
        "separate", "clean up", "restructure", "modularize"
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
        user_message: str,
        context: 'AppContext',
        model: str = "anthropic/claude-sonnet-4",
    ) -> IntentResult:
        """
        Classify the user's intent based on their message and app context.
        
        Args:
            user_message: The user's request
            context: Current app context from ContextAnalyzer
            model: LLM model to use for classification
            
        Returns:
            IntentResult with classified intent and metadata
        """
        message_lower = user_message.lower()
        
        # Quick heuristic: If no existing app, almost always GENERATE_NEW
        if not context.has_existing_app or context.file_count == 0:
            # Unless they're asking to create a table first
            if self._check_keywords(message_lower, self.SCHEMA_KEYWORDS):
                return IntentResult(
                    intent=UserIntent.MODIFY_SCHEMA,
                    confidence=0.85,
                    scope="partial",
                    reasoning="No existing app but user is asking about schema/tables",
                )
            return IntentResult(
                intent=UserIntent.GENERATE_NEW,
                confidence=0.95,
                scope="full",
                reasoning="No existing app - generating from scratch",
            )
        
        # Try heuristic classification first
        heuristic_result = self._heuristic_classify(message_lower, context)
        if heuristic_result and heuristic_result.confidence >= 0.85:
            logger.info(f"Heuristic classification: {heuristic_result.intent.value} ({heuristic_result.confidence:.0%})")
            return heuristic_result
        
        # Use LLM for more nuanced classification
        try:
            llm_result = self._llm_classify(user_message, context, model)
            if llm_result:
                logger.info(f"LLM classification: {llm_result.intent.value} ({llm_result.confidence:.0%})")
                return llm_result
        except Exception as e:
            logger.warning(f"LLM classification failed: {e}, falling back to heuristics")
        
        # Fallback to heuristic result or default
        if heuristic_result:
            return heuristic_result
        
        # Default: If we have an existing app, assume ADD_FEATURE
        return IntentResult(
            intent=UserIntent.ADD_FEATURE,
            confidence=0.5,
            scope="partial",
            reasoning="Could not confidently classify - defaulting to add feature",
        )
    
    def _check_keywords(self, message: str, keywords: List[str]) -> bool:
        """Check if message contains any of the keywords."""
        return any(kw in message for kw in keywords)
    
    def _count_keyword_matches(self, message: str, keywords: List[str]) -> int:
        """Count how many keywords match in the message."""
        return sum(1 for kw in keywords if kw in message)
    
    def _heuristic_classify(
        self,
        message_lower: str,
        context: 'AppContext',
    ) -> Optional[IntentResult]:
        """
        Use keyword heuristics for fast classification.
        
        Detects compound requests that need multiple operations.
        Returns None if confidence is too low.
        """
        # Count all keyword matches
        schema_score = self._count_keyword_matches(message_lower, self.SCHEMA_KEYWORDS)
        field_score = self._count_keyword_matches(message_lower, self.FIELD_KEYWORDS)
        ui_score = self._count_keyword_matches(message_lower, self.UI_KEYWORDS)
        feature_score = self._count_keyword_matches(message_lower, self.ADD_FEATURE_KEYWORDS)
        edit_score = self._count_keyword_matches(message_lower, self.EDIT_KEYWORDS)
        
        # Detect compound requests: field + UI = schema change + code update
        # Example: "Add a due date field to the TODO inputs in the UI"
        if field_score >= 1 and ui_score >= 1:
            # This is a compound request: needs schema change + UI update
            return IntentResult(
                intent=UserIntent.MODIFY_SCHEMA,  # Primary: schema change first
                confidence=0.9,
                scope="partial",
                reasoning=f"Compound request: field addition ({field_score}) + UI changes ({ui_score})",
                is_compound=True,
                secondary_intents=[
                    IntentResult(
                        intent=UserIntent.ADD_FEATURE,
                        confidence=0.85,
                        scope="partial",
                        reasoning="UI/form updates needed after schema change",
                    )
                ],
            )
        
        # Detect schema + feature compound
        if schema_score >= 1 and (feature_score >= 1 or edit_score >= 1):
            secondary_intent = UserIntent.ADD_FEATURE if feature_score >= edit_score else UserIntent.EDIT_CODE
            return IntentResult(
                intent=UserIntent.MODIFY_SCHEMA,
                confidence=0.85,
                scope="partial",
                reasoning=f"Compound request: schema change + {secondary_intent.value}",
                is_compound=True,
                secondary_intents=[
                    IntentResult(
                        intent=secondary_intent,
                        confidence=0.8,
                        scope="partial",
                        reasoning="Code updates needed after schema change",
                    )
                ],
            )
        
        # Pure schema change (no UI/feature keywords)
        if schema_score >= 1 and ui_score == 0 and feature_score == 0:
            return IntentResult(
                intent=UserIntent.MODIFY_SCHEMA,
                confidence=0.9,
                scope="partial",
                reasoning=f"Pure schema modification: {schema_score} keywords",
            )
        
        # Field-only requests with UI context → primarily ADD_FEATURE
        # (the feature handler will handle both UI and any needed data changes)
        if field_score >= 1 and ui_score >= 1 and schema_score == 0:
            return IntentResult(
                intent=UserIntent.ADD_FEATURE,
                confidence=0.85,
                scope="partial",
                reasoning="UI-focused field addition - feature handler will manage schema",
                is_compound=True,
                secondary_intents=[
                    IntentResult(
                        intent=UserIntent.MODIFY_SCHEMA,
                        confidence=0.8,
                        scope="partial",
                        reasoning="Schema change implied by new field requirement",
                    )
                ],
            )
        
        scores = {
            UserIntent.GENERATE_NEW: self._count_keyword_matches(message_lower, self.GENERATE_KEYWORDS),
            UserIntent.EDIT_CODE: edit_score,
            UserIntent.ADD_FEATURE: feature_score + ui_score,  # UI keywords boost feature score
            UserIntent.MODIFY_SCHEMA: schema_score + field_score,
            UserIntent.FIX_BUG: self._count_keyword_matches(message_lower, self.FIX_KEYWORDS),
            UserIntent.REFACTOR: self._count_keyword_matches(message_lower, self.REFACTOR_KEYWORDS),
        }
        
        # Find the intent with highest score
        best_intent = max(scores, key=scores.get)
        best_score = scores[best_intent]
        
        if best_score == 0:
            return None
        
        # Calculate confidence based on score difference
        second_best = sorted(scores.values(), reverse=True)[1] if len(scores) > 1 else 0
        total_matches = sum(scores.values())
        
        if total_matches > 0:
            confidence = (best_score / total_matches) * 0.7 + 0.3
        else:
            confidence = 0.3
        
        # Boost confidence if clear winner
        if best_score >= 2 and second_best <= 1:
            confidence = min(0.9, confidence + 0.15)
        
        # Determine scope
        if best_intent == UserIntent.GENERATE_NEW:
            scope = "full"
        elif best_intent in (UserIntent.EDIT_CODE, UserIntent.FIX_BUG):
            scope = "surgical"
        else:
            scope = "partial"
        
        return IntentResult(
            intent=best_intent,
            confidence=confidence,
            scope=scope,
            reasoning=f"Keyword match: {best_score} keywords for {best_intent.value}",
        )
    
    def _llm_classify(
        self,
        user_message: str,
        context: 'AppContext',
        model: str,
    ) -> Optional[IntentResult]:
        """
        Use LLM for nuanced intent classification.
        """
        # Build context strings for the prompt
        components = ", ".join(context.entry_points[:5]) if context.entry_points else "None"
        tables = ", ".join([t.name for t in context.existing_tables[:5]]) if context.existing_tables else "None"
        
        # Build detailed table columns for schema-aware classification
        table_columns = ""
        if context.existing_tables:
            table_details = []
            for t in context.existing_tables[:5]:
                cols = ", ".join(t.columns[:10]) if t.columns else "unknown"
                table_details.append(f"{t.name}: {cols}")
            table_columns = "; ".join(table_details)
        
        prompt = build_intent_classification_prompt(
            user_message=user_message,
            has_files=context.has_existing_app,
            file_count=context.file_count,
            components=components,
            tables=tables,
            has_data_store=len(context.existing_tables) > 0,
            table_columns=table_columns,
        )
        
        try:
            with httpx.Client(timeout=30.0) as client:
                response = client.post(
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json={
                        "model": model,
                        "messages": [
                            {"role": "system", "content": INTENT_CLASSIFICATION_SYSTEM_PROMPT},
                            {"role": "user", "content": prompt},
                        ],
                        "temperature": 0.1,  # Low temperature for consistent classification
                        "max_tokens": 500,
                    },
                )
                response.raise_for_status()
                
                result = response.json()
                content = result["choices"][0]["message"]["content"]
                
                # Parse JSON response
                return self._parse_llm_response(content)
                
        except Exception as e:
            logger.error(f"LLM classification error: {e}")
            return None
    
    def _parse_llm_response(self, content: str) -> Optional[IntentResult]:
        """Parse the LLM's JSON response into an IntentResult.
        
        Supports compound intent parsing with secondary_intents.
        """
        try:
            # Clean up the response - remove markdown code blocks if present
            content = content.strip()
            if content.startswith("```"):
                # Remove code block markers
                content = re.sub(r'^```(?:json)?\s*', '', content)
                content = re.sub(r'\s*```$', '', content)
            
            # Find JSON object
            json_match = re.search(r'\{.*\}', content, re.DOTALL)
            if not json_match:
                logger.warning(f"No JSON found in LLM response: {content[:100]}")
                return None
            
            data = json.loads(json_match.group())
            
            # Map intent string to enum
            intent_str = data.get("intent", "").upper()
            intent_map = {
                "GENERATE_NEW": UserIntent.GENERATE_NEW,
                "EDIT_CODE": UserIntent.EDIT_CODE,
                "ADD_FEATURE": UserIntent.ADD_FEATURE,
                "MODIFY_SCHEMA": UserIntent.MODIFY_SCHEMA,
                "FIX_BUG": UserIntent.FIX_BUG,
                "REFACTOR": UserIntent.REFACTOR,
            }
            
            intent = intent_map.get(intent_str)
            if not intent:
                logger.warning(f"Unknown intent: {intent_str}")
                return None
            
            # Parse secondary intents if present (compound request)
            secondary_intents = []
            is_compound = data.get("is_compound", False)
            
            if "secondary_intents" in data and isinstance(data["secondary_intents"], list):
                is_compound = True
                for secondary in data["secondary_intents"]:
                    secondary_intent_str = secondary.get("intent", "").upper()
                    secondary_intent = intent_map.get(secondary_intent_str)
                    if secondary_intent:
                        secondary_intents.append(IntentResult(
                            intent=secondary_intent,
                            confidence=float(secondary.get("confidence", 0.7)),
                            affected_files=secondary.get("affected_files", []),
                            affected_tables=secondary.get("affected_tables", []),
                            scope=secondary.get("scope", "partial"),
                            reasoning=secondary.get("reasoning", ""),
                        ))
            
            return IntentResult(
                intent=intent,
                confidence=float(data.get("confidence", 0.7)),
                affected_files=data.get("affected_files", []),
                affected_tables=data.get("affected_tables", []),
                scope=data.get("scope", "partial"),
                reasoning=data.get("reasoning", ""),
                is_compound=is_compound,
                secondary_intents=secondary_intents,
            )
            
        except json.JSONDecodeError as e:
            logger.warning(f"Failed to parse LLM response as JSON: {e}")
            return None
        except Exception as e:
            logger.warning(f"Error parsing LLM response: {e}")
            return None


# Singleton instance
_intent_classifier: Optional[IntentClassifier] = None


def get_intent_classifier() -> IntentClassifier:
    """Get singleton intent classifier instance."""
    global _intent_classifier
    if _intent_classifier is None:
        _intent_classifier = IntentClassifier()
    return _intent_classifier

