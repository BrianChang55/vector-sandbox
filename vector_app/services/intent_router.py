"""
Intent Router

Routes user intents to the appropriate specialized handler.
Central dispatch point for the intent-aware agent architecture.

Supports compound intent execution: when a request requires multiple
operations (e.g., schema change + UI update), handlers are executed
in sequence with results accumulated.
"""
import logging
import time
from typing import Any, Dict, Generator, List, Optional, TYPE_CHECKING

from vector_app.services.intent_classifier import UserIntent, IntentResult
from vector_app.services.context_analyzer import AppContext
from vector_app.services.handlers.base_handler import AgentEvent, FileChange
from vector_app.services.handlers.generate_handler import GenerateHandler
from vector_app.services.handlers.edit_handler import EditHandler
from vector_app.services.handlers.feature_handler import FeatureHandler
from vector_app.services.handlers.schema_handler import SchemaHandler

if TYPE_CHECKING:
    from vector_app.models import InternalApp, AppVersion

logger = logging.getLogger(__name__)


class IntentRouter:
    """
    Routes intents to specialized handlers.
    
    Each handler is optimized for its specific use case:
    - GenerateHandler: Full app generation from scratch
    - EditHandler: Surgical code modifications
    - FeatureHandler: Adding new features
    - SchemaHandler: Data model changes
    
    Supports compound requests that execute multiple handlers in sequence.
    All handlers yield AgentEvent objects for streaming.
    
    Optional plan confirmation mode:
    For high-scope changes, can emit a plan_confirmation_required event
    that the client can use to ask the user for confirmation before proceeding.
    """
    
    # Thresholds for plan confirmation
    CONFIDENCE_THRESHOLD_FOR_CONFIRMATION = 0.8  # Below this, suggest confirmation
    
    def __init__(self):
        # Initialize handlers
        self.generate_handler = GenerateHandler()
        self.edit_handler = EditHandler()
        self.feature_handler = FeatureHandler()
        self.schema_handler = SchemaHandler()
        
        # Configuration
        self.enable_plan_confirmation = True  # Can be toggled
    
    def route(
        self,
        intent: IntentResult,
        context: AppContext,
        user_message: str,
        current_spec: Optional[Dict[str, Any]],
        registry_surface: Dict[str, Any],
        app_name: str,
        model: str,
        app: Optional['InternalApp'] = None,
        version: Optional['AppVersion'] = None,
        **kwargs,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """
        Route to the appropriate handler based on intent.
        
        Supports compound intents: executes multiple handlers in sequence,
        accumulating file changes from each.
        
        Args:
            intent: Classified intent result (may have secondary_intents)
            context: App context from analyzer
            user_message: Original user request
            current_spec: Current app specification
            registry_surface: Available data resources
            app_name: Name of the app
            model: LLM model to use
            app: Optional InternalApp instance
            version: Optional AppVersion instance
            **kwargs: Additional handler-specific arguments
            
        Yields:
            AgentEvent objects for streaming
            
        Returns:
            List of FileChange objects from all handlers
        """
        start_time = time.time()
        all_generated_files: List[FileChange] = []
        
        # Check if this is a compound request
        if intent.is_compound and intent.secondary_intents:
            intents_to_execute = [intent] + intent.secondary_intents
            logger.info(
                f"Compound request detected: {len(intents_to_execute)} intents "
                f"({', '.join(i.intent.value for i in intents_to_execute)})"
            )
            
            yield AgentEvent("thinking", {
                "content": f"This request requires multiple operations: {self._describe_compound_intent(intent)}",
                "type": "reasoning",
            })
        else:
            intents_to_execute = [intent]
            logger.info(
                f"Routing intent {intent.intent.value} "
                f"(confidence: {intent.confidence:.0%}, scope: {intent.scope})"
            )
            
            yield AgentEvent("thinking", {
                "content": f"Detected intent: {self._get_intent_description(intent)}",
                "type": "reasoning",
            })
        
        # Check if plan confirmation is recommended for high-impact changes
        if self._should_request_confirmation(intent, context):
            yield self.emit_plan_confirmation_required(intent, context)
        
        # Execute each intent in sequence
        for idx, current_intent in enumerate(intents_to_execute):
            if len(intents_to_execute) > 1:
                phase_label = f"Phase {idx + 1}/{len(intents_to_execute)}"
                yield AgentEvent("thinking", {
                    "content": f"{phase_label}: {self._get_intent_description(current_intent)}",
                    "type": "decision",
                })
            
            # Select handler based on intent
            handler = self._select_handler(current_intent, context)
            handler_name = handler.__class__.__name__
            logger.info(f"Selected handler: {handler_name} for intent {current_intent.intent.value}")
            
            yield AgentEvent("thinking", {
                "content": f"Using {self._get_handler_description(handler_name)} approach",
                "type": "decision",
            })
            
            # Execute the handler
            try:
                # Pass any previously generated files to help subsequent handlers
                if all_generated_files:
                    kwargs['previous_files'] = all_generated_files
                
                generated_files = yield from handler.execute(
                    intent=current_intent,
                    context=context,
                    user_message=user_message,
                    current_spec=current_spec,
                    registry_surface=registry_surface,
                    app_name=app_name,
                    model=model,
                    app=app,
                    version=version,
                    **kwargs,
                )
                
                # Merge generated files (later files override earlier ones)
                all_generated_files = self._merge_file_changes(all_generated_files, generated_files)
                
                duration_ms = int((time.time() - start_time) * 1000)
                logger.info(
                    f"Handler {handler_name} completed in {duration_ms}ms, "
                    f"generated {len(generated_files)} files (total: {len(all_generated_files)})"
                )
                
            except Exception as e:
                logger.error(f"Handler {handler_name} error: {e}")
                yield AgentEvent("thinking", {
                    "content": f"Error during {handler_name}: {str(e)}",
                    "type": "reflection",
                })
                raise
        
        total_duration_ms = int((time.time() - start_time) * 1000)
        if len(intents_to_execute) > 1:
            logger.info(
                f"Compound request completed in {total_duration_ms}ms, "
                f"total files: {len(all_generated_files)}"
            )
        
        return all_generated_files
    
    def _merge_file_changes(
        self,
        existing: List[FileChange],
        new: List[FileChange],
    ) -> List[FileChange]:
        """Merge file changes, with later changes overriding earlier ones."""
        by_path = {f.path: f for f in existing}
        for f in new:
            by_path[f.path] = f
        return list(by_path.values())
    
    def _describe_compound_intent(self, intent: IntentResult) -> str:
        """Describe a compound intent for user feedback."""
        parts = [self._get_intent_simple_description(intent.intent)]
        for secondary in intent.secondary_intents:
            parts.append(self._get_intent_simple_description(secondary.intent))
        return " → ".join(parts)
    
    def _get_intent_simple_description(self, intent: UserIntent) -> str:
        """Get a simple description of an intent."""
        descriptions = {
            UserIntent.GENERATE_NEW: "build app",
            UserIntent.EDIT_CODE: "update code",
            UserIntent.ADD_FEATURE: "add UI/feature",
            UserIntent.MODIFY_SCHEMA: "modify data schema",
            UserIntent.FIX_BUG: "fix issue",
            UserIntent.REFACTOR: "reorganize code",
        }
        return descriptions.get(intent, intent.value)
    
    def _select_handler(
        self,
        intent: IntentResult,
        context: AppContext,
    ):
        """Select the appropriate handler based on intent."""
        
        # Map intents to handlers
        intent_handler_map = {
            UserIntent.GENERATE_NEW: self.generate_handler,
            UserIntent.EDIT_CODE: self.edit_handler,
            UserIntent.ADD_FEATURE: self.feature_handler,
            UserIntent.MODIFY_SCHEMA: self.schema_handler,
            UserIntent.FIX_BUG: self.edit_handler,  # Bug fixes use edit handler
            UserIntent.REFACTOR: self.edit_handler,  # Refactoring uses edit handler
        }
        
        handler = intent_handler_map.get(intent.intent)
        
        if handler:
            # Additional checks to override handler selection
            
            # If no existing app, always use generate handler
            if not context.has_existing_app and intent.intent != UserIntent.MODIFY_SCHEMA:
                logger.info("No existing app - overriding to GenerateHandler")
                return self.generate_handler
            
            # If scope is "full", prefer generate handler
            if intent.scope == "full" and context.file_count < 3:
                logger.info("Full scope on small app - using GenerateHandler")
                return self.generate_handler
            
            return handler
        
        # Fallback to generate handler
        logger.warning(f"Unknown intent {intent.intent}, falling back to GenerateHandler")
        return self.generate_handler
    
    def _get_intent_description(self, intent: IntentResult) -> str:
        """Get a human-readable description of the intent."""
        descriptions = {
            UserIntent.GENERATE_NEW: "build new app from scratch",
            UserIntent.EDIT_CODE: "modify existing code",
            UserIntent.ADD_FEATURE: "add new feature",
            UserIntent.MODIFY_SCHEMA: "modify data schema",
            UserIntent.FIX_BUG: "fix reported issue",
            UserIntent.REFACTOR: "reorganize code",
        }
        
        desc = descriptions.get(intent.intent, intent.intent.value)
        return f"{desc} (confidence: {intent.confidence:.0%})"
    
    def _get_handler_description(self, handler_name: str) -> str:
        """Get a human-readable description of the handler."""
        descriptions = {
            "GenerateHandler": "full generation",
            "EditHandler": "surgical edit",
            "FeatureHandler": "feature addition",
            "SchemaHandler": "schema modification",
        }
        return descriptions.get(handler_name, handler_name)
    
    def _should_request_confirmation(
        self,
        intent: IntentResult,
        context: AppContext,
    ) -> bool:
        """
        Determine if plan confirmation should be requested.
        
        Returns True if:
        - Intent scope is "full" (complete rebuild)
        - Intent confidence is below threshold
        - Intent is compound with 3+ operations
        - Full rebuild requested on existing app with many files
        """
        if not self.enable_plan_confirmation:
            return False
        
        # Low confidence → request confirmation
        if intent.confidence < self.CONFIDENCE_THRESHOLD_FOR_CONFIRMATION:
            return True
        
        # Full scope changes on existing app with many files
        if intent.scope == "full" and context.has_existing_app and context.file_count > 5:
            return True
        
        # Complex compound requests
        if intent.is_compound and len(intent.secondary_intents) >= 2:
            return True
        
        return False
    
    def _build_confirmation_details(
        self,
        intent: IntentResult,
        context: AppContext,
    ) -> Dict[str, Any]:
        """Build details for the plan confirmation event."""
        details = {
            "primary_intent": intent.intent.value,
            "confidence": intent.confidence,
            "scope": intent.scope,
            "reasoning": intent.reasoning,
            "affected_files_estimate": len(intent.affected_files) or "multiple",
        }
        
        if intent.is_compound:
            details["is_compound"] = True
            details["operations"] = [intent.intent.value] + [
                s.intent.value for s in intent.secondary_intents
            ]
        
        if context.has_existing_app:
            details["existing_file_count"] = context.file_count
            details["will_modify_existing"] = True
        
        return details
    
    def emit_plan_confirmation_required(
        self,
        intent: IntentResult,
        context: AppContext,
    ) -> AgentEvent:
        """
        Emit an event indicating plan confirmation is recommended.
        
        The client can use this to pause and ask the user to confirm
        before proceeding with high-impact changes.
        
        Note: This is informational - the agent will continue unless
        the client explicitly cancels. To implement hard confirmation,
        the client should intercept this event and pause the stream.
        """
        return AgentEvent("plan_confirmation_suggested", {
            "message": self._build_confirmation_message(intent, context),
            "details": self._build_confirmation_details(intent, context),
            "can_proceed": True,  # Agent will proceed unless cancelled
        })
    
    def _build_confirmation_message(
        self,
        intent: IntentResult,
        context: AppContext,
    ) -> str:
        """Build a human-readable confirmation message."""
        if intent.scope == "full" and context.has_existing_app:
            return (
                f"This will rebuild the entire app ({context.file_count} existing files). "
                "Existing code will be replaced."
            )
        
        if intent.confidence < self.CONFIDENCE_THRESHOLD_FOR_CONFIRMATION:
            return (
                f"I'm {intent.confidence:.0%} confident about this interpretation. "
                "Proceeding with: " + self._describe_compound_intent(intent) if intent.is_compound 
                else intent.intent.value
            )
        
        if intent.is_compound:
            return (
                f"This requires multiple operations: {self._describe_compound_intent(intent)}. "
                "Each will be executed in sequence."
            )
        
        return f"Proceeding with {intent.intent.value} ({intent.scope} scope)."


# Singleton instance
_intent_router: Optional[IntentRouter] = None


def get_intent_router() -> IntentRouter:
    """Get singleton intent router instance."""
    global _intent_router
    if _intent_router is None:
        _intent_router = IntentRouter()
    return _intent_router

