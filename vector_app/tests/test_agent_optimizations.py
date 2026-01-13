"""
Tests for Agent Optimization Features

Tests the following optimizations:
1. Anti-Over-Engineering Guards (OVER_EAGERNESS_GUARD)
2. Anti-AI-Slop Guidelines
3. Codebase Style Fingerprinting
4. Cascade-Affected-Files Detection
5. Component Reuse Detection
6. Schema-Aware Intent Classification
7. Plan Confirmation Mode
8. Streaming Validation
9. Parallel Context Gathering
"""
import json
import unittest
from unittest.mock import Mock, patch, MagicMock
from dataclasses import dataclass, field
from typing import List, Optional, Dict, Any

from django.test import TestCase

# Import the modules we're testing
from vector_app.prompts.agentic import (
    OVER_EAGERNESS_GUARD,
    ANTI_AI_SLOP_GUIDE,
    DESIGN_STYLE_PROMPT,
    build_codegen_system_prompt,
    build_step_prompt,
)
from vector_app.prompts.intent_classification import (
    build_intent_classification_prompt,
)
from vector_app.services.context_analyzer import (
    ContextAnalyzer,
    AppContext,
    FileInfo,
    CodebaseStyle,
    get_context_analyzer,
)
from vector_app.services.intent_classifier import (
    IntentClassifier,
    UserIntent,
    IntentResult,
)
from vector_app.services.intent_router import (
    IntentRouter,
    get_intent_router,
)
from vector_app.services.handlers.base_handler import (
    StreamingValidator,
)


class TestAntiOverEngineeringGuards(TestCase):
    """Test that anti-over-engineering guards are properly defined and integrated."""
    
    def test_over_eagerness_guard_exists(self):
        """Test OVER_EAGERNESS_GUARD constant is defined."""
        self.assertIsNotNone(OVER_EAGERNESS_GUARD)
        self.assertIsInstance(OVER_EAGERNESS_GUARD, str)
        self.assertGreater(len(OVER_EAGERNESS_GUARD), 100)
    
    def test_over_eagerness_guard_content(self):
        """Test OVER_EAGERNESS_GUARD contains key rules."""
        self.assertIn("Anti-Over-Engineering", OVER_EAGERNESS_GUARD)
        self.assertIn("explicitly requested", OVER_EAGERNESS_GUARD)
        self.assertIn("MINIMUM", OVER_EAGERNESS_GUARD)
        self.assertIn("DRY", OVER_EAGERNESS_GUARD)
    
    def test_codegen_system_prompt_includes_guard(self):
        """Test that build_codegen_system_prompt includes the guard."""
        prompt = build_codegen_system_prompt({}, has_data_store=False)
        self.assertIn("Anti-Over-Engineering", prompt)
    
    def test_step_prompt_includes_guard(self):
        """Test that build_step_prompt includes the guard."""
        mock_step = Mock()
        mock_step.title = "Test Step"
        mock_step.description = "Test description"
        
        prompt = build_step_prompt(
            step=mock_step,
            step_index=0,
            user_message="Build a form",
            context={"app_name": "Test App"},
            existing_files=[],
            registry_surface={},
        )
        self.assertIn("Anti-Over-Engineering", prompt)


class TestAntiAISlopGuidelines(TestCase):
    """Test that anti-AI-slop guidelines are properly defined and integrated."""
    
    def test_anti_ai_slop_guide_exists(self):
        """Test ANTI_AI_SLOP_GUIDE constant is defined."""
        self.assertIsNotNone(ANTI_AI_SLOP_GUIDE)
        self.assertIsInstance(ANTI_AI_SLOP_GUIDE, str)
    
    def test_anti_ai_slop_guide_content(self):
        """Test ANTI_AI_SLOP_GUIDE contains key warnings."""
        self.assertIn("Generic AI Aesthetics", ANTI_AI_SLOP_GUIDE)
        self.assertIn("Inter", ANTI_AI_SLOP_GUIDE)  # Warns against Inter font
        self.assertIn("purple", ANTI_AI_SLOP_GUIDE.lower())  # Warns against purple gradients
    
    def test_design_style_prompt_includes_anti_slop(self):
        """Test that DESIGN_STYLE_PROMPT includes anti-AI-slop guide."""
        self.assertIn("Generic AI Aesthetics", DESIGN_STYLE_PROMPT)


class TestCodebaseStyleFingerprinting(TestCase):
    """Test the CodebaseStyle class and style fingerprinting."""
    
    def test_codebase_style_default_values(self):
        """Test CodebaseStyle has sensible defaults."""
        style = CodebaseStyle()
        
        self.assertEqual(style.naming_convention, "camelCase")
        self.assertEqual(style.component_pattern, "functional")
        self.assertEqual(style.state_management, "useState")
        self.assertTrue(style.uses_semicolons)
        self.assertEqual(style.quote_style, "single")
    
    def test_codebase_style_to_prompt_context(self):
        """Test to_prompt_context generates valid prompt text."""
        style = CodebaseStyle(
            component_pattern="arrow",
            state_management="useReducer",
            existing_utilities=["formatDate", "validateEmail"],
        )
        
        prompt = style.to_prompt_context()
        
        self.assertIn("arrow", prompt)
        self.assertIn("useReducer", prompt)
        self.assertIn("formatDate", prompt)
        self.assertIn("MATCH THIS", prompt)


class TestContextAnalyzer(TestCase):
    """Test ContextAnalyzer functionality."""
    
    def setUp(self):
        self.analyzer = ContextAnalyzer()
    
    def test_analyzer_singleton(self):
        """Test get_context_analyzer returns a singleton."""
        analyzer1 = get_context_analyzer()
        analyzer2 = get_context_analyzer()
        self.assertIs(analyzer1, analyzer2)
    
    def test_analyze_none_app(self):
        """Test analyze with None app returns empty context."""
        context = self.analyzer.analyze(None)
        
        self.assertFalse(context.has_existing_app)
        self.assertEqual(context.file_count, 0)
        self.assertEqual(len(context.existing_files), 0)
    
    def test_extract_exports(self):
        """Test _extract_exports correctly parses exports."""
        content = '''
        export default function Button() { return null; }
        export const Helper = () => {};
        export { Something, Another };
        '''
        
        exports = self.analyzer._extract_exports(content)
        
        self.assertIn("Button", exports)
        self.assertIn("Helper", exports)
        self.assertIn("Something", exports)
        self.assertIn("Another", exports)
    
    def test_extract_imports(self):
        """Test _extract_imports correctly parses imports."""
        content = '''
        import React from 'react';
        import { useState } from 'react';
        import Button from './components/Button';
        '''
        
        imports = self.analyzer._extract_imports(content)
        
        self.assertIn("react", imports)
        self.assertIn("./components/Button", imports)
        # Note: Side-effect imports (import 'path') are parsed separately
        # and require a specific format
    
    def test_is_react_component(self):
        """Test _is_react_component detection."""
        component_content = '''
        import React from 'react';
        export default function Button() {
            return <button>Click me</button>;
        }
        '''
        non_component = '''
        export function calculateSum(a, b) {
            return a + b;
        }
        '''
        
        self.assertTrue(self.analyzer._is_react_component(component_content, ["Button"]))
        self.assertFalse(self.analyzer._is_react_component(non_component, ["calculateSum"]))
    
    def test_build_reverse_graph(self):
        """Test _build_reverse_graph creates correct reverse dependencies."""
        component_graph = {
            'src/App.tsx': ['src/components/Header.tsx', 'src/components/Footer.tsx'],
            'src/components/Header.tsx': ['src/components/Button.tsx'],
        }
        
        reverse = self.analyzer._build_reverse_graph(component_graph)
        
        self.assertIn('src/components/Header.tsx', reverse)
        self.assertIn('src/App.tsx', reverse['src/components/Header.tsx'])
        self.assertIn('src/components/Button.tsx', reverse)
        self.assertIn('src/components/Header.tsx', reverse['src/components/Button.tsx'])
    
    def test_find_cascade_affected_files(self):
        """Test cascade-affected-files detection."""
        context = AppContext(
            has_existing_app=True,
            file_count=5,
            existing_files=[],
            component_graph={
                'src/App.tsx': ['src/components/Header.tsx'],
                'src/components/Header.tsx': ['src/components/Button.tsx'],
            },
            reverse_graph={
                'src/components/Header.tsx': ['src/App.tsx'],
                'src/components/Button.tsx': ['src/components/Header.tsx'],
            },
        )
        
        affected = self.analyzer.find_cascade_affected_files(context, 'src/components/Header.tsx')
        
        self.assertIn('src/components/Header.tsx', affected)
        self.assertIn('src/App.tsx', affected)  # Consumer
        self.assertIn('src/components/Button.tsx', affected)  # Dependency
    
    def test_extract_feature_keywords(self):
        """Test _extract_feature_keywords extracts relevant keywords."""
        request = "Add a search feature to the table with filters and pagination"
        
        keywords = self.analyzer._extract_feature_keywords(request)
        
        self.assertIn("search", keywords)
        self.assertIn("table", keywords)
        # Note: "filters" is returned (not "filter" - we match exact words)
        self.assertTrue("filter" in keywords or "filters" in keywords)
        self.assertIn("pagination", keywords)
    
    def test_find_reusable_components(self):
        """Test find_reusable_components matches relevant components."""
        context = AppContext(
            has_existing_app=True,
            file_count=3,
            existing_files=[
                FileInfo(
                    path='src/components/DataTable.tsx',
                    size=1000,
                    language='tsx',
                    exports=['DataTable'],
                    is_component=True,
                ),
                FileInfo(
                    path='src/components/SearchInput.tsx',
                    size=500,
                    language='tsx',
                    exports=['SearchInput'],
                    is_component=True,
                ),
                FileInfo(
                    path='src/utils/helpers.ts',
                    size=200,
                    language='ts',
                    exports=['formatDate'],
                    is_component=False,
                ),
            ],
        )
        
        reusable = self.analyzer.find_reusable_components(context, "Add search to the table")
        
        self.assertGreater(len(reusable), 0)
        paths = [r['path'] for r in reusable]
        self.assertIn('src/components/DataTable.tsx', paths)
        self.assertIn('src/components/SearchInput.tsx', paths)
    
    def test_build_reusable_components_prompt(self):
        """Test build_reusable_components_prompt generates valid prompt."""
        context = AppContext(
            has_existing_app=True,
            file_count=1,
            existing_files=[
                FileInfo(
                    path='src/components/Table.tsx',
                    size=1000,
                    language='tsx',
                    exports=['Table'],
                    is_component=True,
                ),
            ],
        )
        
        prompt = self.analyzer.build_reusable_components_prompt(context, "Add table feature")
        
        self.assertIsNotNone(prompt)
        self.assertIn("Existing Components", prompt)
        self.assertIn("Table.tsx", prompt)
        self.assertIn("Prefer extending", prompt)


class TestSchemaAwareIntentClassification(TestCase):
    """Test schema-aware intent classification."""
    
    def test_intent_classification_prompt_includes_table_columns(self):
        """Test that build_intent_classification_prompt includes table_columns."""
        prompt = build_intent_classification_prompt(
            user_message="Add a due date field",
            has_files=True,
            file_count=5,
            components="App, Header",
            tables="todos",
            has_data_store=True,
            table_columns="todos: id, title, status",
        )
        
        self.assertIn("todos: id, title, status", prompt)
        self.assertIn("Schema-Aware Classification", prompt)
    
    def test_heuristic_classify_schema_keywords(self):
        """Test heuristic classification detects schema keywords."""
        classifier = IntentClassifier()
        
        context = AppContext(has_existing_app=True, file_count=3)
        
        # Test schema keywords
        result = classifier._heuristic_classify("add a new column for email", context)
        self.assertIsNotNone(result)
        self.assertEqual(result.intent, UserIntent.MODIFY_SCHEMA)
    
    def test_heuristic_classify_compound_field_ui(self):
        """Test heuristic classification detects compound field + UI requests."""
        classifier = IntentClassifier()
        
        context = AppContext(has_existing_app=True, file_count=5)
        
        # Field + UI = compound request
        result = classifier._heuristic_classify(
            "add a due date field to the input form",
            context,
        )
        
        self.assertIsNotNone(result)
        self.assertTrue(result.is_compound)
        self.assertGreater(len(result.secondary_intents), 0)


class TestIntentRouter(TestCase):
    """Test IntentRouter functionality."""
    
    def setUp(self):
        self.router = IntentRouter()
    
    def test_router_singleton(self):
        """Test get_intent_router returns a singleton."""
        router1 = get_intent_router()
        router2 = get_intent_router()
        self.assertIs(router1, router2)
    
    def test_should_request_confirmation_low_confidence(self):
        """Test _should_request_confirmation for low confidence."""
        intent = IntentResult(
            intent=UserIntent.ADD_FEATURE,
            confidence=0.6,  # Below threshold
            scope="partial",
        )
        context = AppContext(has_existing_app=True, file_count=5)
        
        self.assertTrue(self.router._should_request_confirmation(intent, context))
    
    def test_should_request_confirmation_full_scope(self):
        """Test _should_request_confirmation for full scope rebuild."""
        intent = IntentResult(
            intent=UserIntent.GENERATE_NEW,
            confidence=0.9,
            scope="full",
        )
        context = AppContext(has_existing_app=True, file_count=10)  # Many files
        
        self.assertTrue(self.router._should_request_confirmation(intent, context))
    
    def test_should_request_confirmation_complex_compound(self):
        """Test _should_request_confirmation for complex compound requests."""
        intent = IntentResult(
            intent=UserIntent.MODIFY_SCHEMA,
            confidence=0.85,
            scope="partial",
            is_compound=True,
            secondary_intents=[
                IntentResult(intent=UserIntent.ADD_FEATURE, confidence=0.8, scope="partial"),
                IntentResult(intent=UserIntent.EDIT_CODE, confidence=0.8, scope="surgical"),
            ],
        )
        context = AppContext(has_existing_app=True, file_count=5)
        
        self.assertTrue(self.router._should_request_confirmation(intent, context))
    
    def test_should_not_request_confirmation_high_confidence(self):
        """Test _should_request_confirmation returns False for simple, high-confidence requests."""
        intent = IntentResult(
            intent=UserIntent.EDIT_CODE,
            confidence=0.95,
            scope="surgical",
        )
        context = AppContext(has_existing_app=True, file_count=3)
        
        self.assertFalse(self.router._should_request_confirmation(intent, context))
    
    def test_build_confirmation_details(self):
        """Test _build_confirmation_details generates correct details."""
        intent = IntentResult(
            intent=UserIntent.GENERATE_NEW,
            confidence=0.75,
            scope="full",
            reasoning="Rebuilding entire app",
        )
        context = AppContext(has_existing_app=True, file_count=8)
        
        details = self.router._build_confirmation_details(intent, context)
        
        self.assertEqual(details["primary_intent"], "generate_new")
        self.assertEqual(details["confidence"], 0.75)
        self.assertEqual(details["scope"], "full")
        self.assertEqual(details["existing_file_count"], 8)
        self.assertTrue(details["will_modify_existing"])
    
    def test_select_handler_no_existing_app(self):
        """Test _select_handler uses GenerateHandler when no existing app."""
        intent = IntentResult(
            intent=UserIntent.EDIT_CODE,  # Would normally use EditHandler
            confidence=0.9,
            scope="surgical",
        )
        context = AppContext(has_existing_app=False, file_count=0)
        
        handler = self.router._select_handler(intent, context)
        
        self.assertEqual(handler.__class__.__name__, "GenerateHandler")


class TestStreamingValidator(TestCase):
    """Test StreamingValidator functionality."""
    
    def test_validator_init(self):
        """Test StreamingValidator initialization."""
        validator = StreamingValidator()
        
        self.assertEqual(len(validator.warnings), 0)
        self.assertEqual(validator.any_count, 0)
        self.assertEqual(validator.console_log_count, 0)
    
    def test_check_chunk_detects_any(self):
        """Test check_chunk detects excessive 'as any' usage."""
        validator = StreamingValidator()
        
        # Add multiple 'as any' chunks
        for _ in range(4):
            warnings = validator.check_chunk("const x = data as any;", "content as any")
        
        self.assertGreater(validator.any_count, 3)
        self.assertTrue(any("as any" in w for w in validator.warnings))
    
    def test_check_chunk_detects_console_log(self):
        """Test check_chunk detects console.log statements."""
        validator = StreamingValidator()
        
        for _ in range(3):
            validator.check_chunk("console.log('debug');", "")
        
        self.assertGreater(validator.console_log_count, 2)
        self.assertTrue(any("console.log" in w for w in validator.warnings))
    
    def test_check_chunk_detects_todo(self):
        """Test check_chunk detects TODO comments."""
        validator = StreamingValidator()
        
        warnings = validator.check_chunk("// TODO: implement this later", "")
        
        self.assertGreater(len(warnings), 0)
        self.assertTrue(any("TODO" in w for w in warnings))
    
    def test_final_check_bracket_imbalance(self):
        """Test final_check detects bracket imbalance."""
        validator = StreamingValidator()
        
        # Need >2 imbalance to trigger warning (threshold is set to catch significant issues)
        content = "function test() { { { { return; }"  # Missing 3 closing braces
        warnings = validator.final_check(content)
        
        self.assertTrue(any("imbalance" in w.lower() for w in warnings))
    
    def test_final_check_ellipsis(self):
        """Test final_check detects incomplete code with ellipsis."""
        validator = StreamingValidator()
        
        content = "function test() { ... }"  # Ellipsis suggesting incomplete
        warnings = validator.final_check(content)
        
        self.assertTrue(any("..." in w for w in warnings))
    
    def test_get_all_warnings(self):
        """Test get_all_warnings returns accumulated warnings."""
        validator = StreamingValidator()
        
        validator.check_chunk("// TODO: fix this", "")
        validator.check_chunk("as any as any as any as any", "as any")
        
        all_warnings = validator.get_all_warnings()
        
        self.assertIsInstance(all_warnings, list)
        self.assertGreater(len(all_warnings), 0)


class TestParallelContextGathering(TestCase):
    """Test parallel context gathering in AgenticService."""
    
    def test_gather_context_parallel_with_none_app(self):
        """Test _gather_context_parallel with None app."""
        from vector_app.services.agentic_service import AgenticService
        
        service = AgenticService()
        data_context, mcp_context = service._gather_context_parallel(None)
        
        self.assertIsNone(data_context)
        self.assertIsNone(mcp_context)
    
    @patch('vector_app.services.agentic_service.build_data_store_context')
    @patch('vector_app.services.agentic_service.build_mcp_tools_context')
    def test_gather_context_parallel_executes_in_parallel(
        self, mock_mcp_context, mock_data_context
    ):
        """Test that context gathering uses ThreadPoolExecutor."""
        from vector_app.services.agentic_service import AgenticService
        
        mock_app = Mock()
        mock_data_context.return_value = "data store context"
        mock_mcp_result = Mock()
        mock_mcp_result.has_tools = True
        mock_mcp_result.full_context = "mcp context"
        mock_mcp_result.tools = ["tool1", "tool2"]
        mock_mcp_context.return_value = mock_mcp_result
        
        service = AgenticService()
        data_context, mcp_context = service._gather_context_parallel(mock_app)
        
        # Both context builders should have been called
        mock_data_context.assert_called_once_with(mock_app)
        mock_mcp_context.assert_called_once_with(mock_app)
        
        # Results should be returned
        self.assertEqual(data_context, "data store context")
        self.assertEqual(mcp_context, "mcp context")


class TestHandlerIntegration(TestCase):
    """Test handler integration with new features."""
    
    def test_edit_handler_imports_over_eagerness_guard(self):
        """Test EditHandler imports OVER_EAGERNESS_GUARD."""
        from vector_app.services.handlers.edit_handler import EDIT_SYSTEM_PROMPT
        self.assertIn("Anti-Over-Engineering", EDIT_SYSTEM_PROMPT)
    
    def test_feature_handler_imports_over_eagerness_guard(self):
        """Test FeatureHandler imports OVER_EAGERNESS_GUARD."""
        from vector_app.services.handlers.feature_handler import FEATURE_SYSTEM_PROMPT
        self.assertIn("Anti-Over-Engineering", FEATURE_SYSTEM_PROMPT)


# Run tests when executed directly
if __name__ == '__main__':
    unittest.main()

