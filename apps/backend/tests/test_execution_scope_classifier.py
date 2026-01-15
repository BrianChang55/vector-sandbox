"""
Tests for the Execution Scope Classifier Service

Tests the classification of user requests into cost tiers (XS, S, M, L, XL).
"""
import pytest
from unittest.mock import patch, MagicMock

from vector_app.ai.models import AIModel
from vector_app.services.execution_scope_classifier import (
    ExecutionScopeClassifier,
    ExecutionScope,
    ExecutionScopeResult,
    get_execution_scope_classifier,
    SCOPE_COMPUTE_UNITS,
)


class TestExecutionScopeClassifier:
    """Tests for ExecutionScopeClassifier."""
    
    @pytest.fixture
    def classifier(self):
        """Create a classifier instance."""
        return ExecutionScopeClassifier()
    
    # =========================================================================
    # Heuristic Classification Tests
    # =========================================================================
    
    def test_classify_explanation_as_xs(self, classifier):
        """Test that explanation-only requests are classified as XS."""
        prompts = [
            "What is React?",
            "Explain how useState works",
            "How does authentication work?",
            "Tell me about the architecture",
            "What's the difference between REST and GraphQL?",
        ]
        for prompt in prompts:
            result = classifier.classify(prompt, use_heuristics_only=True)
            assert result.scope == ExecutionScope.XS, f"Expected XS for: {prompt}"
    
    def test_classify_small_change_as_s(self, classifier):
        """Test that small changes are classified as S."""
        prompts = [
            "Change the button color to blue",
            "Fix typo in the header",
            "Rename the variable",
            "Update text in the footer",
            "Adjust padding on the card",
        ]
        for prompt in prompts:
            result = classifier.classify(prompt, use_heuristics_only=True)
            assert result.scope == ExecutionScope.S, f"Expected S for: {prompt}"
    
    def test_classify_feature_addition_as_m(self, classifier):
        """Test that feature additions are classified as M."""
        prompts = [
            "Add a search feature",
            "Add pagination to the list",
            "Create a modal for editing",
            "Add validation to the form",
            "Implement a filter dropdown",
        ]
        for prompt in prompts:
            result = classifier.classify(prompt, use_heuristics_only=True)
            assert result.scope == ExecutionScope.M, f"Expected M for: {prompt}"
    
    def test_classify_app_generation_as_l(self, classifier):
        """Test that app generation requests are classified as L."""
        prompts = [
            "Build me a todo app",
            "Create a dashboard for analytics",
            "Generate a user management system",
            "Make me an e-commerce app",
            "Build an API for the backend",
        ]
        for prompt in prompts:
            result = classifier.classify(prompt, use_heuristics_only=True)
            assert result.scope == ExecutionScope.L, f"Expected L for: {prompt}"
    
    def test_classify_rebuild_as_xl(self, classifier):
        """Test that rebuild/rewrite requests are classified as XL."""
        prompts = [
            "Rebuild the entire application",
            "Start over with a new architecture",
            "Completely rewrite the backend",
            "Major refactor of the codebase",
            "Regenerate the entire frontend from scratch",
        ]
        for prompt in prompts:
            result = classifier.classify(prompt, use_heuristics_only=True)
            assert result.scope == ExecutionScope.XL, f"Expected XL for: {prompt}"
    
    def test_classify_short_prompt_as_s(self, classifier):
        """Test that very short prompts default to S."""
        result = classifier.classify("hello", use_heuristics_only=True)
        assert result.scope == ExecutionScope.S
    
    def test_classify_long_prompt_prefers_larger_bucket(self, classifier):
        """Test that long complex prompts prefer larger buckets."""
        long_prompt = (
            "I need you to update the user profile page to include "
            "additional fields for address, phone number, and preferences. "
            "Also connect it to the notification system and add email verification."
        )
        result = classifier.classify(long_prompt, use_heuristics_only=True)
        assert result.scope in [ExecutionScope.M, ExecutionScope.L, ExecutionScope.XL]
    
    # =========================================================================
    # LLM Response Parsing Tests
    # =========================================================================
    
    def test_parse_llm_response_simple(self, classifier):
        """Test parsing simple LLM responses."""
        for label in ["XS", "S", "M", "L", "XL"]:
            result = classifier._parse_llm_response(label)
            assert result is not None
            assert result.scope.value == label
    
    def test_parse_llm_response_with_whitespace(self, classifier):
        """Test parsing responses with extra whitespace."""
        result = classifier._parse_llm_response("  M  \n")
        assert result is not None
        assert result.scope == ExecutionScope.M
    
    def test_parse_llm_response_with_punctuation(self, classifier):
        """Test parsing responses with punctuation."""
        result = classifier._parse_llm_response("L.")
        assert result is not None
        assert result.scope == ExecutionScope.L
    
    def test_parse_llm_response_lowercase(self, classifier):
        """Test parsing lowercase responses."""
        result = classifier._parse_llm_response("xl")
        assert result is not None
        assert result.scope == ExecutionScope.XL
    
    def test_parse_llm_response_in_sentence(self, classifier):
        """Test extracting scope from a sentence (edge case)."""
        result = classifier._parse_llm_response("The scope is M")
        assert result is not None
        assert result.scope == ExecutionScope.M
    
    def test_parse_llm_response_invalid(self, classifier):
        """Test that invalid responses return None."""
        result = classifier._parse_llm_response("invalid")
        assert result is None
    
    # =========================================================================
    # Compute Units and Cost Estimation Tests
    # =========================================================================
    
    def test_compute_units_mapping(self, classifier):
        """Test that compute units are correctly mapped."""
        assert classifier.get_compute_units(ExecutionScope.XS) == 1
        assert classifier.get_compute_units(ExecutionScope.S) == 2
        assert classifier.get_compute_units(ExecutionScope.M) == 5
        assert classifier.get_compute_units(ExecutionScope.L) == 10
        assert classifier.get_compute_units(ExecutionScope.XL) == 20
    
    def test_estimate_cost(self, classifier):
        """Test cost estimation calculation."""
        # Default rate: $0.01 per unit
        assert classifier.estimate_cost(ExecutionScope.XS) == 0.01
        assert classifier.estimate_cost(ExecutionScope.S) == 0.02
        assert classifier.estimate_cost(ExecutionScope.M) == 0.05
        assert classifier.estimate_cost(ExecutionScope.L) == 0.10
        assert classifier.estimate_cost(ExecutionScope.XL) == 0.20
    
    def test_estimate_cost_custom_rate(self, classifier):
        """Test cost estimation with custom rate."""
        assert classifier.estimate_cost(ExecutionScope.M, base_rate_per_unit=0.05) == 0.25
    
    # =========================================================================
    # Result Serialization Tests
    # =========================================================================
    
    def test_result_to_dict(self):
        """Test ExecutionScopeResult serialization."""
        result = ExecutionScopeResult(
            scope=ExecutionScope.M,
            confidence=0.9,
            raw_response="M",
        )
        data = result.to_dict()
        assert data["scope"] == "M"
        assert data["confidence"] == 0.9
        assert data["raw_response"] == "M"
    
    # =========================================================================
    # Singleton Tests
    # =========================================================================
    
    def test_singleton_pattern(self):
        """Test that get_execution_scope_classifier returns singleton."""
        classifier1 = get_execution_scope_classifier()
        classifier2 = get_execution_scope_classifier()
        assert classifier1 is classifier2


class TestExecutionScopeClassifierWithLLM:
    """Tests that involve mocked LLM calls."""
    
    @pytest.fixture
    def classifier(self):
        return ExecutionScopeClassifier()
    
    @patch('vector_app.services.execution_scope_classifier.httpx.Client')
    def test_llm_classify_success(self, mock_client_class, classifier):
        """Test successful LLM classification."""
        # Mock the response
        mock_response = MagicMock()
        mock_response.json.return_value = {
            "choices": [{"message": {"content": "L"}}]
        }
        mock_response.raise_for_status = MagicMock()
        
        mock_client = MagicMock()
        mock_client.__enter__ = MagicMock(return_value=mock_client)
        mock_client.__exit__ = MagicMock(return_value=False)
        mock_client.post.return_value = mock_response
        mock_client_class.return_value = mock_client
        
        # Set API key
        classifier.api_key = "test-key"
        
        result = classifier._llm_classify("Build me an app", AIModel.CLAUDE_HAIKU_4_5)
        
        assert result is not None
        assert result.scope == ExecutionScope.L
    
    @patch('vector_app.services.execution_scope_classifier.httpx.Client')
    def test_llm_classify_no_api_key(self, mock_client_class, classifier):
        """Test LLM classification without API key returns None."""
        classifier.api_key = None
        
        result = classifier._llm_classify("Build me an app", AIModel.CLAUDE_HAIKU_4_5)
        
        assert result is None
        mock_client_class.assert_not_called()


class TestEdgeCases:
    """Test edge cases and boundary conditions."""
    
    @pytest.fixture
    def classifier(self):
        return ExecutionScopeClassifier()
    
    def test_empty_prompt(self, classifier):
        """Test classification of empty prompt."""
        result = classifier.classify("", use_heuristics_only=True)
        # Empty prompts should default to S (short prompt heuristic)
        assert result.scope == ExecutionScope.S
    
    def test_mixed_signals_prefers_larger(self, classifier):
        """Test that mixed signals prefer larger bucket per spec."""
        # This has both explanation and feature keywords
        prompt = "What is pagination and add it to the list"
        result = classifier.classify(prompt, use_heuristics_only=True)
        # Should not be XS because it also asks for a feature
        assert result.scope in [ExecutionScope.M, ExecutionScope.L]
    
    def test_all_caps_prompt(self, classifier):
        """Test classification handles all caps."""
        result = classifier.classify("BUILD ME A TODO APP", use_heuristics_only=True)
        assert result.scope == ExecutionScope.L
    
    def test_confidence_levels(self, classifier):
        """Test that confidence levels are reasonable."""
        # High confidence for clear matches
        result = classifier.classify("Rebuild everything from scratch", use_heuristics_only=True)
        assert result.confidence >= 0.85
        
        # Lower confidence for ambiguous prompts
        result = classifier.classify("do something with the app", use_heuristics_only=True)
        assert result.confidence <= 0.7

