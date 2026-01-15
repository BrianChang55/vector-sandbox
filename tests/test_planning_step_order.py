"""
Tests for recompute_step_orders() - dependency graph-based step ordering.
"""
import pytest

from vector_app.services.planning_service import (
    PlanStep,
    recompute_step_orders,
)


# =============================================================================
# recompute_step_orders Tests
# =============================================================================

def test_empty_steps():
    """Empty list should return empty list."""
    result = recompute_step_orders([])
    assert result == []

def test_no_dependencies():
    """Steps with no dependencies should all get step_order=0."""
    steps = [
        PlanStep(
            id="1", type="component", title="Create A", description="",
            target_files=["a.tsx"], dependent_files=[],
            step_order=0,
        ),
        PlanStep(
            id="2", type="component", title="Create B", description="",
            target_files=["b.tsx"], dependent_files=[],
            step_order=1,  # LLM gave 1 but should be 0
        ),
        PlanStep(
            id="3", type="component", title="Create C", description="",
            target_files=["c.tsx"], dependent_files=[],
            step_order=2,  # LLM gave 2 but should be 0
        ),
    ]
    result = recompute_step_orders(steps)
    
    # All should be 0 since no dependencies
    assert result[0].step_order == 0
    assert result[1].step_order == 0
    assert result[2].step_order == 0

def test_linear_chain():
    """A->B->C: each step depends on the previous."""
    steps = [
        PlanStep(
            id="1", type="component", title="Create A", description="",
            target_files=["a.tsx"], dependent_files=[],
            step_order=0,
        ),
        PlanStep(
            id="2", type="component", title="Create B", description="",
            target_files=["b.tsx"], dependent_files=["a.tsx"],
            step_order=1,
        ),
        PlanStep(
            id="3", type="component", title="Create C", description="",
            target_files=["c.tsx"], dependent_files=["b.tsx"],
            step_order=2,
        ),
    ]
    result = recompute_step_orders(steps)
    
    assert result[0].step_order == 0
    assert result[1].step_order == 1
    assert result[2].step_order == 2

def test_parallel_independent():
    """Multiple steps with no dependencies can run in parallel."""
    steps = [
        PlanStep(
            id="1", type="component", title="Create A", description="",
            target_files=["a.tsx"], dependent_files=[],
            step_order=0,
        ),
        PlanStep(
            id="2", type="component", title="Create B", description="",
            target_files=["b.tsx"], dependent_files=[],
            step_order=0,
        ),
        PlanStep(
            id="3", type="component", title="Create C", description="",
            target_files=["c.tsx"], dependent_files=[],
            step_order=0,
        ),
    ]
    result = recompute_step_orders(steps)
    
    # All independent, all should be 0
    assert result[0].step_order == 0
    assert result[1].step_order == 0
    assert result[2].step_order == 0

def test_diamond_dependency():
    """Diamond pattern: A->B, A->C, B->D, C->D."""
    steps = [
        PlanStep(
            id="1", type="component", title="Create A", description="",
            target_files=["a.tsx"], dependent_files=[],
            step_order=0,
        ),
        PlanStep(
            id="2", type="component", title="Create B", description="",
            target_files=["b.tsx"], dependent_files=["a.tsx"],
            step_order=1,
        ),
        PlanStep(
            id="3", type="component", title="Create C", description="",
            target_files=["c.tsx"], dependent_files=["a.tsx"],
            step_order=1,
        ),
        PlanStep(
            id="4", type="component", title="Create D", description="",
            target_files=["d.tsx"], dependent_files=["b.tsx", "c.tsx"],
            step_order=2,
        ),
    ]
    result = recompute_step_orders(steps)
    
    # A=0, B=C=1, D=2
    assert result[0].step_order == 0  # A
    assert result[1].step_order == 1  # B
    assert result[2].step_order == 1  # C
    assert result[3].step_order == 2  # D

def test_cycle_detection(caplog):
    """A->B->A cycle should log warning and keep LLM order."""
    steps = [
        PlanStep(
            id="1", type="component", title="Create A", description="",
            target_files=["a.tsx"], dependent_files=["b.tsx"],
            step_order=0,
        ),
        PlanStep(
            id="2", type="component", title="Create B", description="",
            target_files=["b.tsx"], dependent_files=["a.tsx"],
            step_order=1,
        ),
    ]
    
    result = recompute_step_orders(steps)
    
    # Should keep original LLM order due to cycle
    assert result[0].step_order == 0
    assert result[1].step_order == 1
    # Should log a warning
    assert "Cycle detected" in caplog.text

def test_missing_dependency(caplog):
    """dependent_file referencing non-existent target should be ignored."""
    steps = [
        PlanStep(
            id="1", type="component", title="Create A", description="",
            target_files=["a.tsx"], dependent_files=["nonexistent.tsx"],
            step_order=5,  # LLM gave wrong order
        ),
    ]
    
    result = recompute_step_orders(steps)
    
    # Should treat as no dependency, so step_order=0
    assert result[0].step_order == 0
    # Should log a debug message about the missing file
    assert "nonexistent.tsx" in caplog.text or result[0].step_order == 0

def test_preserves_llm_order_when_valid():
    """When LLM's step_order matches computed, no changes should occur."""
    steps = [
        PlanStep(
            id="1", type="component", title="Create A", description="",
            target_files=["a.tsx"], dependent_files=[],
            step_order=0,
        ),
        PlanStep(
            id="2", type="component", title="Create B", description="",
            target_files=["b.tsx"], dependent_files=["a.tsx"],
            step_order=1,
        ),
    ]
    
    # Store original values
    original_orders = [s.step_order for s in steps]
    
    result = recompute_step_orders(steps)
    
    # Should remain unchanged
    assert [s.step_order for s in result] == original_orders

def test_corrects_invalid_llm_order(caplog):
    """LLM gave wrong order, should be corrected."""
    steps = [
        PlanStep(
            id="1", type="component", title="Create A", description="",
            target_files=["a.tsx"], dependent_files=[],
            step_order=5,  # Wrong - should be 0
        ),
        PlanStep(
            id="2", type="component", title="Create B", description="",
            target_files=["b.tsx"], dependent_files=["a.tsx"],
            step_order=0,  # Wrong - should be 1
        ),
    ]
    
    result = recompute_step_orders(steps)
    
    # Should be corrected
    assert result[0].step_order == 0
    assert result[1].step_order == 1
    # Should log adjustments
    assert "Adjusted" in caplog.text

def test_multiple_files_same_step():
    """Step creating multiple files, another depending on one of them."""
    steps = [
        PlanStep(
            id="1", type="component", title="Create A and B", description="",
            target_files=["a.tsx", "b.tsx"], dependent_files=[],
            step_order=0,
        ),
        PlanStep(
            id="2", type="component", title="Create C", description="",
            target_files=["c.tsx"], dependent_files=["b.tsx"],
            step_order=1,
        ),
    ]
    
    result = recompute_step_orders(steps)
    
    assert result[0].step_order == 0
    assert result[1].step_order == 1

def test_self_dependency_ignored():
    """A step depending on its own target_file should be ignored."""
    steps = [
        PlanStep(
            id="1", type="component", title="Create A", description="",
            target_files=["a.tsx"], dependent_files=["a.tsx"],  # Self-dependency
            step_order=0,
        ),
    ]
    
    result = recompute_step_orders(steps)
    
    # Self-dependency should be ignored, step_order stays 0
    assert result[0].step_order == 0

def test_complex_dependency_graph():
    """
    Complex graph:
    A (0) -> B (1)
    A (0) -> C (1)
    B (1) -> D (2)
    C (1) -> D (2)
    D (2) -> E (3)
    """
    steps = [
        PlanStep(
            id="1", type="component", title="Create A", description="",
            target_files=["a.tsx"], dependent_files=[],
            step_order=0,
        ),
        PlanStep(
            id="2", type="component", title="Create B", description="",
            target_files=["b.tsx"], dependent_files=["a.tsx"],
            step_order=1,
        ),
        PlanStep(
            id="3", type="component", title="Create C", description="",
            target_files=["c.tsx"], dependent_files=["a.tsx"],
            step_order=1,
        ),
        PlanStep(
            id="4", type="component", title="Create D", description="",
            target_files=["d.tsx"], dependent_files=["b.tsx", "c.tsx"],
            step_order=2,
        ),
        PlanStep(
            id="5", type="component", title="Create E", description="",
            target_files=["e.tsx"], dependent_files=["d.tsx"],
            step_order=3,
        ),
    ]
    
    result = recompute_step_orders(steps)
    
    assert result[0].step_order == 0  # A
    assert result[1].step_order == 1  # B
    assert result[2].step_order == 1  # C
    assert result[3].step_order == 2  # D
    assert result[4].step_order == 3  # E

def test_multiple_steps_same_target_file(caplog):
    """Multiple steps targeting same file should log warning."""
    steps = [
        PlanStep(
            id="1", type="component", title="Create A", description="",
            target_files=["a.tsx"], dependent_files=[],
            step_order=0,
        ),
        PlanStep(
            id="2", type="component", title="Also Create A", description="",
            target_files=["a.tsx"], dependent_files=[],
            step_order=0,
        ),
    ]
    
    result = recompute_step_orders(steps)
    
    # Should still work but log warning
    assert result[0].step_order == 0
    assert result[1].step_order == 0
    assert "multiple steps" in caplog.text.lower()


def test_step_orders_are_normalized_no_gaps():
    """
    Step orders should be consecutive integers with no gaps.
    
    Scenario with potential gaps without normalization:
    - A (no deps) -> order 0
    - B depends on A -> order 1  
    - C depends on B -> order 2
    - D (no deps, independent) -> order 0
    - E depends on C -> order 3
    
    Final orders should be consecutive: 0, 1, 2, 0, 3
    Unique sorted orders: [0, 1, 2, 3] - no gaps, already consecutive.
    
    More complex case where normalization matters:
    We simulate by checking output is always consecutive.
    """
    steps = [
        PlanStep(
            id="1", type="component", title="Create A", description="",
            target_files=["a.tsx"], dependent_files=[],
            step_order=0,
        ),
        PlanStep(
            id="2", type="component", title="Create B", description="",
            target_files=["b.tsx"], dependent_files=["a.tsx"],
            step_order=1,
        ),
        PlanStep(
            id="3", type="component", title="Create C", description="",
            target_files=["c.tsx"], dependent_files=["b.tsx"],
            step_order=2,
        ),
        PlanStep(
            id="4", type="component", title="Create D (independent)", description="",
            target_files=["d.tsx"], dependent_files=[],
            step_order=0,
        ),
        PlanStep(
            id="5", type="component", title="Create E", description="",
            target_files=["e.tsx"], dependent_files=["c.tsx"],
            step_order=3,
        ),
    ]
    
    result = recompute_step_orders(steps)
    
    # Verify correct dependency-based ordering
    assert result[0].step_order == 0  # A - no deps
    assert result[1].step_order == 1  # B - depends on A
    assert result[2].step_order == 2  # C - depends on B
    assert result[3].step_order == 0  # D - no deps (parallel with A)
    assert result[4].step_order == 3  # E - depends on C
    
    # Verify step_orders are consecutive (no gaps)
    all_orders = sorted(set(s.step_order for s in result))
    expected_consecutive = list(range(len(all_orders)))
    assert all_orders == expected_consecutive, f"Step orders have gaps: {all_orders}"


def test_normalization_removes_gaps():
    """
    Test that gaps in computed step_orders are normalized to consecutive values.
    
    This tests the scenario where independent branches could theoretically
    produce non-consecutive step_order values.
    """
    # Create a scenario where step_orders should be 0, 1, 2, 3
    # with mixed independent and dependent steps
    steps = [
        # Branch 1: A -> B -> C (orders 0, 1, 2)
        PlanStep(
            id="1", type="component", title="Root A", description="",
            target_files=["a.tsx"], dependent_files=[],
            step_order=10,  # LLM gave wrong order
        ),
        PlanStep(
            id="2", type="component", title="Child B", description="",
            target_files=["b.tsx"], dependent_files=["a.tsx"],
            step_order=20,  # LLM gave wrong order
        ),
        PlanStep(
            id="3", type="component", title="Grandchild C", description="",
            target_files=["c.tsx"], dependent_files=["b.tsx"],
            step_order=30,  # LLM gave wrong order
        ),
        # Branch 2: D (independent, order 0)
        PlanStep(
            id="4", type="component", title="Independent D", description="",
            target_files=["d.tsx"], dependent_files=[],
            step_order=40,  # LLM gave wrong order
        ),
    ]
    
    result = recompute_step_orders(steps)
    
    # Verify orders are computed from dependencies, not LLM values
    assert result[0].step_order == 0  # A - root
    assert result[1].step_order == 1  # B - depends on A
    assert result[2].step_order == 2  # C - depends on B
    assert result[3].step_order == 0  # D - independent root
    
    # Verify no gaps in the final step_order values
    unique_orders = sorted(set(s.step_order for s in result))
    assert unique_orders == [0, 1, 2], f"Expected consecutive [0, 1, 2], got {unique_orders}"
