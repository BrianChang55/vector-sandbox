#!/usr/bin/env python
"""
Test cases for the PlanningService (plan generation)
"""
import os
import sys
import django
import json
from unittest.mock import patch, MagicMock

os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'internal_apps.settings')
django.setup()

from vector_app.services.planning_service import PlanningService, PlanStep, AgentPlan


def test_simple_plan_generation():
    """
    Test: Simple Plan Generation
    
    A basic plan request with mocked API response should generate
    a valid AgentPlan with steps.
    """
    mock_response_data = {
        "choices": [{
            "message": {
                "content": json.dumps({
                    "reasoning": "Building a todo app with tasks and categories.",
                    "steps": [
                        {
                            "type": "data",
                            "step_order": 0,
                            "title": "Create Data Tables",
                            "description": "Create table schemas for: tasks (title, completed, due_date), categories (name, color)"
                        },
                        {
                            "type": "component",
                            "step_order": 1,
                            "title": "Build Task List Component",
                            "description": "Create src/components/TaskList.tsx: Fetches tasks from 'tasks' table. Displays list with checkboxes."
                        }
                    ]
                })
            }
        }]
    }
    
    with patch('httpx.Client') as mock_client_class:
        mock_response = MagicMock()
        mock_response.json.return_value = mock_response_data
        mock_response.raise_for_status = MagicMock()
        
        mock_client = MagicMock()
        mock_client.__enter__.return_value = mock_client
        mock_client.__exit__.return_value = None
        mock_client.post.return_value = mock_response
        mock_client_class.return_value = mock_client
        
        service = PlanningService()
        plan = service.create_plan(
            user_message="Create a todo app",
            context={"app_name": "TodoApp", "has_existing_spec": False}
        )
    
    # Validation should succeed
    assert plan is not None, "Plan should not be None"
    assert isinstance(plan, AgentPlan), "Plan should be an AgentPlan instance"
    assert plan.goal == "Create a todo app", f"Plan goal should match: {plan.goal}"
    assert len(plan.steps) == 2, f"Plan should have 2 steps, got {len(plan.steps)}"
    assert plan.steps[0].type == "data", f"First step should be 'data', got {plan.steps[0].type}"
    assert plan.steps[1].type == "component", f"Second step should be 'component', got {plan.steps[1].type}"
    assert plan.reasoning == "Building a todo app with tasks and categories.", f"Reasoning should match: {plan.reasoning}"


def test_plan_with_data_tables():
    """
    Test: Plan with Data Tables
    
    A plan that includes data/table creation step should properly
    parse the data step with step_order=0.
    """
    mock_response_data = {
        "choices": [{
            "message": {
                "content": json.dumps({
                    "reasoning": "Creating a project management app requires multiple tables.",
                    "steps": [
                        {
                            "type": "data",
                            "step_order": 0,
                            "title": "Create Database Tables",
                            "description": "Create table schemas for: projects (name, description, status), tasks (title, project_id FK, assignee_id, due_date), team_members (name, email, role)"
                        }
                    ]
                })
            }
        }]
    }
    
    with patch('httpx.Client') as mock_client_class:
        mock_response = MagicMock()
        mock_response.json.return_value = mock_response_data
        mock_response.raise_for_status = MagicMock()
        
        mock_client = MagicMock()
        mock_client.__enter__.return_value = mock_client
        mock_client.__exit__.return_value = None
        mock_client.post.return_value = mock_response
        mock_client_class.return_value = mock_client
        
        service = PlanningService()
        plan = service.create_plan(
            user_message="Create a project management app",
            context={"app_name": "ProjectApp", "has_existing_spec": False}
        )
    
    assert plan is not None, "Plan should not be None"
    assert len(plan.steps) == 1, f"Plan should have 1 step, got {len(plan.steps)}"
    assert plan.steps[0].type == "data", f"Step type should be 'data', got {plan.steps[0].type}"
    assert "projects" in plan.steps[0].description.lower(), "Description should mention projects table"
    assert "tasks" in plan.steps[0].description.lower(), "Description should mention tasks table"


def test_plan_with_existing_spec():
    """
    Test: Plan with Existing Spec
    
    A plan for modifying an existing app should include context
    about the existing specification.
    """
    mock_response_data = {
        "choices": [{
            "message": {
                "content": json.dumps({
                    "reasoning": "Adding a new feature to the existing app.",
                    "steps": [
                        {
                            "type": "component",
                            "step_order": 1,
                            "title": "Add New Feature Component",
                            "description": "Create src/components/NewFeature.tsx"
                        }
                    ]
                })
            }
        }]
    }
    
    with patch('httpx.Client') as mock_client_class:
        mock_response = MagicMock()
        mock_response.json.return_value = mock_response_data
        mock_response.raise_for_status = MagicMock()
        
        mock_client = MagicMock()
        mock_client.__enter__.return_value = mock_client
        mock_client.__exit__.return_value = None
        mock_client.post.return_value = mock_response
        mock_client_class.return_value = mock_client
        
        service = PlanningService()
        plan = service.create_plan(
            user_message="Add a new dashboard feature",
            context={"app_name": "MyApp", "has_existing_spec": True, "current_pages": 3}
        )
    
    assert plan is not None, "Plan should not be None"
    assert "existing" in plan.reasoning.lower() or "new" in plan.reasoning.lower(), "Reasoning should mention modification"


def test_plan_json_parsing_with_markdown():
    """
    Test: Plan JSON Parsing with Markdown
    
    JSON wrapped in markdown code blocks should be properly extracted.
    """
    mock_response_data = {
        "choices": [{
            "message": {
                "content": """Here's the plan:
```json
{
    "reasoning": "Building a simple app",
    "steps": [
        {
            "type": "component",
            "step_order": 1,
            "title": "Create Component",
            "description": "Build the main component"
        }
    ]
}
```"""
            }
        }]
    }
    
    with patch('httpx.Client') as mock_client_class:
        mock_response = MagicMock()
        mock_response.json.return_value = mock_response_data
        mock_response.raise_for_status = MagicMock()
        
        mock_client = MagicMock()
        mock_client.__enter__.return_value = mock_client
        mock_client.__exit__.return_value = None
        mock_client.post.return_value = mock_response
        mock_client_class.return_value = mock_client
        
        service = PlanningService()
        plan = service.create_plan(
            user_message="Create a simple app",
            context={"app_name": "SimpleApp", "has_existing_spec": False}
        )
    
    assert plan is not None, "Plan should not be None"
    assert len(plan.steps) == 1, f"Plan should have 1 step, got {len(plan.steps)}"
    assert plan.steps[0].title == "Create Component", f"Step title should match: {plan.steps[0].title}"


def test_plan_json_parsing_raw():
    """
    Test: Plan JSON Parsing Raw
    
    Raw JSON without markdown should be parsed correctly.
    """
    mock_response_data = {
        "choices": [{
            "message": {
                "content": json.dumps({
                    "reasoning": "Raw JSON plan",
                    "steps": [
                        {
                            "type": "styling",
                            "step_order": 2,
                            "title": "Apply Styles",
                            "description": "Add Tailwind CSS styling"
                        }
                    ]
                })
            }
        }]
    }
    
    with patch('httpx.Client') as mock_client_class:
        mock_response = MagicMock()
        mock_response.json.return_value = mock_response_data
        mock_response.raise_for_status = MagicMock()
        
        mock_client = MagicMock()
        mock_client.__enter__.return_value = mock_client
        mock_client.__exit__.return_value = None
        mock_client.post.return_value = mock_response
        mock_client_class.return_value = mock_client
        
        service = PlanningService()
        plan = service.create_plan(
            user_message="Style the app",
            context={"app_name": "StyledApp", "has_existing_spec": False}
        )
    
    assert plan is not None, "Plan should not be None"
    assert plan.steps[0].type == "styling", f"Step type should be 'styling', got {plan.steps[0].type}"


def test_plan_fallback_on_api_error():
    """
    Test: Plan Fallback on API Error
    
    When API call fails, should return a fallback plan.
    """
    with patch('httpx.Client') as mock_client_class:
        mock_client = MagicMock()
        mock_client.__enter__.return_value = mock_client
        mock_client.__exit__.return_value = None
        mock_client.post.side_effect = Exception("API Error")
        mock_client_class.return_value = mock_client
        
        service = PlanningService()
        plan = service.create_plan(
            user_message="Create an app",
            context={"app_name": "TestApp", "has_existing_spec": False}
        )
    
    # Should return fallback plan
    assert plan is not None, "Plan should not be None"
    assert isinstance(plan, AgentPlan), "Plan should be an AgentPlan instance"
    assert len(plan.steps) == 5, f"Fallback plan should have 5 steps, got {len(plan.steps)}"
    assert plan.reasoning == "Building a React app based on your request.", "Should use fallback reasoning"


def test_plan_fallback_on_invalid_json():
    """
    Test: Plan Fallback on Invalid JSON
    
    When API returns invalid JSON, should fall back to default plan.
    """
    mock_response_data = {
        "choices": [{
            "message": {
                "content": "This is not valid JSON at all!!!"
            }
        }]
    }
    
    with patch('httpx.Client') as mock_client_class:
        mock_response = MagicMock()
        mock_response.json.return_value = mock_response_data
        mock_response.raise_for_status = MagicMock()
        
        mock_client = MagicMock()
        mock_client.__enter__.return_value = mock_client
        mock_client.__exit__.return_value = None
        mock_client.post.return_value = mock_response
        mock_client_class.return_value = mock_client
        
        service = PlanningService()
        plan = service.create_plan(
            user_message="Create an app",
            context={"app_name": "TestApp", "has_existing_spec": False}
        )
    
    # Should return fallback plan
    assert plan is not None, "Plan should not be None"
    assert len(plan.steps) == 5, f"Fallback plan should have 5 steps, got {len(plan.steps)}"


def test_plan_step_order_assignment():
    """
    Test: Plan Step Order Assignment
    
    Steps should preserve their step_order from the API response
    (though we don't store it in PlanStep, we verify the plan structure).
    """
    mock_response_data = {
        "choices": [{
            "message": {
                "content": json.dumps({
                    "reasoning": "Multi-step plan with ordering",
                    "steps": [
                        {
                            "type": "data",
                            "step_order": 0,
                            "title": "Step 0",
                            "description": "First step"
                        },
                        {
                            "type": "component",
                            "step_order": 1,
                            "title": "Step 1",
                            "description": "Second step"
                        },
                        {
                            "type": "styling",
                            "step_order": 2,
                            "title": "Step 2",
                            "description": "Third step"
                        }
                    ]
                })
            }
        }]
    }
    
    with patch('httpx.Client') as mock_client_class:
        mock_response = MagicMock()
        mock_response.json.return_value = mock_response_data
        mock_response.raise_for_status = MagicMock()
        
        mock_client = MagicMock()
        mock_client.__enter__.return_value = mock_client
        mock_client.__exit__.return_value = None
        mock_client.post.return_value = mock_response
        mock_client_class.return_value = mock_client
        
        service = PlanningService()
        plan = service.create_plan(
            user_message="Create ordered steps",
            context={"app_name": "OrderedApp", "has_existing_spec": False}
        )
    
    assert plan is not None, "Plan should not be None"
    assert len(plan.steps) == 3, f"Plan should have 3 steps, got {len(plan.steps)}"
    assert plan.steps[0].type == "data", "First step should be data"
    assert plan.steps[1].type == "component", "Second step should be component"
    assert plan.steps[2].type == "styling", "Third step should be styling"


def test_plan_parallel_steps():
    """
    Test: Plan Parallel Steps
    
    Steps with the same step_order should be parsed correctly
    (they can run in parallel).
    """
    mock_response_data = {
        "choices": [{
            "message": {
                "content": json.dumps({
                    "reasoning": "Parallel execution plan",
                    "steps": [
                        {
                            "type": "component",
                            "step_order": 1,
                            "title": "Component A",
                            "description": "Create src/components/A.tsx"
                        },
                        {
                            "type": "component",
                            "step_order": 1,
                            "title": "Component B",
                            "description": "Create src/components/B.tsx"
                        },
                        {
                            "type": "component",
                            "step_order": 1,
                            "title": "Component C",
                            "description": "Create src/components/C.tsx"
                        }
                    ]
                })
            }
        }]
    }
    
    with patch('httpx.Client') as mock_client_class:
        mock_response = MagicMock()
        mock_response.json.return_value = mock_response_data
        mock_response.raise_for_status = MagicMock()
        
        mock_client = MagicMock()
        mock_client.__enter__.return_value = mock_client
        mock_client.__exit__.return_value = None
        mock_client.post.return_value = mock_response
        mock_client_class.return_value = mock_client
        
        service = PlanningService()
        plan = service.create_plan(
            user_message="Create parallel components",
            context={"app_name": "ParallelApp", "has_existing_spec": False}
        )
    
    assert plan is not None, "Plan should not be None"
    assert len(plan.steps) == 3, f"Plan should have 3 steps, got {len(plan.steps)}"
    # All steps should be component type
    assert all(s.type == "component" for s in plan.steps), "All steps should be component type"


def test_plan_with_mcp_context():
    """
    Test: Plan with MCP Context
    
    Plan generation should work with MCP/connector context included.
    """
    mock_response_data = {
        "choices": [{
            "message": {
                "content": json.dumps({
                    "reasoning": "Using MCP connectors for data integration",
                    "steps": [
                        {
                            "type": "integration",
                            "step_order": 1,
                            "title": "Integrate MCP Connectors",
                            "description": "Connect to external APIs via MCP tools"
                        }
                    ]
                })
            }
        }]
    }
    
    with patch('httpx.Client') as mock_client_class:
        mock_response = MagicMock()
        mock_response.json.return_value = mock_response_data
        mock_response.raise_for_status = MagicMock()
        
        mock_client = MagicMock()
        mock_client.__enter__.return_value = mock_client
        mock_client.__exit__.return_value = None
        mock_client.post.return_value = mock_response
        mock_client_class.return_value = mock_client
        
        service = PlanningService()
        plan = service.create_plan(
            user_message="Create app with MCP integration",
            context={
                "app_name": "MCPApp",
                "has_existing_spec": False,
                "connectors_summary": "Slack, GitHub, Email connectors available"
            }
        )
    
    assert plan is not None, "Plan should not be None"
    assert len(plan.steps) == 1, f"Plan should have 1 step, got {len(plan.steps)}"
    assert plan.steps[0].type == "integration", f"Step type should be 'integration', got {plan.steps[0].type}"


def test_plan_with_extra_text():
    """
    Test: Plan JSON Parsing with Extra Text
    
    JSON with extra text before/after should be extracted correctly.
    """
    mock_response_data = {
        "choices": [{
            "message": {
                "content": """Here's my analysis and plan:

{
    "reasoning": "Extracted from text",
    "steps": [
        {
            "type": "code",
            "step_order": 1,
            "title": "Write Code",
            "description": "Generate the code"
        }
    ]
}

That's the complete plan."""
            }
        }]
    }
    
    with patch('httpx.Client') as mock_client_class:
        mock_response = MagicMock()
        mock_response.json.return_value = mock_response_data
        mock_response.raise_for_status = MagicMock()
        
        mock_client = MagicMock()
        mock_client.__enter__.return_value = mock_client
        mock_client.__exit__.return_value = None
        mock_client.post.return_value = mock_response
        mock_client_class.return_value = mock_client
        
        service = PlanningService()
        plan = service.create_plan(
            user_message="Create an app",
            context={"app_name": "TextApp", "has_existing_spec": False}
        )
    
    assert plan is not None, "Plan should not be None"
    assert len(plan.steps) == 1, f"Plan should have 1 step, got {len(plan.steps)}"
    assert plan.steps[0].type == "code", f"Step type should be 'code', got {plan.steps[0].type}"


if __name__ == '__main__':
    test_simple_plan_generation()
    print("✓ test_simple_plan_generation")
    
    test_plan_with_data_tables()
    print("✓ test_plan_with_data_tables")
    
    test_plan_with_existing_spec()
    print("✓ test_plan_with_existing_spec")
    
    test_plan_json_parsing_with_markdown()
    print("✓ test_plan_json_parsing_with_markdown")
    
    test_plan_json_parsing_raw()
    print("✓ test_plan_json_parsing_raw")
    
    test_plan_fallback_on_api_error()
    print("✓ test_plan_fallback_on_api_error")
    
    test_plan_fallback_on_invalid_json()
    print("✓ test_plan_fallback_on_invalid_json")
    
    test_plan_step_order_assignment()
    print("✓ test_plan_step_order_assignment")
    
    test_plan_parallel_steps()
    print("✓ test_plan_parallel_steps")
    
    test_plan_with_mcp_context()
    print("✓ test_plan_with_mcp_context")
    
    test_plan_with_extra_text()
    print("✓ test_plan_with_extra_text")
    
    print("\n✅ All tests passed!")
