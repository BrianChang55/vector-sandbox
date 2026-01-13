#!/usr/bin/env python
"""
Test script for PlanningService - runs multiple app ideas and prints plan output
"""
import os
import sys
import django
from typing import List, Tuple, Optional
from concurrent.futures import ThreadPoolExecutor, as_completed

os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'internal_apps.settings')
django.setup()

from vector_app.services.planning_service import PlanningService, AgentPlan

# Boundary constants
BOUNDARY_MAJOR = "=" * 80
BOUNDARY_MINOR = "-" * 80


def get_app_ideas() -> List[str]:
    """
    Returns a list of small app ideas to test planning service.
    
    Returns:
        List of app idea strings
    """
    return [
        "Create a todo app",
        "Build a simple roi calculator",
        "Build a simple expense tracker",
        "Create a recipe searching app",
    ]


def format_plan(idea: str, plan: AgentPlan) -> str:
    """
    Format plan output as a single string.
    
    Args:
        idea: The app idea that was tested
        plan: The generated AgentPlan
        
    Returns:
        Formatted plan string
    """
    lines = [
        BOUNDARY_MAJOR,
        f"PLAN FOR: {idea}",
        BOUNDARY_MAJOR,
        f"Reasoning: {plan.reasoning}",
        f"Total Steps: {len(plan.steps)}",
        BOUNDARY_MINOR,
    ]
    
    for i, step in enumerate(plan.steps, 1):
        lines.append(f"Step {i}: [{step.type}] {step.title} (Order: {step.step_order})")
        lines.append(f"  Operation: {step.operation_type}")
        lines.append(f"  Target Files: {step.target_files}")
        lines.append(f"  Description: {step.description}")
    
    lines.extend([BOUNDARY_MAJOR, ""])
    return "\n".join(lines)


def process_idea(idea: str) -> Tuple[str, Optional[AgentPlan], Optional[str]]:
    """
    Process a single app idea and generate a plan.
    
    Args:
        idea: The app idea to process
        
    Returns:
        Tuple of (idea, plan, error_message)
    """
    service = PlanningService()
    try:
        plan = service.create_plan(
            user_message=idea,
            context={"app_name": "TestApp", "has_existing_spec": False}
        )
        return (idea, plan, None)
    except Exception as e:
        return (idea, None, str(e))


def main():
    """Main function to run planning tests."""
    app_ideas = get_app_ideas()
    
    successful = 0
    failed = 0
    results = []
    
    print(f"Testing PlanningService with {len(app_ideas)} app ideas...\n")
    
    # Process ideas concurrently
    with ThreadPoolExecutor(max_workers=len(app_ideas)) as executor:
        future_to_idea = {executor.submit(process_idea, idea): idea for idea in app_ideas}
        
        for future in as_completed(future_to_idea):
            idea, plan, error = future.result()
            results.append((idea, plan, error))
    
    # Print results
    for idea, plan, error in results:
        if error:
            error_output = "\n".join([
                BOUNDARY_MAJOR,
                f"ERROR FOR: {idea}",
                BOUNDARY_MAJOR,
                f"Error: {error}",
                BOUNDARY_MAJOR,
                ""
            ])
            print(error_output)
            failed += 1
        else:
            print(format_plan(idea, plan))
            successful += 1
    
    # Print summary
    summary_lines = [
        "",
        BOUNDARY_MAJOR,
        "SUMMARY",
        BOUNDARY_MAJOR,
        f"Total app ideas tested: {len(app_ideas)}",
        f"Successful plans: {successful}",
        f"Failed plans: {failed}",
        BOUNDARY_MAJOR,
    ]
    print("\n".join(summary_lines))


if __name__ == '__main__':
    main()
