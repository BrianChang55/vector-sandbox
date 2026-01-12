#!/usr/bin/env python
"""
Batch Test Script for Agentic Code Generation Validation

Runs 10 different simple app generation tests to validate compile-time error fixes.
"""
import json
import os
import sys
import django
import logging
import uuid
import re

# Setup Django
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'internal_apps.settings')
django.setup()

from vector_app.services.planning_service import PlanStep
from vector_app.services.handlers.generate_handler import GenerateHandler
from vector_app.prompts.agentic import (
    build_plan_prompt,
    build_step_prompt,
    build_codegen_system_prompt,
    apply_design_style_prompt,
)

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

# 10 new test cases - variety of simple apps
TEST_CASES = [
    {
        "name": "Expense Tracker",
        "prompt": "Create a simple expense tracker where I can add expenses with a description and amount. Show a list of expenses and the total.",
    },
    {
        "name": "BMI Calculator",
        "prompt": "Build a BMI calculator with fields for height (in feet/inches) and weight (in pounds). Calculate and display the BMI with a health category.",
    },
    {
        "name": "Note Taking App",
        "prompt": "Create a simple note-taking app where I can add notes with a title and content. Display notes in a list and allow deletion.",
    },
    {
        "name": "Quote Generator",
        "prompt": "Build a random quote generator that displays motivational quotes. Include a button to get a new quote.",
    },
    {
        "name": "Color Picker",
        "prompt": "Create a color picker tool with RGB sliders. Show the selected color and display the hex code.",
    },
    {
        "name": "Unit Converter",
        "prompt": "Build a unit converter for length (miles to kilometers, feet to meters). Include input field and conversion buttons.",
    },
    {
        "name": "Flashcard App",
        "prompt": "Create a flashcard study app where I can add question-answer pairs and flip cards to reveal answers.",
    },
    {
        "name": "Habit Tracker",
        "prompt": "Build a daily habit tracker where I can add habits and check them off. Show completion status for today.",
    },
    {
        "name": "Password Generator",
        "prompt": "Create a password generator with options for length and character types (uppercase, lowercase, numbers, symbols).",
    },
    {
        "name": "Tip Calculator",
        "prompt": "Build a tip calculator with bill amount input and tip percentage slider. Calculate tip and total.",
    },
]


class TestHandler(GenerateHandler):
    """Test handler for validation."""
    pass


def run_single_test(test_case: dict, model: str = "anthropic/claude-sonnet-4") -> dict:
    """Run a single test case and return results."""
    logger.info(f"\n{'='*60}")
    logger.info(f"Running test: {test_case['name']}")
    logger.info(f"{'='*60}")
    
    handler = TestHandler()
    
    # Build context
    styled_message = apply_design_style_prompt(test_case['prompt'])
    
    context = {
        "app_name": test_case['name'],
        "has_existing_spec": False,
        "available_resources": [],
        "resource_details": [],
        "data_store_summary": "",
        "has_data_store": False,
        "connectors_summary": "",
        "has_mcp_tools": False,
    }
    
    registry_surface = {"resources": []}
    
    generated_files = []
    
    try:
        # Create plan
        plan_prompt = build_plan_prompt(styled_message, context)
        plan_response = handler.call_llm(
            system_prompt="You are an expert at planning app development tasks.",
            user_prompt=plan_prompt,
            model=model,
            temperature=0.3,
        )
        
        # Parse plan
        json_match = re.search(r'```(?:json)?\s*(\{.*?\})\s*```', plan_response, re.DOTALL)
        if json_match:
            plan_response = json_match.group(1)
        
        plan_response = plan_response.strip()
        if not plan_response.startswith('{'):
            start = plan_response.find('{')
            end = plan_response.rfind('}')
            if start >= 0 and end > start:
                plan_response = plan_response[start:end+1]
        
        try:
            plan_data = json.loads(plan_response)
            plan_steps = [
                PlanStep(
                    id=str(uuid.uuid4()),
                    type=s.get("type", "code"),
                    title=s.get("title", "Generate Code"),
                    description=s.get("description", ""),
                )
                for s in plan_data.get("steps", [])
            ]
        except json.JSONDecodeError:
            plan_steps = [
                handler.create_step("code", "Build Core Components", "Build the main app component"),
            ]
        
        logger.info(f"Plan created with {len(plan_steps)} steps")
        
        # Execute each step
        system_prompt = build_codegen_system_prompt(registry_surface, has_data_store=False)
        
        for step_index, step in enumerate(plan_steps):
            logger.info(f"Executing step {step_index + 1}: {step.title}")
            
            prompt = build_step_prompt(
                step, step_index, styled_message, context,
                generated_files, registry_surface, None, None,
            )
            
            full_content = ""
            for chunk in handler.stream_llm_response(
                system_prompt=system_prompt,
                user_prompt=prompt,
                model=model,
                temperature=0.3,
            ):
                full_content += chunk
            
            files = handler.parse_code_blocks(full_content)
            generated_files.extend(files)
            logger.info(f"  Generated {len(files)} files")
        
        logger.info(f"\nTotal files generated: {len(generated_files)}")
        for f in generated_files:
            logger.info(f"  - {f.path}")
        
        # Validate using the handler's comprehensive type stubs
        validation = handler._validate_typescript(generated_files)
        
        return {
            "test_name": test_case["name"],
            "prompt": test_case["prompt"],
            "files_generated": len(generated_files),
            "file_paths": [f.path for f in generated_files],
            "validation_passed": validation["passed"],
            "errors": validation.get("errors", []),
            "warnings": validation.get("warnings", []),
        }
        
    except Exception as e:
        logger.error(f"Test failed with exception: {e}")
        import traceback
        traceback.print_exc()
        return {
            "test_name": test_case["name"],
            "prompt": test_case["prompt"],
            "files_generated": len(generated_files),
            "file_paths": [f.path for f in generated_files],
            "validation_passed": False,
            "errors": [{"message": str(e), "file": "N/A", "line": 0, "column": 0, "code": "EXCEPTION"}],
            "warnings": [],
            "exception": str(e),
        }


def run_all_tests(model: str = "anthropic/claude-sonnet-4") -> list:
    """Run all test cases and collect results."""
    results = []
    
    for i, test_case in enumerate(TEST_CASES):
        logger.info(f"\n[Test {i+1}/{len(TEST_CASES)}]")
        result = run_single_test(test_case, model)
        results.append(result)
        
        if result["validation_passed"]:
            logger.info(f"✓ {result['test_name']}: PASSED")
        else:
            logger.info(f"✗ {result['test_name']}: FAILED ({len(result['errors'])} errors)")
            for error in result["errors"][:5]:
                logger.info(f"    {error.get('file', 'N/A')}:{error.get('line', 0)} - {error.get('message', 'Unknown')} [{error.get('code', 'N/A')}]")
    
    return results


def main():
    """Run all tests and print analysis."""
    logger.info("Starting batch validation tests...")
    logger.info(f"Running {len(TEST_CASES)} test cases...\n")
    
    # Run tests
    results = run_all_tests()
    
    # Calculate stats
    passed = sum(1 for r in results if r["validation_passed"])
    failed = len(results) - passed
    total_errors = sum(len(r.get("errors", [])) for r in results)
    
    # Error analysis
    error_by_code = {}
    error_by_file = {}
    
    for result in results:
        for error in result.get("errors", []):
            code = error.get("code", "UNKNOWN")
            if code not in error_by_code:
                error_by_code[code] = []
            error_by_code[code].append(error)
            
            file = error.get("file", "unknown")
            if file not in error_by_file:
                error_by_file[file] = []
            error_by_file[file].append(error)
    
    # Print summary
    print("\n" + "="*80)
    print("BATCH TEST RESULTS")
    print("="*80)
    print(f"Total Tests: {len(results)}")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    print(f"Pass Rate: {passed/len(results)*100:.1f}%")
    print(f"Total Errors: {total_errors}")
    
    if error_by_code:
        print("\nErrors by TypeScript Code:")
        for code, errors in sorted(error_by_code.items(), key=lambda x: -len(x[1])):
            print(f"  {code}: {len(errors)}")
    
    if error_by_file:
        print("\nErrors by File:")
        for file, errors in sorted(error_by_file.items(), key=lambda x: -len(x[1]))[:10]:
            print(f"  {file}: {len(errors)}")
    
    # Individual results
    print("\nIndividual Results:")
    for i, result in enumerate(results):
        status = "✓ PASS" if result["validation_passed"] else f"✗ FAIL ({len(result['errors'])} errors)"
        print(f"  {i+1}. {result['test_name']}: {status}")
    
    # Save results
    with open('batch_test_results.json', 'w') as f:
        json.dump(results, f, indent=2)
    print("\nDetailed results saved to batch_test_results.json")
    
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())

