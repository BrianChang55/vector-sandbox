"""
Test Syntax Fix Diff System

Tests that the diff-based syntax fix system correctly parses and applies diffs
for the 3 common TypeScript syntax errors:
1. Inline imports in types (ts(2499))
2. Interface extends with indexed access (ts(2499))
3. Stray quotes

Uses _validate_and_fix_fields directly with FileChange objects.

Run with: python test_syntax_fix_diff.py
"""
import os
import sys
from unittest.mock import patch

# Django setup for imports
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'internal_apps.settings')

import django
django.setup()

from vector_app.services.handlers.generate_handler import GenerateHandler
from vector_app.services.types import FileChange
from unittest.mock import MagicMock


# =============================================================================
# Test Cases
# =============================================================================

TEST_CASES = [
    {
        "name": "Inline imports in types (ts(2499))",
        "file": FileChange(
            path="src/components/DataView.tsx",
            action="create",
            language="tsx",
            content="""import React from 'react';

export function DataView() {
  const items: Database[import('./types').TableSlug.Projects]['row'][] = [];
  return <div>{items.length}</div>;
}
""",
        ),
        "diff_response": """I'll fix the inline import syntax error.

```diff
--- src/components/DataView.tsx
+++ src/components/DataView.tsx
@@ -1,5 +1,7 @@
 import React from 'react';
+import type { Database } from './lib/types';
+import { TableSlug } from './lib/types';
 
 export function DataView() {
-  const items: Database[import('./types').TableSlug.Projects]['row'][] = [];
+  const items: Database[TableSlug.Projects]['row'][] = [];
   return <div>{items.length}</div>;
```
""",
        "expected_content": """import React from 'react';
import type { Database } from './lib/types';
import { TableSlug } from './lib/types';

export function DataView() {
  const items: Database[TableSlug.Projects]['row'][] = [];
  return <div>{items.length}</div>;
}
""",
    },
    {
        "name": "Interface extends with indexed access (ts(2499))",
        "file": FileChange(
            path="src/types/project.ts",
            action="create",
            language="typescript",
            content="""import type { Database, TableSlug } from './lib/types';

interface ProjectRow extends Database[TableSlug.Projects]['row'] {
  extraField: string;
}

export type { ProjectRow };
""",
        ),
        "diff_response": """I'll fix the interface extends syntax error by using a type alias.

```diff
--- src/types/project.ts
+++ src/types/project.ts
@@ -1,7 +1,10 @@
-import type { Database, TableSlug } from './lib/types';
+import type { Database } from './lib/types';
+import { TableSlug } from './lib/types';
 
-interface ProjectRow extends Database[TableSlug.Projects]['row'] {
+type ProjectRowBase = Database[TableSlug.Projects]['row'];
+
+interface ProjectRow extends ProjectRowBase {
   extraField: string;
 }
 
```
""",
        "expected_content": """import type { Database } from './lib/types';
import { TableSlug } from './lib/types';

type ProjectRowBase = Database[TableSlug.Projects]['row'];

interface ProjectRow extends ProjectRowBase {
  extraField: string;
}

export type { ProjectRow };
""",
    },
    {
        "name": "Stray quotes",
        "file": FileChange(
            path="src/utils/helpers.ts",
            action="create",
            language="typescript",
            content="""export function helper() {
  const foo = 'bar';'
  return foo;
}
""",
        ),
        "diff_response": """I'll remove the stray quote.

```diff
--- src/utils/helpers.ts
+++ src/utils/helpers.ts
@@ -1,4 +1,4 @@
 export function helper() {
-  const foo = 'bar';'
+  const foo = 'bar';
   return foo;
 }
```
""",
        "expected_content": """export function helper() {
  const foo = 'bar';
  return foo;
}
""",
    },
    {
        "name": "Multiple files in one response",
        "file": FileChange(
            path="src/App.tsx",
            action="create",
            language="tsx",
            content="""import React from 'react';

function App() {
  const data: Database[import('./types').TableSlug.Users]['row'] = {};
  return <div>Hello</div>;
}
""",
        ),
        "diff_response": """I'll fix the errors in both files.

```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,5 +1,7 @@
 import React from 'react';
+import type { Database } from './lib/types';
+import { TableSlug } from './lib/types';
 
 function App() {
-  const data: Database[import('./types').TableSlug.Users]['row'] = {};
+  const data: Database[TableSlug.Users]['row'] = {};
   return <div>Hello</div>;
```
""",
        "expected_content": """import React from 'react';
import type { Database } from './lib/types';
import { TableSlug } from './lib/types';

function App() {
  const data: Database[TableSlug.Users]['row'] = {};
  return <div>Hello</div>;
}
""",
    },
]


# =============================================================================
# Test Runner
# =============================================================================

def run_test(test_case: dict, handler: GenerateHandler) -> tuple[bool, str]:
    """
    Run a single test case using _validate_and_fix_fields.
    
    Returns:
        (passed, message) tuple
    """
    name = test_case["name"]
    file = test_case["file"]
    diff_response = test_case["diff_response"]
    expected = test_case["expected_content"]
    
    try:
        # Mock stream_llm_response to return our mock diff response
        def mock_stream_llm_response(*args, **kwargs):
            # Yield chunks character by character to simulate streaming
            for char in diff_response:
                yield char
        
        # Create full_content from files (as _validate_and_fix_fields expects)
        full_content = f"```typescript:{file.path}\n{file.content}\n```"
        
        # Create a mock app object (needed to pass the early return check)
        mock_app = MagicMock()
        mock_app.id = 1
        
        # Patch stream_llm_response to use our mock
        with patch.object(handler, 'stream_llm_response', side_effect=mock_stream_llm_response):
            # Call _validate_and_fix_fields
            # We also need to mock _validate_datastore_field_names to skip field validation
            with patch.object(handler, '_validate_datastore_field_names', return_value={"passed": True}):
                files = [file]
                generator = handler._validate_and_fix_fields(
                    files=files,
                    full_content=full_content,
                    app=mock_app,
                    version=None,
                    model="test-model",
                )
                
                # Consume all events from generator
                events = []
                try:
                    while True:
                        event = next(generator)
                        events.append(event)
                except StopIteration as e:
                    # Get return value from generator
                    fixed_files = e.value
                
                if fixed_files is None:
                    return False, "Generator did not return files"
                
                # Find our file in the fixed files
                fixed_file = None
                for f in fixed_files:
                    if f.path == file.path:
                        fixed_file = f
                        break
                
                if not fixed_file:
                    return False, f"No fixed file found for {file.path}. Got: {[f.path for f in fixed_files]}"
                
                # Compare content
                if fixed_file.content == expected:
                    return True, "OK"
                else:
                    return False, f"Content mismatch:\n--- Expected ---\n{repr(expected)}\n--- Got ---\n{repr(fixed_file.content)}"
            
    except Exception as e:
        import traceback
        return False, f"Exception: {e}\n{traceback.format_exc()}"


def main():
    """Run all test cases and print results."""
    print("=" * 70)
    print("Syntax Fix Diff System Tests")
    print("Using _validate_and_fix_fields with FileChange objects")
    print("=" * 70)
    print()
    
    # Create handler instance
    handler = GenerateHandler()
    
    passed = 0
    failed = 0
    
    for i, test_case in enumerate(TEST_CASES, 1):
        name = test_case["name"]
        success, message = run_test(test_case, handler)
        
        if success:
            print(f"[PASS] {i}. {name}")
            passed += 1
        else:
            print(f"[FAIL] {i}. {name}")
            print(f"       {message}")
            failed += 1
        print()
    
    print("=" * 70)
    print(f"Results: {passed} passed, {failed} failed")
    print("=" * 70)
    
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
