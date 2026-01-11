"""
Test Error Fix Service with Diff-Based Approach

Tests that:
1. Prompts now request unified diff format with line numbers
2. _apply_diff_fixes() correctly parses and applies diffs
3. Fallback to _parse_fixed_files() works when no diffs are found
"""
import os
import pytest
import django

# Django setup for tests
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'internal_apps.settings')
django.setup()

from vector_app.services.error_fix_service import ErrorFixService
from vector_app.services.types import FileChange, CompilationError
from vector_app.prompts.error_fix import (
    ERROR_FIX_SYSTEM_PROMPT,
    build_error_fix_prompt,
    build_bundler_error_fix_prompt,
)


# =============================================================================
# Prompt Tests
# =============================================================================

def test_system_prompt_mentions_diff_format():
    """System prompt should mention unified diff output format."""
    assert "Unified Diff" in ERROR_FIX_SYSTEM_PROMPT
    assert "```diff" in ERROR_FIX_SYSTEM_PROMPT
    assert "```filepath:" not in ERROR_FIX_SYSTEM_PROMPT


def test_error_fix_prompt_includes_diff_instructions():
    """build_error_fix_prompt should include diff format instructions."""
    files = [
        FileChange(
            path="src/App.tsx",
            action="create",
            language="tsx",
            content='import React from "react";\n\nfunction App() {\n  return <div>Hello</div>;\n}\n',
        )
    ]
    errors = [
        CompilationError(
            file="src/App.tsx",
            line=3,
            column=10,
            message="Cannot find name 'useState'",
            code="TS2304",
        )
    ]
    
    prompt = build_error_fix_prompt(files, errors, attempt=1, max_attempts=2)
    
    # Should mention diff format
    assert "Unified Diff" in prompt
    assert "@@ -" in prompt
    assert "context line" in prompt.lower()
    
    # Should include line numbers
    assert "LINE NUMBERS" in prompt
    assert "hunk headers" in prompt.lower()
    
    # Should NOT mention old format
    assert "```filepath:" not in prompt


def test_bundler_error_prompt_includes_diff_instructions():
    """build_bundler_error_fix_prompt should include diff format instructions."""
    files = [
        FileChange(
            path="src/App.tsx",
            action="create",
            language="tsx",
            content='import React from "react";\n\nfunction App() {\n  return <div>Hello</div>;\n}\n',
        )
    ]
    bundler_errors = [
        {"title": "ModuleNotFoundError", "file": "src/App.tsx", "line": 1, "message": "Cannot find module './utils'"}
    ]
    
    prompt = build_bundler_error_fix_prompt(files, bundler_errors, attempt=1, max_attempts=2)
    
    # Should mention diff format
    assert "Unified Diff" in prompt
    assert "@@ -" in prompt
    
    # Should NOT mention old format
    assert "```filepath:" not in prompt


# =============================================================================
# Diff Application Tests
# =============================================================================

@pytest.fixture
def error_fix_service():
    """Fixture providing ErrorFixService instance."""
    return ErrorFixService()


def test_apply_simple_diff(error_fix_service):
    """Test applying a simple diff that adds a line."""
    original_files = [
        FileChange(
            path="src/App.tsx",
            action="create",
            language="tsx",
            content='import React from "react";\n\nfunction App() {\n  return <div>Hello</div>;\n}\n',
        )
    ]
    
    # LLM response with a unified diff
    llm_response = '''
I'll fix the missing import error.

```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,4 +1,5 @@
 import React from "react";
+import { useState } from "react";
 
 function App() {
   return <div>Hello</div>;
```
'''
    
    fixed_files = error_fix_service._apply_diff_fixes(llm_response, original_files)
    
    # Should have found and applied the diff
    assert len(fixed_files) == 1
    assert fixed_files[0].path == "src/App.tsx"
    assert 'import { useState } from "react";' in fixed_files[0].content


def test_apply_multiple_hunks(error_fix_service):
    """Test applying a diff with multiple hunks."""
    original_content = '''import React from "react";

function App() {
  return (
    <div>
      <h1>Title</h1>
      <p>Paragraph</p>
    </div>
  );
}

export default App;
'''
    original_files = [
        FileChange(
            path="src/App.tsx",
            action="create",
            language="tsx",
            content=original_content,
        )
    ]
    
    # LLM response with multiple hunks
    llm_response = '''
```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,4 +1,5 @@
 import React from "react";
+import { useState } from "react";
 
 function App() {
   return (
@@ -10,4 +11,4 @@
   );
 }
 
-export default App;
+export { App };
```
'''
    
    fixed_files = error_fix_service._apply_diff_fixes(llm_response, original_files)
    
    assert len(fixed_files) == 1
    assert 'import { useState } from "react";' in fixed_files[0].content
    assert 'export { App };' in fixed_files[0].content
    assert 'export default App;' not in fixed_files[0].content


def test_no_diffs_returns_empty(error_fix_service):
    """Test that no diffs returns empty list (triggers fallback)."""
    original_files = [
        FileChange(
            path="src/App.tsx",
            action="create",
            language="tsx",
            content='import React from "react";\n',
        )
    ]
    
    # LLM response without diffs
    llm_response = '''
I analyzed the code but couldn't find any issues to fix.
'''
    
    fixed_files = error_fix_service._apply_diff_fixes(llm_response, original_files)
    
    assert len(fixed_files) == 0


# =============================================================================
# Fallback Parsing Tests
# =============================================================================

def test_parse_filepath_format(error_fix_service):
    """Test parsing the old filepath: format (fallback)."""
    original_files = [
        FileChange(
            path="src/App.tsx",
            action="create",
            language="tsx",
            content='old content',
        )
    ]
    
    # Old-style LLM response
    llm_response = '''
```filepath:src/App.tsx
import React from "react";

function App() {
  return <div>Fixed</div>;
}
```
'''
    
    fixed_files = error_fix_service._parse_fixed_files(llm_response, original_files)
    
    assert len(fixed_files) == 1
    assert fixed_files[0].path == "src/App.tsx"
    assert "Fixed" in fixed_files[0].content


def test_skips_diff_blocks(error_fix_service):
    """Test that diff blocks are skipped in fallback parsing."""
    original_files = [
        FileChange(
            path="src/App.tsx",
            action="create",
            language="tsx",
            content='old content',
        )
    ]
    
    # Response with diff blocks only
    llm_response = '''
```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,3 +1,4 @@
 import React from "react";
+import { useState } from "react";
```
'''
    
    fixed_files = error_fix_service._parse_fixed_files(llm_response, original_files)
    
    # Should return empty since diff blocks are filtered out
    assert len(fixed_files) == 0


# =============================================================================
# Integration Tests
# =============================================================================

def test_diff_parsing_preferred_over_fallback(error_fix_service):
    """When both diffs and full files are present, diffs should be used."""
    original_files = [
        FileChange(
            path="src/App.tsx",
            action="create",
            language="tsx",
            content='import React from "react";\n\nfunction App() {\n  return <div>Hello</div>;\n}\n',
        )
    ]
    
    # Response with both diff and full file (diff should win)
    llm_response = '''
Here's the fix:

```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,4 +1,5 @@
 import React from "react";
+import { useState } from "react";
 
 function App() {
   return <div>Hello</div>;
```

Or alternatively the full file:

```filepath:src/App.tsx
import React from "react";
import { useEffect } from "react";

function App() {
  return <div>Hello</div>;
}
```
'''
    
    # _apply_diff_fixes should be tried first
    diff_files = error_fix_service._apply_diff_fixes(llm_response, original_files)
    
    # Should have applied the diff (useState, not useEffect)
    assert len(diff_files) == 1
    assert 'useState' in diff_files[0].content
    # useEffect would only be there if fallback was used
    assert 'useEffect' not in diff_files[0].content
