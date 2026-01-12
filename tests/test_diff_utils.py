"""
Tests for diff.py - unified diff parsing and application.
"""

import pytest
from vector_app.services.diff import (
    FileDiff,
    parse_diffs,
    apply_diff,
)


# =============================================================================
# parse_diffs Tests
# =============================================================================

class TestParseDiffs:
    """Tests for parse_diffs function."""
    
    def test_single_file_single_hunk(self):
        """Parse a simple diff with one file and one hunk."""
        llm_response = '''Here's the change:

```diff
--- src/App.tsx
+++ src/App.tsx
@@ -10,6 +10,7 @@ function App() {
   const [count, setCount] = useState(0);
+  const [name, setName] = useState('');
   
   return (
```

That should fix it.'''
        
        diffs = parse_diffs(llm_response)
        
        assert len(diffs) == 1
        assert diffs[0].path == 'src/App.tsx'
        assert len(diffs[0].hunks) == 1
        assert '+  const [name, setName]' in diffs[0].hunks[0]
    
    def test_single_file_multiple_hunks(self):
        """Parse a diff with one file and multiple hunks."""
        llm_response = '''```diff
--- src/App.tsx
+++ src/App.tsx
@@ -10,6 +10,7 @@ function App() {
   const [count, setCount] = useState(0);
+  const [name, setName] = useState('');
   
   return (
@@ -25,4 +26,4 @@ function App() {
-      <button>Old Text</button>
+      <button>New Text</button>
     </div>
```'''
        
        diffs = parse_diffs(llm_response)
        
        assert len(diffs) == 1
        assert diffs[0].path == 'src/App.tsx'
        assert len(diffs[0].hunks) == 2
    
    def test_multiple_files(self):
        """Parse diffs for multiple files."""
        llm_response = '''```diff
--- src/App.tsx
+++ src/App.tsx
@@ -5,3 +5,4 @@
 import React from 'react';
+import { Header } from './Header';
 
--- src/Header.tsx
+++ src/Header.tsx
@@ -1,4 +1,4 @@
-export const Header = () => <h1>Old</h1>;
+export const Header = () => <h1>New</h1>;
```'''
        
        diffs = parse_diffs(llm_response)
        
        assert len(diffs) == 2
        assert diffs[0].path == 'src/App.tsx'
        assert diffs[1].path == 'src/Header.tsx'
    
    def test_git_style_paths(self):
        """Parse diffs with git-style a/ and b/ prefixes."""
        llm_response = '''```diff
--- a/src/App.tsx
+++ b/src/App.tsx
@@ -1,3 +1,4 @@
 import React from 'react';
+import { useState } from 'react';
```'''
        
        diffs = parse_diffs(llm_response)
        
        assert len(diffs) == 1
        assert diffs[0].path == 'src/App.tsx'  # a/ prefix stripped
    
    def test_empty_input(self):
        """Handle empty input gracefully."""
        diffs = parse_diffs('')
        assert diffs == []
    
    def test_no_diff_blocks(self):
        """Handle input with no diff blocks."""
        llm_response = '''Here's some text without any diffs.
        
```typescript
const x = 1;
```
'''
        diffs = parse_diffs(llm_response)
        assert diffs == []
    
    def test_multiple_diff_blocks(self):
        """Parse multiple separate diff code blocks."""
        llm_response = '''First change:

```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,3 +1,4 @@
 line1
+added line
```

Second change:

```diff
--- src/Header.tsx
+++ src/Header.tsx
@@ -5,3 +5,4 @@
 old line
+new line
```'''
        
        diffs = parse_diffs(llm_response)
        
        assert len(diffs) == 2
        assert diffs[0].path == 'src/App.tsx'
        assert diffs[1].path == 'src/Header.tsx'
    
    def test_same_file_multiple_blocks_merged(self):
        """Hunks for same file across multiple blocks are merged."""
        llm_response = '''```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,3 +1,4 @@
+import line
```

More changes to same file:

```diff
--- src/App.tsx
+++ src/App.tsx
@@ -50,3 +51,4 @@
+another line
```'''
        
        diffs = parse_diffs(llm_response)
        
        assert len(diffs) == 1
        assert diffs[0].path == 'src/App.tsx'
        assert len(diffs[0].hunks) == 2


# =============================================================================
# apply_diff Tests
# =============================================================================

class TestApplyDiff:
    """Tests for apply_diff function."""
    
    def test_simple_single_line_addition(self):
        """Add a single line to a file."""
        original = '''import React from 'react';

function App() {
  return <div>Hello</div>;
}'''
        
        diff = FileDiff(
            path='src/App.tsx',
            hunks=['''@@ -1,4 +1,5 @@
 import React from 'react';
+import { useState } from 'react';
 
 function App() {''']
        )
        
        result = apply_diff(original, diff)
        
        assert "import { useState } from 'react';" in result
        assert "import React from 'react';" in result
    
    def test_simple_single_line_removal(self):
        """Remove a single line from a file."""
        original = '''import React from 'react';
import { useState } from 'react';

function App() {
  return <div>Hello</div>;
}'''
        
        diff = FileDiff(
            path='src/App.tsx',
            hunks=['''@@ -1,5 +1,4 @@
 import React from 'react';
-import { useState } from 'react';
 
 function App() {''']
        )
        
        result = apply_diff(original, diff)
        
        assert "import { useState } from 'react';" not in result
        assert "import React from 'react';" in result
    
    def test_single_line_replacement(self):
        """Replace a single line."""
        original = '''function greet() {
  return "Hello, World!";
}'''
        
        diff = FileDiff(
            path='src/greet.ts',
            hunks=['''@@ -1,3 +1,3 @@
 function greet() {
-  return "Hello, World!";
+  return "Hello, Universe!";
 }''']
        )
        
        result = apply_diff(original, diff)
        
        assert 'return "Hello, Universe!";' in result
        assert 'return "Hello, World!";' not in result
    
    def test_multi_line_addition(self):
        """Add multiple lines."""
        original = '''function App() {
  return (
    <div>
    </div>
  );
}'''
        
        diff = FileDiff(
            path='src/App.tsx',
            hunks=['''@@ -2,4 +2,7 @@ function App() {
   return (
     <div>
+      <h1>Title</h1>
+      <p>Description</p>
+      <button>Click me</button>
     </div>
   );''']
        )
        
        result = apply_diff(original, diff)
        
        assert '<h1>Title</h1>' in result
        assert '<p>Description</p>' in result
        assert '<button>Click me</button>' in result
    
    def test_multi_line_removal(self):
        """Remove multiple lines."""
        original = '''function App() {
  const unused1 = 1;
  const unused2 = 2;
  const unused3 = 3;
  return <div>Hello</div>;
}'''
        
        diff = FileDiff(
            path='src/App.tsx',
            hunks=['''@@ -1,6 +1,3 @@
 function App() {
-  const unused1 = 1;
-  const unused2 = 2;
-  const unused3 = 3;
   return <div>Hello</div>;
 }''']
        )
        
        result = apply_diff(original, diff)
        
        assert 'unused1' not in result
        assert 'unused2' not in result
        assert 'unused3' not in result
        assert 'return <div>Hello</div>;' in result
    
    def test_multiple_hunks_same_file(self):
        """Apply multiple hunks to the same file."""
        original = '''import React from 'react';

function Header() {
  return <h1>Old Header</h1>;
}

function Footer() {
  return <footer>Old Footer</footer>;
}'''
        
        diff = FileDiff(
            path='src/App.tsx',
            hunks=[
                '''@@ -3,3 +3,3 @@ import React from 'react';
 function Header() {
-  return <h1>Old Header</h1>;
+  return <h1>New Header</h1>;
 }''',
                '''@@ -7,3 +7,3 @@ function Header() {
 function Footer() {
-  return <footer>Old Footer</footer>;
+  return <footer>New Footer</footer>;
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        assert 'New Header' in result
        assert 'New Footer' in result
        assert 'Old Header' not in result
        assert 'Old Footer' not in result
    
    def test_change_at_file_start(self):
        """Handle changes at the very start of file."""
        original = '''old first line
second line
third line'''
        
        diff = FileDiff(
            path='src/test.ts',
            hunks=['''@@ -1,3 +1,3 @@
-old first line
+new first line
 second line
 third line''']
        )
        
        result = apply_diff(original, diff)
        
        assert result.startswith('new first line')
        assert 'old first line' not in result
    
    def test_change_at_file_end(self):
        """Handle changes at the very end of file."""
        original = '''first line
second line
old last line'''
        
        diff = FileDiff(
            path='src/test.ts',
            hunks=['''@@ -1,3 +1,3 @@
 first line
 second line
-old last line
+new last line''']
        )
        
        result = apply_diff(original, diff)
        
        assert result.endswith('new last line')
        assert 'old last line' not in result
    
    def test_empty_diff(self):
        """Handle empty diff (no hunks)."""
        original = 'original content'
        diff = FileDiff(path='src/test.ts', hunks=[])
        
        result = apply_diff(original, diff)
        
        assert result == original
    
    def test_context_fuzzy_matching(self):
        """Match context even with slight whitespace differences."""
        original = '''function test() {
    const x = 1;
    return x;
}'''
        
        # Note: context has slightly different indentation
        diff = FileDiff(
            path='src/test.ts',
            hunks=['''@@ -1,4 +1,4 @@
 function test() {
-    const x = 1;
+    const x = 2;
     return x;
 }''']
        )
        
        result = apply_diff(original, diff)
        
        assert 'const x = 2;' in result
    
    def test_preserves_line_endings(self):
        """Ensure result maintains proper line structure."""
        original = '''line1
line2
line3'''
        
        diff = FileDiff(
            path='src/test.ts',
            hunks=['''@@ -1,3 +1,4 @@
 line1
+inserted
 line2
 line3''']
        )
        
        result = apply_diff(original, diff)
        lines = result.split('\n')
        
        assert len(lines) == 4
        assert lines[0] == 'line1'
        assert lines[1] == 'inserted'
        assert lines[2] == 'line2'
        assert lines[3] == 'line3'

# =============================================================================
# Integration Tests
# =============================================================================

class TestIntegration:
    """End-to-end integration tests."""
    
    def test_full_workflow(self):
        """Test complete parse -> apply workflow."""
        original_file = '''import React from 'react';

function App() {
  const [count, setCount] = useState(0);

  return (
    <div>
      <h1>Counter: {count}</h1>
      <button onClick={() => setCount(count + 1)}>
        Increment
      </button>
    </div>
  );
}

export default App;'''
        
        llm_response = '''I'll add a reset button and change the title:

```diff
--- src/App.tsx
+++ src/App.tsx
@@ -6,7 +6,8 @@ function App() {

   return (
     <div>
-      <h1>Counter: {count}</h1>
+      <h1>My Counter App: {count}</h1>
       <button onClick={() => setCount(count + 1)}>
         Increment
       </button>
+      <button onClick={() => setCount(0)}>Reset</button>
     </div>
```

This adds a reset button and updates the title.'''
        
        # Parse
        diffs = parse_diffs(llm_response)
        assert len(diffs) == 1
        
        # Apply
        result = apply_diff(original_file, diffs[0])
        
        # Verify changes
        assert 'My Counter App' in result
        assert 'Reset</button>' in result
        assert 'Counter: {count}' not in result  # Old title removed
    
    def test_react_component_edit(self):
        """Test editing a typical React component."""
        original = '''import React from 'react';
import { Button } from './Button';

interface Props {
  title: string;
}

export function Card({ title }: Props) {
  return (
    <div className="card">
      <h2>{title}</h2>
      <Button>Click me</Button>
    </div>
  );
}'''
        
        llm_response = '''```diff
--- src/components/Card.tsx
+++ src/components/Card.tsx
@@ -3,6 +3,7 @@ import { Button } from './Button';
 
 interface Props {
   title: string;
+  subtitle?: string;
 }
 
@@ -8,7 +9,8 @@ interface Props {
-export function Card({ title }: Props) {
+export function Card({ title, subtitle }: Props) {
   return (
     <div className="card">
       <h2>{title}</h2>
+      {subtitle && <p className="text-gray-500">{subtitle}</p>}
       <Button>Click me</Button>
```'''
        
        diffs = parse_diffs(llm_response)
        result = apply_diff(original, diffs[0])
        
        assert 'subtitle?: string;' in result
        assert 'subtitle }: Props' in result
        assert 'className="text-gray-500"' in result
