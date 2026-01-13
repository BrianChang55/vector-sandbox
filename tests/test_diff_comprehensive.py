"""
Comprehensive Test Suite for AI-Generated Diffs

Tests various types of diffs that AI models commonly generate:
- Code movement (full component moves up/down, function relocation)
- Compiler/type error fixes (missing imports, type mismatches)
- Refactoring patterns (rename, extract, convert)
- Scale tests (100+ line diffs, many hunks, many files)
- Edge cases and error recovery
- Real-world AI output patterns
"""
import pytest
from vector_app.services.diff import (
    FileDiff,
    parse_diffs,
    apply_diff,
)


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def sample_react_component():
    """A typical React component file (~50 lines)."""
    return '''import React from 'react';
import { useState, useEffect } from 'react';
import { Button } from './Button';
import { Card } from './Card';

interface HeaderProps {
  title: string;
  subtitle?: string;
}

function Header({ title, subtitle }: HeaderProps) {
  return (
    <header className="header">
      <h1>{title}</h1>
      {subtitle && <p>{subtitle}</p>}
    </header>
  );
}

interface FooterProps {
  year: number;
}

function Footer({ year }: FooterProps) {
  return (
    <footer className="footer">
      <p>&copy; {year} My Company</p>
    </footer>
  );
}

export default function App() {
  const [count, setCount] = useState(0);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    setLoading(false);
  }, []);

  return (
    <div className="app">
      <Header title="Welcome" subtitle="To my app" />
      <main>
        <Card>
          <p>Count: {count}</p>
          <Button onClick={() => setCount(count + 1)}>Increment</Button>
        </Card>
      </main>
      <Footer year={2024} />
    </div>
  );
}
'''


@pytest.fixture
def sample_multi_component_file():
    """File with multiple clearly separated components for move tests."""
    return '''import React from 'react';

// ========== ComponentA ==========
function ComponentA() {
  return (
    <div className="component-a">
      <h2>Component A</h2>
      <p>This is the first component</p>
    </div>
  );
}

// ========== ComponentB ==========
function ComponentB() {
  return (
    <div className="component-b">
      <h2>Component B</h2>
      <p>This is the second component</p>
    </div>
  );
}

// ========== ComponentC ==========
function ComponentC() {
  return (
    <div className="component-c">
      <h2>Component C</h2>
      <p>This is the third component</p>
    </div>
  );
}

export { ComponentA, ComponentB, ComponentC };
'''


@pytest.fixture
def large_typescript_file():
    """A large TypeScript file (500+ lines) for scale tests."""
    lines = ['// Large TypeScript file for scale testing']
    lines.append('import { Injectable } from "@nestjs/common";')
    lines.append('import { Repository } from "typeorm";')
    lines.append('')
    
    # Generate 50 functions, each ~10 lines
    for i in range(50):
        lines.append(f'export function processItem{i}(data: unknown): string {{')
        lines.append(f'  const id = "item-{i}";')
        lines.append(f'  const timestamp = Date.now();')
        lines.append(f'  const value = String(data);')
        lines.append(f'  ')
        lines.append(f'  if (!value) {{')
        lines.append(f'    throw new Error("Invalid data for item {i}");')
        lines.append(f'  }}')
        lines.append(f'  ')
        lines.append(f'  return `${{id}}-${{timestamp}}-${{value}}`;')
        lines.append(f'}}')
        lines.append('')
    
    return '\n'.join(lines)


def generate_large_diff_hunk(start_line: int, num_changes: int) -> str:
    """Helper to generate a large hunk with many line changes."""
    lines = [f'@@ -{start_line},{num_changes + 4} +{start_line},{num_changes + 4} @@']
    lines.append(' // Context line before')
    lines.append(' ')
    
    for i in range(num_changes):
        lines.append(f'-  const oldVar{i} = {i};')
        lines.append(f'+  const newVar{i} = {i * 2};')
    
    lines.append(' ')
    lines.append(' // Context line after')
    return '\n'.join(lines)


# =============================================================================
# TestCodeMovement - Full component and function movement
# =============================================================================

class TestCodeMovement:
    """Tests for code that's moved within files."""

    def test_move_full_component_up(self, sample_multi_component_file):
        """Move ComponentC from bottom to top of file."""
        original = sample_multi_component_file
        
        # Diff that deletes ComponentC from bottom and adds it at top
        diff = FileDiff(
            path='src/components.tsx',
            hunks=[
                # Hunk 1: Add ComponentC after imports
                '''@@ -1,4 +1,14 @@
 import React from 'react';
 
+// ========== ComponentC ==========
+function ComponentC() {
+  return (
+    <div className="component-c">
+      <h2>Component C</h2>
+      <p>This is the third component</p>
+    </div>
+  );
+}
+
 // ========== ComponentA ==========''',
                # Hunk 2: Remove ComponentC from bottom
                '''@@ -23,14 +33,4 @@
   );
 }
 
-// ========== ComponentC ==========
-function ComponentC() {
-  return (
-    <div className="component-c">
-      <h2>Component C</h2>
-      <p>This is the third component</p>
-    </div>
-  );
-}
-
 export { ComponentA, ComponentB, ComponentC };'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        # ComponentC should appear before ComponentA now
        c_pos = result.find('ComponentC')
        a_pos = result.find('ComponentA')
        assert c_pos < a_pos, "ComponentC should be before ComponentA after move"
        
        # Should only have one definition of ComponentC
        assert result.count('function ComponentC()') == 1
        
    def test_move_full_component_down(self):
        """Move ComponentA from top to bottom of file."""
        # Use a simpler file structure for reliable move testing
        original = '''import React from 'react';

function ComponentA() {
  return <div>Component A</div>;
}

function ComponentB() {
  return <div>Component B</div>;
}

function ComponentC() {
  return <div>Component C</div>;
}

export { ComponentA, ComponentB, ComponentC };
'''
        
        # Diff that deletes ComponentA from top and adds it at bottom
        # Using a single hunk that does the full replacement for reliability
        diff = FileDiff(
            path='src/components.tsx',
            hunks=[
                '''@@ -1,17 +1,17 @@
 import React from 'react';
 
-function ComponentA() {
-  return <div>Component A</div>;
-}
-
 function ComponentB() {
   return <div>Component B</div>;
 }
 
 function ComponentC() {
   return <div>Component C</div>;
 }
 
+function ComponentA() {
+  return <div>Component A</div>;
+}
+
 export { ComponentA, ComponentB, ComponentC };'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        # ComponentA should appear after ComponentC now
        a_pos = result.find('function ComponentA')
        c_pos = result.find('function ComponentC')
        assert a_pos > c_pos, "ComponentA should be after ComponentC after move"
        
        # Should only have one definition of ComponentA
        assert result.count('function ComponentA()') == 1

    def test_move_function_within_file(self, sample_react_component):
        """Move a helper function to different position."""
        original = sample_react_component
        
        # Move Footer function above Header
        diff = FileDiff(
            path='src/App.tsx',
            hunks=[
                # Add Footer after imports, before Header interface
                '''@@ -4,6 +4,16 @@
 import { Card } from './Card';
 
+interface FooterProps {
+  year: number;
+}
+
+function Footer({ year }: FooterProps) {
+  return (
+    <footer className="footer">
+      <p>&copy; {year} My Company</p>
+    </footer>
+  );
+}
+
 interface HeaderProps {''',
                # Remove Footer from its original location
                '''@@ -20,16 +30,6 @@
   );
 }
 
-interface FooterProps {
-  year: number;
-}
-
-function Footer({ year }: FooterProps) {
-  return (
-    <footer className="footer">
-      <p>&copy; {year} My Company</p>
-    </footer>
-  );
-}
-
 export default function App() {'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        # Footer should now be before Header in the file
        footer_pos = result.find('function Footer')
        header_pos = result.find('function Header')
        assert footer_pos < header_pos, "Footer should be before Header after move"
        
        # Both functions should still exist
        assert 'function Footer' in result
        assert 'function Header' in result

    def test_reorder_imports(self):
        """Reorder import statements."""
        original = '''import { z } from 'zod';
import { a } from 'alpha';
import { m } from 'middle';

export const schema = z.object({});
'''
        
        # Alphabetize imports
        diff = FileDiff(
            path='src/schema.ts',
            hunks=[
                '''@@ -1,4 +1,4 @@
+import { a } from 'alpha';
+import { m } from 'middle';
 import { z } from 'zod';
-import { a } from 'alpha';
-import { m } from 'middle';
 
 export const schema = z.object({});'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        # Check order: a, m, z
        a_pos = result.find("import { a }")
        m_pos = result.find("import { m }")
        z_pos = result.find("import { z }")
        
        assert a_pos < m_pos < z_pos, "Imports should be alphabetically ordered"

    def test_extract_code_block_with_modifications(self):
        """Extract inline code to a separate function with modifications."""
        original = '''function processData(items: string[]) {
  const results = [];
  
  for (const item of items) {
    const trimmed = item.trim();
    const upper = trimmed.toUpperCase();
    const prefixed = "ITEM_" + upper;
    results.push(prefixed);
  }
  
  return results;
}
'''
        
        # Extract loop body to helper function
        diff = FileDiff(
            path='src/process.ts',
            hunks=[
                '''@@ -1,12 +1,16 @@
+function transformItem(item: string): string {
+  const trimmed = item.trim();
+  const upper = trimmed.toUpperCase();
+  return "ITEM_" + upper;
+}
+
 function processData(items: string[]) {
   const results = [];
   
   for (const item of items) {
-    const trimmed = item.trim();
-    const upper = trimmed.toUpperCase();
-    const prefixed = "ITEM_" + upper;
-    results.push(prefixed);
+    results.push(transformItem(item));
   }
   
   return results;
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        # New helper function should exist
        assert 'function transformItem' in result
        # Loop should use the helper
        assert 'transformItem(item)' in result
        # Old inline code should be gone
        assert 'const prefixed = "ITEM_"' not in result

    def test_swap_two_components(self, sample_multi_component_file):
        """Swap positions of ComponentA and ComponentB."""
        original = sample_multi_component_file
        
        # This requires 4 hunks: remove A, add B in A's place, remove B, add A in B's place
        # Simplified: we can do it with 2 hunks by replacing both at once
        diff = FileDiff(
            path='src/components.tsx',
            hunks=[
                # Replace ComponentA with ComponentB content
                '''@@ -3,10 +3,10 @@
 
-// ========== ComponentA ==========
-function ComponentA() {
+// ========== ComponentB ==========
+function ComponentB() {
   return (
-    <div className="component-a">
-      <h2>Component A</h2>
-      <p>This is the first component</p>
+    <div className="component-b">
+      <h2>Component B</h2>
+      <p>This is the second component</p>
     </div>
   );
 }''',
                # Replace ComponentB with ComponentA content
                '''@@ -14,10 +14,10 @@
 
-// ========== ComponentB ==========
-function ComponentB() {
+// ========== ComponentA ==========
+function ComponentA() {
   return (
-    <div className="component-b">
-      <h2>Component B</h2>
-      <p>This is the second component</p>
+    <div className="component-a">
+      <h2>Component A</h2>
+      <p>This is the first component</p>
     </div>
   );
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        # ComponentB should now come before ComponentA
        b_pos = result.find('function ComponentB')
        a_pos = result.find('function ComponentA')
        assert b_pos < a_pos, "ComponentB should be before ComponentA after swap"

    def test_move_component_from_below_to_above(self):
        """Move ComponentB from below ComponentA to above it (swap order)."""
        original = '''import React from 'react';
import { useState, useEffect } from 'react';
import { Button } from './Button';
import { Card } from './Card';
import { Modal } from './Modal';

interface ComponentAProps {
  title: string;
  description?: string;
  onAction?: () => void;
}

function ComponentA({ title, description, onAction }: ComponentAProps) {
  const [isExpanded, setIsExpanded] = useState(false);
  const [data, setData] = useState<string[]>([]);
  
  useEffect(() => {
    // Simulate data loading
    const loadData = async () => {
      const response = await fetch('/api/data');
      const result = await response.json();
      setData(result.items);
    };
    loadData();
  }, []);
  
  const handleToggle = () => {
    setIsExpanded(!isExpanded);
    onAction?.();
  };
  
  return (
    <Card className="component-a-wrapper">
      <div className="component-a-header">
        <h2 className="text-2xl font-bold text-gray-800">{title}</h2>
        {description && (
          <p className="text-sm text-gray-600 mt-2">{description}</p>
        )}
      </div>
      
      <div className="component-a-content mt-4">
        <Button 
          onClick={handleToggle}
          variant={isExpanded ? "secondary" : "primary"}
          className="w-full"
        >
          {isExpanded ? 'Collapse' : 'Expand'}
        </Button>
        
        {isExpanded && (
          <div className="mt-4 space-y-2">
            {data.length > 0 ? (
              data.map((item, index) => (
                <div key={index} className="p-3 bg-gray-100 rounded">
                  {item}
                </div>
              ))
            ) : (
              <p className="text-gray-500">No data available</p>
            )}
          </div>
        )}
      </div>
    </Card>
  );
}

interface ComponentBProps {
  items: string[];
  onSelect?: (item: string) => void;
}

function ComponentB({ items, onSelect }: ComponentBProps) {
  const [selectedIndex, setSelectedIndex] = useState<number | null>(null);
  const [searchQuery, setSearchQuery] = useState('');
  
  const filteredItems = items.filter(item =>
    item.toLowerCase().includes(searchQuery.toLowerCase())
  );
  
  const handleItemClick = (item: string, index: number) => {
    setSelectedIndex(index);
    onSelect?.(item);
  };
  
  return (
    <Modal isOpen={true} onClose={() => {}}>
      <div className="component-b-container p-6">
        <h2 className="text-3xl font-semibold mb-4 text-blue-600">
          Component B
        </h2>
        
        <div className="search-section mb-4">
          <input
            type="text"
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            placeholder="Search items..."
            className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
          />
        </div>
        
        <div className="items-list space-y-2">
          {filteredItems.length > 0 ? (
            filteredItems.map((item, index) => (
              <div
                key={index}
                onClick={() => handleItemClick(item, index)}
                className={`p-4 rounded-lg cursor-pointer transition-all ${
                  selectedIndex === index
                    ? 'bg-blue-500 text-white shadow-lg'
                    : 'bg-white hover:bg-gray-50 border border-gray-200'
                }`}
              >
                <div className="flex items-center justify-between">
                  <span className="font-medium">{item}</span>
                  {selectedIndex === index && (
                    <span className="text-sm">✓ Selected</span>
                  )}
                </div>
              </div>
            ))
          ) : (
            <div className="text-center py-8 text-gray-500">
              <p>No items found matching "{searchQuery}"</p>
            </div>
          )}
        </div>
        
        {selectedIndex !== null && (
          <div className="mt-4 p-4 bg-green-50 border border-green-200 rounded-lg">
            <p className="text-green-800">
              Selected: <strong>{filteredItems[selectedIndex]}</strong>
            </p>
          </div>
        )}
      </div>
    </Modal>
  );
}

export { ComponentA, ComponentB };
'''
        
        # Move ComponentB from below ComponentA to above it
        # This requires: remove B from bottom, add B at top (after imports, before A)
        diff = FileDiff(
            path='src/components.tsx',
            hunks=[
                # Hunk 1: Add ComponentB after imports, before ComponentA
                '''@@ -5,4 +5,60 @@
 import { Modal } from './Modal';
 
+interface ComponentBProps {
+  items: string[];
+  onSelect?: (item: string) => void;
+}
+
+function ComponentB({ items, onSelect }: ComponentBProps) {
+  const [selectedIndex, setSelectedIndex] = useState<number | null>(null);
+  const [searchQuery, setSearchQuery] = useState('');
+  
+  const filteredItems = items.filter(item =>
+    item.toLowerCase().includes(searchQuery.toLowerCase())
+  );
+  
+  const handleItemClick = (item: string, index: number) => {
+    setSelectedIndex(index);
+    onSelect?.(item);
+  };
+  
+  return (
+    <Modal isOpen={true} onClose={() => {}}>
+      <div className="component-b-container p-6">
+        <h2 className="text-3xl font-semibold mb-4 text-blue-600">
+          Component B
+        </h2>
+        
+        <div className="search-section mb-4">
+          <input
+            type="text"
+            value={searchQuery}
+            onChange={(e) => setSearchQuery(e.target.value)}
+            placeholder="Search items..."
+            className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
+          />
+        </div>
+        
+        <div className="items-list space-y-2">
+          {filteredItems.length > 0 ? (
+            filteredItems.map((item, index) => (
+              <div
+                key={index}
+                onClick={() => handleItemClick(item, index)}
+                className={`p-4 rounded-lg cursor-pointer transition-all ${
+                  selectedIndex === index
+                    ? 'bg-blue-500 text-white shadow-lg'
+                    : 'bg-white hover:bg-gray-50 border border-gray-200'
+                }`}
+              >
+                <div className="flex items-center justify-between">
+                  <span className="font-medium">{item}</span>
+                  {selectedIndex === index && (
+                    <span className="text-sm">✓ Selected</span>
+                  )}
+                </div>
+              </div>
+            ))
+          ) : (
+            <div className="text-center py-8 text-gray-500">
+              <p>No items found matching "{searchQuery}"</p>
+            </div>
+          )}
+        </div>
+        
+        {selectedIndex !== null && (
+          <div className="mt-4 p-4 bg-green-50 border border-green-200 rounded-lg">
+            <p className="text-green-800">
+              Selected: <strong>{filteredItems[selectedIndex]}</strong>
+            </p>
+          </div>
+        )}
+      </div>
+    </Modal>
+  );
+}
+
 interface ComponentAProps {''',
                # Hunk 2: Remove ComponentB from its original position (below ComponentA)
                '''@@ -67,75 +67,1 @@
 
-interface ComponentBProps {
-  items: string[];
-  onSelect?: (item: string) => void;
-}
-
-function ComponentB({ items, onSelect }: ComponentBProps) {
-  const [selectedIndex, setSelectedIndex] = useState<number | null>(null);
-  const [searchQuery, setSearchQuery] = useState('');
-  
-  const filteredItems = items.filter(item =>
-    item.toLowerCase().includes(searchQuery.toLowerCase())
-  );
-  
-  const handleItemClick = (item: string, index: number) => {
-    setSelectedIndex(index);
-    onSelect?.(item);
-  };
-  
-  return (
-    <Modal isOpen={true} onClose={() => {}}>
-      <div className="component-b-container p-6">
-        <h2 className="text-3xl font-semibold mb-4 text-blue-600">
-          Component B
-        </h2>
-        
-        <div className="search-section mb-4">
-          <input
-            type="text"
-            value={searchQuery}
-            onChange={(e) => setSearchQuery(e.target.value)}
-            placeholder="Search items..."
-            className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
-          />
-        </div>
-        
-        <div className="items-list space-y-2">
-          {filteredItems.length > 0 ? (
-            filteredItems.map((item, index) => (
-              <div
-                key={index}
-                onClick={() => handleItemClick(item, index)}
-                className={`p-4 rounded-lg cursor-pointer transition-all ${
-                  selectedIndex === index
-                    ? 'bg-blue-500 text-white shadow-lg'
-                    : 'bg-white hover:bg-gray-50 border border-gray-200'
-                }`}
-              >
-                <div className="flex items-center justify-between">
-                  <span className="font-medium">{item}</span>
-                  {selectedIndex === index && (
-                    <span className="text-sm">✓ Selected</span>
-                  )}
-                </div>
-              </div>
-            ))
-          ) : (
-            <div className="text-center py-8 text-gray-500">
-              <p>No items found matching "{searchQuery}"</p>
-            </div>
-          )}
-        </div>
-        
-        {selectedIndex !== null && (
-          <div className="mt-4 p-4 bg-green-50 border border-green-200 rounded-lg">
-            <p className="text-green-800">
-              Selected: <strong>{filteredItems[selectedIndex]}</strong>
-            </p>
-          </div>
-        )}
-      </div>
-    </Modal>
-  );
-}
-
 export { ComponentA, ComponentB };'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        # ComponentB should now appear before ComponentA
        b_pos = result.find('function ComponentB')
        a_pos = result.find('function ComponentA')
        assert b_pos < a_pos, "ComponentB should be before ComponentA after move"
        
        # Should only have one definition of each component
        assert result.count('function ComponentA({') == 1
        assert result.count('function ComponentB({') == 1
        
        # Verify the order: imports -> ComponentB -> ComponentA -> exports
        lines = result.split('\n')
        import_idx = next(i for i, line in enumerate(lines) if "import React" in line)
        b_idx = next(i for i, line in enumerate(lines) if "function ComponentB({" in line)
        a_idx = next(i for i, line in enumerate(lines) if "function ComponentA({" in line)
        export_idx = next(i for i, line in enumerate(lines) if "export {" in line)
        
        assert import_idx < b_idx < a_idx < export_idx, "Components should be in correct order"


# =============================================================================
# TestCompilerFixes - Common TypeScript/React error fix patterns
# =============================================================================

class TestCompilerFixes:
    """Tests for common compiler/type error fix patterns."""

    def test_add_missing_import_ts2304(self):
        """Fix TS2304: Cannot find name 'useState'."""
        original = '''import React from 'react';

function Counter() {
  const [count, setCount] = useState(0);
  return <button onClick={() => setCount(count + 1)}>{count}</button>;
}
'''
        
        diff = FileDiff(
            path='src/Counter.tsx',
            hunks=[
                '''@@ -1,4 +1,5 @@
 import React from 'react';
+import { useState } from 'react';
 
 function Counter() {'''
            ]
        )
        
        result = apply_diff(original, diff)
        assert "import { useState } from 'react';" in result

    def test_add_property_to_interface_ts2339(self):
        """Fix TS2339: Property 'email' does not exist on type 'User'."""
        original = '''interface User {
  id: string;
  name: string;
}

function displayUser(user: User) {
  return `${user.name} <${user.email}>`;
}
'''
        
        diff = FileDiff(
            path='src/types.ts',
            hunks=[
                '''@@ -1,4 +1,5 @@
 interface User {
   id: string;
   name: string;
+  email: string;
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        assert 'email: string;' in result

    def test_fix_type_mismatch_ts2322(self):
        """Fix TS2322: Type 'string' is not assignable to type 'number'."""
        original = '''function getAge(): number {
  return "25";
}
'''
        
        diff = FileDiff(
            path='src/utils.ts',
            hunks=[
                '''@@ -1,3 +1,3 @@
 function getAge(): number {
-  return "25";
+  return 25;
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        assert 'return 25;' in result
        assert 'return "25"' not in result

    def test_add_null_check(self):
        """Add null/undefined check to prevent runtime error."""
        original = '''function getUserName(user: User | null): string {
  return user.name;
}
'''
        
        diff = FileDiff(
            path='src/utils.ts',
            hunks=[
                '''@@ -1,3 +1,6 @@
 function getUserName(user: User | null): string {
+  if (!user) {
+    return 'Unknown';
+  }
   return user.name;
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        assert "if (!user)" in result
        assert "return 'Unknown';" in result

    def test_fix_react_hooks_dependencies(self):
        """Fix missing dependency in useEffect."""
        original = '''function UserProfile({ userId }: Props) {
  const [user, setUser] = useState(null);
  
  useEffect(() => {
    fetchUser(userId).then(setUser);
  }, []);
  
  return <div>{user?.name}</div>;
}
'''
        
        diff = FileDiff(
            path='src/UserProfile.tsx',
            hunks=[
                '''@@ -4,7 +4,7 @@ function UserProfile({ userId }: Props) {
   
   useEffect(() => {
     fetchUser(userId).then(setUser);
-  }, []);
+  }, [userId]);
   
   return <div>{user?.name}</div>;'''
            ]
        )
        
        result = apply_diff(original, diff)
        assert '}, [userId]);' in result
        assert '}, []);' not in result

    def test_add_async_keyword(self):
        """Add missing async keyword to function using await."""
        original = '''function fetchData(url: string) {
  const response = await fetch(url);
  return response.json();
}
'''
        
        diff = FileDiff(
            path='src/api.ts',
            hunks=[
                '''@@ -1,4 +1,4 @@
-function fetchData(url: string) {
+async function fetchData(url: string) {
   const response = await fetch(url);
   return response.json();
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        assert 'async function fetchData' in result

    def test_add_await_keyword(self):
        """Add missing await to async call."""
        original = '''async function processUser(id: string) {
  const user = fetchUser(id);
  console.log(user.name);
}
'''
        
        diff = FileDiff(
            path='src/process.ts',
            hunks=[
                '''@@ -1,4 +1,4 @@
 async function processUser(id: string) {
-  const user = fetchUser(id);
+  const user = await fetchUser(id);
   console.log(user.name);
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        assert 'await fetchUser(id)' in result

    def test_fix_return_type_annotation(self):
        """Add explicit return type to function."""
        original = '''export function calculateTotal(items: Item[]) {
  return items.reduce((sum, item) => sum + item.price, 0);
}
'''
        
        diff = FileDiff(
            path='src/calc.ts',
            hunks=[
                '''@@ -1,3 +1,3 @@
-export function calculateTotal(items: Item[]) {
+export function calculateTotal(items: Item[]): number {
   return items.reduce((sum, item) => sum + item.price, 0);
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        assert 'calculateTotal(items: Item[]): number' in result


# =============================================================================
# TestRefactoringPatterns - Common refactoring operations
# =============================================================================

class TestRefactoringPatterns:
    """Tests for common refactoring patterns."""

    def test_rename_variable_multiple_occurrences(self):
        """Rename a variable throughout a function."""
        original = '''function processItems(data: Item[]) {
  const temp = data.filter(d => d.active);
  const temp2 = temp.map(t => t.name);
  console.log("Processing:", temp.length);
  return temp2;
}
'''
        
        # Rename 'temp' to 'activeItems'
        diff = FileDiff(
            path='src/process.ts',
            hunks=[
                '''@@ -1,6 +1,6 @@
 function processItems(data: Item[]) {
-  const temp = data.filter(d => d.active);
-  const temp2 = temp.map(t => t.name);
-  console.log("Processing:", temp.length);
-  return temp2;
+  const activeItems = data.filter(d => d.active);
+  const names = activeItems.map(t => t.name);
+  console.log("Processing:", activeItems.length);
+  return names;
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        assert 'activeItems' in result
        assert 'temp' not in result
        assert 'temp2' not in result

    def test_extract_magic_number_to_constant(self):
        """Extract magic numbers to named constants."""
        original = '''function calculateDiscount(price: number): number {
  if (price > 100) {
    return price * 0.9;
  } else if (price > 50) {
    return price * 0.95;
  }
  return price;
}
'''
        
        diff = FileDiff(
            path='src/pricing.ts',
            hunks=[
                '''@@ -1,9 +1,14 @@
+const HIGH_VALUE_THRESHOLD = 100;
+const MID_VALUE_THRESHOLD = 50;
+const HIGH_VALUE_DISCOUNT = 0.9;
+const MID_VALUE_DISCOUNT = 0.95;
+
 function calculateDiscount(price: number): number {
-  if (price > 100) {
-    return price * 0.9;
-  } else if (price > 50) {
-    return price * 0.95;
+  if (price > HIGH_VALUE_THRESHOLD) {
+    return price * HIGH_VALUE_DISCOUNT;
+  } else if (price > MID_VALUE_THRESHOLD) {
+    return price * MID_VALUE_DISCOUNT;
   }
   return price;
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        assert 'HIGH_VALUE_THRESHOLD = 100' in result
        assert 'price > HIGH_VALUE_THRESHOLD' in result
        assert 'price > 100' not in result

    def test_convert_callback_to_async_await(self):
        """Convert promise chain to async/await."""
        original = '''function loadUser(id: string) {
  return fetchUser(id)
    .then(user => fetchPosts(user.id))
    .then(posts => ({ posts }))
    .catch(err => {
      console.error(err);
      return { posts: [] };
    });
}
'''
        
        diff = FileDiff(
            path='src/user.ts',
            hunks=[
                '''@@ -1,10 +1,12 @@
-function loadUser(id: string) {
-  return fetchUser(id)
-    .then(user => fetchPosts(user.id))
-    .then(posts => ({ posts }))
-    .catch(err => {
-      console.error(err);
-      return { posts: [] };
-    });
+async function loadUser(id: string) {
+  try {
+    const user = await fetchUser(id);
+    const posts = await fetchPosts(user.id);
+    return { posts };
+  } catch (err) {
+    console.error(err);
+    return { posts: [] };
+  }
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        assert 'async function loadUser' in result
        assert 'await fetchUser(id)' in result
        assert '.then(' not in result

    def test_wrap_in_try_catch(self):
        """Wrap code in try-catch block."""
        original = '''function parseConfig(json: string): Config {
  const data = JSON.parse(json);
  return validateConfig(data);
}
'''
        
        diff = FileDiff(
            path='src/config.ts',
            hunks=[
                '''@@ -1,4 +1,10 @@
 function parseConfig(json: string): Config {
-  const data = JSON.parse(json);
-  return validateConfig(data);
+  try {
+    const data = JSON.parse(json);
+    return validateConfig(data);
+  } catch (error) {
+    console.error('Failed to parse config:', error);
+    throw new ConfigError('Invalid configuration');
+  }
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        assert 'try {' in result
        assert '} catch (error) {' in result
        assert 'ConfigError' in result

    def test_convert_class_to_functional_component(self):
        """Convert React class component to functional component."""
        original = '''class Counter extends React.Component {
  state = { count: 0 };
  
  increment = () => {
    this.setState({ count: this.state.count + 1 });
  };
  
  render() {
    return (
      <div>
        <p>{this.state.count}</p>
        <button onClick={this.increment}>+</button>
      </div>
    );
  }
}
'''
        
        diff = FileDiff(
            path='src/Counter.tsx',
            hunks=[
                '''@@ -1,17 +1,14 @@
-class Counter extends React.Component {
-  state = { count: 0 };
-  
-  increment = () => {
-    this.setState({ count: this.state.count + 1 });
-  };
-  
-  render() {
-    return (
-      <div>
-        <p>{this.state.count}</p>
-        <button onClick={this.increment}>+</button>
-      </div>
-    );
-  }
+function Counter() {
+  const [count, setCount] = useState(0);
+  
+  const increment = () => {
+    setCount(count + 1);
+  };
+  
+  return (
+    <div>
+      <p>{count}</p>
+      <button onClick={increment}>+</button>
+    </div>
+  );
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        assert 'function Counter()' in result
        assert 'useState(0)' in result
        assert 'class Counter' not in result
        assert 'this.state' not in result


# =============================================================================
# TestScaleDiffs - Large diffs, many hunks, many files
# =============================================================================

class TestScaleDiffs:
    """Tests for scale: large diffs, many hunks, many files."""

    def test_long_single_hunk_100_plus_lines(self):
        """Apply a single hunk with 100+ line changes."""
        # Generate a file with 150 lines
        original_lines = ['// Generated file']
        for i in range(150):
            original_lines.append(f'const var{i} = {i};')
        original = '\n'.join(original_lines)
        
        # Generate a hunk that changes 100 consecutive lines (lines 25-124)
        hunk_lines = ['@@ -25,102 +25,102 @@']
        hunk_lines.append(f' const var23 = 23;')
        hunk_lines.append(f' const var24 = 24;')
        for i in range(25, 125):
            hunk_lines.append(f'-const var{i} = {i};')
            hunk_lines.append(f'+const var{i} = {i * 10};')
        hunk_lines.append(f' const var125 = 125;')
        hunk_lines.append(f' const var126 = 126;')
        
        diff = FileDiff(
            path='src/generated.ts',
            hunks=['\n'.join(hunk_lines)]
        )
        
        result = apply_diff(original, diff)
        
        # Check that changes were applied
        assert 'const var50 = 500;' in result  # 50 * 10
        assert 'const var100 = 1000;' in result  # 100 * 10
        # Check unchanged lines
        assert 'const var10 = 10;' in result  # unchanged
        assert 'const var140 = 140;' in result  # unchanged

    def test_many_hunks_10_plus(self, large_typescript_file):
        """Apply 10+ hunks to a single file."""
        original = large_typescript_file
        
        # Create 12 hunks, each modifying a different function
        hunks = []
        for i in range(12):
            # Each function starts around line (i * 11 + 4) due to header
            start_line = i * 11 + 5
            hunk = f'''@@ -{start_line},3 +{start_line},3 @@
 export function processItem{i}(data: unknown): string {{
-  const id = "item-{i}";
+  const id = "modified-item-{i}";
   const timestamp = Date.now();'''
            hunks.append(hunk)
        
        diff = FileDiff(
            path='src/large.ts',
            hunks=hunks
        )
        
        result = apply_diff(original, diff)
        
        # Check multiple modifications were applied
        assert 'const id = "modified-item-0";' in result
        assert 'const id = "modified-item-5";' in result
        assert 'const id = "modified-item-11";' in result
        # Original IDs should be gone
        assert 'const id = "item-0";' not in result

    def test_many_files_simultaneously(self):
        """Apply diffs to 5+ files at once."""
        # Create 6 original files
        original_files = {}
        for i in range(6):
            original_files[f'src/file{i}.ts'] = f'''// File {i}
export function func{i}() {{
  return "original-{i}";
}}
'''
        
        # Parse diffs for all files from LLM response
        llm_response = '''Here are the changes:

```diff
--- src/file0.ts
+++ src/file0.ts
@@ -2,3 +2,3 @@ // File 0
 export function func0() {
-  return "original-0";
+  return "modified-0";
 }
--- src/file1.ts
+++ src/file1.ts
@@ -2,3 +2,3 @@ // File 1
 export function func1() {
-  return "original-1";
+  return "modified-1";
 }
--- src/file2.ts
+++ src/file2.ts
@@ -2,3 +2,3 @@ // File 2
 export function func2() {
-  return "original-2";
+  return "modified-2";
 }
--- src/file3.ts
+++ src/file3.ts
@@ -2,3 +2,3 @@ // File 3
 export function func3() {
-  return "original-3";
+  return "modified-3";
 }
--- src/file4.ts
+++ src/file4.ts
@@ -2,3 +2,3 @@ // File 4
 export function func4() {
-  return "original-4";
+  return "modified-4";
 }
--- src/file5.ts
+++ src/file5.ts
@@ -2,3 +2,3 @@ // File 5
 export function func5() {
-  return "original-5";
+  return "modified-5";
 }
```
'''
        
        diffs = parse_diffs(llm_response)
        
        # Should have parsed 6 file diffs
        assert len(diffs) == 6
        
        # Apply each diff
        results = {}
        for diff in diffs:
            original = original_files.get(diff.path)
            if original:
                results[diff.path] = apply_diff(original, diff)
        
        # Verify all files were modified
        assert len(results) == 6
        for i in range(6):
            path = f'src/file{i}.ts'
            assert f'return "modified-{i}"' in results[path]
            assert f'return "original-{i}"' not in results[path]

    def test_large_file_500_plus_lines(self, large_typescript_file):
        """Apply diff to a file with 500+ lines."""
        original = large_typescript_file
        
        # Verify file is large enough
        line_count = len(original.split('\n'))
        assert line_count > 500, f"Test file should have 500+ lines, has {line_count}"
        
        # Apply a change near the end of the file
        diff = FileDiff(
            path='src/large.ts',
            hunks=[
                '''@@ -540,4 +540,5 @@
 export function processItem48(data: unknown): string {
   const id = "item-48";
+  // Added comment near end of large file
   const timestamp = Date.now();'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        assert '// Added comment near end of large file' in result

    def test_combination_many_hunks_and_long_changes(self):
        """Combine many hunks with substantial changes in each."""
        # Generate a 200-line file
        original_lines = ['// Config file']
        for i in range(200):
            original_lines.append(f'export const CONFIG_{i} = "{i}";')
        original = '\n'.join(original_lines)
        
        # Create 5 hunks, each changing 20 lines
        hunks = []
        for h in range(5):
            start = h * 40 + 2  # Space out hunks
            hunk_lines = [f'@@ -{start},22 +{start},22 @@']
            hunk_lines.append(f' export const CONFIG_{start-2} = "{start-2}";')
            hunk_lines.append(f' export const CONFIG_{start-1} = "{start-1}";')
            for i in range(20):
                line_num = start + i
                hunk_lines.append(f'-export const CONFIG_{line_num} = "{line_num}";')
                hunk_lines.append(f'+export const CONFIG_{line_num} = "MODIFIED_{line_num}";')
            hunks.append('\n'.join(hunk_lines))
        
        diff = FileDiff(
            path='src/config.ts',
            hunks=hunks
        )
        
        result = apply_diff(original, diff)
        
        # Check changes from different hunks
        assert 'CONFIG_5 = "MODIFIED_5"' in result
        assert 'CONFIG_45 = "MODIFIED_45"' in result
        assert 'CONFIG_85 = "MODIFIED_85"' in result


# =============================================================================
# TestEdgeCasesAndRecovery - Problematic diffs and recovery
# =============================================================================

class TestEdgeCasesAndRecovery:
    """Tests for edge cases and error recovery."""

    def test_wrong_line_numbers_relocated(self):
        """Apply hunk with completely wrong line numbers (should relocate)."""
        original = '''function first() {
  return 1;
}

function second() {
  return 2;
}

function third() {
  return 3;
}
'''
        
        # Hunk claims to be at line 100, but content is at line 5
        diff = FileDiff(
            path='src/funcs.ts',
            hunks=[
                '''@@ -100,3 +100,3 @@
 function second() {
-  return 2;
+  return 22;
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        # Should relocate and apply
        assert 'return 22;' in result
        assert 'return 2;' not in result

    def test_misordered_hunks(self):
        """Apply hunks provided in wrong order (bottom first)."""
        original = '''line 1
line 2
line 3
line 4
line 5
line 6
line 7
line 8
'''
        
        # Hunks in wrong order (line 7 before line 2)
        diff = FileDiff(
            path='src/test.ts',
            hunks=[
                '''@@ -6,3 +6,3 @@
 line 6
-line 7
+LINE SEVEN MODIFIED
 line 8''',
                '''@@ -1,3 +1,3 @@
 line 1
-line 2
+LINE TWO MODIFIED
 line 3'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        # Both changes should apply despite wrong order
        assert 'LINE TWO MODIFIED' in result
        assert 'LINE SEVEN MODIFIED' in result

    def test_incorrect_line_counts_in_header(self):
        """Apply hunk with wrong line counts in @@ header."""
        original = '''const a = 1;
const b = 2;
const c = 3;
'''
        
        # Header says 5 lines but there are only 3
        diff = FileDiff(
            path='src/test.ts',
            hunks=[
                '''@@ -1,5 +1,5 @@
 const a = 1;
-const b = 2;
+const b = 22;
 const c = 3;'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        # Should still apply (header gets fixed)
        assert 'const b = 22;' in result

    def test_partial_hunk_failure(self):
        """When some hunks fail, others should still apply."""
        original = '''function exists() {
  return true;
}

function alsoExists() {
  return true;
}
'''
        
        diff = FileDiff(
            path='src/test.ts',
            hunks=[
                # This hunk should apply
                '''@@ -1,3 +1,3 @@
 function exists() {
-  return true;
+  return false;
 }''',
                # This hunk should fail (content doesn't exist)
                '''@@ -50,3 +50,3 @@
 function doesNotExist() {
-  return "nope";
+  return "still nope";
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        # First hunk should have applied
        assert 'return false;' in result
        # Original content should be preserved where second hunk failed
        assert 'function alsoExists()' in result

    def test_whitespace_only_changes(self):
        """Apply diff that only changes whitespace."""
        original = '''function test() {
    const x = 1;
    return x;
}
'''
        
        # Change from 4-space to 2-space indent
        diff = FileDiff(
            path='src/test.ts',
            hunks=[
                '''@@ -1,4 +1,4 @@
 function test() {
-    const x = 1;
-    return x;
+  const x = 1;
+  return x;
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        # Should have 2-space indentation now
        assert '  const x = 1;' in result
        assert '    const x = 1;' not in result

    def test_empty_lines_in_context(self):
        """Handle empty lines as context correctly."""
        original = '''import React from 'react';

function App() {

  return <div>Hello</div>;

}
'''
        
        diff = FileDiff(
            path='src/App.tsx',
            hunks=[
                '''@@ -3,5 +3,5 @@ import React from 'react';
 function App() {
 
-  return <div>Hello</div>;
+  return <div>World</div>;
 
 }'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        assert '<div>World</div>' in result
        assert '<div>Hello</div>' not in result

    def test_minimal_context_single_line(self):
        """Apply hunk with minimal (single line) context."""
        original = '''a
b
c
d
e
'''
        
        diff = FileDiff(
            path='src/test.txt',
            hunks=[
                '''@@ -2,2 +2,2 @@
-b
+B
 c'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        assert '\nB\n' in result
        assert '\nb\n' not in result

    def test_duplicate_hunks_last_wins(self):
        """When duplicate hunks exist, last one should win."""
        original = '''const value = "original";
const other = "other";
'''
        
        # Same line modified twice - last should win
        diff = FileDiff(
            path='src/test.ts',
            hunks=[
                '''@@ -1,2 +1,2 @@
-const value = "original";
+const value = "first change";
 const other = "other";''',
                '''@@ -1,2 +1,2 @@
-const value = "original";
+const value = "second change";
 const other = "other";'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        # Second/last hunk should have been applied
        assert 'const value = "second change"' in result


# =============================================================================
# TestRealWorldAIPatterns - Patterns from actual AI output
# =============================================================================

class TestRealWorldAIPatterns:
    """Tests for patterns commonly seen in real AI output."""

    def test_multiple_diff_blocks_in_response(self):
        """Parse and apply diffs from multiple code blocks."""
        original_app = '''import React from 'react';

export function App() {
  return <div>App</div>;
}
'''
        original_utils = '''export function helper() {
  return null;
}
'''
        
        llm_response = '''I'll make changes to both files.

First, let's update the App:

```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,4 +1,5 @@
 import React from 'react';
+import { helper } from './utils';
 
 export function App() {
```

Now let's update the utils:

```diff
--- src/utils.ts
+++ src/utils.ts
@@ -1,3 +1,3 @@
 export function helper() {
-  return null;
+  return "data";
 }
```

That should fix the issue.'''
        
        diffs = parse_diffs(llm_response)
        
        assert len(diffs) == 2
        
        # Apply to App
        app_diff = next(d for d in diffs if 'App' in d.path)
        app_result = apply_diff(original_app, app_diff)
        assert "import { helper } from './utils';" in app_result
        
        # Apply to utils  
        utils_diff = next(d for d in diffs if 'utils' in d.path)
        utils_result = apply_diff(original_utils, utils_diff)
        assert 'return "data";' in utils_result

    def test_same_file_multiple_blocks_merged(self):
        """Hunks for same file across multiple blocks get merged."""
        original = '''import React from 'react';

function Component() {
  const x = 1;
  const y = 2;
  return null;
}

export default Component;
'''
        
        llm_response = '''First change:

```diff
--- src/Component.tsx
+++ src/Component.tsx
@@ -1,4 +1,5 @@
 import React from 'react';
+import { useState } from 'react';
 
 function Component() {
```

Second change to same file:

```diff
--- src/Component.tsx
+++ src/Component.tsx
@@ -5,4 +6,4 @@
   const y = 2;
-  return null;
+  return <div>{x + y}</div>;
 }
```
'''
        
        diffs = parse_diffs(llm_response)
        
        # Should be merged into single file diff
        assert len(diffs) == 1
        assert len(diffs[0].hunks) == 2
        
        result = apply_diff(original, diffs[0])
        
        # Both changes should be present
        assert "import { useState } from 'react';" in result
        assert '<div>{x + y}</div>' in result

    def test_git_style_a_b_prefixes(self):
        """Handle git-style a/ and b/ path prefixes."""
        original = '''export const VERSION = "1.0.0";
'''
        
        llm_response = '''```diff
--- a/src/version.ts
+++ b/src/version.ts
@@ -1 +1 @@
-export const VERSION = "1.0.0";
+export const VERSION = "2.0.0";
```'''
        
        diffs = parse_diffs(llm_response)
        
        assert len(diffs) == 1
        assert diffs[0].path == 'src/version.ts'  # Prefixes stripped
        
        result = apply_diff(original, diffs[0])
        assert 'VERSION = "2.0.0"' in result

    def test_function_context_in_hunk_header(self):
        """Diffs with function name in @@ header."""
        original = '''function outer() {
  function inner() {
    const value = "old";
    return value;
  }
  return inner();
}
'''
        
        # AI often includes function context after @@
        diff = FileDiff(
            path='src/nested.ts',
            hunks=[
                '''@@ -2,4 +2,4 @@ function outer() {
   function inner() {
-    const value = "old";
+    const value = "new";
     return value;
   }'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        assert 'const value = "new";' in result
        assert 'const value = "old";' not in result

    def test_new_file_creation_pattern(self):
        """Handle diff for creating a new file."""
        # For new files, original is empty
        original = ''
        
        diff = FileDiff(
            path='src/newfile.ts',
            hunks=[
                '''@@ -0,0 +1,5 @@
+// New file
+export function newFunction() {
+  return "brand new";
+}
+'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        assert '// New file' in result
        assert 'export function newFunction()' in result
        assert 'return "brand new";' in result

    def test_file_with_trailing_newline_preservation(self):
        """Preserve trailing newline behavior."""
        original = '''const x = 1;
const y = 2;
'''  # Has trailing newline
        
        diff = FileDiff(
            path='src/test.ts',
            hunks=[
                '''@@ -1,2 +1,3 @@
 const x = 1;
 const y = 2;
+const z = 3;'''
            ]
        )
        
        result = apply_diff(original, diff)
        
        assert 'const z = 3;' in result
        # All three consts should be present
        assert 'const x = 1;' in result
        assert 'const y = 2;' in result
