"""
Tests for DiffApplicationService

Tests the unified diff application service that consolidates
diff parsing and application logic from across handlers.
"""

import pytest
from unittest.mock import Mock, patch, MagicMock
from typing import List

from vector_app.ai.models import AIModel
from vector_app.services.diff_application_service import (
    DiffApplicationService,
    DiffApplicationConfig,
    _apply_diffs_from_llm_response,
    _normalize_path,
    _extract_new_file_content,
    _get_language,
    get_diff_application_service,
)
from vector_app.services.diff import FileDiff
from vector_app.services.types import FileChange


class TestGetLanguage:
    """Tests for _get_language helper function."""
    
    def test_tsx_extension(self):
        assert _get_language("src/App.tsx") == "tsx"
    
    def test_ts_extension(self):
        assert _get_language("src/utils.ts") == "ts"
    
    def test_jsx_extension(self):
        assert _get_language("src/App.jsx") == "jsx"
    
    def test_js_extension(self):
        assert _get_language("src/utils.js") == "js"
    
    def test_css_extension(self):
        assert _get_language("src/styles.css") == "css"
    
    def test_json_extension(self):
        assert _get_language("data.json") == "json"
    
    def test_html_extension(self):
        assert _get_language("index.html") == "html"
    
    def test_unknown_extension_defaults_to_tsx(self):
        assert _get_language("file.unknown") == "tsx"
    
    def test_no_extension_defaults_to_tsx(self):
        assert _get_language("Dockerfile") == "tsx"


class TestNormalizePath:
    """Tests for _normalize_path helper function."""
    
    def test_exact_match(self):
        file_contents = {"src/App.tsx": "content"}
        assert _normalize_path("src/App.tsx", file_contents) == "src/App.tsx"
    
    def test_adds_src_prefix(self):
        file_contents = {"src/App.tsx": "content"}
        assert _normalize_path("App.tsx", file_contents) == "src/App.tsx"
    
    def test_removes_src_prefix(self):
        file_contents = {"App.tsx": "content"}
        assert _normalize_path("src/App.tsx", file_contents) == "App.tsx"
    
    def test_no_match_returns_original(self):
        file_contents = {"other/file.tsx": "content"}
        assert _normalize_path("src/App.tsx", file_contents) == "src/App.tsx"


class TestExtractNewFileContent:
    """Tests for _extract_new_file_content helper function."""
    
    def test_extracts_added_lines(self):
        diff = FileDiff(
            path="src/NewFile.tsx",
            hunks=[
                "@@ -0,0 +1,3 @@\n+line 1\n+line 2\n+line 3"
            ],
            lines_added=3,
            lines_removed=0,
        )
        content = _extract_new_file_content(diff)
        assert content == "line 1\nline 2\nline 3"
    
    def test_skips_header_lines(self):
        diff = FileDiff(
            path="src/NewFile.tsx",
            hunks=[
                "@@ -0,0 +1,2 @@\n+++ src/NewFile.tsx\n+actual content\n+more content"
            ],
            lines_added=2,
            lines_removed=0,
        )
        content = _extract_new_file_content(diff)
        # Should skip the +++ line
        assert "+++ src/NewFile.tsx" not in content
        assert "actual content" in content
    
    def test_multiple_hunks(self):
        diff = FileDiff(
            path="src/NewFile.tsx",
            hunks=[
                "@@ -0,0 +1,2 @@\n+line 1\n+line 2",
                "@@ -0,0 +3,2 @@\n+line 3\n+line 4",
            ],
            lines_added=4,
            lines_removed=0,
        )
        content = _extract_new_file_content(diff)
        assert "line 1" in content
        assert "line 4" in content


class TestApplyDiffsFromLLMResponse:
    """Tests for the main _apply_diffs_from_llm_response function."""
    
    def test_simple_modification(self):
        """Test applying a simple modification diff."""
        llm_response = '''Here's the fix:

```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,3 +1,3 @@
 import React from 'react';
-const App = () => <div>Hello</div>;
+const App = () => <div>Hello World</div>;
 export default App;
```
'''
        file_contents = {
            "src/App.tsx": "import React from 'react';\nconst App = () => <div>Hello</div>;\nexport default App;"
        }
        
        result = _apply_diffs_from_llm_response(llm_response, file_contents)
        
        assert len(result) == 1
        assert result[0].path == "src/App.tsx"
        assert result[0].action == "modify"
        assert "Hello World" in result[0].content
        assert result[0].previous_content == file_contents["src/App.tsx"]
    
    def test_new_file_creation(self):
        """Test creating a new file via diff."""
        llm_response = '''Creating a new file:

```diff
--- /dev/null
+++ src/NewComponent.tsx
@@ -0,0 +1,5 @@
+import React from 'react';
+
+export const NewComponent = () => {
+  return <div>New!</div>;
+};
```
'''
        file_contents = {}
        config = DiffApplicationConfig(allow_new_files=True)
        
        result = _apply_diffs_from_llm_response(llm_response, file_contents, config)
        
        assert len(result) == 1
        assert result[0].path == "src/NewComponent.tsx"
        assert result[0].action == "create"
        assert "NewComponent" in result[0].content
    
    def test_new_file_blocked_when_disabled(self):
        """Test that new files are skipped when allow_new_files=False."""
        llm_response = '''```diff
--- /dev/null
+++ src/NewComponent.tsx
@@ -0,0 +1,1 @@
+export const X = 1;
```
'''
        file_contents = {}
        config = DiffApplicationConfig(allow_new_files=False)
        
        result = _apply_diffs_from_llm_response(llm_response, file_contents, config)
        
        assert len(result) == 0
    
    def test_protected_files_skipped(self):
        """Test that protected files are skipped."""
        llm_response = '''```diff
--- src/index.tsx
+++ src/index.tsx
@@ -1,1 +1,1 @@
-ReactDOM.render(<App />, document.getElementById('root'));
+ReactDOM.createRoot(document.getElementById('root')!).render(<App />);
```
'''
        file_contents = {
            "src/index.tsx": "ReactDOM.render(<App />, document.getElementById('root'));"
        }
        config = DiffApplicationConfig(protected_files={"src/index.tsx"})
        
        result = _apply_diffs_from_llm_response(llm_response, file_contents, config)
        
        assert len(result) == 0
    
    def test_path_normalization(self):
        """Test that path normalization works when enabled."""
        llm_response = '''```diff
--- App.tsx
+++ App.tsx
@@ -1,1 +1,1 @@
-old content
+new content
```
'''
        file_contents = {
            "src/App.tsx": "old content"
        }
        config = DiffApplicationConfig(normalize_paths=True)
        
        result = _apply_diffs_from_llm_response(llm_response, file_contents, config)
        
        assert len(result) == 1
        assert result[0].path == "src/App.tsx"
    
    def test_fallback_to_full_file_parsing(self):
        """Test fallback when no diffs found."""
        llm_response = '''Here's the complete file:

```src/App.tsx
export const App = () => <div>Complete file</div>;
```
'''
        file_contents = {"src/App.tsx": "old content"}
        
        # Mock fallback function
        mock_fallback = Mock(return_value=[
            FileChange(
                path="src/App.tsx",
                action="modify",
                language="tsx",
                content="export const App = () => <div>Complete file</div>;",
            )
        ])
        config = DiffApplicationConfig(fallback_to_full_file=True)
        
        result = _apply_diffs_from_llm_response(
            llm_response, 
            file_contents, 
            config,
            parse_full_files_fallback=mock_fallback,
        )
        
        assert len(result) == 1
        mock_fallback.assert_called_once_with(llm_response)
    
    def test_no_fallback_returns_empty(self):
        """Test that empty list is returned when no diffs and no fallback."""
        llm_response = "Just some text without diffs"
        file_contents = {"src/App.tsx": "content"}
        config = DiffApplicationConfig(fallback_to_full_file=False)
        
        result = _apply_diffs_from_llm_response(llm_response, file_contents, config)
        
        assert len(result) == 0
    
    def test_verify_changes_skips_unchanged(self):
        """Test that verify_changes skips files with no actual changes."""
        # A diff that would result in the same content (empty hunk)
        llm_response = '''```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,1 +1,1 @@
 same content
```
'''
        file_contents = {"src/App.tsx": "same content"}
        config = DiffApplicationConfig(verify_changes=True)
        
        result = _apply_diffs_from_llm_response(llm_response, file_contents, config)
        
        # Should be empty since content didn't change
        assert len(result) == 0
    
    def test_multiple_files(self):
        """Test applying diffs to multiple files."""
        llm_response = '''```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,1 +1,1 @@
-old app
+new app
```

```diff
--- src/utils.ts
+++ src/utils.ts
@@ -1,1 +1,1 @@
-old utils
+new utils
```
'''
        file_contents = {
            "src/App.tsx": "old app",
            "src/utils.ts": "old utils",
        }
        
        result = _apply_diffs_from_llm_response(llm_response, file_contents)
        
        assert len(result) == 2
        paths = {f.path for f in result}
        assert paths == {"src/App.tsx", "src/utils.ts"}


class TestDiffApplicationService:
    """Tests for the DiffApplicationService class."""
    
    @pytest.fixture
    def service(self):
        return DiffApplicationService()
    
    def test_apply_diffs_calls_internal_function(self, service):
        """Test that apply_diffs method works correctly."""
        llm_response = '''```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,1 +1,1 @@
-old
+new
```
'''
        file_contents = {"src/App.tsx": "old"}
        
        result = service.apply_diffs(llm_response, file_contents)
        
        assert len(result) == 1
        assert result[0].action == "modify"
    
    def test_apply_diffs_with_config(self, service):
        """Test apply_diffs respects configuration."""
        llm_response = '''```diff
--- protected.tsx
+++ protected.tsx
@@ -1,1 +1,1 @@
-old
+new
```
'''
        file_contents = {"protected.tsx": "old"}
        config = DiffApplicationConfig(protected_files={"protected.tsx"})
        
        result = service.apply_diffs(llm_response, file_contents, config)
        
        assert len(result) == 0


class TestGetDiffApplicationService:
    """Tests for the singleton getter."""
    
    def test_returns_singleton(self):
        """Test that get_diff_application_service returns the same instance."""
        service1 = get_diff_application_service()
        service2 = get_diff_application_service()
        
        assert service1 is service2
    
    def test_returns_diff_application_service_instance(self):
        """Test that the returned object is a DiffApplicationService."""
        service = get_diff_application_service()
        
        assert isinstance(service, DiffApplicationService)


class TestStreamAndApplyDiffs:
    """Tests for the stream_and_apply_diffs method."""
    
    @pytest.fixture
    def service(self):
        return DiffApplicationService()
    
    @pytest.fixture
    def mock_response(self):
        """Create a mock streaming response."""
        diff_content = '''```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,1 +1,1 @@
-old content
+new content
```'''
        
        # Create SSE chunks
        chunks = []
        for i, char in enumerate(diff_content):
            chunk = {
                "choices": [{"delta": {"content": char}}]
            }
            chunks.append(f"data: {__import__('json').dumps(chunk)}")
        chunks.append("data: [DONE]")
        
        return chunks
    
    def test_stream_and_apply_basic(self, service, mock_response):
        """Test basic streaming and diff application."""
        file_contents = {"src/App.tsx": "old content"}
        
        with patch.object(service, '_build_headers', return_value={}):
            with patch('httpx.Client') as mock_client:
                # Set up the mock response
                mock_stream = MagicMock()
                mock_stream.__enter__ = Mock(return_value=mock_stream)
                mock_stream.__exit__ = Mock(return_value=False)
                mock_stream.iter_lines = Mock(return_value=iter(mock_response))
                mock_stream.raise_for_status = Mock()
                
                mock_client_instance = MagicMock()
                mock_client_instance.__enter__ = Mock(return_value=mock_client_instance)
                mock_client_instance.__exit__ = Mock(return_value=False)
                mock_client_instance.stream = Mock(return_value=mock_stream)
                
                mock_client.return_value = mock_client_instance
                
                # Run the generator
                events = []
                result = None
                gen = service.stream_and_apply_diffs(
                    system_prompt="You are helpful",
                    user_prompt="Fix the code",
                    file_contents=file_contents,
                    model=AIModel.CLAUDE_SONNET_4_5,
                )
                
                try:
                    while True:
                        event = next(gen)
                        events.append(event)
                except StopIteration as e:
                    result = e.value
                
                # Should have file_generated events
                file_events = [e for e in events if e.type == "file_generated"]
                assert len(file_events) == 1
                
                # Should return the files
                assert len(result) == 1
                assert result[0].path == "src/App.tsx"
                assert "new content" in result[0].content
    
    def test_stream_with_on_chunk_callback(self, service, mock_response):
        """Test that on_chunk callback is called during streaming."""
        file_contents = {"src/App.tsx": "old content"}
        chunks_received = []
        
        def on_chunk(chunk, count):
            chunks_received.append((chunk, count))
            return None
        
        with patch.object(service, '_build_headers', return_value={}):
            with patch('httpx.Client') as mock_client:
                mock_stream = MagicMock()
                mock_stream.__enter__ = Mock(return_value=mock_stream)
                mock_stream.__exit__ = Mock(return_value=False)
                mock_stream.iter_lines = Mock(return_value=iter(mock_response))
                mock_stream.raise_for_status = Mock()
                
                mock_client_instance = MagicMock()
                mock_client_instance.__enter__ = Mock(return_value=mock_client_instance)
                mock_client_instance.__exit__ = Mock(return_value=False)
                mock_client_instance.stream = Mock(return_value=mock_stream)
                
                mock_client.return_value = mock_client_instance
                
                gen = service.stream_and_apply_diffs(
                    system_prompt="System",
                    user_prompt="User", 
                    file_contents=file_contents,
                    model=AIModel.CLAUDE_SONNET_4_5,
                    on_chunk=on_chunk,
                )
                
                # Consume the generator
                try:
                    while True:
                        next(gen)
                except StopIteration:
                    pass
                
                # Callback should have been called for each chunk
                assert len(chunks_received) > 0
    
    def test_stream_with_streaming_validator(self, service, mock_response):
        """Test that streaming validator is called during streaming."""
        file_contents = {"src/App.tsx": "old content"}
        
        mock_validator = Mock()
        mock_validator.check_chunk = Mock(return_value=["Warning: test warning"])
        mock_validator.final_check = Mock(return_value=[])
        
        with patch.object(service, '_build_headers', return_value={}):
            with patch('httpx.Client') as mock_client:
                mock_stream = MagicMock()
                mock_stream.__enter__ = Mock(return_value=mock_stream)
                mock_stream.__exit__ = Mock(return_value=False)
                mock_stream.iter_lines = Mock(return_value=iter(mock_response))
                mock_stream.raise_for_status = Mock()
                
                mock_client_instance = MagicMock()
                mock_client_instance.__enter__ = Mock(return_value=mock_client_instance)
                mock_client_instance.__exit__ = Mock(return_value=False)
                mock_client_instance.stream = Mock(return_value=mock_stream)
                
                mock_client.return_value = mock_client_instance
                
                gen = service.stream_and_apply_diffs(
                    system_prompt="System",
                    user_prompt="User",
                    file_contents=file_contents,
                    model=AIModel.CLAUDE_SONNET_4_5,
                    streaming_validator=mock_validator,
                )
                
                events = []
                try:
                    while True:
                        events.append(next(gen))
                except StopIteration:
                    pass
                
                # Validator should have been called
                assert mock_validator.check_chunk.called
                assert mock_validator.final_check.called
                
                # Should have streaming_warning events
                warning_events = [e for e in events if e.type == "streaming_warning"]
                assert len(warning_events) > 0


class TestIntegrationWithRealDiffs:
    """Integration tests with realistic diff scenarios."""
    
    def test_react_component_modification(self):
        """Test modifying a React component."""
        llm_response = '''I'll update the component to add state:

```diff
--- src/components/Counter.tsx
+++ src/components/Counter.tsx
@@ -1,7 +1,12 @@
-import React from 'react';
+import React, { useState } from 'react';
 
 export const Counter = () => {
+  const [count, setCount] = useState(0);
+
   return (
-    <div>Counter</div>
+    <div>
+      <p>Count: {count}</p>
+      <button onClick={() => setCount(c => c + 1)}>Increment</button>
+    </div>
   );
 };
```
'''
        file_contents = {
            "src/components/Counter.tsx": """import React from 'react';

export const Counter = () => {
  return (
    <div>Counter</div>
  );
};"""
        }
        
        result = _apply_diffs_from_llm_response(llm_response, file_contents)
        
        assert len(result) == 1
        content = result[0].content
        assert "useState" in content
        assert "count" in content
        assert "setCount" in content
        assert "Increment" in content
    
    def test_multiple_file_refactor(self):
        """Test refactoring across multiple files."""
        llm_response = '''Refactoring to extract utility:

```diff
--- src/App.tsx
+++ src/App.tsx
@@ -1,5 +1,6 @@
 import React from 'react';
+import { formatDate } from './utils';
 
 export const App = () => {
-  const date = new Date().toISOString();
+  const date = formatDate(new Date());
   return <div>{date}</div>;
 };
```

```diff
--- /dev/null
+++ src/utils.ts
@@ -0,0 +1,3 @@
+export const formatDate = (date: Date): string => {
+  return date.toLocaleDateString();
+};
```
'''
        file_contents = {
            "src/App.tsx": """import React from 'react';

export const App = () => {
  const date = new Date().toISOString();
  return <div>{date}</div>;
};"""
        }
        config = DiffApplicationConfig(allow_new_files=True)
        
        result = _apply_diffs_from_llm_response(llm_response, file_contents, config)
        
        assert len(result) == 2
        
        app_file = next(f for f in result if f.path == "src/App.tsx")
        utils_file = next(f for f in result if f.path == "src/utils.ts")
        
        assert app_file.action == "modify"
        assert "formatDate" in app_file.content
        assert "import { formatDate }" in app_file.content
        
        assert utils_file.action == "create"
        assert "export const formatDate" in utils_file.content
