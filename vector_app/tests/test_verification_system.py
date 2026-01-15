"""
Tests for the Handler Verification System

Integration tests covering:
- Verifier registry and TypeScript verifier
- VerificationMixin (verify_file, verify_and_retry)
- Verification events
- Storage service
- Retry prompts
"""

from datetime import datetime
from unittest.mock import Mock

try:
    import pytest
except ImportError:
    pytest = None  # Tests can run without pytest decorator support

from vector_app.services.types import (
    FileChange,
    FileVerificationResult,
    VerificationAttempt,
)
from vector_app.types import VerificationStatus
from vector_app.services.handlers.base_handler import BaseHandler
from vector_app.services.verifiers import get_verifier_registry
from vector_app.services.verification_storage_service import (
    VerificationStorageService,
    get_verification_storage_service,
)
from vector_app.prompts.verification_retry import (
    build_verification_retry_prompt,
    format_verification_error,
)


# Mock handler for testing mixin methods
class MockHandler(BaseHandler):
    """Mock handler for testing VerificationMixin methods."""

    def execute(self, *args, **kwargs):
        pass


class TestVerifierRegistry:
    """Tests for the verifier registry."""

    def test_get_verifier_registry_singleton(self):
        """Registry should return same instance."""
        registry1 = get_verifier_registry()
        registry2 = get_verifier_registry()
        assert registry1 is registry2

    def test_get_verifiers_for_typescript(self):
        """Should return TypeScript verifier for .ts files."""
        registry = get_verifier_registry()
        verifiers = registry.get_verifiers("src/app.ts")
        assert len(verifiers) >= 1
        assert any(v.name == "typescript" for v in verifiers)

    def test_get_verifiers_for_tsx(self):
        """Should return TypeScript verifier for .tsx files."""
        registry = get_verifier_registry()
        verifiers = registry.get_verifiers("src/App.tsx")
        assert len(verifiers) >= 1
        assert any(v.name == "typescript" for v in verifiers)

    def test_get_verifiers_for_unknown_extension(self):
        """Should return empty list for unknown extensions."""
        registry = get_verifier_registry()
        verifiers = registry.get_verifiers("README.md")
        assert len(verifiers) == 0

    def test_typescript_verifier_is_blocking(self):
        """TypeScript verifier should be blocking by default."""
        registry = get_verifier_registry()
        verifiers = registry.get_verifiers("src/app.ts")
        ts_verifier = next((v for v in verifiers if v.name == "typescript"), None)
        assert ts_verifier is not None
        assert ts_verifier.is_blocking is True


class TestVerificationMixin:
    """Tests for VerificationMixin methods."""

    def test_verify_file_skipped_for_unknown_type(self):
        """verify_file should return SKIPPED for unknown file types."""
        handler = MockHandler()
        file = FileChange(
            path="README.md",
            action="create",
            language="md",
            content="# Hello",
        )
        result = handler.verify_file(file)
        assert result.status == VerificationStatus.SKIPPED
        assert result.file_path == "README.md"

    def test_verify_file_tracks_result(self):
        """verify_file should track result in _verification_results."""
        handler = MockHandler()
        file = FileChange(
            path="src/test.ts",
            action="create",
            language="ts",
            content="const x = 1;",
        )
        handler.verify_file(file)
        stored = handler.get_verification_result("src/test.ts")
        assert stored is not None
        assert stored.file_path == "src/test.ts"

    def test_clear_verification_results(self):
        """clear_verification_results should empty the results dict."""
        handler = MockHandler()
        file = FileChange(
            path="src/test.ts",
            action="create",
            language="ts",
            content="const x = 1;",
        )
        handler.verify_file(file)
        assert len(handler.get_verification_results()) > 0
        handler.clear_verification_results()
        assert len(handler.get_verification_results()) == 0

    def test_verify_and_retry_passes_on_first_attempt(self):
        """verify_and_retry should not call callback if verification passes."""
        handler = MockHandler()
        file = FileChange(
            path="README.md",  # Will be skipped (no verifier)
            action="create",
            language="md",
            content="# Test",
        )
        callback = Mock(side_effect=Exception("Should not be called"))
        final_file, result = handler.verify_and_retry(file, callback)
        assert result.status == VerificationStatus.SKIPPED
        callback.assert_not_called()

    def test_verify_and_retry_tracks_attempts(self):
        """verify_and_retry should track all attempts."""
        handler = MockHandler()
        file = FileChange(
            path="src/test.ts",
            action="create",
            language="ts",
            content="const x = 1;",
        )
        callback = Mock()
        final_file, result = handler.verify_and_retry(file, callback)
        assert result.attempt_count >= 1
        assert len(result.attempts) >= 1
        assert result.attempts[0].attempt_number == 1


class TestVerificationEvents:
    """Tests for verification event emission."""

    def test_emit_verification_started(self):
        """emit_verification_started should return correct event."""
        handler = MockHandler()
        event = handler.emit_verification_started("src/App.tsx", "typescript")
        assert event.type == "verification_started"
        assert event.data["file_path"] == "src/App.tsx"
        assert event.data["verifier"] == "typescript"

    def test_emit_verification_passed(self):
        """emit_verification_passed should return correct event."""
        handler = MockHandler()
        event = handler.emit_verification_passed("src/App.tsx", "typescript")
        assert event.type == "verification_passed"
        assert event.data["file_path"] == "src/App.tsx"

    def test_emit_verification_failed(self):
        """emit_verification_failed should include is_blocking flag."""
        handler = MockHandler()
        event = handler.emit_verification_failed(
            "src/App.tsx", "typescript", "TS1005: error", is_blocking=True
        )
        assert event.type == "verification_failed"
        assert event.data["is_blocking"] is True
        assert event.data["error_message"] == "TS1005: error"

    def test_emit_verification_retry_started(self):
        """emit_verification_retry_started should include attempt info."""
        handler = MockHandler()
        event = handler.emit_verification_retry_started(
            "src/App.tsx", 2, 3, "Previous error"
        )
        assert event.type == "verification_retry_started"
        assert event.data["attempt_number"] == 2
        assert event.data["max_attempts"] == 3

    def test_emit_verification_skipped(self):
        """emit_verification_skipped should include reason."""
        handler = MockHandler()
        event = handler.emit_verification_skipped("README.md", "no verifier")
        assert event.type == "verification_skipped"
        assert event.data["reason"] == "no verifier"

    def test_event_sse_format(self):
        """Events should format correctly as SSE."""
        handler = MockHandler()
        event = handler.emit_verification_passed("src/App.tsx", "typescript")
        sse = event.to_sse()
        assert "event: verification_passed" in sse
        assert "data:" in sse


class TestVerificationRetryPrompts:
    """Tests for retry prompt building."""

    def test_build_retry_prompt_basic(self):
        """build_verification_retry_prompt should include error context."""
        file = FileChange(
            path="src/App.tsx",
            action="create",
            language="tsx",
            content="const App = () => <div>Test</div>",
        )
        prompt = build_verification_retry_prompt(file, "TS1005: Expected semicolon")
        assert "src/App.tsx" in prompt
        assert "TS1005" in prompt
        assert "VERIFICATION ERROR" in prompt

    def test_build_retry_prompt_append_to_original(self):
        """build_verification_retry_prompt should append to original prompt."""
        file = FileChange(
            path="src/App.tsx",
            action="create",
            language="tsx",
            content="const x = 1",
        )
        original = "Generate a React component."
        prompt = build_verification_retry_prompt(
            file, "error", original_prompt=original
        )
        assert prompt.startswith(original)
        assert "VERIFICATION ERROR" in prompt

    def test_build_retry_prompt_language_detection(self):
        """build_verification_retry_prompt should detect language for highlighting."""
        ts_file = FileChange(
            path="src/utils.ts", action="create", language="ts", content="x"
        )
        py_file = FileChange(
            path="script.py", action="create", language="py", content="x"
        )

        ts_prompt = build_verification_retry_prompt(ts_file, "error")
        py_prompt = build_verification_retry_prompt(py_file, "error")

        assert "```typescript" in ts_prompt
        assert "```python" in py_prompt

    def test_format_verification_error(self):
        """format_verification_error should format error message."""
        formatted = format_verification_error("TS1005: error", "src/App.tsx")
        assert "Verification failed" in formatted
        assert "src/App.tsx" in formatted


class TestVerificationStorageService:
    """Tests for verification storage service."""

    def test_factory_getter_singleton(self):
        """get_verification_storage_service should return same instance."""
        service1 = get_verification_storage_service()
        service2 = get_verification_storage_service()
        assert service1 is service2

    def test_save_verification_result(self):
        """save_verification_result should create database records."""
        # Note: Requires django_db marker when run with pytest
        from vector_app.models import FileVerificationRecord

        service = VerificationStorageService()
        result = FileVerificationResult(
            file_path="src/test_save.tsx",
            file_type=".tsx",
            status=VerificationStatus.PASSED,
            verifier_name="typescript",
            final_content="const x = 1;",
            started_at=datetime.now(),
            completed_at=datetime.now(),
            attempts=[
                VerificationAttempt(
                    attempt_number=1,
                    content="const x = 1;",
                    status=VerificationStatus.PASSED,
                    timestamp=datetime.now(),
                )
            ],
        )

        record = service.save_verification_result(result)
        assert record.id is not None
        assert record.file_path == "src/test_save.tsx"
        assert record.attempt_count == 1

        # Cleanup
        FileVerificationRecord.objects.filter(  # pylint: disable=no-member
            file_path="src/test_save.tsx"
        ).delete()

    def test_save_verification_result_with_multiple_attempts(self):
        """save_verification_result should save all attempts."""
        # Note: Requires django_db marker when run with pytest
        from vector_app.models import FileVerificationRecord

        service = VerificationStorageService()
        result = FileVerificationResult(
            file_path="src/test_retry.tsx",
            file_type=".tsx",
            status=VerificationStatus.PASSED,
            verifier_name="typescript",
            final_content="fixed code",
            started_at=datetime.now(),
            completed_at=datetime.now(),
            attempts=[
                VerificationAttempt(
                    attempt_number=1,
                    content="broken code",
                    status=VerificationStatus.FAILED,
                    error_message="Syntax error",
                    timestamp=datetime.now(),
                ),
                VerificationAttempt(
                    attempt_number=2,
                    content="fixed code",
                    status=VerificationStatus.PASSED,
                    timestamp=datetime.now(),
                ),
            ],
        )

        record = service.save_verification_result(result)
        assert record.attempt_count == 2
        attempts = list(record.attempts.all())
        assert attempts[0].status == VerificationStatus.FAILED.value
        assert attempts[1].status == VerificationStatus.PASSED.value

        # Cleanup
        FileVerificationRecord.objects.filter(  # pylint: disable=no-member
            file_path="src/test_retry.tsx"
        ).delete()


class TestIntegration:
    """Integration tests for the complete verification flow."""

    def test_full_verification_flow(self):
        """Test complete flow: verify -> track -> emit event."""
        handler = MockHandler()

        # Create a file
        file = FileChange(
            path="src/integration_test.ts",
            action="create",
            language="ts",
            content="const x: number = 1;",
        )

        # Emit start event
        start_event = handler.emit_verification_started(file.path, "typescript")
        assert start_event.type == "verification_started"

        # Verify the file
        result = handler.verify_file(file)
        assert result.file_path == file.path

        # Emit result event
        if result.status == VerificationStatus.PASSED:
            event = handler.emit_verification_passed(file.path, result.verifier_name)
        elif result.status == VerificationStatus.FAILED:
            event = handler.emit_verification_failed(
                file.path, result.verifier_name, result.error_message or ""
            )
        else:
            event = handler.emit_verification_skipped(file.path)

        assert event.type in [
            "verification_passed",
            "verification_failed",
            "verification_skipped",
        ]

        # Verify result is tracked
        stored = handler.get_verification_result(file.path)
        assert stored is not None

    def test_retry_flow_with_mock_callback(self):
        """Test retry flow with mock regeneration callback."""
        handler = MockHandler()
        call_count = [0]

        def mock_regenerate(file: FileChange, error: str) -> FileChange:
            call_count[0] += 1
            return FileChange(
                path=file.path,
                action=file.action,
                language=file.language,
                content=file.content,  # Same content (passes on first try anyway)
            )

        file = FileChange(
            path="src/retry_test.ts",
            action="create",
            language="ts",
            content="const x = 1;",
        )

        final_file, result = handler.verify_and_retry(file, mock_regenerate, max_attempts=3)

        # Should complete (either pass or exhaust retries)
        assert result is not None
        assert result.attempt_count >= 1
        assert final_file.path == file.path
