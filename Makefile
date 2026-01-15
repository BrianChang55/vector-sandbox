.PHONY: help format format-all format-check format-check-all lint lint-all static static-all check check-all check-diff install-dev sync clean

# Default target
help:
	@echo "Static Analysis Targets:"
	@echo "  make sync              - Sync all dependencies with uv"
	@echo "  make install-dev       - Install dev dependencies with uv"
	@echo "  make format            - Format changed files with black and isort"
	@echo "  make format-all        - Format entire codebase"
	@echo "  make format-check      - Check formatting of changed files"
	@echo "  make format-check-all  - Check formatting of entire codebase"
	@echo "  make lint              - Run prospector on changed files"
	@echo "  make lint-all          - Run prospector on entire codebase"
	@echo "  make static            - Run mypy on changed files"
	@echo "  make static-all        - Run mypy on entire codebase"
	@echo "  make check             - Run all checks on changed files (default)"
	@echo "  make check-all         - Run all checks on entire codebase"
	@echo "  make check-diff        - Alias for 'check' (changed files only)"
	@echo "  make clean             - Remove cache directories"
	@echo ""
	@echo "Example workflow:"
	@echo "  1. make sync            (install all dependencies)"
	@echo "  2. make format          (format your changed files)"
	@echo "  3. make check           (verify changed files pass)"

# Sync all dependencies with uv
sync:
	uv sync

# Install dev dependencies (alias for sync since uv installs dev deps by default)
install-dev:
	uv sync

# Get changed Python files compared to main branch (includes uncommitted changes)
GET_CHANGED_FILES = $$(git diff --name-only --diff-filter=ACMR $$(git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null | sed 's@^refs/remotes/origin/@@' || echo "main") | grep '\.py$$' | xargs -I {} sh -c 'test -f "{}" && echo "{}"' || true)

# Format changed files with black and isort
format:
	@CHANGED_FILES=$(GET_CHANGED_FILES); \
	if [ -z "$$CHANGED_FILES" ]; then \
		echo "No Python files changed"; \
	else \
		echo "Formatting changed files:"; \
		echo "$$CHANGED_FILES" | tr ' ' '\n' | sed 's/^/  - /'; \
		echo "Running isort..."; \
		uv run isort --line-length 110 --profile black $$CHANGED_FILES; \
		echo "Running black..."; \
		uv run black --line-length 110 $$CHANGED_FILES; \
		echo "✓ Code formatting complete"; \
	fi

# Format entire codebase
format-all:
	@echo "Running isort..."
	uv run isort --line-length 110 --profile black vector_app/ internal_apps/ tests/ *.py
	@echo "Running black..."
	uv run black --line-length 110 vector_app/ internal_apps/ tests/ *.py
	@echo "✓ Code formatting complete"

# Check if changed files are formatted
format-check:
	@CHANGED_FILES=$(GET_CHANGED_FILES); \
	if [ -z "$$CHANGED_FILES" ]; then \
		echo "No Python files changed"; \
	else \
		echo "Checking formatting of changed files:"; \
		echo "$$CHANGED_FILES" | tr ' ' '\n' | sed 's/^/  - /'; \
		uv run isort --line-length 110 --profile black --check-only --diff $$CHANGED_FILES; \
		uv run black --line-length 110 --check --diff $$CHANGED_FILES; \
		echo "✓ Code formatting check complete"; \
	fi

# Check if entire codebase is formatted
format-check-all:
	@echo "Checking code formatting..."
	uv run isort --line-length 110 --profile black --check-only --diff vector_app/ internal_apps/ tests/ *.py
	uv run black --line-length 110 --check --diff vector_app/ internal_apps/ tests/ *.py
	@echo "✓ Code formatting check complete"

# Run linting on changed files with prospector
lint:
	@CHANGED_FILES=$(GET_CHANGED_FILES); \
	if [ -z "$$CHANGED_FILES" ]; then \
		echo "No Python files changed"; \
	else \
		echo "Running prospector on changed files:"; \
		echo "$$CHANGED_FILES" | tr ' ' '\n' | sed 's/^/  - /'; \
		uv run prospector --profile prospector.yml $$CHANGED_FILES; \
		echo "✓ Linting complete"; \
	fi

# Run linting on entire codebase
lint-all:
	@echo "Running prospector..."
	uv run prospector --profile prospector.yml vector_app/ internal_apps/ tests/
	@echo "✓ Linting complete"

# Run type checking on changed files with mypy
static:
	@CHANGED_FILES=$(GET_CHANGED_FILES); \
	if [ -z "$$CHANGED_FILES" ]; then \
		echo "No Python files changed"; \
	else \
		echo "Running mypy on changed files:"; \
		echo "$$CHANGED_FILES" | tr ' ' '\n' | sed 's/^/  - /'; \
		echo "Note: mypy analyzes full codebase but shows only changed files"; \
		MYPY_OUTPUT=$$(uv run mypy --config-file mypy.ini 2>&1 || true); \
		FILTERED_OUTPUT=""; \
		for file in $$CHANGED_FILES; do \
			FILTERED_OUTPUT="$$FILTERED_OUTPUT$$(echo "$$MYPY_OUTPUT" | grep "^$$file:" || true)\n"; \
		done; \
		if [ -n "$$(echo "$$FILTERED_OUTPUT" | tr -d '[:space:]')" ]; then \
			echo "$$FILTERED_OUTPUT"; \
			exit 1; \
		else \
			echo "✓ Type checking complete"; \
		fi; \
	fi

# Run type checking on entire codebase
static-all:
	@echo "Running mypy type checking..."
	uv run mypy --config-file mypy.ini
	@echo "✓ Type checking complete"

# Run all checks on changed files only (default)
check:
	@./scripts/check_static_main_diff.sh

# Alias for check (changed files only)
check-diff: check

# Run all checks on entire codebase
check-all: format-check-all lint-all static-all
	@echo "✓ All static analysis checks passed!"

# Clean up cache directories
clean:
	@echo "Cleaning cache directories..."
	find . -type d -name "__pycache__" -not -path "./env/*" -not -path "./venv/*" -not -path "./.venv/*" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name ".mypy_cache" -not -path "./env/*" -not -path "./venv/*" -not -path "./.venv/*" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name ".pytest_cache" -not -path "./env/*" -not -path "./venv/*" -not -path "./.venv/*" -exec rm -rf {} + 2>/dev/null || true
	find . -type f -name "*.pyc" -not -path "./env/*" -not -path "./venv/*" -not -path "./.venv/*" -delete
	find . -type f -name "*.pyo" -not -path "./venv/*" -not -path "./env/*" -not -path "./.venv/*" -delete
	@echo "✓ Cache cleanup complete"
