.PHONY: help format lint static check install-static clean

# Default target
help:
	@echo "Static Analysis Targets:"
	@echo "  make install-static  - Install static analysis dependencies"
	@echo "  make format         - Format code with black and isort"
	@echo "  make lint           - Run prospector linting"
	@echo "  make static         - Run mypy type checking"
	@echo "  make check          - Run all checks (format check + lint + static)"
	@echo "  make clean          - Remove cache directories"
	@echo ""
	@echo "Example workflow:"
	@echo "  1. make install-static  (one-time setup)"
	@echo "  2. make format          (format your code)"
	@echo "  3. make check           (verify everything passes)"

# Install static analysis dependencies
install-static:
	pip install -r requirements/static.txt

# Format code with black and isort
format:
	@echo "Running isort..."
	isort --line-length 110 --profile black vector_app/ internal_apps/ tests/ *.py
	@echo "Running black..."
	black --line-length 110 vector_app/ internal_apps/ tests/ *.py
	@echo "✓ Code formatting complete"

# Check if code is formatted (CI-friendly, doesn't modify files)
format-check:
	@echo "Checking code formatting..."
	isort --line-length 110 --profile black --check-only --diff vector_app/ internal_apps/ tests/ *.py
	black --line-length 110 --check --diff vector_app/ internal_apps/ tests/ *.py
	@echo "✓ Code formatting check complete"

# Run linting with prospector
lint:
	@echo "Running prospector..."
	prospector --profile prospector.yml vector_app/ internal_apps/ tests/
	@echo "✓ Linting complete"

# Run type checking with mypy
static:
	@echo "Running mypy type checking..."
	mypy --config-file mypy.ini
	@echo "✓ Type checking complete"

# Run all checks
check: format-check lint static
	@echo "✓ All static analysis checks passed!"

# Clean up cache directories
clean:
	@echo "Cleaning cache directories..."
	find . -type d -name "__pycache__" -not -path "./env/*" -not -path "./venv/*" -not -path "./.venv/*" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name ".mypy_cache" -not -path "./env/*" -not -path "./venv/*" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name ".pytest_cache" -not -path "./env/*" -not -path "./venv/*" -exec rm -rf {} + 2>/dev/null || true
	find . -type f -name "*.pyc" -not -path "./env/*" -not -path "./venv/*" -delete
	find . -type f -name "*.pyo" -not -path "./venv/*" -not -path "./env/*" -delete
	@echo "✓ Cache cleanup complete"
