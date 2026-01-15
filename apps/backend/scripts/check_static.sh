#!/usr/bin/env bash
# check_static.sh - Run all static analysis checks on the codebase

set -e  # Exit on error

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "========================================"
echo "Running Static Analysis Checks"
echo "========================================"
echo ""

# Set Django settings module
export DJANGO_SETTINGS_MODULE=internal_apps.settings

# Track overall status
FAILED=0

# Function to run a check and track status
run_check() {
    local name=$1
    local command=$2

    echo "----------------------------------------"
    echo "Running: $name"
    echo "----------------------------------------"

    if eval "$command"; then
        echo -e "${GREEN}✓ $name passed${NC}"
    else
        echo -e "${RED}✗ $name failed${NC}"
        FAILED=1
    fi
    echo ""
}

# 1. Check code formatting (isort)
run_check "isort (import sorting)" \
    "isort --line-length 110 --profile black --check-only --diff vector_app/ internal_apps/ tests/ *.py"

# 2. Check code formatting (black)
run_check "black (code formatting)" \
    "black --line-length 110 --check --diff vector_app/ internal_apps/ tests/ *.py"

# 3. Run prospector linting
run_check "prospector (linting)" \
    "prospector --profile prospector.yml vector_app/ internal_apps/ tests/"

# 4. Run mypy type checking
run_check "mypy (type checking)" \
    "mypy --config-file mypy.ini"

# Summary
echo "========================================"
if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All static analysis checks passed!${NC}"
    echo "========================================"
    exit 0
else
    echo -e "${RED}Some static analysis checks failed!${NC}"
    echo "========================================"
    exit 1
fi
