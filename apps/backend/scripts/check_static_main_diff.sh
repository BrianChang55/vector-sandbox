#!/usr/bin/env bash
# check_static_main_diff.sh - Run static analysis only on files changed from main

set -e  # Exit on error

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "========================================"
echo "Running Static Analysis on Changed Files"
echo "========================================"
echo ""

# Set Django settings module
export DJANGO_SETTINGS_MODULE=internal_apps.settings

# Get the main branch name (could be 'main' or 'master')
MAIN_BRANCH=$(git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null | sed 's@^refs/remotes/origin/@@' || echo "main")

echo -e "${BLUE}Comparing against branch: $MAIN_BRANCH${NC}"
echo ""

# Get list of changed Python files
CHANGED_FILES=$(git diff --name-only --diff-filter=ACMR "$MAIN_BRANCH"...HEAD | grep '\.py$' || true)

# Filter out files that don't exist (deleted files)
EXISTING_FILES=""
for file in $CHANGED_FILES; do
    if [ -f "$file" ]; then
        EXISTING_FILES="$EXISTING_FILES $file"
    fi
done

# Remove leading/trailing whitespace
EXISTING_FILES=$(echo $EXISTING_FILES | xargs)

if [ -z "$EXISTING_FILES" ]; then
    echo -e "${YELLOW}No Python files changed compared to $MAIN_BRANCH${NC}"
    echo "========================================"
    exit 0
fi

echo -e "${BLUE}Changed Python files:${NC}"
for file in $EXISTING_FILES; do
    echo "  - $file"
done
echo ""

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

# 1. Check code formatting (isort) on changed files
run_check "isort (import sorting)" \
    "isort --line-length 110 --profile black --check-only --diff $EXISTING_FILES"

# 2. Check code formatting (black) on changed files
run_check "black (code formatting)" \
    "black --line-length 110 --check --diff $EXISTING_FILES"

# 3. Run prospector on changed files
run_check "prospector (linting)" \
    "prospector --profile prospector.yml $EXISTING_FILES"

# 4. Run mypy on changed files
# Note: mypy needs to analyze the entire codebase for accurate type checking
# but we can filter output to only show issues in changed files
echo "----------------------------------------"
echo "Running: mypy (type checking)"
echo "----------------------------------------"
echo -e "${YELLOW}Note: mypy analyzes full codebase but shows only changed files${NC}"

# Run mypy and filter output to only show errors in changed files
MYPY_OUTPUT=$(mypy --config-file mypy.ini 2>&1 || true)

# Filter output to only include changed files
FILTERED_OUTPUT=""
for file in $EXISTING_FILES; do
    FILTERED_OUTPUT+=$(echo "$MYPY_OUTPUT" | grep "^$file:" || true)
    FILTERED_OUTPUT+=$'\n'
done

if [ -n "$(echo "$FILTERED_OUTPUT" | tr -d '[:space:]')" ]; then
    echo "$FILTERED_OUTPUT"
    echo -e "${RED}✗ mypy (type checking) failed${NC}"
    FAILED=1
else
    echo -e "${GREEN}✓ mypy (type checking) passed${NC}"
fi
echo ""

# Summary
echo "========================================"
if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All static analysis checks passed on changed files!${NC}"
    echo "========================================"
    exit 0
else
    echo -e "${RED}Some static analysis checks failed on changed files!${NC}"
    echo "========================================"
    exit 1
fi
