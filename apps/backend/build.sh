#!/usr/bin/env bash
# Build script for Render deployment

set -o errexit  # Exit on error

# Install system dependencies for cairosvg (SVG to PNG conversion)
apt-get update && apt-get install -y \
    libcairo2-dev \
    libpango1.0-dev \
    libgdk-pixbuf2.0-dev \
    libffi-dev \
    shared-mime-info

# Install uv if not present
if ! command -v uv &> /dev/null; then
    pip install uv
fi

# Install Python dependencies with uv
uv sync --frozen

# Collect static files
uv run python manage.py collectstatic --no-input

# Run database migrations
uv run python manage.py migrate
