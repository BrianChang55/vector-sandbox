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

# Install uv via official installer
curl -LsSf https://astral.sh/uv/install.sh | sh
export PATH="$HOME/.local/bin:$PATH"

# Install Python dependencies with uv
uv sync --frozen

# Collect static files
uv run python manage.py collectstatic --no-input

# Run database migrations
uv run python manage.py migrate
