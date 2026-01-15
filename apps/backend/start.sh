#!/usr/bin/env bash
# Start script for Render deployment
export PATH="$HOME/.local/bin:$PATH"
uv run gunicorn internal_apps.wsgi:application
