#!/bin/bash
# Post-attach command: runs each time VS Code attaches to the container

set -e

export PATH="$HOME/.local/bin:$PATH"

# Setup mise and install dependencies
mise trust
mise install
mise setup
mise migrate

# Set ports to public
if [ -n "$CODESPACE_NAME" ]; then
    echo "Setting ports to public for Codespaces..."
    gh codespace ports visibility 5176:public 8001:public 5555:public -c "$CODESPACE_NAME" || true
fi
