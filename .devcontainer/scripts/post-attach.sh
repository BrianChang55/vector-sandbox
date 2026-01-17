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
.devcontainer/scripts/set-ports-public.sh
