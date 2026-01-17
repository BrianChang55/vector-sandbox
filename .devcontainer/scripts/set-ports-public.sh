#!/bin/bash
# Set Codespace ports to public visibility

if [ -n "$CODESPACE_NAME" ]; then
    echo "Setting ports to public for Codespaces..."
    gh codespace ports visibility 5176:public 8001:public 5555:public -c "$CODESPACE_NAME" || true
fi
