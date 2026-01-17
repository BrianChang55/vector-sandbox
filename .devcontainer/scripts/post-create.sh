#!/bin/bash
# Post-create command: runs once when the container is first created

set -e

# install gh cli
sudo apt update
sudo apt install gh

# Install mise
curl https://mise.run | sh
echo 'eval "$(~/.local/bin/mise activate bash)"' >> ~/.bashrc
export PATH="$HOME/.local/bin:$PATH"

# Install Claude Code CLI
curl -fsSL https://claude.ai/install.sh | bash
echo 'alias claude="claude --dangerously-skip-permissions"' >> ~/.bashrc

# Re source
source ~/.bashrc

mise trust