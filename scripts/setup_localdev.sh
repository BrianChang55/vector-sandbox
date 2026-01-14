#!/bin/bash

# =============================================================================
# Local Development Setup Script for Internal Apps
# =============================================================================
# This script sets up your local development environment from scratch.
# Run this from the backend repo directory.
#
# What it does:
#   1. Checks/installs prerequisites (Python, Node, Redis)
#   2. Creates workspace directory structure
#   3. Sets up Python virtual environment and dependencies
#   4. Installs frontend dependencies
#   5. Configures environment variables
#   6. Runs database migrations
#   7. Copies dev.sh to workspace root
#
# After running this script, you can start everything with:
#   cd <workspace>/internal-apps
#   ./dev.sh --start
# =============================================================================

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color
BOLD='\033[1m'

# Get the directory where this script lives
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BACKEND_DIR="$(dirname "$SCRIPT_DIR")"
BACKEND_NAME="internal-apps-backend"
FRONTEND_NAME="internal-apps-web-app"
WORKSPACE_NAME="internal-apps"

print_header() {
    echo ""
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${CYAN}  $1${NC}"
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

print_step() {
    echo -e "\n${BLUE}▶ $1${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

print_info() {
    echo -e "${CYAN}ℹ $1${NC}"
}

# Detect OS
detect_os() {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        echo "macos"
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        echo "linux"
    else
        echo "unknown"
    fi
}

OS=$(detect_os)

# =============================================================================
# Prerequisite Checks
# =============================================================================

check_homebrew() {
    if [[ "$OS" == "macos" ]]; then
        if ! command -v brew &> /dev/null; then
            print_warning "Homebrew not found. Installing..."
            /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
            
            # Add to PATH for this session
            if [[ -f /opt/homebrew/bin/brew ]]; then
                eval "$(/opt/homebrew/bin/brew shellenv)"
            elif [[ -f /usr/local/bin/brew ]]; then
                eval "$(/usr/local/bin/brew shellenv)"
            fi
            print_success "Homebrew installed"
        else
            print_success "Homebrew found"
        fi
    fi
}

check_python() {
    print_step "Checking Python 3..."
    
    if command -v python3 &> /dev/null; then
        PYTHON_VERSION=$(python3 --version 2>&1 | cut -d' ' -f2)
        PYTHON_MAJOR=$(echo "$PYTHON_VERSION" | cut -d'.' -f1)
        PYTHON_MINOR=$(echo "$PYTHON_VERSION" | cut -d'.' -f2)
        
        if [[ "$PYTHON_MAJOR" -ge 3 ]] && [[ "$PYTHON_MINOR" -ge 10 ]]; then
            print_success "Python $PYTHON_VERSION found"
            return 0
        else
            print_warning "Python $PYTHON_VERSION found, but 3.10+ recommended"
        fi
    fi
    
    # Try to install Python
    if [[ "$OS" == "macos" ]]; then
        print_info "Installing Python 3 via Homebrew..."
        brew install python@3.11
        print_success "Python installed"
    elif [[ "$OS" == "linux" ]]; then
        print_error "Python 3.10+ not found. Please install it:"
        echo "  Ubuntu/Debian: sudo apt install python3 python3-venv python3-pip"
        echo "  Fedora: sudo dnf install python3"
        exit 1
    else
        print_error "Python 3.10+ not found. Please install Python manually."
        exit 1
    fi
}

check_node() {
    print_step "Checking Node.js..."
    
    if command -v node &> /dev/null; then
        NODE_VERSION=$(node --version | cut -c2-)
        NODE_MAJOR=$(echo "$NODE_VERSION" | cut -d'.' -f1)
        
        if [[ "$NODE_MAJOR" -ge 18 ]]; then
            print_success "Node.js $NODE_VERSION found"
            return 0
        else
            print_warning "Node.js $NODE_VERSION found, but 18+ recommended"
        fi
    fi
    
    # Try to install Node
    if [[ "$OS" == "macos" ]]; then
        print_info "Installing Node.js via Homebrew..."
        brew install node
        print_success "Node.js installed"
    elif [[ "$OS" == "linux" ]]; then
        print_error "Node.js 18+ not found. Please install it:"
        echo "  Visit: https://nodejs.org/en/download/"
        echo "  Or use nvm: https://github.com/nvm-sh/nvm"
        exit 1
    else
        print_error "Node.js 18+ not found. Please install Node.js manually."
        exit 1
    fi
}

check_redis() {
    print_step "Checking Redis..."
    
    if command -v redis-server &> /dev/null; then
        print_success "Redis found"
    else
        if [[ "$OS" == "macos" ]]; then
            print_info "Installing Redis via Homebrew..."
            brew install redis
            print_success "Redis installed"
        elif [[ "$OS" == "linux" ]]; then
            print_error "Redis not found. Please install it:"
            echo "  Ubuntu/Debian: sudo apt install redis-server"
            echo "  Fedora: sudo dnf install redis"
            exit 1
        else
            print_error "Redis not found. Please install Redis manually."
            exit 1
        fi
    fi
}

# =============================================================================
# Workspace Setup
# =============================================================================

setup_workspace() {
    print_step "Setting up workspace directory structure..."
    
    # Determine where to create workspace
    # If we're already in the right structure, use it
    CURRENT_DIR=$(pwd)
    PARENT_DIR=$(dirname "$BACKEND_DIR")
    PARENT_NAME=$(basename "$PARENT_DIR")
    
    if [[ "$PARENT_NAME" == "$WORKSPACE_NAME" ]]; then
        # Already in correct structure
        WORKSPACE_DIR="$PARENT_DIR"
        print_success "Workspace already set up at $WORKSPACE_DIR"
    else
        # Need to create workspace
        WORKSPACE_DIR="$PARENT_DIR/$WORKSPACE_NAME"
        
        if [[ -d "$WORKSPACE_DIR" ]]; then
            print_success "Workspace directory exists at $WORKSPACE_DIR"
        else
            print_info "Creating workspace at $WORKSPACE_DIR"
            mkdir -p "$WORKSPACE_DIR"
        fi
        
        # Move backend into workspace if not already there
        if [[ ! -d "$WORKSPACE_DIR/$BACKEND_NAME" ]]; then
            print_info "Moving backend repo into workspace..."
            mv "$BACKEND_DIR" "$WORKSPACE_DIR/$BACKEND_NAME"
            BACKEND_DIR="$WORKSPACE_DIR/$BACKEND_NAME"
            print_success "Backend moved to $BACKEND_DIR"
        fi
    fi
    
    # Check for frontend
    FRONTEND_DIR="$WORKSPACE_DIR/$FRONTEND_NAME"
    if [[ ! -d "$FRONTEND_DIR" ]]; then
        echo ""
        print_warning "Frontend repo not found at $FRONTEND_DIR"
        echo ""
        echo -e "${BOLD}Please clone the frontend repository:${NC}"
        echo ""
        echo -e "  ${CYAN}cd $WORKSPACE_DIR${NC}"
        echo -e "  ${CYAN}git clone https://github.com/macruntime/internal-apps-web-app.git $FRONTEND_NAME${NC}"
        echo ""
        echo "Then run this script again."
        exit 1
    else
        print_success "Frontend found at $FRONTEND_DIR"
    fi
}

# =============================================================================
# Python Environment Setup
# =============================================================================

setup_python_env() {
    print_step "Setting up Python virtual environment..."
    
    cd "$BACKEND_DIR"
    
    if [[ ! -d "venv" ]]; then
        print_info "Creating virtual environment..."
        python3 -m venv venv
    fi
    
    print_info "Activating virtual environment..."
    source venv/bin/activate
    
    print_info "Upgrading pip..."
    pip install --upgrade pip -q
    
    print_info "Installing Python dependencies..."
    pip install -r requirements.txt -q
    
    # Install watchdog for Celery auto-reload
    pip install watchdog -q
    
    print_success "Python environment ready"
}

# =============================================================================
# Node Dependencies Setup
# =============================================================================

setup_node_deps() {
    print_step "Installing frontend dependencies..."
    
    cd "$FRONTEND_DIR"
    
    if [[ -f "package-lock.json" ]]; then
        npm ci --silent
    else
        npm install --silent
    fi
    
    print_success "Frontend dependencies installed"
}

# =============================================================================
# Environment Configuration
# =============================================================================

setup_env_file() {
    print_step "Setting up environment configuration..."
    
    ENV_FILE="$BACKEND_DIR/.env"
    ENV_EXAMPLE="$BACKEND_DIR/.env.example"
    
    if [[ -f "$ENV_FILE" ]]; then
        print_success ".env file already exists"
        return 0
    fi
    
    echo ""
    print_warning "No .env file found. Creating from template..."
    
    if [[ -f "$ENV_EXAMPLE" ]]; then
        cp "$ENV_EXAMPLE" "$ENV_FILE"
        print_success "Created .env from .env.example"
    else
        print_error ".env.example not found. Please create .env manually."
        return 1
    fi
    
    echo ""
    echo -e "${BOLD}Please edit .env and add your API keys before running the app.${NC}"
    echo "  $ENV_FILE"
    echo ""
}

# =============================================================================
# Database Setup
# =============================================================================

setup_database() {
    print_step "Running database migrations..."
    
    cd "$BACKEND_DIR"
    source venv/bin/activate
    
    python manage.py migrate --no-input
    
    print_success "Database migrations complete"
}

# =============================================================================
# Dev Script Setup
# =============================================================================

setup_dev_script() {
    print_step "Setting up dev.sh script..."
    
    DEV_SCRIPT_SRC="$BACKEND_DIR/scripts/dev.sh"
    DEV_SCRIPT_DST="$WORKSPACE_DIR/dev.sh"
    
    # Check if dev.sh exists in scripts folder, if not check root
    if [[ ! -f "$DEV_SCRIPT_SRC" ]]; then
        if [[ -f "$WORKSPACE_DIR/dev.sh" ]]; then
            print_success "dev.sh already exists in workspace"
            return 0
        else
            print_warning "dev.sh not found, skipping..."
            return 0
        fi
    fi
    
    cp "$DEV_SCRIPT_SRC" "$DEV_SCRIPT_DST"
    chmod +x "$DEV_SCRIPT_DST"
    
    print_success "dev.sh copied to workspace root"
}

# =============================================================================
# Final Instructions
# =============================================================================

print_final_instructions() {
    print_header "Setup Complete! 🎉"
    
    echo ""
    echo -e "${BOLD}Your workspace is ready at:${NC}"
    echo -e "  ${CYAN}$WORKSPACE_DIR${NC}"
    echo ""
    echo -e "${BOLD}Directory structure:${NC}"
    echo "  $WORKSPACE_NAME/"
    echo "  ├── dev.sh              # Start everything with one command"
    echo "  ├── logs/               # Log files (created on first run)"
    echo "  ├── $BACKEND_NAME/"
    echo "  └── $FRONTEND_NAME/"
    echo ""
    
    if [[ ! -f "$BACKEND_DIR/.env" ]] || grep -q "your-openrouter-api-key" "$BACKEND_DIR/.env" 2>/dev/null; then
        echo -e "${YELLOW}${BOLD}⚠ Before running, configure your API keys:${NC}"
        echo -e "  ${CYAN}Edit: $BACKEND_DIR/.env${NC}"
        echo ""
    fi
    
    echo -e "${BOLD}To start development:${NC}"
    echo ""
    echo -e "  ${CYAN}cd $WORKSPACE_DIR${NC}"
    echo -e "  ${CYAN}./dev.sh --start${NC}"
    echo ""
    echo "This will:"
    echo "  • Start Django backend (http://localhost:8001)"
    echo "  • Start Celery worker"
    echo "  • Start React frontend (http://localhost:5176)"
    echo "  • Auto-login and open Chrome"
    echo ""
    echo -e "${BOLD}Other commands:${NC}"
    echo "  ./dev.sh --stop      Stop all services"
    echo "  ./dev.sh --restart   Restart all services"
    echo "  ./dev.sh --logs      View live logs"
    echo ""
}

# =============================================================================
# Main
# =============================================================================

main() {
    print_header "Internal Apps - Local Development Setup"
    
    echo ""
    echo "This script will set up your local development environment."
    echo ""
    
    # Prerequisites
    if [[ "$OS" == "macos" ]]; then
        check_homebrew
    fi
    check_python
    check_node
    check_redis
    
    # Setup
    setup_workspace
    setup_python_env
    setup_node_deps
    setup_env_file
    setup_database
    setup_dev_script
    
    # Done
    print_final_instructions
}

main "$@"
