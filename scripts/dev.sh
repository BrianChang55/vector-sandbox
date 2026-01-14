#!/bin/bash

# Development script for internal-apps
# Manages backend (Django + Celery) and frontend (React)

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
LOG_DIR="$SCRIPT_DIR/logs"
FRONTEND_URL="http://localhost:5176"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

# Open or refresh browser tab with auto-login
open_dev_browser() {
    echo -e "${BLUE}Setting up dev session...${NC}"
    
    # Wait for services to be ready
    echo -e "${YELLOW}Waiting for services to start...${NC}"
    local max_attempts=30
    local attempt=0
    
    while ! curl -s "http://localhost:8001/api/v1/public/integrations/" > /dev/null 2>&1; do
        attempt=$((attempt + 1))
        if [ $attempt -ge $max_attempts ]; then
            echo -e "${RED}Backend failed to start in time${NC}"
            return 1
        fi
        sleep 1
    done
    
    while ! curl -s "$FRONTEND_URL" > /dev/null 2>&1; do
        attempt=$((attempt + 1))
        if [ $attempt -ge $max_attempts ]; then
            echo -e "${RED}Frontend failed to start in time${NC}"
            return 1
        fi
        sleep 1
    done
    
    # Generate dev tokens
    echo -e "${GREEN}Generating dev tokens...${NC}"
    local tokens=$(python "$SCRIPT_DIR/internal-apps-backend/scripts/dev_login.py" 2>/dev/null)
    
    if [ -z "$tokens" ]; then
        echo -e "${RED}Failed to generate dev tokens${NC}"
        return 1
    fi
    
    local access_token=$(echo "$tokens" | python3 -c "import sys, json; print(json.load(sys.stdin)['access'])")
    local refresh_token=$(echo "$tokens" | python3 -c "import sys, json; print(json.load(sys.stdin)['refresh'])")
    
    # Build the dev auth URL
    local dev_url="$FRONTEND_URL/dev-auth?access=$access_token&refresh=$refresh_token"
    
    # Try to refresh existing Chrome tab, or open new one
    osascript <<EOF 2>/dev/null || open "$dev_url"
tell application "Google Chrome"
    set found to false
    repeat with w in windows
        repeat with t in tabs of w
            if URL of t starts with "$FRONTEND_URL" then
                set URL of t to "$dev_url"
                set active tab index of w to (index of t)
                set index of w to 1
                activate
                set found to true
                exit repeat
            end if
        end repeat
        if found then exit repeat
    end repeat
    
    if not found then
        open location "$dev_url"
        activate
    end if
end tell
EOF
    
    echo -e "${GREEN}Dev browser opened!${NC}"
}

stop_services() {
    echo -e "${YELLOW}Stopping services...${NC}"
    pkill -f "python manage.py runserver 8001" 2>/dev/null && echo "Stopped Django" || true
    pkill -f "celery -A internal_apps worker" 2>/dev/null && echo "Stopped Celery" || true
    pkill -f "watchmedo auto-restart" 2>/dev/null && echo "Stopped Celery watcher" || true
    pkill -f "vite.*5176" 2>/dev/null && echo "Stopped Frontend" || true
    
    # Kill any processes on our ports
    lsof -ti:8001 | xargs kill -9 2>/dev/null && echo "Killed process on port 8001" || true
    lsof -ti:5176 | xargs kill -9 2>/dev/null && echo "Killed process on port 5176" || true
    
    echo -e "${RED}All services stopped.${NC}"
}

start_services() {
    # Stop any existing services first
    echo -e "${YELLOW}Cleaning up any existing processes...${NC}"
    stop_services 2>/dev/null || true
    sleep 1

    # Create logs directory
    mkdir -p "$LOG_DIR"
    
    # Clear old logs
    > "$LOG_DIR/django.log"
    > "$LOG_DIR/celery.log"
    > "$LOG_DIR/frontend.log"

    echo -e "${GREEN}Starting Internal Apps...${NC}"

    # Check Redis
    if ! pgrep -x "redis-server" > /dev/null; then
        echo -e "${YELLOW}Starting Redis...${NC}"
        brew services start redis
        sleep 2
    fi

    # Activate venv once
    source "$SCRIPT_DIR/internal-apps-backend/venv/bin/activate"

    # Ensure watchdog is installed
    if ! python -c "import watchdog" 2>/dev/null; then
        echo -e "${YELLOW}Installing watchdog...${NC}"
        pip install watchdog -q
    fi

    BACKEND_DIR="$SCRIPT_DIR/internal-apps-backend"
    FRONTEND_DIR="$SCRIPT_DIR/internal-apps-web-app"

    # Run migrations
    echo -e "${GREEN}Running migrations...${NC}"
    python "$BACKEND_DIR/manage.py" migrate

    # Start Django
    echo -e "${GREEN}Starting Django (port 8001)...${NC}"
    python "$BACKEND_DIR/manage.py" runserver 8001 > "$LOG_DIR/django.log" 2>&1 &

    # Start Celery with auto-reload (watchmedo watches backend dir for .py changes)
    echo -e "${GREEN}Starting Celery (auto-reload enabled)...${NC}"
    watchmedo auto-restart \
        --directory="$BACKEND_DIR" \
        --pattern='*.py' \
        --recursive \
        -- celery -A internal_apps --workdir="$BACKEND_DIR" worker --loglevel=info \
        > "$LOG_DIR/celery.log" 2>&1 &

    # Start Frontend
    echo -e "${GREEN}Starting Frontend (port 5176)...${NC}"
    npm run dev --prefix "$FRONTEND_DIR" > "$LOG_DIR/frontend.log" 2>&1 &

    echo ""
    echo -e "${GREEN}All services started!${NC}"
    echo -e "Backend:  http://localhost:8001"
    echo -e "Frontend: http://localhost:5176"
    echo ""
    echo -e "${BLUE}Logs:${NC}"
    echo "  Django:   tail -f $LOG_DIR/django.log"
    echo "  Celery:   tail -f $LOG_DIR/celery.log"
    echo "  Frontend: tail -f $LOG_DIR/frontend.log"
    echo "  All:      tail -f $LOG_DIR/*.log"
    echo ""
    
    # Auto-login and open browser in background
    open_dev_browser &
    
    echo -e "Press Ctrl+C to stop all services"

    # Wait and handle Ctrl+C
    trap 'stop_services; exit 0' INT TERM
    while true; do sleep 1; done
}

show_help() {
    echo "Usage: ./dev.sh [COMMAND]"
    echo ""
    echo "Commands:"
    echo "  --start     Start all services and auto-login (default)"
    echo "  --stop      Stop all services"
    echo "  --restart   Restart all services"
    echo "  --logs      Tail all log files"
    echo "  --help      Show this help"
    echo ""
    echo "Auto-login: Creates a dev user (dev@localhost) and opens Chrome with a"
    echo "logged-in session. Reuses existing tab if one is already open."
    echo ""
    echo "Log files are stored in: $LOG_DIR/"
}

case "${1:---start}" in
    --start)   start_services ;;
    --stop)    stop_services ;;
    --restart) stop_services; sleep 2; start_services ;;
    --logs)    tail -f "$LOG_DIR"/*.log ;;
    --help)    show_help ;;
    *)         echo -e "${RED}Invalid command. Use --help for usage.${NC}"; exit 1 ;;
esac
