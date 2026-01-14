# Internal Apps Backend

Django + Celery backend for the Internal Apps platform.

## Prerequisites

Before getting started, you'll need to obtain the `.env` secrets file. **Ask someone on the team** for the `.env` file or the required environment variables.

## Quick Start

### One-Command Setup

Run the setup script to configure everything automatically:

```bash
bash scripts/setup_localdev.sh
```

This will:
- ✅ Check/install prerequisites (Python 3, Node.js, Redis)
- ✅ Create the workspace directory structure
- ✅ Set up Python virtual environment
- ✅ Install all dependencies (backend + frontend)
- ✅ Run database migrations
- ✅ Create a `.env` template for your API keys

### After Setup

1. **Configure your API keys** (if not already done):
   ```bash
   # Edit the .env file and add your keys
   nano .env
   ```

   Required keys:
   - `OPENROUTER_API_KEY` - For AI features
   - `MERGE_TOOL_PACK_ID` and `MERGE_ACCESS_KEY` - For connectors

2. **Start development**:
   ```bash
   cd ..  # Go to workspace root (internal-apps/)
   ./dev.sh --start
   ```

That's it! The app will:
- Start all services (Django, Celery, React frontend)
- Auto-create a dev user and open Chrome logged in
- Auto-reload on file changes

---

## Development Commands

From the workspace root (`internal-apps/`):

```bash
./dev.sh --start    # Start all services + auto-login
./dev.sh --stop     # Stop all services
./dev.sh --restart  # Restart all services
./dev.sh --logs     # View live logs (streaming)
./dev.sh --help     # Show help
```

### Viewing Logs

```bash
./dev.sh --logs              # All logs (live streaming)
tail -f logs/django.log      # Django only
tail -f logs/celery.log      # Celery only
tail -f logs/frontend.log    # Frontend only
```

> **Note:** Opening log files in your editor won't auto-refresh. Use `./dev.sh --logs` or `tail -f` in a terminal for live updates.

---

## Workspace Structure

After setup, your workspace should look like:

```
internal-apps/                 # Workspace root
├── dev.sh                     # Start everything with one command
├── logs/                      # Log files
├── internal-apps-backend/     # This repo (Django + Celery)
│   ├── .env                   # Your API keys (not committed)
│   ├── venv/                  # Python virtual environment
│   └── ...
└── internal-apps-web-app/     # Frontend repo (React)
    └── ...
```

---

## Manual Setup (Alternative)

If you prefer to set things up manually:

### Prerequisites

- Python 3.10+
- Node.js 18+
- Redis

### Steps

```bash
# 1. Create workspace
mkdir internal-apps && cd internal-apps

# 2. Clone repos
git clone <backend-repo-url> internal-apps-backend
git clone <frontend-repo-url> internal-apps-web-app

# 3. Backend setup
cd internal-apps-backend
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt

# 4. Create .env with your API keys
cp .env.example .env  # or create manually

# 5. Run migrations
python manage.py migrate

# 6. Frontend setup
cd ../internal-apps-web-app
npm install

# 7. Copy dev.sh to workspace root
cd ..
cp internal-apps-backend/scripts/dev.sh .
chmod +x dev.sh

# 8. Start Redis
brew services start redis  # macOS
# or: sudo systemctl start redis  # Linux

# 9. Start everything
./dev.sh --start
```

---

## Running Services Individually

If you need to run services separately:

### Django

```bash
cd internal-apps-backend
source venv/bin/activate
python manage.py runserver 8001
```

### Celery

```bash
cd internal-apps-backend
source venv/bin/activate
celery -A internal_apps worker --loglevel=info
```

### Frontend

```bash
cd internal-apps-web-app
npm run dev
```
