## Startup Instructions (BE)

### Main App

Ask for API keys (or env file)

```
# Backend
cd internal-apps-backend
python -m venv venv
source venv/bin/activate
pip install -r requirements.txt

# Create .env with:
# OPENROUTER_API_KEY=your-key
# MERGE_TOOL_PACK_ID=your-tool-pack-id
# MERGE_ACCESS_KEY=your-access-key

python manage.py migrate
python manage.py runserver 8001  # Uses port 8001
```

### Celery

```
brew install redis
brew services start redis
celery -A internal_apps worker --loglevel=debug
```