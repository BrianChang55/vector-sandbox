# Keycloak Auth Proxy on Render (Free Tier)

This guide sets up Keycloak as an OAuth proxy, allowing wildcard redirect URIs for dynamic environments like GitHub Codespaces.

## Why?

Google OAuth doesn't support wildcard redirect URIs. Keycloak does:
- Configure Google OAuth once → pointing to Keycloak
- Keycloak accepts wildcards like `https://*-8001.app.github.dev/*`
- Your dynamic codespaces/preview environments just work

## 1. Deploy Keycloak to Render

### Create a PostgreSQL Database

1. Go to [render.com](https://render.com) and sign up/login
2. Click **New** → **PostgreSQL**
3. Configure:
   - Name: `keycloak-db`
   - Database: `keycloak`
   - User: `keycloak`
   - Region: Choose nearest
   - Plan: **Free**
4. Click **Create Database**
5. Copy the **Internal Database URL** for later

### Deploy Keycloak Service

1. Click **New** → **Web Service**
2. Select **Deploy an existing image from a registry**
3. Image URL: `quay.io/keycloak/keycloak:latest`
4. Configure:
   - Name: `keycloak`
   - Region: Same as database
   - Plan: **Free**
5. Add **Environment Variables**:
   ```
   KC_DB=postgres
   KC_DB_URL=<Internal Database URL from above>
   KC_HOSTNAME_STRICT=false
   KC_PROXY=edge
   KC_HTTP_ENABLED=true
   KEYCLOAK_ADMIN=admin
   KEYCLOAK_ADMIN_PASSWORD=<choose-a-strong-password>
   ```
6. Set **Docker Command**:
   ```
   start --optimized --hostname-strict=false --proxy=edge --http-enabled=true
   ```

   If that fails, try:
   ```
   start-dev
   ```
7. Click **Create Web Service**
8. Wait for deployment (takes a few minutes)
9. Note your Keycloak URL: `https://keycloak-xxxx.onrender.com`

## 2. Configure Keycloak

### Access Admin Console

1. Go to `https://keycloak-xxxx.onrender.com/admin`
2. Login with `admin` / `<your-password>`

### Create a Realm

1. Click dropdown next to "Keycloak" (top-left) → **Create realm**
2. Name: `vector`
3. Click **Create**

### Configure Google as Identity Provider

1. Go to **Identity Providers** → **Add provider** → **Google**
2. Keep this page open - you'll need the **Redirect URI** shown

### Set up Google OAuth (in Google Cloud Console)

1. Go to [Google Cloud Console](https://console.cloud.google.com)
2. Navigate to **APIs & Services** → **Credentials**
3. Click **Create Credentials** → **OAuth client ID**
4. Application type: **Web application**
5. Name: `Keycloak`
6. Authorized redirect URIs: Add the Redirect URI from Keycloak (looks like):
   ```
   https://keycloak-xxxx.onrender.com/realms/vector/broker/google/endpoint
   ```
7. Click **Create**
8. Copy **Client ID** and **Client Secret**

### Finish Google IdP Setup in Keycloak

1. Back in Keycloak, paste:
   - Client ID
   - Client Secret
2. Click **Save**

### Create a Client for Your App

1. Go to **Clients** → **Create client**
2. Client type: **OpenID Connect**
3. Client ID: `vector-app`
4. Click **Next**
5. Client authentication: **On**
6. Click **Next**
7. Configure URIs:
   - Root URL: (leave empty)
   - Valid redirect URIs:
     ```
     https://*-8001.app.github.dev/*
     https://*-5176.app.github.dev/*
     http://localhost:8001/*
     http://localhost:5176/*
     ```
   - Web origins: `+` (allows all origins from redirect URIs)
8. Click **Save**
9. Go to **Credentials** tab → Copy the **Client secret**

## 3. Configure Your App

Update your backend `.env`:

```bash
# Keycloak OIDC Configuration
OAUTH_PROVIDER=keycloak
KEYCLOAK_URL=https://keycloak-xxxx.onrender.com
KEYCLOAK_REALM=vector
KEYCLOAK_CLIENT_ID=vector-app
KEYCLOAK_CLIENT_SECRET=<client-secret-from-keycloak>

# Dynamic redirect URI (set at runtime)
# In Codespaces, this is auto-generated from CODESPACE_NAME
OAUTH_REDIRECT_URI=https://${CODESPACE_NAME}-8001.app.github.dev/api/auth/callback/
```

### Backend Integration Example (Django)

```python
# settings.py or auth config
import os

KEYCLOAK_CONFIG = {
    "url": os.environ.get("KEYCLOAK_URL"),
    "realm": os.environ.get("KEYCLOAK_REALM", "vector"),
    "client_id": os.environ.get("KEYCLOAK_CLIENT_ID"),
    "client_secret": os.environ.get("KEYCLOAK_CLIENT_SECRET"),
}

# OIDC endpoints are at:
# Authorization: {url}/realms/{realm}/protocol/openid-connect/auth
# Token: {url}/realms/{realm}/protocol/openid-connect/token
# Userinfo: {url}/realms/{realm}/protocol/openid-connect/userinfo
```

## 4. Test the Flow

1. Start your app in Codespaces
2. Navigate to your login page
3. Click "Login with Google"
4. You should be redirected to Keycloak → Google → back to your Codespace

## Troubleshooting

### "Invalid redirect URI" from Keycloak
- Check the wildcard patterns in your Keycloak client settings
- Ensure the port number matches (8001 vs 5176)

### Keycloak is slow/sleeping on Render free tier
- Free tier services sleep after 15 min of inactivity
- First request may take 30-60 seconds to wake up
- Consider upgrading to paid tier ($7/mo) for always-on

### Database connection errors
- Make sure you're using the **Internal** Database URL, not External
- Check that the database is in the same region as the web service

## Security Notes

- Change the admin password after initial setup
- In production, use proper secrets management
- Enable HTTPS only (Render handles this automatically)
- Regularly update Keycloak to the latest version
