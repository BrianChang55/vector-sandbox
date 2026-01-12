"""
Authentication views for user registration, login, and token management.
"""

import logging
import secrets
import hashlib
import hmac
import requests
from django.db import transaction
from django.contrib.auth import get_user_model
from django.core.mail import send_mail
from django.conf import settings
from django.template.loader import render_to_string
from django.utils.http import urlsafe_base64_encode, urlsafe_base64_decode
from django.utils.encoding import force_bytes, force_str
from rest_framework import status
from rest_framework.permissions import AllowAny, IsAuthenticated
from rest_framework.response import Response
from rest_framework.views import APIView
from rest_framework_simplejwt.tokens import RefreshToken
from urllib.parse import urlencode

from ..models import User, UserOrganization, Organization, MagicLinkToken
from ..serializers.auth_serializers import (
    UserSerializer,
    SignUpSerializer,
    LoginSerializer,
    MagicLinkRequestSerializer,
    MagicLinkVerifySerializer,
)

logger = logging.getLogger(__name__)
User = get_user_model()


def get_tokens_for_user(user):
    """
    Generate JWT tokens for a user.
    """
    refresh = RefreshToken.for_user(user)
    return {
        "refresh": str(refresh),
        "access": str(refresh.access_token),
    }


class SignUpView(APIView):
    """
    POST /auth/signup
    Register a new user with email and password.
    Creates a "Default" organization for the user.
    """

    permission_classes = [AllowAny]

    def post(self, request):
        try:
            serializer = SignUpSerializer(data=request.data)
            if not serializer.is_valid():
                return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)

            with transaction.atomic():
                user = serializer.save()

                # Create "Default" organization for this user
                # Generate unique slug based on user email
                base_slug = user.email.split("@")[0].lower().replace(".", "-").replace("_", "-")
                slug = base_slug
                counter = 1

                # Ensure slug is unique
                while Organization.objects.filter(slug=slug).exists():
                    slug = f"{base_slug}-{counter}"
                    counter += 1

                default_org = Organization.objects.create(name="Default", slug=slug)

                # Add user as admin member of Default organization
                UserOrganization.objects.create(
                    user=user, organization=default_org, role=UserOrganization.ROLE_ADMIN
                )

            # Generate JWT tokens
            tokens = get_tokens_for_user(user)

            # Serialize user
            user_serializer = UserSerializer(user)

            logger.info(f"User signed up: {user.email}")

            return Response(
                {
                    "user": user_serializer.data,
                    "tokens": tokens,
                    "message": "User created successfully",
                },
                status=status.HTTP_201_CREATED,
            )

        except Exception as e:
            logger.error(f"Error in SignUpView: {str(e)}")
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)


class LoginView(APIView):
    """
    POST /auth/login
    Authenticate user with email and password.
    Returns JWT access and refresh tokens.
    """

    permission_classes = [AllowAny]

    def post(self, request):
        try:
            serializer = LoginSerializer(data=request.data, context={"request": request})
            if not serializer.is_valid():
                return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)

            user = serializer.validated_data["user"]

            # Generate JWT tokens
            tokens = get_tokens_for_user(user)

            # Serialize user
            user_serializer = UserSerializer(user)

            logger.info(f"User logged in: {user.email}")

            return Response(
                {
                    "user": user_serializer.data,
                    "tokens": tokens,
                },
                status=status.HTTP_200_OK,
            )

        except Exception as e:
            logger.error(f"Error in LoginView: {str(e)}")
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)


class RefreshTokenView(APIView):
    """
    POST /auth/refresh
    Refresh access token using refresh token.
    """

    permission_classes = [AllowAny]

    def post(self, request):
        try:
            refresh_token = request.data.get("refresh")

            if not refresh_token:
                return Response({"error": "Refresh token is required"}, status=status.HTTP_400_BAD_REQUEST)

            try:
                refresh = RefreshToken(refresh_token)
                access_token = str(refresh.access_token)

                return Response(
                    {
                        "access": access_token,
                    },
                    status=status.HTTP_200_OK,
                )

            except Exception as e:
                return Response({"error": "Invalid refresh token"}, status=status.HTTP_401_UNAUTHORIZED)

        except Exception as e:
            logger.error(f"Error in RefreshTokenView: {str(e)}")
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)


class AuthMeView(APIView):
    """
    GET /auth/me
    Returns current authenticated user + organizations.
    """

    permission_classes = [IsAuthenticated]

    def get(self, request):
        try:
            user = request.user

            # Get user's organizations
            user_orgs = UserOrganization.objects.filter(user=user).select_related("organization")
            organizations = [
                {
                    "id": str(uo.organization.id),
                    "name": uo.organization.name,
                    "slug": uo.organization.slug,
                    "role": uo.role,
                }
                for uo in user_orgs
            ]

            user_serializer = UserSerializer(user)

            return Response(
                {
                    "user": user_serializer.data,
                    "organizations": organizations,
                }
            )

        except Exception as e:
            logger.error(f"Error in AuthMeView: {str(e)}")
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)


# ============================================================================
# Magic Link Authentication Views
# ============================================================================


class MagicLinkRequestView(APIView):
    """
    POST /auth/magic-link/request
    Request a magic link for authentication (works for both signin and signup).

    For new users (signup): provide email, first_name, last_name (required)
    For existing users (signin): provide email only

    Rate limited to 3 requests per 5 minutes per email.
    """

    permission_classes = [AllowAny]

    # Rate limiting configuration
    MAX_REQUESTS_PER_WINDOW = 3
    RATE_LIMIT_WINDOW_MINUTES = 5
    TOKEN_EXPIRY_MINUTES = 15

    def get_client_ip(self, request):
        """Extract client IP from request headers."""
        x_forwarded_for = request.META.get("HTTP_X_FORWARDED_FOR")
        if x_forwarded_for:
            return x_forwarded_for.split(",")[0].strip()
        return request.META.get("REMOTE_ADDR")

    def post(self, request):
        try:
            serializer = MagicLinkRequestSerializer(data=request.data)
            if not serializer.is_valid():
                return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)

            email = serializer.validated_data["email"]
            first_name = serializer.validated_data.get("first_name", "").strip()
            last_name = serializer.validated_data.get("last_name", "").strip()

            # Check if user exists
            user_exists = User.objects.filter(email=email).exists()

            # For new users, require first and last name
            if not user_exists:
                if not first_name or not last_name:
                    return Response(
                        {
                            "error": "First name and last name are required for new accounts.",
                            "is_new_user": True,
                        },
                        status=status.HTTP_400_BAD_REQUEST,
                    )

            # Rate limiting: check recent requests
            recent_count = MagicLinkToken.get_recent_requests_count(
                email, minutes=self.RATE_LIMIT_WINDOW_MINUTES
            )
            if recent_count >= self.MAX_REQUESTS_PER_WINDOW:
                return Response(
                    {"error": "Too many magic link requests. Please wait a few minutes and try again."},
                    status=status.HTTP_429_TOO_MANY_REQUESTS,
                )

            # Get client info for security tracking
            ip_address = self.get_client_ip(request)
            user_agent = request.META.get("HTTP_USER_AGENT", "")[:500]  # Limit length

            # Create the magic link token
            magic_link, raw_token = MagicLinkToken.create_token(
                email=email,
                first_name=first_name if not user_exists else None,
                last_name=last_name if not user_exists else None,
                ip_address=ip_address,
                user_agent=user_agent,
                expiry_minutes=self.TOKEN_EXPIRY_MINUTES,
            )

            # Build the magic link URL
            frontend_url = getattr(settings, "FRONTEND_URL", "http://localhost:5176")
            magic_link_url = f"{frontend_url}/auth/magic-link/verify?token={raw_token}"

            # Send the email
            try:
                html_message = render_to_string(
                    "reception/magic_link_email.html",
                    {
                        "magic_link_url": magic_link_url,
                        "email": email,
                        "first_name": first_name or "there",
                        "expiry_minutes": self.TOKEN_EXPIRY_MINUTES,
                        "is_new_user": not user_exists,
                    },
                )

                plain_message = f"""
Hi {first_name or 'there'},

{"Welcome to Internal Apps!" if not user_exists else "Sign in to Internal Apps"}

Click the link below to {"complete your signup" if not user_exists else "sign in"}:

{magic_link_url}

This link will expire in {self.TOKEN_EXPIRY_MINUTES} minutes.

If you didn't request this email, you can safely ignore it.

- The Internal Apps Team
                """.strip()

                send_mail(
                    subject="Your Internal Apps Sign-in Link",
                    message=plain_message,
                    from_email=getattr(settings, "DEFAULT_FROM_EMAIL", "noreply@internalapps.com"),
                    recipient_list=[email],
                    html_message=html_message,
                    fail_silently=False,
                )

                logger.info(f"Magic link sent to {email} (new_user={not user_exists})")

            except Exception as e:
                logger.error(f"Failed to send magic link email to {email}: {str(e)}")
                # In development, log the URL for testing
                if settings.DEBUG:
                    logger.info(f"[DEBUG] Magic link URL: {magic_link_url}")
                else:
                    return Response(
                        {"error": "Failed to send email. Please try again later."},
                        status=status.HTTP_500_INTERNAL_SERVER_ERROR,
                    )

            return Response(
                {
                    "message": "Magic link sent! Check your email.",
                    "email": email,
                    "is_new_user": not user_exists,
                },
                status=status.HTTP_200_OK,
            )

        except Exception as e:
            logger.error(f"Error in MagicLinkRequestView: {str(e)}")
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)


class MagicLinkVerifyView(APIView):
    """
    POST /auth/magic-link/verify
    Verify a magic link token and authenticate the user.

    For new users: creates account and "Default" organization.
    For existing users: returns JWT tokens.
    """

    permission_classes = [AllowAny]

    def post(self, request):
        try:
            serializer = MagicLinkVerifySerializer(data=request.data)
            if not serializer.is_valid():
                return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)

            # Get the verified magic link from serializer
            magic_link = serializer.magic_link
            email = magic_link.email

            with transaction.atomic():
                # Mark the token as used
                magic_link.is_used = True
                magic_link.save()

                # Get or create user
                user, created = User.objects.get_or_create(
                    email=email,
                    defaults={
                        "username": email,
                        "first_name": magic_link.first_name or "",
                        "last_name": magic_link.last_name or "",
                        "is_active": True,
                    },
                )

                # If new user, create "Default" organization
                if created:
                    # Generate unique slug based on user email
                    base_slug = user.email.split("@")[0].lower().replace(".", "-").replace("_", "-")
                    slug = base_slug
                    counter = 1

                    # Ensure slug is unique
                    while Organization.objects.filter(slug=slug).exists():
                        slug = f"{base_slug}-{counter}"
                        counter += 1

                    default_org = Organization.objects.create(name="Default", slug=slug)

                    # Add user as admin member of Default organization
                    UserOrganization.objects.create(
                        user=user, organization=default_org, role=UserOrganization.ROLE_ADMIN
                    )

            # Generate JWT tokens
            tokens = get_tokens_for_user(user)

            # Serialize user
            user_serializer = UserSerializer(user)

            logger.info(f"User authenticated via magic link: {email} (new_user={created})")

            return Response(
                {
                    "user": user_serializer.data,
                    "tokens": tokens,
                    "is_new_user": created,
                    "message": "Welcome to Internal Apps!" if created else "Welcome back!",
                },
                status=status.HTTP_200_OK,
            )

        except Exception as e:
            logger.error(f"Error in MagicLinkVerifyView: {str(e)}")
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)


# ============================================================================
# Google OAuth Views
# ============================================================================


def _generate_oauth_state():
    """Generate a signed OAuth state parameter for CSRF protection.

    Uses HMAC with the secret key to sign a timestamp, avoiding session dependency.
    """
    import time

    timestamp = str(int(time.time()))
    nonce = secrets.token_urlsafe(16)
    message = f"{timestamp}:{nonce}"
    signature = hmac.new(settings.SECRET_KEY.encode(), message.encode(), hashlib.sha256).hexdigest()[:16]
    return f"{message}:{signature}"


def _verify_oauth_state(state):
    """Verify the signed OAuth state parameter.

    Returns True if valid and not expired (within 10 minutes).
    """
    import time

    try:
        parts = state.split(":")
        if len(parts) != 3:
            return False
        timestamp, nonce, signature = parts

        # Check if state is not expired (10 minute window)
        if abs(int(time.time()) - int(timestamp)) > 600:
            return False

        # Verify signature
        message = f"{timestamp}:{nonce}"
        expected_signature = hmac.new(
            settings.SECRET_KEY.encode(), message.encode(), hashlib.sha256
        ).hexdigest()[:16]

        return hmac.compare_digest(signature, expected_signature)
    except Exception:
        return False


class GoogleOAuthView(APIView):
    """
    GET /auth/google
    Initiates Google OAuth flow.
    Returns OAuth URL for frontend to redirect to.
    """

    permission_classes = [AllowAny]

    def get(self, request):
        try:
            client_id = settings.GOOGLE_OAUTH_CLIENT_ID
            redirect_uri = settings.GOOGLE_OAUTH_REDIRECT_URI

            if not client_id:
                return Response(
                    {"error": "Google OAuth not configured"}, status=status.HTTP_500_INTERNAL_SERVER_ERROR
                )

            # Generate signed state for CSRF protection (no session required)
            state = _generate_oauth_state()

            # Google OAuth URL
            params = {
                "client_id": client_id,
                "redirect_uri": redirect_uri,
                "response_type": "code",
                "scope": "openid email profile",
                "access_type": "offline",
                "prompt": "consent",
                "state": state,
            }

            google_oauth_url = f"https://accounts.google.com/o/oauth2/v2/auth?{urlencode(params)}"

            return Response(
                {
                    "oauth_url": google_oauth_url,
                },
                status=status.HTTP_200_OK,
            )

        except Exception as e:
            logger.error(f"Error in GoogleOAuthView: {str(e)}")
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)


class GoogleOAuthCallbackView(APIView):
    """
    GET /auth/google/callback
    Handles Google OAuth callback.
    Creates/authenticates user and returns JWT tokens.
    """

    permission_classes = [AllowAny]

    def get(self, request):
        try:
            code = request.GET.get("code")
            state = request.GET.get("state")
            error = request.GET.get("error")

            if error:
                return Response({"error": f"OAuth error: {error}"}, status=status.HTTP_400_BAD_REQUEST)

            if not code:
                return Response(
                    {"error": "Authorization code not provided"}, status=status.HTTP_400_BAD_REQUEST
                )

            # Verify signed state (CSRF protection - no session required)
            if not state or not _verify_oauth_state(state):
                logger.warning(f"Invalid OAuth state parameter: {state}")
                return Response(
                    {"error": "Invalid or expired state parameter. Please try again."},
                    status=status.HTTP_400_BAD_REQUEST,
                )

            # Exchange code for access token
            client_id = settings.GOOGLE_OAUTH_CLIENT_ID
            client_secret = settings.GOOGLE_OAUTH_CLIENT_SECRET
            redirect_uri = settings.GOOGLE_OAUTH_REDIRECT_URI

            token_response = requests.post(
                "https://oauth2.googleapis.com/token",
                data={
                    "code": code,
                    "client_id": client_id,
                    "client_secret": client_secret,
                    "redirect_uri": redirect_uri,
                    "grant_type": "authorization_code",
                },
                timeout=10,
            )

            if token_response.status_code != 200:
                logger.error(f"Google OAuth token error: {token_response.text}")
                return Response(
                    {"error": "Failed to exchange authorization code"}, status=status.HTTP_400_BAD_REQUEST
                )

            token_data = token_response.json()
            access_token = token_data.get("access_token")

            # Get user info from Google
            user_info_response = requests.get(
                "https://www.googleapis.com/oauth2/v2/userinfo",
                headers={"Authorization": f"Bearer {access_token}"},
                timeout=10,
            )

            if user_info_response.status_code != 200:
                return Response(
                    {"error": "Failed to fetch user info from Google"}, status=status.HTTP_400_BAD_REQUEST
                )

            user_info = user_info_response.json()

            # Get or create user
            google_id = user_info.get("id")
            email = user_info.get("email")
            name = user_info.get("name", "")
            picture = user_info.get("picture")

            if not email:
                return Response({"error": "Email not provided by Google"}, status=status.HTTP_400_BAD_REQUEST)

            # Split name into first/last
            name_parts = name.split(maxsplit=1)
            first_name = name_parts[0] if len(name_parts) > 0 else ""
            last_name = name_parts[1] if len(name_parts) > 1 else ""

            with transaction.atomic():
                # Get or create user
                user, created = User.objects.get_or_create(
                    email=email,
                    defaults={
                        "google_id": google_id,
                        "first_name": first_name,
                        "last_name": last_name,
                        "username": email,
                        "profile_image_url": picture,
                        "is_active": True,
                    },
                )

                # Update user if not created
                if not created:
                    updated = False
                    if google_id and user.google_id != google_id:
                        user.google_id = google_id
                        updated = True
                    if picture and user.profile_image_url != picture:
                        user.profile_image_url = picture
                        updated = True
                    if first_name and user.first_name != first_name:
                        user.first_name = first_name
                        updated = True
                    if last_name and user.last_name != last_name:
                        user.last_name = last_name
                        updated = True

                    if updated:
                        user.save()

                # If new user, create "Default" organization
                if created:
                    # Generate unique slug based on user email
                    base_slug = user.email.split("@")[0].lower().replace(".", "-").replace("_", "-")
                    slug = base_slug
                    counter = 1

                    # Ensure slug is unique
                    while Organization.objects.filter(slug=slug).exists():
                        slug = f"{base_slug}-{counter}"
                        counter += 1

                    default_org = Organization.objects.create(name="Default", slug=slug)

                    # Add user as admin member of Default organization
                    UserOrganization.objects.create(
                        user=user, organization=default_org, role=UserOrganization.ROLE_ADMIN
                    )

            # Generate JWT tokens
            tokens = get_tokens_for_user(user)

            # Serialize user
            user_serializer = UserSerializer(user)
            import json

            user_json = json.dumps(user_serializer.data)
            user_b64 = urlsafe_base64_encode(force_bytes(user_json))

            # Redirect to frontend callback page with tokens in URL hash
            # Using hash to avoid tokens in server logs
            frontend_callback_url = getattr(settings, "FRONTEND_URL", "http://localhost:5176")
            redirect_url = (
                f"{frontend_callback_url}/auth/google/callback"
                f"#access_token={tokens['access']}"
                f"&refresh_token={tokens['refresh']}"
                f"&user={user_b64}"
            )

            # Redirect to frontend callback page
            from django.shortcuts import redirect

            return redirect(redirect_url)

        except Exception as e:
            logger.error(f"Error in GoogleOAuthCallbackView: {str(e)}")
            # Redirect to frontend with error
            frontend_callback_url = getattr(settings, "FRONTEND_URL", "http://localhost:5176")
            from django.shortcuts import redirect
            from urllib.parse import quote

            return redirect(f"{frontend_callback_url}/auth/google/callback?error={quote(str(e))}")
