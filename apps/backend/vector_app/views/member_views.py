"""
Organization member management views.

Endpoints for managing organization members and invitations.
"""
import logging
from django.shortcuts import get_object_or_404
from django.core.mail import send_mail
from django.conf import settings
from django.template.loader import render_to_string
from rest_framework import status
from rest_framework.permissions import IsAuthenticated, AllowAny
from rest_framework.response import Response
from rest_framework.views import APIView

from internal_apps.utils.enum import choices
from ..models import Organization, OrganizationInvite, User, UserOrganization, UserOrganizationRole
from ..serializers.member_serializers import (
    OrgMemberSerializer,
    OrgMemberUpdateSerializer,
    OrgInviteSerializer,
    OrgInviteCreateSerializer,
    InviteAcceptSerializer,
    InviteVerifySerializer,
    InviteDetailsSerializer,
)

logger = logging.getLogger(__name__)


def get_user_org_membership(user, organization):
    """Get the user's membership in an organization."""
    return UserOrganization.objects.filter(
        user=user,
        organization=organization
    ).first()


def require_org_admin(user, organization):
    """Check if user is admin of the organization. Returns membership or None."""
    membership = get_user_org_membership(user, organization)
    if membership and membership.is_admin():
        return membership
    return None


class OrgMembersListView(APIView):
    """
    GET /api/v1/orgs/:org_id/members/
    List all members and pending invites for an organization.
    
    Available to all organization members.
    """
    permission_classes = [IsAuthenticated]
    
    def get(self, request, org_id):
        try:
            organization = get_object_or_404(Organization, id=org_id)
            
            # Verify user is a member
            membership = get_user_org_membership(request.user, organization)
            if not membership:
                return Response(
                    {'error': 'You are not a member of this organization'},
                    status=status.HTTP_403_FORBIDDEN
                )
            
            # Get all members
            members = UserOrganization.objects.filter(
                organization=organization
            ).select_related('user').order_by('created_at')
            
            # Get pending invites (only if admin, otherwise empty)
            if membership.is_admin():
                pending_invites = OrganizationInvite.get_pending_for_org(organization)
            else:
                pending_invites = []
            
            return Response({
                'members': OrgMemberSerializer(members, many=True).data,
                'pending_invites': OrgInviteSerializer(pending_invites, many=True).data,
                'current_user_role': membership.role,
                'can_manage_members': membership.can_manage_members(),
            })
            
        except Exception as e:
            logger.error(f"Error listing members: {str(e)}")
            return Response(
                {'error': str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )


class OrgMemberDetailView(APIView):
    """
    PATCH /api/v1/orgs/:org_id/members/:member_id/
    Update a member's role.
    
    DELETE /api/v1/orgs/:org_id/members/:member_id/
    Remove a member from the organization.
    
    Admin only.
    """
    permission_classes = [IsAuthenticated]
    
    def patch(self, request, org_id, member_id):
        """Update a member's role."""
        try:
            organization = get_object_or_404(Organization, id=org_id)
            
            # Verify user is admin
            if not require_org_admin(request.user, organization):
                return Response(
                    {'error': 'Only admins can update member roles'},
                    status=status.HTTP_403_FORBIDDEN
                )
            
            # Get the member to update
            member = get_object_or_404(
                UserOrganization,
                id=member_id,
                organization=organization
            )
            
            serializer = OrgMemberUpdateSerializer(
                data=request.data,
                context={'request': request, 'member': member}
            )
            
            if not serializer.is_valid():
                return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
            
            # Update the role
            member.role = serializer.validated_data['role']
            member.save(update_fields=['role', 'updated_at'])
            
            logger.info(f"Member {member.user.email} role updated to {member.role} by {request.user.email}")
            
            return Response({
                'member': OrgMemberSerializer(member).data,
                'message': 'Member role updated successfully',
            })
            
        except Exception as e:
            logger.error(f"Error updating member: {str(e)}")
            return Response(
                {'error': str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )
    
    def delete(self, request, org_id, member_id):
        """Remove a member from the organization."""
        try:
            organization = get_object_or_404(Organization, id=org_id)
            
            # Verify user is admin
            if not require_org_admin(request.user, organization):
                return Response(
                    {'error': 'Only admins can remove members'},
                    status=status.HTTP_403_FORBIDDEN
                )
            
            # Get the member to remove
            member = get_object_or_404(
                UserOrganization,
                id=member_id,
                organization=organization
            )
            
            # Cannot remove yourself
            if member.user == request.user:
                return Response(
                    {'error': 'You cannot remove yourself from the organization'},
                    status=status.HTTP_400_BAD_REQUEST
                )
            
            # Check if this is the last admin
            if member.is_admin():
                admin_count = UserOrganization.objects.filter(
                    organization=organization,
                    role=UserOrganizationRole.ADMIN
                ).count()
                if admin_count <= 1:
                    return Response(
                        {'error': 'Cannot remove the last admin from the organization'},
                        status=status.HTTP_400_BAD_REQUEST
                    )
            
            email = member.user.email
            member.delete()
            
            logger.info(f"Member {email} removed from {organization.name} by {request.user.email}")
            
            return Response({
                'message': 'Member removed successfully',
            })
            
        except Exception as e:
            logger.error(f"Error removing member: {str(e)}")
            return Response(
                {'error': str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )


class OrgInviteListView(APIView):
    """
    POST /api/v1/orgs/:org_id/invites/
    Send an invitation to join the organization.
    
    Admin only.
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request, org_id):
        """Create and send an invitation."""
        try:
            organization = get_object_or_404(Organization, id=org_id)
            
            # Verify user is admin
            if not require_org_admin(request.user, organization):
                return Response(
                    {'error': 'Only admins can invite members'},
                    status=status.HTTP_403_FORBIDDEN
                )
            
            serializer = OrgInviteCreateSerializer(
                data=request.data,
                context={'request': request, 'organization': organization}
            )
            
            if not serializer.is_valid():
                return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
            
            invite = serializer.save()
            raw_token = invite._raw_token
            
            # Send invitation email
            self._send_invite_email(invite, raw_token, request)
            
            logger.info(f"Invitation sent to {invite.email} for {organization.name} by {request.user.email}")
            
            return Response({
                'invite': OrgInviteSerializer(invite).data,
                'message': f'Invitation sent to {invite.email}',
            }, status=status.HTTP_201_CREATED)
            
        except Exception as e:
            logger.error(f"Error creating invitation: {str(e)}")
            return Response(
                {'error': str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )
    
    def _send_invite_email(self, invite, raw_token, request):
        """Send the invitation email."""
        frontend_url = getattr(settings, 'FRONTEND_URL', 'http://localhost:5176')
        invite_url = f"{frontend_url}/invite/accept?token={raw_token}"
        
        inviter_name = ""
        if invite.invited_by:
            first = invite.invited_by.first_name or ''
            last = invite.invited_by.last_name or ''
            inviter_name = f"{first} {last}".strip() or invite.invited_by.email
        
        role_display = dict(choices(UserOrganizationRole)).get(invite.role, invite.role)
        
        try:
            html_message = render_to_string('reception/invite_email.html', {
                'invite_url': invite_url,
                'frontend_url': frontend_url,
                'organization_name': invite.organization.name,
                'inviter_name': inviter_name,
                'role': role_display,
                'email': invite.email,
            })
            
            plain_message = f"""
You've been invited to join {invite.organization.name}!

{inviter_name} has invited you to join {invite.organization.name} as {role_display}.

Click the link below to accept the invitation:

{invite_url}

This invitation will expire in 7 days.

If you didn't expect this invitation, you can safely ignore this email.

- The Vector Team
            """.strip()
            
            send_mail(
                subject=f"You're invited to join {invite.organization.name}",
                message=plain_message,
                from_email=getattr(settings, 'DEFAULT_FROM_EMAIL', 'noreply@govector.ai'),
                recipient_list=[invite.email],
                html_message=html_message,
                fail_silently=False,
            )
            
        except Exception as e:
            logger.error(f"Failed to send invitation email to {invite.email}: {str(e)}")
            if settings.DEBUG:
                logger.info(f"[DEBUG] Invitation URL: {invite_url}")
            # Re-raise so the API returns an error
            raise Exception(f"Failed to send invitation email: {str(e)}")


class OrgInviteDetailView(APIView):
    """
    DELETE /api/v1/orgs/:org_id/invites/:invite_id/
    Cancel a pending invitation.
    
    Admin only.
    """
    permission_classes = [IsAuthenticated]
    
    def delete(self, request, org_id, invite_id):
        """Cancel a pending invitation."""
        try:
            organization = get_object_or_404(Organization, id=org_id)
            
            # Verify user is admin
            if not require_org_admin(request.user, organization):
                return Response(
                    {'error': 'Only admins can cancel invitations'},
                    status=status.HTTP_403_FORBIDDEN
                )
            
            invite = get_object_or_404(
                OrganizationInvite,
                id=invite_id,
                organization=organization,
                is_accepted=False
            )
            
            email = invite.email
            invite.delete()
            
            logger.info(f"Invitation to {email} cancelled for {organization.name} by {request.user.email}")
            
            return Response({
                'message': 'Invitation cancelled',
            })
            
        except Exception as e:
            logger.error(f"Error cancelling invitation: {str(e)}")
            return Response(
                {'error': str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )


class OrgInviteResendView(APIView):
    """
    POST /api/v1/orgs/:org_id/invites/:invite_id/resend/
    Resend an invitation email.
    
    Admin only.
    """
    permission_classes = [IsAuthenticated]
    
    def post(self, request, org_id, invite_id):
        """Resend an invitation email."""
        try:
            organization = get_object_or_404(Organization, id=org_id)
            
            # Verify user is admin
            if not require_org_admin(request.user, organization):
                return Response(
                    {'error': 'Only admins can resend invitations'},
                    status=status.HTTP_403_FORBIDDEN
                )
            
            # Get the existing invite
            old_invite = get_object_or_404(
                OrganizationInvite,
                id=invite_id,
                organization=organization,
                is_accepted=False
            )
            
            # Create a new invite (this will delete the old one)
            new_invite, raw_token = OrganizationInvite.create_invite(
                organization=organization,
                email=old_invite.email,
                role=old_invite.role,
                invited_by=request.user,
            )
            
            # Send the email
            self._send_invite_email(new_invite, raw_token)
            
            logger.info(f"Invitation resent to {new_invite.email} for {organization.name} by {request.user.email}")
            
            return Response({
                'invite': OrgInviteSerializer(new_invite).data,
                'message': f'Invitation resent to {new_invite.email}',
            })
            
        except Exception as e:
            logger.error(f"Error resending invitation: {str(e)}")
            return Response(
                {'error': str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )
    
    def _send_invite_email(self, invite, raw_token):
        """Send the invitation email (reuse from OrgInviteListView)."""
        frontend_url = getattr(settings, 'FRONTEND_URL', 'http://localhost:5176')
        invite_url = f"{frontend_url}/invite/accept?token={raw_token}"
        
        inviter_name = ""
        if invite.invited_by:
            first = invite.invited_by.first_name or ''
            last = invite.invited_by.last_name or ''
            inviter_name = f"{first} {last}".strip() or invite.invited_by.email
        
        role_display = dict(choices(UserOrganizationRole)).get(invite.role, invite.role)
        
        try:
            html_message = render_to_string('reception/invite_email.html', {
                'invite_url': invite_url,
                'frontend_url': frontend_url,
                'organization_name': invite.organization.name,
                'inviter_name': inviter_name,
                'role': role_display,
                'email': invite.email,
            })
            
            plain_message = f"""
You've been invited to join {invite.organization.name}!

{inviter_name} has invited you to join {invite.organization.name} as {role_display}.

Click the link below to accept the invitation:

{invite_url}

This invitation will expire in 7 days.

If you didn't expect this invitation, you can safely ignore this email.

- The Vector Team
            """.strip()
            
            send_mail(
                subject=f"You're invited to join {invite.organization.name}",
                message=plain_message,
                from_email=getattr(settings, 'DEFAULT_FROM_EMAIL', 'noreply@govector.ai'),
                recipient_list=[invite.email],
                html_message=html_message,
                fail_silently=False,
            )
            
        except Exception as e:
            logger.error(f"Failed to send invitation email to {invite.email}: {str(e)}")
            if settings.DEBUG:
                logger.info(f"[DEBUG] Invitation URL: {invite_url}")
            # Re-raise so the API returns an error
            raise Exception(f"Failed to send invitation email: {str(e)}")


class InviteVerifyView(APIView):
    """
    POST /api/v1/auth/invite/verify/
    Verify an invitation token and return details (without accepting).
    
    Public endpoint - no authentication required.
    """
    permission_classes = [AllowAny]
    
    def post(self, request):
        """Verify an invitation token."""
        try:
            serializer = InviteVerifySerializer(data=request.data)
            
            if not serializer.is_valid():
                return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
            
            invite = serializer.invite
            
            # Get logo URL if available
            logo_url = None
            if invite.organization.logo:
                logo_url = request.build_absolute_uri(invite.organization.logo.url)
            
            inviter_name = None
            if invite.invited_by:
                first = invite.invited_by.first_name or ''
                last = invite.invited_by.last_name or ''
                inviter_name = f"{first} {last}".strip() or invite.invited_by.email
            
            role_display = dict(choices(UserOrganizationRole)).get(invite.role, invite.role)
            
            return Response({
                'organization_name': invite.organization.name,
                'organization_logo_url': logo_url,
                'role': invite.role,
                'role_display': role_display,
                'invited_by_name': inviter_name,
                'email': invite.email,
                'expires_at': invite.expires_at.isoformat(),
            })
            
        except Exception as e:
            logger.error(f"Error verifying invitation: {str(e)}")
            return Response(
                {'error': str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )


class InviteAcceptView(APIView):
    """
    POST /api/v1/auth/invite/accept/
    Accept an invitation and join the organization.
    
    For existing users: requires authentication
    For new users: creates account and returns JWT tokens
    """
    permission_classes = [AllowAny]
    
    def post(self, request):
        """Accept an invitation."""
        try:
            serializer = InviteAcceptSerializer(data=request.data)
            
            if not serializer.is_valid():
                return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
            
            invite = serializer.invite
            
            # Check if user exists
            user = User.objects.filter(email=invite.email).first()
            
            if user:
                # Existing user - accept the invite
                membership = invite.accept(user)
                
                logger.info(f"User {user.email} joined {invite.organization.name} as {membership.role}")
                
                return Response({
                    'message': f'Welcome to {invite.organization.name}!',
                    'organization': {
                        'id': str(invite.organization.id),
                        'name': invite.organization.name,
                        'slug': invite.organization.slug,
                    },
                    'role': membership.role,
                    'is_new_user': False,
                })
            else:
                # New user - they need to sign up first
                # Return info about the invite so the frontend can redirect to signup
                return Response({
                    'requires_signup': True,
                    'email': invite.email,
                    'organization_name': invite.organization.name,
                    'message': 'Please create an account to accept this invitation.',
                }, status=status.HTTP_200_OK)
            
        except Exception as e:
            logger.error(f"Error accepting invitation: {str(e)}")
            return Response(
                {'error': str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )

