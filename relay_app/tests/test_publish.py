import json
from django.test import TestCase, Client
from django.contrib.auth import get_user_model

from relay_app.models import (
    Organization,
    UserOrganization,
    BackendConnection,
    InternalApp,
    AppVersion,
    VersionFile,
    ResourceRegistryEntry,
)


User = get_user_model()


class PublishFlowTests(TestCase):
    """Integration coverage for the publish endpoint."""

    def setUp(self):
        self.client = Client()
        self.user = User.objects.create_user(
            username='publisher',
            email='publisher@example.com',
            password='strong-password',
        )
        self.org = Organization.objects.create(name='Publish Org', slug='publish-org')
        UserOrganization.objects.create(
            user=self.user,
            organization=self.org,
            role=UserOrganization.ROLE_ADMIN,
        )
        self.client.force_login(self.user)

        self.backend = BackendConnection.objects.create(
            organization=self.org,
            adapter_type=BackendConnection.ADAPTER_SUPABASE,
            display_name='Supabase',
            config_encrypted='',
        )
        self.backend.set_config({'supabase_url': 'https://example.supabase.co', 'service_role_key': 'role'})
        self.backend.save()

    def _create_app_with_version(self, generation_status=AppVersion.GEN_STATUS_COMPLETE):
        app = InternalApp.objects.create(
            organization=self.org,
            name='Orders Dashboard',
            backend_connection=self.backend,
            created_by=self.user,
        )
        base_version = AppVersion.objects.create(
            internal_app=app,
            version_number=1,
            source=AppVersion.SOURCE_AI_EDIT,
            intent_message='Initial app creation',
            spec_json={'appName': 'Orders Dashboard', 'pages': []},
            created_by=self.user,
            generation_status=generation_status,
        )
        VersionFile.objects.create(
            app_version=base_version,
            path='src/app/page.tsx',
            content='// page content',
        )
        VersionFile.objects.create(
            app_version=base_version,
            path='src/lib/runtimeClient.ts',
            content='// runtime client',
        )
        return app, base_version

    def test_publish_creates_snapshot_and_copies_files(self):
        """Publishing should create a new version, snapshot registry, and mark app published."""
        app, base_version = self._create_app_with_version()

        # Registry state should be captured into scope_snapshot_json
        ResourceRegistryEntry.objects.create(
            organization=self.org,
            backend_connection=self.backend,
            resource_id='public.orders',
            resource_name='Orders',
            schema_json={},
            enabled=True,
            exposed_fields_json=['id', 'status'],
            allowed_actions_json=[{'action_id': 'orders.create', 'input_schema': {}}],
        )

        response = self.client.post(f'/api/v1/apps/{app.id}/publish/')
        self.assertEqual(response.status_code, 201, response.content)

        data = json.loads(response.content)
        publish_version = AppVersion.objects.get(pk=data['id'])
        app.refresh_from_db()

        self.assertEqual(app.status, InternalApp.STATUS_PUBLISHED)
        self.assertEqual(publish_version.version_number, 2)
        self.assertEqual(publish_version.parent_version_id, base_version.id)
        self.assertEqual(len(publish_version.scope_snapshot_json or []), 1)
        self.assertEqual(publish_version.scope_snapshot_json[0]['resource_id'], 'public.orders')

        self.assertEqual(publish_version.files.count(), base_version.files.count())
        self.assertSetEqual(
            set(publish_version.files.values_list('path', flat=True)),
            set(base_version.files.values_list('path', flat=True)),
        )
        # Ensure hashes copied and populated
        for file in publish_version.files.all():
            self.assertTrue(file.content_hash)

    def test_publish_requires_completed_generation(self):
        """Publishing should be blocked while latest version is still generating."""
        app, base_version = self._create_app_with_version(generation_status=AppVersion.GEN_STATUS_GENERATING)

        response = self.client.post(f'/api/v1/apps/{app.id}/publish/')
        self.assertEqual(response.status_code, 400)
        # With the fix, publishing requires a stable (complete) version
        self.assertIn(b'No stable version to publish', response.content)

        app.refresh_from_db()
        self.assertEqual(app.status, InternalApp.STATUS_DRAFT)

    def test_publish_without_versions_returns_400(self):
        """Publishing with no existing versions should fail with a clear message."""
        app = InternalApp.objects.create(
            organization=self.org,
            name='Empty App',
            backend_connection=self.backend,
            created_by=self.user,
        )

        response = self.client.post(f'/api/v1/apps/{app.id}/publish/')
        self.assertEqual(response.status_code, 400)
        # With the fix, publishing requires a stable (complete) version
        self.assertIn(b'No stable version to publish', response.content)

