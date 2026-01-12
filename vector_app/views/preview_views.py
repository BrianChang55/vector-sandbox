"""
Preview Views for Internal Apps

Serves the generated app preview as an HTML page with bundled React code.

Authorization:
- All requests must be authenticated (session or JWT)
- User must be a member of the app's organization
"""

import logging
import json
from django.http import HttpResponse, HttpResponseForbidden, Http404
from django.views import View
from django.shortcuts import get_object_or_404
from rest_framework_simplejwt.authentication import JWTAuthentication
from rest_framework.request import Request

from ..models import InternalApp, AppVersion, VersionFile, UserOrganization
from ..services.version_service import VersionService

logger = logging.getLogger(__name__)


class AppPreviewView(View):
    """
    Serves the app preview as an HTML page.

    GET /preview/apps/:app_id?version=...

    Authorization:
    - Must be authenticated (session or JWT)
    - Must be member of app's organization
    """

    def _get_authenticated_user(self, request):
        """
        Get authenticated user from session or JWT token.

        Returns (user, error_response) tuple.
        """
        # Try session auth first
        if request.user and request.user.is_authenticated:
            return request.user, None

        # Try JWT from Authorization header
        auth_header = request.META.get("HTTP_AUTHORIZATION", "")
        if auth_header:
            try:
                jwt_auth = JWTAuthentication()
                drf_request = Request(request)
                auth_result = jwt_auth.authenticate(drf_request)
                if auth_result:
                    user, _ = auth_result
                    return user, None
            except Exception as e:
                logger.debug(f"JWT auth failed for preview: {e}")

        # Check for token query parameter (for iframe embedding)
        token = request.GET.get("token")
        if token:
            try:
                from rest_framework_simplejwt.tokens import AccessToken

                access_token = AccessToken(token)
                from django.contrib.auth import get_user_model

                User = get_user_model()
                user = User.objects.get(id=access_token["user_id"])
                return user, None
            except Exception as e:
                logger.debug(f"Token param auth failed: {e}")

        return None, HttpResponseForbidden("Authentication required")

    def get(self, request, app_id):
        """Render preview HTML for an app version."""
        # Authenticate user
        user, error = self._get_authenticated_user(request)
        if error:
            return self._allow_iframe(error)

        version_id = request.GET.get("version")

        try:
            app = InternalApp.objects.select_related("organization").get(pk=app_id)

            # Verify user is member of app's organization
            if not UserOrganization.objects.filter(user=user, organization=app.organization).exists():
                response = HttpResponseForbidden("You don't have access to this app")
                return self._allow_iframe(response)

            if version_id:
                version = AppVersion.objects.get(pk=version_id, internal_app=app)
            else:
                # Get latest STABLE version (complete generation) for preview
                version = VersionService.get_latest_stable_version(app)

                if not version:
                    response = HttpResponse(self._render_empty_preview(app), content_type="text/html")
                    return self._allow_iframe(response)

            # Fallback: if the selected version has no pages, use the latest stable version that does
            if not self._has_pages(version):
                fallback_version = None
                # Only look at stable versions for fallback
                for candidate in AppVersion.objects.filter(
                    internal_app=app, generation_status=AppVersion.GEN_STATUS_COMPLETE
                ).order_by("-version_number"):
                    if self._has_pages(candidate):
                        fallback_version = candidate
                        break
                if fallback_version:
                    version = fallback_version
                else:
                    return self._empty_response(app)

            response = HttpResponse(self._render_preview(app, version), content_type="text/html")
            return self._allow_iframe(response)

        except InternalApp.DoesNotExist:
            raise Http404("App not found")
        except AppVersion.DoesNotExist:
            # If a version was requested but doesn't exist (or has been deleted),
            # fall back to the empty preview rather than surfacing a hard 404.
            # This keeps the first-load experience clean when no versions exist yet.
            try:
                app = InternalApp.objects.get(pk=app_id)
                return self._empty_response(app)
            except InternalApp.DoesNotExist:
                raise Http404("App not found")

    def _allow_iframe(self, response: HttpResponse) -> HttpResponse:
        """
        Ensure the preview can be embedded in an iframe (used by the web app).
        XFrameOptionsMiddleware defaults to DENY, so override here.
        """
        response["X-Frame-Options"] = "ALLOWALL"
        response["Content-Security-Policy"] = "frame-ancestors *"
        return response

    def _empty_response(self, app: InternalApp) -> HttpResponse:
        """Return the empty preview HTML wrapped with iframe allowances."""
        response = HttpResponse(self._render_empty_preview(app), content_type="text/html")
        return self._allow_iframe(response)

    def _has_pages(self, version: AppVersion) -> bool:
        """Return True when the version has at least one page in its spec."""
        spec = version.spec_json or {}
        pages = spec.get("pages") if isinstance(spec, dict) else None
        return bool(pages)

    def _render_preview(self, app: InternalApp, version: AppVersion) -> str:
        """Render the preview HTML with embedded React code."""

        # Get version files
        files = {f.path: f.content for f in version.files.all()}

        # Get the page component
        page_content = files.get("src/app/page.tsx", "")
        table_view = files.get("src/components/TableView.tsx", "")
        detail_drawer = files.get("src/components/DetailDrawer.tsx", "")
        runtime_client = files.get("src/lib/runtimeClient.ts", "")

        spec_json = json.dumps(version.spec_json)

        return f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{app.name} - Preview</title>
    <script src="https://unpkg.com/react@18/umd/react.development.js"></script>
    <script src="https://unpkg.com/react-dom@18/umd/react-dom.development.js"></script>
    <script src="https://unpkg.com/@babel/standalone/babel.min.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
    <script src="https://unpkg.com/axios/dist/axios.min.js"></script>
    <style>
        body {{
            margin: 0;
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
            background: #fafafa;
        }}
        .loading {{
            display: flex;
            align-items: center;
            justify-content: center;
            height: 100vh;
            color: #666;
        }}
        .error {{
            padding: 20px;
            background: #fee;
            border: 1px solid #fcc;
            border-radius: 8px;
            margin: 20px;
            color: #c00;
        }}
    </style>
</head>
<body>
    <div id="root">
        <div class="loading">Loading preview...</div>
    </div>
    
    <script>
        // Runtime configuration
        window.__VECTOR_CONFIG__ = {{
            appId: '{str(app.id)}',
            versionId: '{str(version.id)}',
            appName: '{app.name}',
            apiBaseUrl: window.location.origin + '/api/v1'
        }};
        
        // App spec
        window.__APP_SPEC__ = {spec_json};
    </script>
    
    <script type="text/babel" data-presets="react,typescript">
        const {{ useState, useEffect }} = React;
        
        // Runtime Client
        const API_BASE_URL = window.__VECTOR_CONFIG__.apiBaseUrl;
        
        async function runtimeQuery(params) {{
            try {{
                const response = await axios.post(`${{API_BASE_URL}}/runtime/query/`, {{
                    appId: window.__VECTOR_CONFIG__.appId,
                    versionId: window.__VECTOR_CONFIG__.versionId,
                    ...params
                }});
                return response.data;
            }} catch (error) {{
                console.error('Query error:', error);
                return {{ data: [], count: 0 }};
            }}
        }}
        
        async function runtimeAction(params) {{
            try {{
                const response = await axios.post(`${{API_BASE_URL}}/runtime/action/`, {{
                    appId: window.__VECTOR_CONFIG__.appId,
                    versionId: window.__VECTOR_CONFIG__.versionId,
                    ...params
                }});
                return response.data;
            }} catch (error) {{
                console.error('Action error:', error);
                return {{ success: false, error: error.message }};
            }}
        }}
        
        // Table View Component
        function TableView({{ resourceId, spec, onRowClick }}) {{
            const [data, setData] = useState([]);
            const [loading, setLoading] = useState(true);
            const [error, setError] = useState(null);
            
            useEffect(() => {{
                async function fetchData() {{
                    try {{
                        setLoading(true);
                        const result = await runtimeQuery({{
                            resourceId,
                            querySpec: {{
                                select: spec.columns?.map(c => c.field) || ['*'],
                                limit: spec.pagination?.pageSize || 50,
                            }}
                        }});
                        setData(result.data || []);
                    }} catch (err) {{
                        setError(err.message);
                    }} finally {{
                        setLoading(false);
                    }}
                }}
                if (resourceId) fetchData();
            }}, [resourceId]);
            
            if (loading) {{
                return (
                    <div className="flex items-center justify-center py-12">
                        <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-violet-600"></div>
                    </div>
                );
            }}
            
            if (error) {{
                return (
                    <div className="bg-red-50 border border-red-200 rounded-lg p-4 text-red-700">
                        {{error}}
                    </div>
                );
            }}
            
            const columns = spec.columns || [];
            
            return (
                <div className="overflow-x-auto bg-white rounded-xl shadow-sm border border-gray-200">
                    <table className="min-w-full divide-y divide-gray-200">
                        <thead className="bg-gray-50">
                            <tr>
                                {{columns.map(col => (
                                    <th 
                                        key={{col.field}} 
                                        className="px-6 py-3 text-left text-xs font-semibold text-gray-600 uppercase tracking-wider"
                                    >
                                        {{col.label || col.field}}
                                    </th>
                                ))}}
                            </tr>
                        </thead>
                        <tbody className="bg-white divide-y divide-gray-100">
                            {{data.length === 0 ? (
                                <tr>
                                    <td colSpan={{columns.length}} className="px-6 py-12 text-center text-gray-500">
                                        No data found
                                    </td>
                                </tr>
                            ) : data.map((row, idx) => (
                                <tr 
                                    key={{idx}} 
                                    className="hover:bg-gray-50 cursor-pointer transition-colors"
                                    onClick={{() => onRowClick?.(row)}}
                                >
                                    {{columns.map(col => (
                                        <td key={{col.field}} className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                                            {{renderCellValue(row[col.field], col.type)}}
                                        </td>
                                    ))}}
                                </tr>
                            ))}}
                        </tbody>
                    </table>
                </div>
            );
        }}
        
        function renderCellValue(value, type) {{
            if (value === null || value === undefined) return '-';
            
            switch (type) {{
                case 'badge':
                    const colors = {{
                        active: 'bg-green-100 text-green-800',
                        inactive: 'bg-gray-100 text-gray-800',
                        pending: 'bg-yellow-100 text-yellow-800',
                        error: 'bg-red-100 text-red-800',
                    }};
                    const colorClass = colors[value?.toLowerCase?.()] || 'bg-blue-100 text-blue-800';
                    return (
                        <span className={{`px-2 py-1 text-xs font-medium rounded-full ${{colorClass}}`}}>
                            {{value}}
                        </span>
                    );
                case 'date':
                    return new Date(value).toLocaleDateString();
                case 'avatar':
                    return (
                        <div className="h-8 w-8 rounded-full bg-violet-100 flex items-center justify-center text-violet-700 font-medium text-sm">
                            {{String(value).charAt(0).toUpperCase()}}
                        </div>
                    );
                default:
                    return String(value);
            }}
        }}
        
        // Detail Drawer Component
        function DetailDrawer({{ isOpen, onClose, data, spec }}) {{
            if (!isOpen) return null;
            
            return (
                <div className="fixed inset-0 z-50 flex">
                    <div className="absolute inset-0 bg-black/50" onClick={{onClose}} />
                    <div className="absolute right-0 top-0 bottom-0 w-96 bg-white shadow-2xl overflow-y-auto">
                        <div className="sticky top-0 bg-white border-b border-gray-200 px-6 py-4 flex items-center justify-between">
                            <h2 className="text-lg font-semibold text-gray-900">
                                {{spec?.titleField ? data?.[spec.titleField] : 'Details'}}
                            </h2>
                            <button 
                                onClick={{onClose}}
                                className="p-2 hover:bg-gray-100 rounded-lg transition-colors"
                            >
                                <svg className="h-5 w-5 text-gray-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M6 18L18 6M6 6l12 12" />
                                </svg>
                            </button>
                        </div>
                        <div className="p-6 space-y-4">
                            {{(spec?.fields || []).map(field => (
                                <div key={{field.field}}>
                                    <label className="block text-sm font-medium text-gray-600 mb-1">
                                        {{field.label || field.field}}
                                    </label>
                                    <div className="text-gray-900">
                                        {{data?.[field.field] ?? '-'}}
                                    </div>
                                </div>
                            ))}}
                        </div>
                    </div>
                </div>
            );
        }}
        
        // Main App Component
        function App() {{
            const spec = window.__APP_SPEC__;
            const [selectedRow, setSelectedRow] = useState(null);
            const [drawerOpen, setDrawerOpen] = useState(false);
            
            if (!spec || !spec.pages || spec.pages.length === 0) {{
                return (
                    <div className="min-h-screen bg-gray-50 flex items-center justify-center">
                        <div className="text-center">
                            <div className="w-16 h-16 mx-auto mb-4 bg-violet-100 rounded-2xl flex items-center justify-center">
                                <svg className="h-8 w-8 text-violet-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
                                </svg>
                            </div>
                            <h2 className="text-xl font-semibold text-gray-800 mb-2">No Pages Yet</h2>
                            <p className="text-gray-500">Use the AI builder to create your first page.</p>
                        </div>
                    </div>
                );
            }}
            
            const page = spec.pages[0];
            const tableSpec = page.view?.table || {{}};
            const drawerSpec = page.view?.detailDrawer || {{}};
            
            const handleRowClick = (row) => {{
                setSelectedRow(row);
                setDrawerOpen(true);
            }};
            
            return (
                <div className="min-h-screen bg-gray-50">
                    <header className="bg-white border-b border-gray-200 px-6 py-4">
                        <h1 className="text-xl font-bold text-gray-900">{{spec.appName}}</h1>
                    </header>
                    
                    <main className="p-6">
                        <div className="mb-6">
                            <h2 className="text-lg font-semibold text-gray-800">{{page.title}}</h2>
                        </div>
                        
                        <TableView 
                            resourceId={{page.primaryResource}}
                            spec={{tableSpec}}
                            onRowClick={{handleRowClick}}
                        />
                    </main>
                    
                    <DetailDrawer
                        isOpen={{drawerOpen}}
                        onClose={{() => setDrawerOpen(false)}}
                        data={{selectedRow}}
                        spec={{drawerSpec}}
                    />
                </div>
            );
        }}
        
        // Render
        const root = ReactDOM.createRoot(document.getElementById('root'));
        root.render(<App />);
    </script>
</body>
</html>"""

    def _render_empty_preview(self, app: InternalApp) -> str:
        """Render empty state preview."""
        return f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{app.name} - Preview</title>
    <script src="https://cdn.tailwindcss.com"></script>
</head>
<body class="bg-gray-50 min-h-screen flex items-center justify-center px-4">
    <div class="bg-white border border-gray-200 rounded-lg shadow-sm max-w-lg w-full p-8 text-center">
        <div class="w-14 h-14 mx-auto mb-4 rounded-lg bg-gray-100 border border-gray-200 flex items-center justify-center">
            <svg class="h-8 w-8 text-gray-500" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6v12m6-6H6" />
            </svg>
        </div>
        <h1 class="text-xl font-semibold text-gray-900 mb-2">Create an App</h1>
        <p class="text-sm text-gray-600 mb-4">No versions found. Create your first version.</p>
        <div class="flex items-center justify-center gap-2">
            <span class="flex items-center gap-1.5 text-xs px-2.5 py-1 rounded-full bg-gray-100 text-gray-600 border border-gray-200">
                <svg class="h-3.5 w-3.5 text-gray-500" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6v12m6-6H6" />
                </svg>
                Draft
            </span>
        </div>
    </div>
</body>
</html>"""
