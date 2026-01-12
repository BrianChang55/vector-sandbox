"""
Context Analyzer Service

Deep analysis of existing app state for intent-aware routing.
Parses files, extracts component structure, and builds dependency graphs.
"""

import logging
import re
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set, TYPE_CHECKING

if TYPE_CHECKING:
    from vector_app.models import InternalApp, AppVersion, VersionFile, AppDataTable

logger = logging.getLogger(__name__)


@dataclass
class FileInfo:
    """Information about a single file."""

    path: str
    size: int
    language: str
    exports: List[str] = field(default_factory=list)
    imports: List[str] = field(default_factory=list)
    is_component: bool = False
    is_entry_point: bool = False


@dataclass
class CodebaseStyle:
    """
    Style fingerprint of the codebase.

    Used to ensure generated code matches existing patterns.
    """

    naming_convention: str = "camelCase"  # camelCase, snake_case, PascalCase
    component_pattern: str = "functional"  # functional, arrow, class
    state_management: str = "useState"  # useState, useReducer, context, redux
    uses_semicolons: bool = True
    quote_style: str = "single"  # single, double
    indent_style: str = "spaces"  # spaces, tabs
    common_imports: List[str] = field(default_factory=list)
    existing_utilities: List[str] = field(default_factory=list)

    def to_prompt_context(self) -> str:
        """Generate a prompt snippet describing the codebase style."""
        parts = [
            "## Detected Codebase Style (MATCH THIS)",
            f"- Naming: {self.naming_convention}",
            f"- Components: {self.component_pattern} functions",
            f"- State: {self.state_management}",
            f"- Semicolons: {'Yes' if self.uses_semicolons else 'No'}",
            f"- Quotes: {self.quote_style}",
        ]
        if self.existing_utilities:
            parts.append(f"- Existing utilities: {', '.join(self.existing_utilities[:5])}")
        return "\n".join(parts)


@dataclass
class TableInfo:
    """Information about a data table."""

    slug: str
    name: str
    column_count: int
    columns: List[str] = field(default_factory=list)
    row_count: int = 0


@dataclass
class AppContext:
    """
    Complete context of an existing app.

    Used by handlers to understand what exists and make
    intelligent decisions about what to modify vs generate.
    """

    has_existing_app: bool
    file_count: int
    existing_files: List[FileInfo] = field(default_factory=list)
    existing_tables: List[TableInfo] = field(default_factory=list)
    component_graph: Dict[str, List[str]] = field(default_factory=dict)
    reverse_graph: Dict[str, List[str]] = field(default_factory=dict)  # Files that import each file
    entry_points: List[str] = field(default_factory=list)
    codebase_style: Optional[CodebaseStyle] = None

    # Quick lookups
    file_paths: Set[str] = field(default_factory=set)
    component_names: Set[str] = field(default_factory=set)
    table_slugs: Set[str] = field(default_factory=set)

    def get_file(self, path: str) -> Optional[FileInfo]:
        """Get file info by path."""
        for f in self.existing_files:
            if f.path == path:
                return f
        return None

    def get_table(self, slug: str) -> Optional[TableInfo]:
        """Get table info by slug."""
        for t in self.existing_tables:
            if t.slug == slug:
                return t
        return None

    def to_summary(self) -> str:
        """Generate a human-readable summary of the context."""
        parts = []

        if self.has_existing_app:
            parts.append(f"Existing app with {self.file_count} files")
            if self.entry_points:
                parts.append(f"Entry points: {', '.join(self.entry_points[:3])}")
            if self.existing_tables:
                parts.append(f"Tables: {', '.join([t.name for t in self.existing_tables[:3]])}")
        else:
            parts.append("No existing app")

        return ". ".join(parts)


class ContextAnalyzer:
    """
    Analyzes existing app state to provide rich context for handlers.

    Extracts:
    - File structure and sizes
    - Component exports and imports
    - Data table schemas
    - Dependency graphs
    """

    def analyze(
        self,
        app: Optional["InternalApp"],
        version: Optional["AppVersion"] = None,
    ) -> AppContext:
        """
        Analyze the current state of an app.

        Args:
            app: The InternalApp to analyze
            version: Specific version to analyze (uses latest if None)

        Returns:
            AppContext with all analyzed information
        """
        if not app:
            return AppContext(
                has_existing_app=False,
                file_count=0,
            )

        # Get the version to analyze
        if not version:
            version = self._get_latest_version(app)

        if not version:
            return AppContext(
                has_existing_app=False,
                file_count=0,
            )

        # Analyze files
        files = self._analyze_files(version)

        # Analyze tables
        tables = self._analyze_tables(app)

        # Build component graph and reverse graph
        component_graph = self._build_component_graph(files)
        reverse_graph = self._build_reverse_graph(component_graph)

        # Identify entry points
        entry_points = self._find_entry_points(files)

        # Analyze codebase style
        codebase_style = self._analyze_codebase_style(version, files)

        return AppContext(
            has_existing_app=len(files) > 0,
            file_count=len(files),
            existing_files=files,
            existing_tables=tables,
            component_graph=component_graph,
            reverse_graph=reverse_graph,
            entry_points=entry_points,
            codebase_style=codebase_style,
            file_paths=set(f.path for f in files),
            component_names=set(f.exports[0] for f in files if f.exports and f.is_component),
            table_slugs=set(t.slug for t in tables),
        )

    def _get_latest_version(self, app: "InternalApp") -> Optional["AppVersion"]:
        """Get the latest stable version of an app."""
        from vector_app.models import AppVersion

        return (
            AppVersion.objects.filter(
                internal_app=app,
                is_active=True,
                generation_status=AppVersion.GEN_STATUS_COMPLETE,
            )
            .order_by("-version_number")
            .first()
        )

    def _analyze_files(self, version: "AppVersion") -> List[FileInfo]:
        """Analyze all files in a version."""
        files = []

        try:
            version_files = version.files.all()

            for vf in version_files:
                file_info = self._analyze_single_file(vf)
                if file_info:
                    files.append(file_info)

        except Exception as e:
            logger.warning(f"Error analyzing files: {e}")

        return files

    def _analyze_single_file(self, version_file: "VersionFile") -> Optional[FileInfo]:
        """Analyze a single file for exports, imports, and structure."""
        path = version_file.path
        content = version_file.content or ""

        # Determine language from extension
        ext = path.split(".")[-1] if "." in path else ""
        lang_map = {
            "tsx": "tsx",
            "ts": "ts",
            "jsx": "jsx",
            "js": "js",
            "css": "css",
            "json": "json",
        }
        language = lang_map.get(ext, "unknown")

        # Skip non-code files
        if language not in ("tsx", "ts", "jsx", "js"):
            return FileInfo(
                path=path,
                size=len(content),
                language=language,
            )

        # Extract exports
        exports = self._extract_exports(content)

        # Extract imports
        imports = self._extract_imports(content)

        # Check if it's a React component
        is_component = self._is_react_component(content, exports)

        # Check if it's an entry point
        is_entry_point = path in ("src/App.tsx", "src/App.jsx", "src/index.tsx", "src/main.tsx")

        return FileInfo(
            path=path,
            size=len(content),
            language=language,
            exports=exports,
            imports=imports,
            is_component=is_component,
            is_entry_point=is_entry_point,
        )

    def _extract_exports(self, content: str) -> List[str]:
        """Extract exported names from a file."""
        exports = []

        # export default function/class Name
        default_matches = re.findall(r"export\s+default\s+(?:function|class|const)\s+(\w+)", content)
        exports.extend(default_matches)

        # export default Name
        default_direct = re.findall(r"export\s+default\s+(\w+)\s*[;\n]", content)
        exports.extend(default_direct)

        # export function/class/const Name
        named_matches = re.findall(r"export\s+(?:function|class|const|let|var)\s+(\w+)", content)
        exports.extend(named_matches)

        # export { Name, Name2 }
        bracket_exports = re.findall(r"export\s*\{([^}]+)\}", content)
        for match in bracket_exports:
            names = [n.strip().split(" as ")[0].strip() for n in match.split(",")]
            exports.extend(names)

        return list(set(exports))

    def _extract_imports(self, content: str) -> List[str]:
        """Extract import paths from a file."""
        imports = []

        # import ... from 'path'
        import_matches = re.findall(r'import\s+.*?\s+from\s+[\'"]([^\'"]+)[\'"]', content)
        imports.extend(import_matches)

        # import 'path' (side effect imports)
        side_effect = re.findall(r'^import\s+[\'"]([^\'"]+)[\'"]', content, re.MULTILINE)
        imports.extend(side_effect)

        return imports

    def _is_react_component(self, content: str, exports: List[str]) -> bool:
        """Check if a file exports a React component."""
        # Check for JSX
        has_jsx = "</" in content or "/>" in content

        # Check for React imports
        has_react = "from 'react'" in content or 'from "react"' in content

        # Check for function returning JSX
        has_return_jsx = re.search(r"return\s*\(?\s*<", content) is not None

        return (has_jsx or has_return_jsx) and (has_react or has_jsx)

    def _analyze_tables(self, app: "InternalApp") -> List[TableInfo]:
        """Analyze data tables for the app."""
        tables = []

        try:
            from vector_app.models import AppDataTable

            app_tables = AppDataTable.objects.filter(
                internal_app=app,
            )

            for table in app_tables:
                schema = table.schema_json or {}
                columns = schema.get("columns", [])
                column_names = [c.get("name", "") for c in columns if c.get("name")]

                tables.append(
                    TableInfo(
                        slug=table.slug,
                        name=table.name,
                        column_count=len(columns),
                        columns=column_names,
                        row_count=table.rows.count() if hasattr(table, "rows") else 0,
                    )
                )

        except Exception as e:
            logger.warning(f"Error analyzing tables: {e}")

        return tables

    def _build_component_graph(self, files: List[FileInfo]) -> Dict[str, List[str]]:
        """
        Build a dependency graph of components.

        Returns a dict mapping file paths to list of files they import.
        """
        graph = {}

        # Build a map of export names to file paths
        export_to_file = {}
        for f in files:
            for export in f.exports:
                export_to_file[export] = f.path

        # Build the graph
        for f in files:
            dependencies = []

            for imp in f.imports:
                # Skip external imports
                if not imp.startswith(".") and not imp.startswith("/"):
                    continue

                # Resolve relative imports to file paths
                # This is a simplified resolution
                if imp.startswith("./"):
                    imp_path = imp[2:]
                elif imp.startswith("../"):
                    imp_path = imp
                else:
                    imp_path = imp

                # Find matching file
                for other in files:
                    if imp_path in other.path or other.path.endswith(f"{imp_path}.tsx"):
                        dependencies.append(other.path)
                        break

            if dependencies:
                graph[f.path] = dependencies

        return graph

    def _find_entry_points(self, files: List[FileInfo]) -> List[str]:
        """Find the main entry point files."""
        entry_points = []

        # Standard entry points
        standard_entries = ["src/App.tsx", "src/App.jsx", "src/index.tsx", "src/main.tsx"]

        for f in files:
            if f.path in standard_entries:
                entry_points.append(f.path)
            elif f.is_entry_point:
                entry_points.append(f.path)

        # Also include files with export default that look like main components
        for f in files:
            if f.is_component and "App" in (f.exports or []):
                if f.path not in entry_points:
                    entry_points.append(f.path)

        return entry_points

    def find_affected_files(
        self,
        context: AppContext,
        keywords: List[str],
    ) -> List[str]:
        """
        Find files that are likely affected by a change.

        Uses keyword matching against file paths and exports.
        """
        affected = []
        keywords_lower = [k.lower() for k in keywords]

        for f in context.existing_files:
            # Check path
            path_lower = f.path.lower()
            if any(kw in path_lower for kw in keywords_lower):
                affected.append(f.path)
                continue

            # Check exports
            for export in f.exports:
                if any(kw in export.lower() for kw in keywords_lower):
                    affected.append(f.path)
                    break

        return affected

    def get_file_contents(
        self,
        version: "AppVersion",
        file_paths: List[str],
    ) -> Dict[str, str]:
        """Get the contents of specific files from a version."""
        contents = {}

        try:
            for vf in version.files.filter(path__in=file_paths):
                contents[vf.path] = vf.content or ""
        except Exception as e:
            logger.warning(f"Error getting file contents: {e}")

        return contents

    def _build_reverse_graph(self, component_graph: Dict[str, List[str]]) -> Dict[str, List[str]]:
        """
        Build reverse dependency graph.

        Maps each file to the list of files that import it.
        Used for cascade-affected-files detection.
        """
        reverse = {}

        for file_path, imports in component_graph.items():
            for imported_file in imports:
                if imported_file not in reverse:
                    reverse[imported_file] = []
                reverse[imported_file].append(file_path)

        return reverse

    def _analyze_codebase_style(
        self,
        version: "AppVersion",
        files: List[FileInfo],
    ) -> Optional[CodebaseStyle]:
        """
        Analyze the codebase to detect coding style patterns.

        Examines a sample of files to determine:
        - Naming conventions
        - Component patterns
        - State management approach
        - Quote and semicolon styles
        """
        if not files:
            return None

        try:
            # Get content of a few representative files
            sample_paths = [f.path for f in files if f.is_component][:3]
            if not sample_paths and files:
                sample_paths = [f.path for f in files[:3]]

            contents = self.get_file_contents(version, sample_paths)
            if not contents:
                return None

            # Analyze combined content
            all_content = "\n".join(contents.values())

            # Detect component pattern
            component_pattern = "functional"
            if "class " in all_content and "extends" in all_content:
                component_pattern = "class"
            elif "const " in all_content and " = (" in all_content and "=>" in all_content:
                component_pattern = "arrow"

            # Detect state management
            state_management = "useState"
            if "useReducer" in all_content:
                state_management = "useReducer"
            elif "createContext" in all_content or "useContext" in all_content:
                state_management = "context"
            elif "useSelector" in all_content or "useDispatch" in all_content:
                state_management = "redux"

            # Detect semicolons
            lines_with_semicolon = all_content.count(";\n")
            lines_without = all_content.count("\n") - lines_with_semicolon
            uses_semicolons = lines_with_semicolon > lines_without * 0.5

            # Detect quote style
            single_quotes = all_content.count("'")
            double_quotes = all_content.count('"')
            quote_style = "single" if single_quotes > double_quotes else "double"

            # Find existing utilities
            utilities = []
            utility_patterns = ["utils/", "lib/", "helpers/", "hooks/"]
            for f in files:
                for pattern in utility_patterns:
                    if pattern in f.path:
                        utilities.extend(f.exports)

            # Detect common imports
            common_imports = []
            import_counts = {}
            for f in files:
                for imp in f.imports:
                    if not imp.startswith("."):
                        import_counts[imp] = import_counts.get(imp, 0) + 1
            common_imports = sorted(import_counts.keys(), key=lambda x: -import_counts[x])[:5]

            return CodebaseStyle(
                component_pattern=component_pattern,
                state_management=state_management,
                uses_semicolons=uses_semicolons,
                quote_style=quote_style,
                common_imports=common_imports,
                existing_utilities=utilities[:10],
            )

        except Exception as e:
            logger.warning(f"Error analyzing codebase style: {e}")
            return None

    def find_cascade_affected_files(
        self,
        context: AppContext,
        primary_file: str,
        max_depth: int = 2,
    ) -> List[str]:
        """
        Find all files affected by changes to a primary file.

        Uses the component dependency graph to find:
        1. Files that import the primary file (consumers)
        2. Files that the primary file imports (dependencies)

        Args:
            context: The app context with component graph
            primary_file: The file being modified
            max_depth: How many levels of imports to traverse

        Returns:
            List of file paths that may be affected
        """
        affected = {primary_file}

        # Files that import the primary file (may need prop/interface updates)
        if primary_file in context.reverse_graph:
            for consumer in context.reverse_graph[primary_file]:
                affected.add(consumer)

        # Files that the primary file imports (understand dependencies)
        if primary_file in context.component_graph:
            for dependency in context.component_graph[primary_file]:
                affected.add(dependency)

        # Second level: files that import the consumers
        if max_depth > 1:
            first_level = list(affected)
            for file_path in first_level:
                if file_path in context.reverse_graph:
                    for consumer in context.reverse_graph[file_path]:
                        affected.add(consumer)

        return list(affected)

    def find_reusable_components(
        self,
        context: AppContext,
        feature_request: str,
    ) -> List[Dict[str, Any]]:
        """
        Find existing components that could be reused for a feature.

        Analyzes the feature request and matches against existing
        component names and exports.

        Args:
            context: The app context
            feature_request: The user's feature request

        Returns:
            List of dicts with 'path', 'exports', 'relevance' keys
        """
        # Extract keywords from the feature request
        keywords = self._extract_feature_keywords(feature_request)
        if not keywords:
            return []

        reusable = []

        for f in context.existing_files:
            if not f.is_component:
                continue

            # Calculate relevance score
            relevance = 0
            matched_keywords = []

            # Check file path
            path_lower = f.path.lower()
            for kw in keywords:
                if kw in path_lower:
                    relevance += 2
                    matched_keywords.append(kw)

            # Check exports
            for export in f.exports:
                export_lower = export.lower()
                for kw in keywords:
                    if kw in export_lower:
                        relevance += 3
                        if kw not in matched_keywords:
                            matched_keywords.append(kw)

            if relevance > 0:
                reusable.append(
                    {
                        "path": f.path,
                        "exports": f.exports,
                        "relevance": relevance,
                        "matched_keywords": matched_keywords,
                    }
                )

        # Sort by relevance
        reusable.sort(key=lambda x: -x["relevance"])

        return reusable[:5]  # Return top 5 matches

    def _extract_feature_keywords(self, feature_request: str) -> List[str]:
        """Extract meaningful keywords from a feature request."""
        # Common UI component keywords
        component_keywords = {
            "table",
            "list",
            "form",
            "input",
            "button",
            "modal",
            "dialog",
            "card",
            "header",
            "footer",
            "sidebar",
            "menu",
            "nav",
            "navigation",
            "dropdown",
            "select",
            "checkbox",
            "radio",
            "toggle",
            "switch",
            "tabs",
            "tab",
            "panel",
            "accordion",
            "tooltip",
            "popover",
            "search",
            "filter",
            "sort",
            "pagination",
            "loading",
            "spinner",
            "alert",
            "toast",
            "notification",
            "badge",
            "avatar",
            "icon",
            "chart",
            "graph",
            "dashboard",
            "grid",
            "layout",
            "container",
        }

        # Tokenize and filter
        words = re.findall(r"\b[a-z]+\b", feature_request.lower())
        keywords = [w for w in words if w in component_keywords or len(w) > 4]

        # Deduplicate while preserving order
        seen = set()
        unique = []
        for kw in keywords:
            if kw not in seen:
                seen.add(kw)
                unique.append(kw)

        return unique[:10]  # Limit to 10 keywords

    def build_reusable_components_prompt(
        self,
        context: AppContext,
        feature_request: str,
    ) -> Optional[str]:
        """
        Build a prompt section listing reusable components.

        Returns None if no reusable components are found.
        """
        reusable = self.find_reusable_components(context, feature_request)
        if not reusable:
            return None

        lines = [
            "## Existing Components You Can Reuse",
            "",
            "IMPORTANT: Prefer extending or composing these existing components over creating new ones.",
            "",
        ]

        for comp in reusable:
            exports = ", ".join(comp["exports"][:3])
            lines.append(f"- `{comp['path']}`: exports {exports}")
            if comp["matched_keywords"]:
                lines.append(f"  (matches: {', '.join(comp['matched_keywords'])})")

        return "\n".join(lines)


# Singleton instance
_context_analyzer: Optional[ContextAnalyzer] = None


def get_context_analyzer() -> ContextAnalyzer:
    """Get singleton context analyzer instance."""
    global _context_analyzer
    if _context_analyzer is None:
        _context_analyzer = ContextAnalyzer()
    return _context_analyzer
