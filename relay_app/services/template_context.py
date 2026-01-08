"""
Template Context Service

Loads and formats the relay-manifest.json for injection into AI prompts.
This provides the AI with information about available pre-built components and hooks.
"""

import json
import os
from pathlib import Path
from typing import Any, Dict, Optional


def get_manifest_path() -> Path:
    """Get the path to the relay-manifest.json file."""
    # The manifest is located in the relay-template directory
    base_path = Path(__file__).parent.parent.parent.parent.parent
    manifest_path = base_path / "relay-template" / "relay-manifest.json"
    return manifest_path


def load_manifest() -> Optional[Dict[str, Any]]:
    """Load the relay-manifest.json file."""
    manifest_path = get_manifest_path()
    
    if not manifest_path.exists():
        return None
    
    try:
        with open(manifest_path, "r") as f:
            return json.load(f)
    except (json.JSONDecodeError, IOError):
        return None


def format_components_for_prompt(manifest: Dict[str, Any]) -> str:
    """Format component information for inclusion in prompts."""
    components = manifest.get("components", {})
    
    if not components:
        return ""
    
    lines = ["### Available UI Components\n"]
    
    for name, info in components.items():
        import_stmt = info.get("import", "")
        description = info.get("description", "")
        example = info.get("example", "")
        
        lines.append(f"**{name}**: {description}")
        lines.append(f"- Import: `{import_stmt}`")
        if example:
            lines.append(f"- Example: `{example}`")
        lines.append("")
    
    return "\n".join(lines)


def format_hooks_for_prompt(manifest: Dict[str, Any]) -> str:
    """Format hook information for inclusion in prompts."""
    hooks = manifest.get("hooks", {})
    
    if not hooks:
        return ""
    
    lines = ["### Available Hooks\n"]
    
    for name, info in hooks.items():
        import_stmt = info.get("import", "")
        description = info.get("description", "")
        returns = info.get("returns", "")
        example = info.get("example", "")
        
        lines.append(f"**{name}**: {description}")
        lines.append(f"- Import: `{import_stmt}`")
        if returns:
            lines.append(f"- Returns: `{returns}`")
        if example:
            lines.append(f"- Example: `{example}`")
        lines.append("")
    
    return "\n".join(lines)


def format_styling_guidelines(manifest: Dict[str, Any]) -> str:
    """Format styling guidelines for inclusion in prompts."""
    styling = manifest.get("styling", {})
    
    if not styling:
        return ""
    
    lines = ["### Styling Guidelines\n"]
    
    # Color tokens
    color_tokens = styling.get("colorTokens", {})
    if color_tokens:
        lines.append("**Color Tokens:**")
        for name, value in color_tokens.items():
            lines.append(f"- {name}: `{value}`")
        lines.append("")
    
    # Guidelines
    guidelines = styling.get("guidelines", [])
    if guidelines:
        lines.append("**Design Rules:**")
        for guideline in guidelines:
            lines.append(f"- {guideline}")
        lines.append("")
    
    return "\n".join(lines)


def get_template_context() -> str:
    """
    Get the full template context for injection into prompts.
    
    Returns a formatted string describing all available components,
    hooks, and styling guidelines from the relay-manifest.json.
    """
    manifest = load_manifest()
    
    if not manifest:
        # Return empty string if manifest not found - prompts will use fallback
        return ""
    
    sections = []
    
    # Add version info
    version = manifest.get("version", "1.0")
    description = manifest.get("description", "")
    sections.append(f"## Template Library (v{version})\n")
    if description:
        sections.append(f"{description}\n")
    
    # Add components
    components_section = format_components_for_prompt(manifest)
    if components_section:
        sections.append(components_section)
    
    # Add hooks
    hooks_section = format_hooks_for_prompt(manifest)
    if hooks_section:
        sections.append(hooks_section)
    
    # Add styling
    styling_section = format_styling_guidelines(manifest)
    if styling_section:
        sections.append(styling_section)
    
    return "\n".join(sections)


def get_component_info(component_name: str) -> Optional[Dict[str, Any]]:
    """Get information about a specific component."""
    manifest = load_manifest()
    if not manifest:
        return None
    
    components = manifest.get("components", {})
    return components.get(component_name)


def get_hook_info(hook_name: str) -> Optional[Dict[str, Any]]:
    """Get information about a specific hook."""
    manifest = load_manifest()
    if not manifest:
        return None
    
    hooks = manifest.get("hooks", {})
    return hooks.get(hook_name)


def get_all_component_names() -> list:
    """Get list of all available component names."""
    manifest = load_manifest()
    if not manifest:
        return []
    
    return list(manifest.get("components", {}).keys())


def get_all_hook_names() -> list:
    """Get list of all available hook names."""
    manifest = load_manifest()
    if not manifest:
        return []
    
    return list(manifest.get("hooks", {}).keys())

