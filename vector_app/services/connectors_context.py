"""
Connectors Context Generator for LLM Consumption.

Provides functions to generate clean, structured string representations
of available connectors for use in LLM prompts during code generation.

Similar pattern to data_store_context.py but for external integrations.
"""

from typing import Any, Dict, List, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from ..models import InternalApp, ConnectorCache


def build_connectors_context(app: "InternalApp", include_tools: bool = True) -> str:
    """
    Generate an LLM-friendly representation of available connectors.

    Only shows connectors that the organization has connected.

    Args:
        app: The InternalApp to describe connectors for
        include_tools: Whether to include tool details

    Returns:
        A formatted string describing connectors and available operations.
    """
    from ..models import ConnectorCache, MergeIntegrationProvider, OrganizationConnectorLink

    # Get the organization's integration provider
    try:
        provider = MergeIntegrationProvider.objects.get(
            organization=app.organization,
            is_active=True,
        )
    except MergeIntegrationProvider.DoesNotExist:
        return _build_no_integrations_context()

    # Get organization's connected connector IDs
    connected_ids = set(
        OrganizationConnectorLink.objects.filter(
            provider=provider,
            is_connected=True,
        ).values_list("connector__connector_id", flat=True)
    )

    # Get only connected connectors
    connectors = ConnectorCache.objects.filter(
        provider=provider,
        is_enabled=True,
        connector_id__in=connected_ids,
    ).order_by("connector_name")

    if not connectors.exists():
        return _build_no_connected_context()

    sections = ["## External Integrations\n"]
    sections.append(_build_connectors_section(list(connectors), include_tools))
    sections.append(_build_usage_section())

    return "\n".join(sections)


def _build_no_integrations_context() -> str:
    """Build context when no integration provider is configured."""
    return """## External Integrations

No integration provider is configured for this organization.

To enable external integrations (Jira, Slack, Linear, etc.), configure an 
integration provider in the organization settings.
"""


def _build_no_connectors_context() -> str:
    """Build context when no connectors are available."""
    return """## External Integrations

An integration provider is configured, but no connectors have been synced yet.

Use the "Sync Connectors" action in organization settings to fetch available 
connectors from the integration provider.
"""


def _build_no_connected_context() -> str:
    """Build context when no connectors are connected."""
    return """## External Integrations

An integration provider is configured, but no connectors have been connected yet.

An organization member needs to connect integrations (Jira, Slack, etc.) in the 
Resources page before they can be used in apps.
"""


def _build_connectors_section(connectors: List["ConnectorCache"], include_tools: bool) -> str:
    """Build the section describing available connectors."""
    lines = ["### Available Connectors\n"]

    # Group by category
    by_category: Dict[str, List] = {}
    for connector in connectors:
        category = connector.category or "other"
        if category not in by_category:
            by_category[category] = []
        by_category[category].append(connector)

    for category, category_connectors in sorted(by_category.items()):
        category_display = category.replace("_", " ").title()
        lines.append(f"**{category_display}**\n")

        for connector in category_connectors:
            lines.append(_format_connector(connector, include_tools))

        lines.append("")  # Empty line between categories

    return "\n".join(lines)


def _format_connector(connector: "ConnectorCache", include_tools: bool) -> str:
    """Format a single connector's description."""
    lines = [f"**{connector.connector_name}** (`{connector.connector_id}`)"]

    if connector.description:
        lines.append(f"  {connector.description}")

    if include_tools:
        tools = connector.tools_json or []
        if tools:
            lines.append("\nAvailable tools:")
            for tool in tools[:10]:  # Limit to 10 tools for readability
                tool_id = tool.get("id", tool.get("name", ""))
                tool_name = tool.get("name", tool_id)
                description = tool.get("description", "")

                if description:
                    # Truncate long descriptions
                    if len(description) > 80:
                        description = description[:77] + "..."
                    lines.append(f"- `{tool_id}`: {description}")
                else:
                    lines.append(f"- `{tool_id}`: {tool_name}")

            if len(tools) > 10:
                lines.append(f"  ... and {len(tools) - 10} more tools")

    return "\n".join(lines)


def _build_usage_section() -> str:
    """Build the section describing how to use connectors in code."""
    return """### Using Connectors in Code

Use the `connectors` client from `./lib/connectors.ts` to interact with external services:

```typescript
import { connectors } from './lib/connectors';

// List available connectors
const available = await connectors.list();

// Execute a tool on a connector
const result = await connectors.execute('jira', 'create_issue', {
  project: 'PROJ',
  summary: 'New task from app',
  description: 'Created via internal app',
  issue_type: 'Task'
});

// Handle the result
if (result.success) {
  console.log('Issue created:', result.data);
} else {
  console.error('Failed:', result.error);
}

// Get available tools for a specific connector
const jiraTools = await connectors.getTools('jira');
```

**Common Patterns:**

1. **Creating items** - Use `create_*` tools (e.g., `create_issue`, `create_task`)
2. **Fetching data** - Use `get_*` or `list_*` tools
3. **Updating items** - Use `update_*` tools
4. **Sending messages** - Use `send_message` or similar tools

**Error Handling:**

```typescript
try {
  const result = await connectors.execute('slack', 'send_message', {
    channel: '#general',
    message: 'Hello from the app!'
  });
  
  if (!result.success) {
    // Handle API-level errors
    showError(result.error);
  }
} catch (error) {
  // Handle network or unexpected errors
  showError('Failed to connect to Slack');
}
```
"""


def get_connectors_summary(app: "InternalApp") -> str:
    """
    Get a brief summary of connected connectors (for use in planning prompts).

    Only shows connectors that the organization has connected.

    Args:
        app: The InternalApp to summarize connectors for

    Returns:
        A brief string listing connector names and tool counts.
    """
    from ..models import ConnectorCache, MergeIntegrationProvider, OrganizationConnectorLink

    try:
        provider = MergeIntegrationProvider.objects.get(
            organization=app.organization,
            is_active=True,
        )
    except MergeIntegrationProvider.DoesNotExist:
        return "No external integrations configured."

    # Get organization's connected connector IDs
    connected_ids = set(
        OrganizationConnectorLink.objects.filter(
            provider=provider,
            is_connected=True,
        ).values_list("connector__connector_id", flat=True)
    )

    if not connected_ids:
        return "No external integrations connected."

    connectors = ConnectorCache.objects.filter(
        provider=provider,
        is_enabled=True,
        connector_id__in=connected_ids,
    ).order_by("connector_name")

    if not connectors.exists():
        return "No connectors available."

    summaries = []
    for connector in connectors:
        tool_count = len(connector.tools_json or [])
        summaries.append(f"- {connector.connector_name} ({connector.connector_id}): {tool_count} tools")

    return "Connected integrations:\n" + "\n".join(summaries)


def get_connector_tools_for_llm(connector: "ConnectorCache") -> str:
    """
    Get a detailed description of a connector's tools for LLM use.

    Args:
        connector: The ConnectorCache to describe

    Returns:
        A formatted string with tool details and parameter info.
    """
    lines = [f"## {connector.connector_name} Tools\n"]

    tools = connector.tools_json or []
    if not tools:
        lines.append("No tools available for this connector.")
        return "\n".join(lines)

    for tool in tools:
        tool_id = tool.get("id", tool.get("name", ""))
        tool_name = tool.get("name", tool_id)
        description = tool.get("description", "No description")
        parameters = tool.get("parameters", {})

        lines.append(f"### `{tool_id}`")
        lines.append(f"{description}\n")

        if parameters:
            lines.append("**Parameters:**")

            # Handle different parameter formats
            if isinstance(parameters, dict):
                props = parameters.get("properties", parameters)
                required = parameters.get("required", [])

                for param_name, param_info in props.items():
                    if isinstance(param_info, dict):
                        param_type = param_info.get("type", "any")
                        param_desc = param_info.get("description", "")
                        is_required = param_name in required

                        req_marker = " (required)" if is_required else ""
                        lines.append(f"- `{param_name}`: {param_type}{req_marker}")
                        if param_desc:
                            lines.append(f"  {param_desc}")
                    else:
                        lines.append(f"- `{param_name}`: {param_info}")

        lines.append("")  # Empty line between tools

    return "\n".join(lines)
