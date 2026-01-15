# Connectors/Integrations prompt template - injected when external integrations are available
CONNECTORS_PROMPT_TEMPLATE = """
## External Integrations

Your app can interact with external services through connectors. These integrations allow your app to create issues in Jira, send Slack messages, manage tasks in Linear, and more.

{connectors_context}

### Using Connectors in Code

The connectors client is available at `./lib/connectors.ts`:

```typescript
import {{ connectors }} from './lib/connectors';

// List available connectors
const available = await connectors.list();
// Returns: {{ connectors: [{{ id: 'jira', name: 'Jira', category: 'project_management', tool_count: 5 }}, ...] }}

// Get tools available for a specific connector
const jiraTools = await connectors.getTools('jira');
// Returns: {{ tools: [{{ id: 'create_issue', name: 'Create Issue', description: '...', parameters: {{...}} }}, ...] }}

// Execute a tool on a connector
const result = await connectors.execute('jira', 'create_issue', {{
  project: 'PROJ',
  summary: 'New task from app',
  description: 'Created via internal app',
  issue_type: 'Task'
}});
// Returns: {{ success: true, data: {{...}} }} or {{ success: false, error: '...' }}
```

### Connectors Client Implementation

If you need to generate the connectors client, use this template for `src/lib/connectors.ts`:

```typescript
const getConfig = () => {{
  const config = (window as any).__VECTOR_CONFIG__;
  return {{
    apiBaseUrl: config?.apiBaseUrl || 'http://localhost:8001/api/v1',
    appId: config?.appId || '',
  }};
}};

async function apiCall<T>(
  connectorId: string,
  toolId: string,
  params: Record<string, any>
): Promise<T> {{
  const config = getConfig();
  const response = await fetch(`${{config.apiBaseUrl}}/runtime/connectors/`, {{
    method: 'POST',
    headers: {{ 'Content-Type': 'application/json' }},
    body: JSON.stringify({{
      appId: config.appId,
      connectorId,
      toolId,
      params,
    }}),
  }});
  if (!response.ok) {{
    const error = await response.json().catch(() => ({{}}));
    throw new Error(error.error || 'Connector request failed');
  }}
  return response.json();
}}

interface Connector {{
  id: string;
  name: string;
  category: string;
  icon_url?: string;
  tool_count: number;
}}

interface Tool {{
  id: string;
  name: string;
  description: string;
  parameters: Record<string, any>;
}}

export const connectors = {{
  list: () => apiCall<{{connectors: Connector[]}}>('_meta', 'list', {{}}),
  getTools: (connectorId: string) => apiCall<{{tools: Tool[]}}>('_meta', 'tools', {{ connectorId }}),
  execute: <T = any>(connectorId: string, toolId: string, params: Record<string, any>) =>
    apiCall<{{success: boolean; data?: T; error?: string}}>(connectorId, toolId, params),
}};
```

### Common Patterns

1. **Creating items** - Use `create_*` tools (e.g., `connectors.execute('jira', 'create_issue', {{...}})`)
2. **Fetching data** - Use `get_*` or `list_*` tools
3. **Updating items** - Use `update_*` tools
4. **Sending notifications** - Use `send_message` or similar tools

### Error Handling

```typescript
const handleCreateTask = async () => {{
  setLoading(true);
  try {{
    const result = await connectors.execute('jira', 'create_issue', {{
      project: selectedProject,
      summary: taskTitle,
      description: taskDescription,
    }});
    
    if (result.success) {{
      showToast('Task created successfully!');
      // result.data contains the created issue details
    }} else {{
      showError(result.error || 'Failed to create task');
    }}
  }} catch (error) {{
    showError('Network error - please try again');
  }} finally {{
    setLoading(false);
  }}
}};
```
"""

