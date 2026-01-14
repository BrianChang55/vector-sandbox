"""
Schema Extraction Service

Extracts database schema from execution plans using LLM analysis.
"""
import json
import logging
import re
from typing import Any, Dict, List, Optional

from django.conf import settings
import httpx

from vector_app.services.openrouter_service import AIModel
from vector_app.services.planning_service import PlanStep


logger = logging.getLogger(__name__)
FORCE_OPUS_SCHEMA_EXTRACTION = True

class SchemaExtractionService:
    """
    Service for extracting database schema from execution plans.
    
    Analyzes the plan steps and descriptions to determine what database
    tables and fields are needed, then generates TABLE_DEFINITION blocks.
    """
    
    OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions"
    
    def __init__(self):
        self.api_key = getattr(settings, 'OPENROUTER_API_KEY', None) or \
                      getattr(settings, 'OPENAI_API_KEY', None)
        self.app_name = getattr(settings, 'OPENROUTER_APP_NAME', 'Internal Apps Builder')
        self.site_url = getattr(settings, 'BASE_URL', 'http://localhost:8001')
    
    def _build_headers(self) -> Dict[str, str]:
        """Build API headers."""
        return {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json",
            "HTTP-Referer": self.site_url,
            "X-Title": self.app_name,
        }
    
    def extract_schema_from_plan(
        self,
        plan: Any,
        user_message: str,
        model: str = AIModel.CLAUDE_OPUS_4_5.value,
    ) -> str:
        """
        Extract database schema from a plan.
        
        Args:
            plan: The AgentPlan object
            user_message: The original user request
            model: LLM model to use for extraction
            
        Returns:
            String containing TABLE_DEFINITION blocks for all needed tables
        """
        prompt = self._build_extraction_prompt(plan, user_message)
        if FORCE_OPUS_SCHEMA_EXTRACTION:
            model = AIModel.CLAUDE_OPUS_4_5.value
        try:
            with httpx.Client(timeout=180.0) as client:
                response = client.post(
                    self.OPENROUTER_API_URL,
                    headers=self._build_headers(),
                    json={
                        "model": model,
                        "messages": [
                            {"role": "user", "content": prompt}
                        ],
                        "temperature": 0.2,
                    },
                )
                response.raise_for_status()
                
                result = response.json()
                content = result["choices"][0]["message"]["content"]
                
                logger.info("=" * 80)
                logger.info("📊 SCHEMA EXTRACTION RESULT")
                logger.info("=" * 80)
                logger.info(content)
                logger.info("=" * 80)
                
                return content
                
        except Exception as e:
            logger.error(f"Schema extraction error: {e}")
            return ""
    
    def _build_extraction_prompt(self, plan: List[PlanStep], user_message: str) -> str:
        """Build prompt for schema extraction."""
        # Serialize plan steps
        steps_text = []
        for i, step in enumerate(plan.steps, 1):
            steps_text.append(f"**Step {i}: {step.title}**")
            steps_text.append(f"Type: {step.type}")
            steps_text.append(f"Description: {step.description}")
            if step.target_files:
                steps_text.append(f"Files: {', '.join(step.target_files)}")
            steps_text.append("")

        steps_str = "\n".join(steps_text)

        return f"""You are a database schema designer. Analyze this execution plan and extract ALL database tables needed.

**User's Original Request:**
{user_message}

**Execution Plan:**
{plan.reasoning}

**Steps:**
{steps_str}

**Your Task:**
1. **READ EVERY STEP CAREFULLY** - Each step description may mention entities, lists, forms, or relationships that require database tables
2. **Identify ALL entities** that need to be stored:
   - Look for nouns: "tasks", "projects", "team members", "comments", "categories"
   - Look for relationships: "assign users to projects", "tasks belong to projects", "members on teams"
   - Look for forms: "TaskForm", "UserForm" → these create/edit data, so they need tables
   - Look for lists/displays: "TaskList", "ProjectBoard" → these display data from tables
3. **For EACH entity, determine:**
   - Table name (use kebab-case slug like 'tasks', 'team-members', 'project-assignments')
   - Display name (human-readable like "Tasks", "Team Members")
   - All fields needed based on the step descriptions
   - Field types: string, text, integer, float, boolean, datetime, date, enum, json, reference
   - Field constraints: nullable, unique, default values
   - Relationships: Use 'reference' type for foreign keys

**Available Field Types:**
- `string` - Short text (max 255 chars), for names, titles, emails
- `text` - Long text (unlimited), for descriptions, notes, content
- `integer` - Whole numbers, for counts, IDs, quantities
- `float` - Decimal numbers, for prices, percentages, measurements
- `boolean` - True/false, for flags, toggles
- `datetime` - Date and time with timezone, for timestamps
- `date` - Date only, for birthdays, due dates, deadlines
- `enum` - Fixed set of values, for status, priority, category
  - MUST include `enum_values: [value1, value2, value3]`
- `json` - Arbitrary JSON data, for flexible structured data
- `reference` - Foreign key to another table's 'id' field
  - MUST include `reference_table: other-table-slug`
  - Automatically references the 'id' column of the target table

**Critical Rules:**
- **DO NOT define 'id', 'created_at', or 'updated_at'** - these are auto-generated for EVERY table
- **Identify join/junction tables** - When you see "assign X to Y" or "X belongs to Y", you often need:
  1. Table for X
  2. Table for Y
  3. Join table (e.g., 'project-members' linking projects ↔ members)
- **Look for implicit relationships** - "project members", "task assignments", "user roles" often need separate tables
- Use `nullable: false` ONLY for truly required fields
- Use `enum` for status/priority fields with fixed values (include enum_values array!)
- Use `reference` for foreign keys (include reference_table!)

**Example 1: Simple Entity**
If plan mentions "task list showing title, status, and due date":

{{
  "tables": [
    {{
      "slug": "tasks",
      "name": "Tasks",
      "description": "User tasks with status tracking",
      "columns": [
        {{"name": "title", "type": "string", "nullable": false}},
        {{"name": "status", "type": "enum", "enum_values": ["todo", "in_progress", "done"], "default": "todo"}},
        {{"name": "due_date", "type": "date"}},
        {{"name": "description", "type": "text"}}
      ]
    }}
  ]
}}

**Example 2: Relationships & Foreign Keys**
If plan mentions "projects with tasks, assign team members to projects":

{{
  "tables": [
    {{
      "slug": "projects",
      "name": "Projects",
      "description": "Project management records",
      "columns": [
        {{"name": "name", "type": "string", "nullable": false}},
        {{"name": "description", "type": "text"}},
        {{"name": "status", "type": "enum", "enum_values": ["planning", "active", "completed", "archived"], "default": "planning"}},
        {{"name": "start_date", "type": "date"}},
        {{"name": "end_date", "type": "date"}}
      ]
    }},
    {{
      "slug": "tasks",
      "name": "Tasks",
      "description": "Tasks within projects",
      "columns": [
        {{"name": "title", "type": "string", "nullable": false}},
        {{"name": "description", "type": "text"}},
        {{"name": "project_id", "type": "reference", "reference_table": "projects", "nullable": false}},
        {{"name": "status", "type": "enum", "enum_values": ["todo", "in_progress", "done"], "default": "todo"}},
        {{"name": "priority", "type": "enum", "enum_values": ["low", "medium", "high"], "default": "medium"}},
        {{"name": "due_date", "type": "date"}}
      ]
    }},
    {{
      "slug": "team-members",
      "name": "Team Members",
      "description": "Team member profiles",
      "columns": [
        {{"name": "name", "type": "string", "nullable": false}},
        {{"name": "email", "type": "string", "nullable": false, "unique": true}},
        {{"name": "role", "type": "string"}},
        {{"name": "avatar_url", "type": "string"}}
      ]
    }},
    {{
      "slug": "project-members",
      "name": "Project Members",
      "description": "Assignment of team members to projects (join table)",
      "columns": [
        {{"name": "project_id", "type": "reference", "reference_table": "projects", "nullable": false}},
        {{"name": "member_id", "type": "reference", "reference_table": "team-members", "nullable": false}},
        {{"name": "role", "type": "string"}},
        {{"name": "joined_at", "type": "datetime"}}
      ]
    }}
  ]
}}

**Verification Checklist (CRITICAL - DO THIS BEFORE RESPONDING):**
□ Read EVERY step description thoroughly
□ For each noun mentioned (tasks, users, projects, etc.), did I create a table?
□ For each relationship mentioned ("assign to", "belongs to"), did I create appropriate foreign keys or join tables?
□ For each form mentioned (TaskForm, UserForm), did I create a table for that entity?
□ Did I include ALL fields mentioned in step descriptions?
□ Did I use 'reference' type for all foreign keys with reference_table specified?
□ Did I use 'enum' type for status/category fields with enum_values specified?
□ Did I avoid defining 'id', 'created_at', 'updated_at' (auto-generated)?

**Output Format:**
Return a JSON object with a "tables" array. Each table should have this structure:

{{
  "tables": [
    {{
      "slug": "table-slug",
      "name": "Display Name",
      "description": "What this table stores",
      "columns": [
        {{
          "name": "field_name",
          "type": "string",
          "nullable": false,
          "unique": false,
          "default": null
        }},
        {{
          "name": "status",
          "type": "enum",
          "enum_values": ["draft", "active", "archived"],
          "default": "draft"
        }},
        {{
          "name": "related_id",
          "type": "reference",
          "reference_table": "other-table"
        }}
      ]
    }}
  ]
}}

If no database is needed, return: {{"tables": []}}

**CRITICAL**: Return ONLY valid JSON, no markdown code blocks, no explanatory text.

Now analyze the plan above and output the JSON schema:
"""
    
    def parse_table_definitions(self, content: str) -> List[Dict[str, Any]]:
        """
        Parse JSON table definitions from LLM response.

        Args:
            content: LLM response containing JSON with table definitions

        Returns:
            List of table definition dictionaries with structure:
            {
                'slug': 'table-name',
                'name': 'Display Name',
                'description': 'Optional description',
                'columns': [{'name': 'field', 'type': 'string', ...}, ...]
            }
        """
        try:
            # Parse JSON directly - we instruct the LLM to return ONLY JSON
            data = json.loads(content.strip())
            logger.debug(f"Schema extraction response: {data}")
            tables = data.get('tables', [])

            if not tables:
                logger.warning("No tables found in schema extraction response")
                return []

            # Validate and return table definitions
            valid_tables = []
            for table in tables:
                slug = table.get('slug', '')
                name = table.get('name', '')
                columns = table.get('columns', [])

                if not slug or not columns:
                    logger.warning(f"Skipping invalid table definition: {table}")
                    continue

                # Validate columns have required fields
                valid_columns = []
                for col in columns:
                    if col.get('name') and col.get('type'):
                        valid_columns.append(col)
                    else:
                        logger.warning(f"Skipping invalid column in table '{slug}': {col}")

                if valid_columns:
                    valid_tables.append({
                        'slug': slug,
                        'name': name,
                        'description': table.get('description', ''),
                        'columns': valid_columns
                    })

            logger.info(f"✅ Parsed {len(valid_tables)} table definitions from JSON")
            return valid_tables

        except json.JSONDecodeError as e:
            logger.error(f"Failed to parse JSON from schema extraction response: {e}")
            logger.error(f"Content: {content[:500]}...")
            return []
        except Exception as e:
            logger.error(f"Error parsing table definitions: {e}")
            return []


# Singleton instance
_schema_extraction_service: Optional[SchemaExtractionService] = None


def get_schema_extraction_service() -> SchemaExtractionService:
    """Get singleton schema extraction service instance."""
    global _schema_extraction_service
    if _schema_extraction_service is None:
        _schema_extraction_service = SchemaExtractionService()
    return _schema_extraction_service
