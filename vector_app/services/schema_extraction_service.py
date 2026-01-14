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
        model: str = AIModel.CLAUDE_SONNET_4_5.value,
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
        
        try:
            with httpx.Client(timeout=60.0) as client:
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
1. Analyze the plan steps and identify ALL entities that need to be stored in a database
2. For each entity, determine:
   - Table name (use kebab-case slug like 'project-tasks', 'user-profiles')
   - Display name
   - All fields needed based on the step descriptions
   - Field types (string, text, integer, float, boolean, datetime, date, enum, json, reference)
   - Field constraints (nullable, unique, default values)
   - Relationships between tables (use 'reference' type)

3. Output TABLE_DEFINITION blocks for each table:

```table:table-slug
name: Display Name
description: Brief description of what this table stores
columns:
  - name: field_name, type: string, nullable: false
  - name: other_field, type: integer, default: 0
  - name: status, type: enum, enum_values: [draft, active, archived]
  - name: related_id, type: reference, reference_table: other-table
```

**Important Rules:**
- DO NOT define 'id', 'created_at', or 'updated_at' - these are auto-generated
- Use descriptive field names that match the domain
- Infer sensible defaults and constraints
- Use 'reference' type for foreign keys (auto-references the 'id' field)
- Use 'enum' for status fields with fixed values
- Use 'text' for long content, 'string' for short text (max 255 chars)
- Use 'datetime' for timestamps, 'date' for date-only fields
- Make fields 'nullable: false' only if they're truly required

**Example:**
If the plan mentions "Create a task list showing title, status, and due date", you would create:

```table:tasks
name: Tasks
description: User tasks with status tracking
columns:
  - name: title, type: string, nullable: false
  - name: status, type: enum, enum_values: [todo, in_progress, done], default: todo
  - name: due_date, type: date
  - name: description, type: text
```

Now analyze the plan above and output ALL necessary TABLE_DEFINITION blocks. If no database is needed, output "NO_TABLES_NEEDED".
"""
    
    def parse_table_definitions(self, content: str) -> List[str]:
        """
        Extract TABLE_DEFINITION blocks from LLM response.
        
        Args:
            content: LLM response containing table definitions
            
        Returns:
            List of table definition strings (including ```table:slug markers)
        """
        # Pattern to match table definition blocks
        pattern = r'```table:([a-z0-9-]+)\n(.*?)```'
        matches = re.findall(pattern, content, re.DOTALL | re.IGNORECASE)
        
        definitions = []
        for slug, table_content in matches:
            # Reconstruct the full block
            full_block = f"```table:{slug}\n{table_content}```"
            definitions.append(full_block)
        
        return definitions


# Singleton instance
_schema_extraction_service: Optional[SchemaExtractionService] = None


def get_schema_extraction_service() -> SchemaExtractionService:
    """Get singleton schema extraction service instance."""
    global _schema_extraction_service
    if _schema_extraction_service is None:
        _schema_extraction_service = SchemaExtractionService()
    return _schema_extraction_service
