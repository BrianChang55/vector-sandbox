"""
Edit Handler

Surgical code modifications without regenerating the entire app.
Used for targeted changes like styling, text updates, and small fixes.
"""
import logging
import time
from typing import Any, Dict, Generator, List, Optional, TYPE_CHECKING

from .base_handler import BaseHandler, AgentEvent, FileChange, PlanStep
from .diff_utils import parse_diffs, apply_diff

if TYPE_CHECKING:
    from relay_app.models import InternalApp, AppVersion
    from relay_app.services.intent_classifier import IntentResult
    from relay_app.services.context_analyzer import AppContext

logger = logging.getLogger(__name__)


# Import design style and guards from prompts
from relay_app.prompts.agentic import DESIGN_STYLE_PROMPT, OVER_EAGERNESS_GUARD


# Prompt for surgical edits using unified diff format
EDIT_SYSTEM_PROMPT = f"""You are an expert React/TypeScript developer making targeted, surgical edits to existing code using unified diffs.

{OVER_EAGERNESS_GUARD}

## Surgical Edit Rules (CRITICAL)

1. **Preserve Everything Except What's Requested**
   - You MUST keep all existing functionality intact
   - Do NOT "improve" or "clean up" adjacent code
   - Do NOT add features "while you're at it"
   - Do NOT refactor unless explicitly asked

2. **Minimal Changes Only**
   - Change ONLY the specific lines needed
   - Keep existing variable names, patterns, and style
   - Maintain the exact same imports unless changes require new ones
   - Do NOT reorganize or restructure code

3. **Match Existing Style Exactly**
   - Use the same indentation (tabs vs spaces)
   - Use the same quote style (single vs double)
   - Use the same semicolon conventions
   - Match the existing naming conventions

4. **Output Format: Unified Diff**
   - Output changes as unified diffs (like git diff)
   - Include 3 lines of context before and after each change
   - Use `-` prefix for removed lines, `+` for added lines
   - Space prefix for unchanged context lines
   - Multiple hunks allowed for non-adjacent changes in the same file

{DESIGN_STYLE_PROMPT}"""


EDIT_PROMPT_TEMPLATE = """Make the following TARGETED change to the existing code.

## User Request
{user_message}

## Current File Contents
{files_context}

## Surgical Edit Instructions

1. **Analyze the request**: Identify exactly what needs to change
2. **Locate the target**: Find the specific line(s) or section(s) to modify
3. **Make minimal changes**: Change ONLY what's needed to fulfill the request
4. **Preserve everything else**: Keep all other code exactly as-is

## Output Format: Unified Diff

For each file you modify, output a unified diff inside a ```diff code block:

```diff
--- src/path/to/file.tsx
+++ src/path/to/file.tsx
@@ -LINE,COUNT +LINE,COUNT @@ optional function/class context
 context line (unchanged, starts with space)
 context line (unchanged, starts with space)
 context line (unchanged, starts with space)
-line to remove (starts with minus)
+line to add (starts with plus)
 context line (unchanged, starts with space)
 context line (unchanged, starts with space)
 context line (unchanged, starts with space)
```

### Diff Rules
- Start with `--- path` and `+++ path` header lines
- Each hunk starts with `@@ -old_start,old_count +new_start,new_count @@`
- Include exactly 3 lines of unchanged context before and after changes
- Lines starting with ` ` (space) are unchanged context
- Lines starting with `-` are removed
- Lines starting with `+` are added
- For multiple non-adjacent changes in the same file, use multiple hunks

### Example

If changing a button's text from "Submit" to "Save":

```diff
--- src/components/Form.tsx
+++ src/components/Form.tsx
@@ -15,7 +15,7 @@ function Form() {{
   return (
     <form onSubmit={{handleSubmit}}>
       <input value={{value}} onChange={{handleChange}} />
-      <button type="submit">Submit</button>
+      <button type="submit">Save</button>
     </form>
   );
 }}
```

### Example with longer removals

When removing multi-line blocks (functions, JSX elements, conditionals), you MUST include BOTH the opening AND closing parts. Never leave orphaned brackets, braces, or tags.

```diff
--- src/components/Form.tsx
+++ src/components/Form.tsx
@@ -1,16 +1,8 @@
 import React, {{ useState }} from "react";

-function validate(value: string): boolean {{
-  return value.trim().length > 0;
-}}
-
 function Form() {{
   const [value, setValue] = useState("");
-  const [hasError, setHasError] = useState(false);

   function handleChange(e: React.ChangeEvent<HTMLInputElement>) {{
-    const next = e.target.value;
-    setValue(next);
-    setHasError(!validate(next));
+    setValue(e.target.value);
   }}

   return (
@@ -20,9 +12,6 @@ function Form() {{
     <form>
       <input value={{value}} onChange={{handleChange}} />
-      {{hasError && (
-        <span className="error">Required</span>
-      )}}
       <button type="submit">Save</button>
     </form>
   );
 }}
```

Key points for removals:
- Remove the ENTIRE `validate` function including its closing `}}`
- Remove the ENTIRE conditional `{{hasError && (...)}}` including both `{{` and `)}}`
- Never leave unmatched brackets, braces, parentheses, or tags

## Critical Reminders
- Output ONLY the diff, not the complete file
- Do NOT add new features or "improvements"
- Do NOT refactor or reorganize code
- Do NOT change styling unless specifically requested
- If unsure about something, leave it unchanged"""


class EditHandler(BaseHandler):
    """
    Handler for surgical code modifications.
    
    This handler is used when:
    - User wants to change specific code (styling, text, behavior)
    - Intent is classified as EDIT_CODE or REFACTOR
    - Changes are targeted and don't require rebuilding the app
    """
    
    def execute(
        self,
        intent: 'IntentResult',
        context: 'AppContext',
        user_message: str,
        current_spec: Optional[Dict[str, Any]],
        registry_surface: Dict[str, Any],
        app_name: str,
        model: str,
        app: Optional['InternalApp'] = None,
        version: Optional['AppVersion'] = None,
        **kwargs,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """
        Execute surgical code edits.
        
        Only modifies the files that need to change.
        """
        start_time = time.time()
        generated_files: List[FileChange] = []
        
        # Get additional context from kwargs
        data_store_context = kwargs.get('data_store_context')
        mcp_tools_context = kwargs.get('mcp_tools_context')
        
        # ===== PHASE 1: ANALYZE =====
        yield self.emit_phase_change("researching", "Analyzing what needs to change...")
        
        # Determine which files to modify
        target_files = self._identify_target_files(intent, context, user_message)
        
        if not target_files:
            # Fallback: modify the main App.tsx
            target_files = ['src/App.tsx']
            if context.entry_points:
                target_files = context.entry_points[:1]
        
        yield self.emit_thinking(
            f"Identified {len(target_files)} file(s) to modify: {', '.join(target_files)}",
            "observation",
        )
        
        # ===== PHASE 2: PLAN =====
        yield self.emit_phase_change("planning", "Planning modifications...")
        
        plan_steps = [
            self.create_step("code", "Apply Changes", 
                            f"Modify {len(target_files)} file(s) based on request"),
            self.create_step("validation", "Validate Changes",
                            "Ensure code compiles and works correctly"),
        ]
        
        yield self.emit_plan_created(
            steps=plan_steps,
            explored_dirs=1,
            explored_files=len(target_files),
            searches=0,
        )
        
        # ===== PHASE 3: EXECUTE EDITS =====
        yield self.emit_phase_change("executing", "Applying changes...")
        
        step = plan_steps[0]
        step_start = time.time()
        
        yield self.emit_step_started(step, 0)
        yield self.emit_step_start(step, 0)
        
        # Get current file contents
        file_contents = self._get_file_contents(version, target_files)
        
        if not file_contents:
            yield self.emit_thinking(
                "Could not read existing files - generating new content",
                "reflection",
            )
        # Generate edits
        try:
            edited_files = yield from self._generate_edits(
                user_message=user_message,
                file_contents=file_contents,
                model=model,
                data_store_context=data_store_context,
                mcp_tools_context=mcp_tools_context,
            )
            
            generated_files.extend(edited_files)
            
            step.status = "complete"
            step.duration = int((time.time() - step_start) * 1000)
            
            yield self.emit_step_completed(step, 0, step.duration)
            yield self.emit_step_complete(0, "complete", step.duration)
            
        except Exception as e:
            logger.error(f"Edit execution error: {e}")
            step.status = "error"
            yield self.emit_step_complete(0, "error", int((time.time() - step_start) * 1000))
            yield self.emit_thinking(f"Error during edit: {str(e)}", "reflection")
        
        # ===== PHASE 4: VALIDATE & FIX =====
        if generated_files:
            step = plan_steps[1]
            step_start = time.time()
            
            yield self.emit_step_started(step, 1)
            yield self.emit_step_start(step, 1)
            
            # Merge with existing files for validation
            all_files = self._merge_with_existing(context, version, generated_files)
            
            # Run TypeScript validation with error fixing
            validation_passed, fix_attempts = yield from self.validate_and_fix(
                generated_files=all_files,
                model=model,
            )
            
            # Update generated_files with any fixes
            generated_paths = {f.path for f in generated_files}
            for f in all_files:
                if f.path in generated_paths:
                    # Find and update the corresponding file
                    for i, gf in enumerate(generated_files):
                        if gf.path == f.path:
                            generated_files[i] = f
                            break
            
            step.duration = int((time.time() - step_start) * 1000)
            yield self.emit_step_completed(step, 1, step.duration)
            yield self.emit_step_complete(1, "complete", step.duration)
            
            yield self.emit_validation_result(
                passed=validation_passed,
                fix_attempts=fix_attempts,
            )
        
        return generated_files
    
    def _identify_target_files(
        self,
        intent: 'IntentResult',
        context: 'AppContext',
        user_message: str,
    ) -> List[str]:
        """Identify which files need to be modified.
        
        Uses cascade detection to find all potentially affected files.
        """
        target_files = []
        
        # First, use files identified by intent classifier
        if intent.affected_files:
            for path in intent.affected_files:
                if path in context.file_paths:
                    target_files.append(path)
        
        if target_files:
            # Use cascade detection to find additional affected files
            from relay_app.services.context_analyzer import get_context_analyzer
            analyzer = get_context_analyzer()
            
            cascade_affected = set()
            for primary_file in target_files:
                affected = analyzer.find_cascade_affected_files(context, primary_file, max_depth=1)
                cascade_affected.update(affected)
            
            # Add cascade files that aren't already in target_files
            for path in cascade_affected:
                if path not in target_files and path in context.file_paths:
                    target_files.append(path)
            
            return target_files
        
        # Try to find files based on keywords in the message
        message_lower = user_message.lower()
        
        # Map common keywords to file patterns
        keyword_patterns = {
            'header': ['Header', 'header', 'Nav', 'nav'],
            'button': ['Button', 'button'],
            'form': ['Form', 'form', 'Input', 'input'],
            'table': ['Table', 'table', 'List', 'list'],
            'card': ['Card', 'card'],
            'modal': ['Modal', 'modal', 'Dialog', 'dialog'],
            'sidebar': ['Sidebar', 'sidebar', 'Menu', 'menu'],
            'footer': ['Footer', 'footer'],
            'app': ['App'],
        }
        
        for keyword, patterns in keyword_patterns.items():
            if keyword in message_lower:
                for f in context.existing_files:
                    for pattern in patterns:
                        if pattern in f.path:
                            if f.path not in target_files:
                                target_files.append(f.path)
        
        # If still no matches, use main entry point
        if not target_files and context.entry_points:
            target_files = [context.entry_points[0]]
        
        # Fallback to App.tsx
        if not target_files:
            if 'src/App.tsx' in context.file_paths:
                target_files = ['src/App.tsx']
        
        return target_files
    
    def _get_file_contents(
        self,
        version: Optional['AppVersion'],
        file_paths: List[str],
    ) -> Dict[str, str]:
        """Get the current contents of files."""
        if not version:
            return {}
        
        try:
            from relay_app.services.context_analyzer import get_context_analyzer
            analyzer = get_context_analyzer()
            return analyzer.get_file_contents(version, file_paths)
        except Exception as e:
            logger.warning(f"Error getting file contents: {e}")
            return {}
    
    def _get_language(self, path: str) -> str:
        """Determine language from file extension."""
        lang_map = {
            'tsx': 'tsx',
            'ts': 'ts',
            'jsx': 'tsx',
            'js': 'ts',
            'css': 'css',
            'json': 'json',
            'html': 'html',
        }
        ext = path.split('.')[-1] if '.' in path else 'tsx'
        return lang_map.get(ext, 'tsx')

    def _generate_edits(
        self,
        user_message: str,
        file_contents: Dict[str, str],
        model: str,
        data_store_context: Optional[str] = None,
        mcp_tools_context: Optional[str] = None,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """Generate edited versions of files."""
        files = []
        
        # Build context of files to edit
        files_context = ""
        for path, content in file_contents.items():
            files_context += f"\n### {path}\n```\n{content}\n```\n"
        
        if not files_context:
            files_context = "No existing files to modify."
        
        # Escape curly braces in file contents to prevent .format() errors
        # JSX code contains {variable} which breaks Python's .format()
        escaped_files_context = files_context.replace('{', '{{').replace('}', '}}')
        escaped_user_message = user_message.replace('{', '{{').replace('}', '}}')

        # Build the user prompt with additional context
        formatted_template = EDIT_PROMPT_TEMPLATE.format(
            user_message=escaped_user_message,
            files_context=escaped_files_context,
        )

        prompt_parts = [formatted_template]
        
        # Add data store context if available
        if data_store_context and "No data tables" not in data_store_context:
            prompt_parts.append(f"\n## Available Data Store\n{data_store_context}")
        
        # Add MCP tools context if available
        if mcp_tools_context:
            prompt_parts.append(f"\n## Available Integrations\n{mcp_tools_context}")
        
        prompt = "\n".join(prompt_parts)
        
        try:
            full_content = ""
            chunk_count = 0
            
            # Create streaming validator for real-time checks
            validator = self.create_streaming_validator()
            
            for chunk in self.stream_llm_response(
                system_prompt=EDIT_SYSTEM_PROMPT,
                user_prompt=prompt,
                model=model,
                temperature=0.2,  # Lower temperature for more consistent edits
            ):
                full_content += chunk
                chunk_count += 1
                
                # Real-time validation during streaming
                streaming_warnings = validator.check_chunk(chunk, full_content)
                for warning in streaming_warnings:
                    yield self.emit_streaming_warning(warning)
                
                if chunk_count % 10 == 0:
                    yield self.emit_step_progress(0, min(80, chunk_count), "Generating edits...")

            # Final validation check
            final_warnings = validator.final_check(full_content)
            for warning in final_warnings:
                yield self.emit_streaming_warning(warning)

            # TODO: Add some type of validation + fallback to full file generation here
            # Parse unified diffs from LLM response
            diffs = parse_diffs(full_content)

            if diffs:
                # Apply diffs to original files
                for diff in diffs:
                    original = file_contents.get(diff.path, "")
                    if not original:
                        logger.warning(f"No original content found for {diff.path}, skipping")
                        continue

                    new_content = apply_diff(original, diff)
                    file_change = FileChange(
                        path=diff.path,
                        action='modify',
                        language=self._get_language(diff.path),
                        content=new_content,
                    )
                    yield self.emit_file_generated(file_change)
                    files.append(file_change)
            else:
                # Fallback: try parsing as complete file blocks (backwards compatibility)
                logger.warning("No diffs found in LLM response, falling back to full file parsing")
                edited_files = self.parse_code_blocks(full_content)

                for f in edited_files:
                    f.action = 'modify'
                    yield self.emit_file_generated(f)
                    files.append(f)

            return files

        except Exception as e:
            logger.error(f"Edit generation error: {e}")
            raise
    
    def _merge_with_existing(
        self,
        context: 'AppContext',
        version: Optional['AppVersion'],
        edited_files: List[FileChange],
    ) -> List[FileChange]:
        """Merge edited files with existing files for validation."""
        all_files = []
        edited_paths = {f.path for f in edited_files}
        
        # Get all existing files
        if version:
            try:
                for vf in version.files.all():
                    if vf.path in edited_paths:
                        # Use edited version
                        continue
                    all_files.append(FileChange(
                        path=vf.path,
                        action='create',
                        language=vf.path.split('.')[-1] if '.' in vf.path else 'tsx',
                        content=vf.content or '',
                    ))
            except Exception as e:
                logger.warning(f"Error getting existing files: {e}")
        
        # Add edited files
        all_files.extend(edited_files)
        
        return all_files
    
    def _quick_validate(self, files: List[FileChange]) -> bool:
        """Quick validation of edited files."""
        for f in files:
            if not f.content or len(f.content) < 10:
                return False
            
            # Basic syntax check for TSX/TS files
            if f.language in ('tsx', 'ts'):
                # Check for obvious syntax issues
                if f.content.count('{') != f.content.count('}'):
                    logger.warning(f"Mismatched braces in {f.path}")
                    return False
                if f.content.count('(') != f.content.count(')'):
                    logger.warning(f"Mismatched parentheses in {f.path}")
                    return False
        
        return True

