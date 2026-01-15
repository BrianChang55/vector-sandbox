# Coding Conventions

## Architecture

**Layer separation** - business logic in services, not views:
- `vector_app/views/` - HTTP handling only
- `vector_app/serializers/` - validation and transformation
- `vector_app/services/` - all business logic
- `vector_app/models.py` - data schema only

**Data flow:** Request → ViewSet → Serializer → Service → Model → Response

## Code Style

### Formatting

| Tool | Config | Command |
|------|--------|---------|
| black | line-length 110 | `make format` |
| isort | profile black, line-length 110 | `make format` |

### Type Checking

| Tool | Config | Command |
|------|--------|---------|
| mypy | `mypy.ini`, Python 3.13 | `make static` |

### Linting

| Tool | Config | Command |
|------|--------|---------|
| prospector | `prospector.yml`, max complexity 15 | `make lint` |

## Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Classes | PascalCase | `UserService`, `Organization` |
| Functions | snake_case | `get_user_profile`, `create_organization` |
| Constants | UPPER_SNAKE_CASE | `MAX_RETRY_ATTEMPTS` |
| Private methods | _leading_underscore | `_internal_helper` |
| Files | snake_case | `user_service.py` |

## Django Models

Inherit from `BaseModel` (provides UUID `id`, `created_at`, `updated_at`):

```python
from internal_apps.utils.base_model import BaseModel

class Organization(BaseModel):
    name = models.CharField(max_length=255)
```

## Data Structures

**Prefer dataclasses or Pydantic models over dicts.** Never create large nested dicts.

```python
# Good - structured and typed
@dataclass
class GenerationResult:
    code: str
    errors: list[str]
    metadata: ResultMetadata

# Good - Pydantic with validation (inherit from StrictBaseModel)
from vector_app.utils.pydantic_utils import StrictBaseModel

class UserRequest(StrictBaseModel):
    email: str
    count: int

# Bad - untyped nested dict
result = {"code": "...", "metadata": {"errors": [...], "info": {...}}}
```

## Services

Use factory getter pattern. See `vector_app/services/validation_service.py` for canonical example.

```python
class MyService:
    def do_thing(self, data: dict) -> Result:
        # Business logic here
        pass

def get_my_service() -> MyService:
    return MyService()
```

## Query Optimization

Always use `select_related()` for ForeignKey and `prefetch_related()` for reverse/M2M:

```python
# Avoid N+1
Organization.objects.select_related('owner').prefetch_related('members')
```

## Calling LLMs via LLMClient

When implementing AI features, use `LLMClient` from `vector_app.ai`.

### Basic Call

```python
from vector_app.ai.client import get_llm_client
from vector_app.ai.models import AIModel
from vector_app.ai.types import LLMSettings

result = get_llm_client().run(
    system_prompt=SYSTEM_PROMPT,
    user_prompt=formatted_prompt,
    llm_settings=LLMSettings(
        model=AIModel.CLAUDE_SONNET_4_5,
        temperature=0.2,
        max_tokens=500,
        timeout=30.0,
    ),
)
content = result.validated(default="")
```

### Model Selection

- `CLAUDE_HAIKU_4_5` - Fast/cheap, simple classification
- `CLAUDE_SONNET_4_5` - Default, general tasks
- `CLAUDE_OPUS_4_5` - Complex reasoning, agentic tasks

### Temperature Guidelines

- `0.0-0.2` - Classification, structured output, deterministic
- `0.5-0.7` - Generation, creative tasks

### Result Handling

Always use `result.validated(formatter, default=)`:

```python
# Raw content
content = result.validated(default="")

# JSON parsing
data = result.validated(json.loads, default=None)

# Custom parser
data = result.validated(self._parse_json, default=None)
```

### Prompt Organization

Store prompts in `vector_app/prompts/` with:
- System prompt constant: `FEATURE_SYSTEM_PROMPT`
- User prompt template: `FEATURE_PROMPT` with `{placeholders}`
- Builder function: `build_feature_prompt(...)` to format

### LLM Service Pattern

Follow the pattern in `intent_classifier.py`:
1. Dataclass for results
2. Class with `classify()` method
3. Heuristic classification first (fast path)
4. LLM classification with try/except fallback
5. Singleton via `get_*()` factory function

### Error Handling

```python
try:
    llm_result = self._llm_classify(...)
    if llm_result:
        return llm_result
except Exception as e:
    logger.warning("LLM classification failed: %s", e)

# Fallback to heuristic or default
return heuristic_result or default_result
```

## Avoid

### Inline imports

Keep imports at the top of the file, not inside functions.

```python
# Bad
def process():
    from some_module import helper  # inline import
    return helper()

# Good - import at top of file
from some_module import helper

def process():
    return helper()
```

### Inline string formatting

Put format helpers in dedicated files (e.g., `prompts/`, `utils/`), not inline.

```python
# Bad - building complex strings inline
def generate():
    prompt = f"You are a {role}. Given {context}, do {task}..."

# Good - use prompt templates/helpers
from vector_app.prompts.generation import build_generation_prompt

def generate():
    prompt = build_generation_prompt(role=role, context=context, task=task)
```

### F-strings in logging

Use lazy `%s` formatting for logs (avoids string interpolation when log level disabled).

```python
# Bad - f-string always evaluated
logger.info(f"Processing user {user_id} with data {data}")

# Good - lazy formatting
logger.info("Processing user %s with data %s", user_id, data)
```

## Canonical Examples

| Pattern | File |
|---------|------|
| ViewSet pattern | `vector_app/views/organization_views.py` |
| Service pattern | `vector_app/services/validation_service.py` |
| Serializer pattern | `vector_app/serializers/internal_app.py` |
| LLM calls | `vector_app/services/intent_classifier.py` |
| Prompts | `vector_app/prompts/execution_scope.py` |
| Action classifier | `vector_app/action_classification/action_classifier.py` |

## Import Organization

1. Standard library imports
2. Third-party library imports
3. Django imports
4. Local imports (from internal_apps)
5. Relative imports

## Tooling Commands

| Task | Command |
|------|---------|
| Format (changed files) | `make format` |
| Format (all files) | `make format-all` |
| Lint | `make lint` |
| Type check | `make static` |
| All checks | `make check` |
