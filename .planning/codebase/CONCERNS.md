# Technical Debt & Concerns

## High Priority

### Silent Exception Handling

Bare `pass` in exception handlers swallows errors without logging.

| Location | Issue |
|----------|-------|
| `services/validation_service.py:202-203` | `except Exception: pass` |
| `services/cloud_storage_service.py:280` | Silent exception suppression |
| `services/datastore/validator.py:414` | Swallowed exception |
| `services/handlers/feature_handler.py:585` | Silent failure |

**Fix:** Add logging or specific exception handling.

### N+1 Query Problems

Database queries in loops without prefetching.

| Location | Issue |
|----------|-------|
| `services/snapshot_service.py:116-117` | `version.files.all()` in loop |
| `services/handlers/feature_handler.py:573` | Unprefetched file loop |
| `services/handlers/generate_handler.py:1249` | Same issue |
| `views/streaming_views.py:98-101` | `s.messages.count()` per session |
| `views/publish_views.py:95` | Unprefetched `files.all()` |

**Fix:** Add `select_related()` / `prefetch_related()` to querysets.

### Weak Input Validation

Request data processed without sufficient validation.

| Location | Issue |
|----------|-------|
| `views/data_runtime_views.py:75-97` | Type validation missing |
| `views/streaming_views.py:216-218` | URL params not validated |
| `views/auth_views.py:563-565` | OAuth params unvalidated |

**Fix:** Add Pydantic validation schemas.

## Medium Priority

### Large Files

Files exceeding recommended complexity (800+ lines).

| File | Lines | Issue |
|------|-------|-------|
| `models.py` | 2,174 | Monolithic models file |
| `prompts/agentic.py` | 2,156 | Massive prompt file |
| `views/streaming_views.py` | 1,785 | Complex streaming logic |
| `services/handlers/generate_handler.py` | 1,632 | Multi-phase orchestrator |
| `services/validation_service.py` | 1,391 | Mixed responsibilities |
| `services/agentic_service.py` | 1,338 | Multiple concerns |
| `services/enhanced_codegen.py` | 1,245 | Should split by language |

**Fix:** Split into smaller, focused modules.

### TODO Comments

Deferred work acknowledged but not addressed.

| Location | Issue |
|----------|-------|
| `action_classification/tool_matcher.py:116` | Need semantic matching |
| `action_classification/tool_matcher.py:229` | Need structured output validation |
| `services/diff_application_service.py:336` | Move prompts to prompts/ |
| `serializers/resource_registry.py:41` | Need ActionDef validation |

### Broad Exception Catching

`except Exception as e:` catches too broadly.

| Location |
|----------|
| `services/intent_classifier.py:279,484,558` |
| `services/planning_service.py:167` |
| `services/error_fix_service.py:202,354` |
| `services/agentic_service.py:149-160` |

**Fix:** Use specific exception types.

### Missing Documentation

Complex algorithms lack explanation.

| File | Lines | Issue |
|------|-------|-------|
| `services/diff.py` | 830 | Diff parsing undocumented |
| `services/merge_service.py` | 912 | API workflow undocumented |
| `services/context_analyzer.py` | 839 | Context priorities unclear |
| `services/handlers/base_handler.py` | 993 | Streaming thresholds undocumented |

## Low Priority

### DateTime Imports

Multiple inline imports instead of module-level.

| Location | Issue |
|----------|-------|
| `models.py` (5 occurrences) | `from datetime import timedelta` inside methods |

### Git Artifact

Unresolved merge conflict file.

| File | Issue |
|------|-------|
| `Oops.rej` | Merge reject file needs cleanup |

### Protected Files Hardcoded

```python
# services/handlers/generate_handler.py:53-55
PROTECTED_FILES = {...}  # Should be configuration
```

## Summary

| Category | Count | Priority |
|----------|-------|----------|
| Silent exceptions | 4+ | High |
| N+1 queries | 5+ | High |
| Weak validation | 3+ | High |
| Large files | 7 | Medium |
| TODO comments | 4 | Medium |
| Broad exceptions | 6+ | Medium |
| Missing docs | 4 | Medium |
| Minor issues | 3 | Low |

## Recommendations

1. **CRITICAL**: Add `prefetch_related()` to all queryset loops
2. **HIGH**: Replace bare `except: pass` with logging
3. **HIGH**: Add Pydantic input validation schemas
4. **MEDIUM**: Split monolithic files (models.py, agentic.py)
5. **MEDIUM**: Document complex algorithms
6. **MEDIUM**: Use specific exception types
7. **LOW**: Resolve Oops.rej merge artifact
8. **LOW**: Move datetime imports to module level
