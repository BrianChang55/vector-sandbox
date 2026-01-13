"""
Action Classification Module

Classifies user requests to determine what MCP tool operations
the user's app will need to perform, and matches them to available tools.

Components:
- types.py: ActionType enum (QUERY, CREATE, UPDATE, DELETE, SEND, OTHER)
- action_classifier.py: Classifies user messages to determine needed operations
- tool_matcher.py: Matches classified actions to available MCP tools

Usage:
    from vector_app.action_classification.types import ActionType
    from vector_app.action_classification.action_classifier import get_action_classifier
    from vector_app.action_classification.tool_matcher import get_tool_matcher
"""
