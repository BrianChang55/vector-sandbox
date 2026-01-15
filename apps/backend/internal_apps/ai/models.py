from dataclasses import dataclass
from enum import StrEnum
from typing import Dict, List


class AIModel(StrEnum):
    """Available AI models via OpenRouter - Agent-capable models only."""
    # Claude Models (Agent-capable)
    CLAUDE_OPUS_4_5 = "anthropic/claude-opus-4.5"
    CLAUDE_OPUS_4_5_HIGH = "anthropic/claude-opus-4.5"  # Same model, different settings
    CLAUDE_SONNET_4_5 = "anthropic/claude-sonnet-4.5"
    CLAUDE_HAIKU_4_5 = "anthropic/claude-haiku-4.5"

    # OpenAI Models (Agent-capable)
    GPT_5_1 = "openai/gpt-5.1"  # Best available
    GPT_5_1_HIGH = "openai/gpt-5.1"  # Same model, different settings

    # Google Models (Agent-capable)
    GEMINI_3_PRO = "google/gemini-3-pro-preview"  # Best available


@dataclass
class ModelConfig:
    """Configuration for an AI model."""
    model_id: str
    display_name: str
    description: str
    context_length: int
    supports_streaming: bool
    supports_json_mode: bool
    cost_per_million_input: float
    cost_per_million_output: float
    category: str
    recommended_for: List[str]


# Model configurations - Agent-capable models only
MODEL_CONFIGS: Dict[str, ModelConfig] = {
    "anthropic/claude-opus-4.5": ModelConfig(
        model_id="anthropic/claude-opus-4.5",
        display_name="Opus 4.5",
        description="Most capable Claude model for complex agentic tasks",
        context_length=200000,
        supports_streaming=True,
        supports_json_mode=True,
        cost_per_million_input=15.0,
        cost_per_million_output=75.0,
        category="premium",
        recommended_for=["complex_apps", "agents", "reasoning"],
    ),
    "anthropic/claude-sonnet-4.5": ModelConfig(
        model_id="anthropic/claude-sonnet-4.5",
        display_name="Sonnet 4.5",
        description="Best balance of speed and capability for agents",
        context_length=200000,
        supports_streaming=True,
        supports_json_mode=True,
        cost_per_million_input=3.0,
        cost_per_million_output=15.0,
        category="standard",
        recommended_for=["general_coding", "agents", "ui_generation"],
    ),
    "anthropic/claude-haiku-4.5": ModelConfig(
        model_id="anthropic/claude-haiku-4.5",
        display_name="Haiku 4.5",
        description="Fast and efficient for quick agentic tasks",
        context_length=200000,
        supports_streaming=True,
        supports_json_mode=True,
        cost_per_million_input=0.8,
        cost_per_million_output=4.0,
        category="economy",
        recommended_for=["quick_edits", "simple_agents", "fast_iteration"],
    ),
    "openai/gpt-5.1": ModelConfig(
        model_id="openai/gpt-5.1",
        display_name="GPT-5.1",
        description="OpenAI's most capable agent model",
        context_length=128000,
        supports_streaming=True,
        supports_json_mode=True,
        cost_per_million_input=2.5,
        cost_per_million_output=10.0,
        category="standard",
        recommended_for=["general_coding", "agents", "api_design"],
    ),
    "google/gemini-3-pro-preview": ModelConfig(
        model_id="google/gemini-3-pro-preview",
        display_name="Gemini 3 Pro",
        description="Google's most capable agent model with huge context",
        context_length=1000000,
        supports_streaming=True,
        supports_json_mode=True,
        cost_per_million_input=1.25,
        cost_per_million_output=5.0,
        category="standard",
        recommended_for=["large_context", "agents", "complex_apps"],
    ),
}
