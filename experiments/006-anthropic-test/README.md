# Experiment 006: Anthropic Claude Provider Test

This experiment tests the Anthropic Claude API provider implementation.

## Prerequisites

1. Anthropic API key (get from https://console.anthropic.com)
2. Guile 3.0+ with required dependencies

## Setup

```bash
export ANTHROPIC_API_KEY="your-api-key-here"
```

## Running Tests

```bash
# From repository root
guile -L src -l experiments/006-anthropic-test/test-anthropic.scm

# Or make it executable and run directly
chmod +x experiments/006-anthropic-test/test-anthropic.scm
./experiments/006-anthropic-test/test-anthropic.scm
```

## Test Cases

1. **api-key-required**: Verifies that API key is mandatory
2. **provider-creation**: Tests basic provider instantiation
3. **llm-provider-interface**: Tests provider abstraction compliance
4. **provider-registration**: Tests global registry integration
5. **list-models**: Tests model listing (static list)
6. **simple-completion**: Tests basic text generation
7. **chat-completion**: Tests chat message format
8. **system-prompt**: Tests system prompt functionality

## Notes

- Tests use `claude-3-haiku-20240307` by default (fastest/cheapest)
- Anthropic doesn't have a public embeddings API
- System prompts are passed separately, not in the messages array
