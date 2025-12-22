# OpenAI Provider Test

This experiment tests the OpenAI provider implementation for the Scheme LLM Toolkit.

## Files

- `test-openai.scm` - Comprehensive test suite for the OpenAI provider
- `demo.scm` - Interactive demonstration of OpenAI provider features

## Prerequisites

1. **OpenAI API Key**: You need a valid OpenAI API key. Set it as an environment variable:
   ```bash
   export OPENAI_API_KEY="your-api-key-here"
   ```

2. **Guile Scheme**: Make sure you have Guile installed and can run Scheme scripts.

## Running the Tests

```bash
# Run the test suite
cd experiments/005-openai-test
guile test-openai.scm

# Run the interactive demo
guile demo.scm
```

## What the Tests Cover

### test-openai.scm

1. **API Key Validation** - Ensures API key is required
2. **Provider Creation** - Tests basic provider instantiation
3. **Provider Interface** - Verifies standard provider interface compliance
4. **Provider Registration** - Tests global provider registry integration
5. **Model Listing** - Tests fetching available models from OpenAI
6. **Simple Completion** - Tests basic text completion
7. **Chat Completion** - Tests conversational chat interface

### demo.scm

1. **Basic Usage** - Shows how to register and use the provider
2. **Chat Conversations** - Demonstrates multi-turn conversations
3. **with-provider Syntax** - Shows convenient syntax for provider usage
4. **Streaming** - Demonstrates real-time streaming responses (optional)

## Expected Output

If your API key is valid, you should see output like:

```
=== OpenAI Provider Test Suite ===

Testing API key validation...
PASS: Correctly requires API key

Testing provider creation...
PASS: Provider created successfully

Testing provider interface...
Provider name: openai
Capabilities: (streaming chat embeddings system-prompts context-window json-mode function-calling usage-tracking rate-limits)
Info: ((name . openai) (capabilities . (...)) (models . (...)) ...)
PASS: Provider interface working

Testing provider registration...
PASS: Provider registered successfully
Registered providers: (openai)

Testing model listing...
API accessible, listing models...
Found 57 models
  - gpt-4o-mini-2024-07-18
  - gpt-4o-mini
  - gpt-4o-2024-08-06
  - gpt-4o-2024-05-13
  - gpt-4o
PASS: Model listing successful

Testing simple completion...
Making completion request...
Response: Hello from OpenAI
PASS: Completion successful

Testing chat completion...
Making chat request...
Chat response: 4
PASS: Chat completion successful

=== Test Suite Complete ===
```

## Common Issues

1. **No API Key**: Tests will skip if `OPENAI_API_KEY` is not set
2. **Invalid API Key**: Tests will fail if the API key is invalid
3. **Rate Limits**: OpenAI has rate limits; tests may fail if you've exceeded them
4. **Network Issues**: Tests require internet connectivity to reach OpenAI's API

## Provider Features

The OpenAI provider supports:

- ✅ Text completion via chat interface
- ✅ Multi-turn conversations
- ✅ Streaming responses
- ✅ Model selection (gpt-3.5-turbo, gpt-4, etc.)
- ✅ System prompts
- ✅ Temperature and other parameters
- ✅ Embeddings generation
- ✅ Model listing
- ✅ Error handling with authentication
- ✅ Standard provider interface compliance

## Configuration Options

The OpenAI provider accepts these configuration parameters:

- `api-key` (required) - Your OpenAI API key
- `base-url` (optional) - API base URL (default: https://api.openai.com/v1)
- `model` (optional) - Default model to use (default: gpt-3.5-turbo)
- `timeout` (optional) - Request timeout in seconds (default: 300)

## Integration with Toolkit

Once working, the OpenAI provider can be used anywhere in the toolkit:

```scheme
;; Register the provider
(register-openai-provider! #:api-key "your-key")

;; Use with provider interface
(with-provider openai
  (provider-complete provider "Hello, world!"))

;; Or get directly from registry
(let ((provider (get-provider 'openai)))
  (provider-chat provider '(((role . "user") (content . "Hi!")))))
```