# Experiment 007: Function Calling / Tool Use

This experiment demonstrates the function calling (tool use) capabilities
of the scheme-llm-toolkit.

## Overview

Function calling allows LLMs to request the execution of specific functions
defined by the application. This enables:

- Structured data extraction
- API integrations
- Multi-step reasoning with tool access
- Agent-like behavior

## Core Module

The `(llm core tools)` module provides:

- `make-tool` - Create tool definitions
- `tool->openai-schema` / `tool->anthropic-schema` - Format conversion
- `execute-tool` / `execute-tool-calls` - Tool execution
- `define-tool` - Convenience macro

## Example Usage

```scheme
(use-modules (llm core tools)
             (llm providers openai))

;; Define a tool
(define weather-tool
  (make-tool
   #:name "get_weather"
   #:description "Get current weather"
   #:parameters '((type . "object")
                  (properties . ((city . ((type . "string"))))))
   #:handler (lambda (city) (format #f "Weather in ~a: sunny" city))))

;; Use with OpenAI
(let* ((provider (make-openai-provider #:api-key (getenv "OPENAI_API_KEY")))
       (tools (tools->openai-format (list weather-tool)))
       (response (openai-chat provider
                             '(((role . "user")
                                (content . "What's the weather in Paris?")))
                             #:tools tools)))
  ;; Handle tool calls in response
  ...)
```

## Running Tests

```bash
# Unit tests (no API key needed)
guile -L ../../src -l test-tools.scm

# With API integration
export OPENAI_API_KEY="your-key"
export ANTHROPIC_API_KEY="your-key"
./test-tools.scm
```

## Provider Support

| Provider  | Tool Format | Status |
|-----------|-------------|--------|
| OpenAI    | function    | Supported |
| Anthropic | tool_use    | Supported |
| Ollama    | tools       | Planned |

## Response Handling

When tools are invoked, the response changes from a simple string to
a structured response containing tool calls:

```scheme
;; OpenAI format
'((role . "assistant")
  (content . #f)
  (tool_calls . ((id . "call_abc")
                 (function . ((name . "get_weather")
                             (arguments . "{\"city\":\"Paris\"}"))))))

;; Anthropic format
'((role . "assistant")
  (content . #f)
  (tool_use . ((type . "tool_use")
               (id . "toolu_abc")
               (name . "get_weather")
               (input . ((city . "Paris"))))))
```

## Multi-turn Tool Conversations

For complete tool usage, you need to:

1. Send initial request with tools
2. Receive tool call response
3. Execute tools locally
4. Send tool results back
5. Receive final response

See `demo.scm` for a complete example.
