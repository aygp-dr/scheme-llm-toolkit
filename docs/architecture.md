# Scheme LLM Toolkit Architecture

This document provides a comprehensive overview of the Scheme LLM Toolkit's architecture, including control flow, data flow, and sequence diagrams.

## Table of Contents

1. [System Overview](#system-overview)
2. [Architecture Components](#architecture-components)
3. [Control Flow](#control-flow)
4. [Data Flow](#data-flow)
5. [Sequence Diagrams](#sequence-diagrams)
6. [Module Structure](#module-structure)

## System Overview

The Scheme LLM Toolkit provides a functional interface for integrating Large Language Models into Guile Scheme applications. It follows a modular, provider-agnostic design that emphasizes composability and type safety.

```mermaid
graph TB
    subgraph "Application Layer"
        A[User Application]
    end
    
    subgraph "Toolkit Core"
        B[LLM Core API]
        C[Provider Abstraction]
        D[Streaming Interface]
        E[Prompt DSL]
    end
    
    subgraph "Provider Layer"
        F[Ollama Provider]
        G[OpenAI Provider]
        H[Anthropic Provider]
        I[Hugging Face Provider]
    end
    
    subgraph "Utility Layer"
        J[HTTP Client]
        K[JSON Parser]
        L[Type System]
    end
    
    A --> B
    B --> C
    B --> D
    B --> E
    C --> F
    C --> G
    C --> H
    C --> I
    F --> J
    F --> K
    G --> J
    G --> K
    H --> J
    H --> K
    I --> J
    I --> K
    B --> L
```

## Architecture Components

### Core Components

1. **LLM Core API** (`llm/core/`)
   - Central interface for all LLM operations
   - Manages provider selection and routing
   - Handles response normalization

2. **Provider Abstraction** (`llm/providers/`)
   - Defines common interface for all providers
   - Implements provider-specific logic
   - Manages authentication and configuration

3. **Streaming Interface**
   - Functional streaming for real-time responses
   - Event-based callbacks for token processing
   - Backpressure handling

4. **Prompt DSL**
   - S-expression based prompt construction
   - Composable prompt templates
   - Type-safe parameter injection

### Provider Implementations

```mermaid
classDiagram
    class Provider {
        <<interface>>
        +name: string
        +complete(prompt, options): Response
        +stream(prompt, options): Stream
        +validate-config(): boolean
    }
    
    class OllamaProvider {
        +base-url: string
        +model: string
        +complete(prompt, options): Response
        +stream(prompt, options): Stream
    }
    
    class OpenAIProvider {
        +api-key: string
        +organization: string
        +complete(prompt, options): Response
        +stream(prompt, options): Stream
    }
    
    class AnthropicProvider {
        +api-key: string
        +complete(prompt, options): Response
        +stream(prompt, options): Stream
    }
    
    Provider <|-- OllamaProvider
    Provider <|-- OpenAIProvider
    Provider <|-- AnthropicProvider
```

## Control Flow

### Request Processing Flow

```mermaid
flowchart TD
    A[User Request] --> B{Provider Selection}
    B --> C[Load Provider Config]
    C --> D[Validate Request]
    D --> E{Streaming?}
    E -->|Yes| F[Initialize Stream]
    E -->|No| G[Prepare Batch Request]
    F --> H[Send Streaming Request]
    G --> I[Send Batch Request]
    H --> J[Process Token Stream]
    I --> K[Process Complete Response]
    J --> L[Token Callback]
    L --> M[Accumulate Response]
    K --> N[Response Callback]
    M --> O[Complete Callback]
    N --> P[Return Response]
    O --> P
    P --> Q[User Application]
```

### Error Handling Flow

```mermaid
flowchart TD
    A[Operation] --> B{Success?}
    B -->|Yes| C[Continue]
    B -->|No| D{Error Type}
    D -->|Network| E[Retry Logic]
    D -->|Auth| F[Re-authenticate]
    D -->|Rate Limit| G[Backoff & Retry]
    D -->|Invalid| H[Return Error]
    E --> I{Retry Count}
    I -->|< Max| A
    I -->|>= Max| H
    F --> J{Auth Success?}
    J -->|Yes| A
    J -->|No| H
    G --> K[Wait Period]
    K --> A
```

## Data Flow

### Request/Response Data Flow

```mermaid
graph LR
    subgraph "Input Processing"
        A[S-expression Prompt] --> B[Prompt Compiler]
        B --> C[JSON Request Body]
    end
    
    subgraph "Network Layer"
        C --> D[HTTP Client]
        D --> E[Provider API]
        E --> F[HTTP Response]
    end
    
    subgraph "Response Processing"
        F --> G[JSON Parser]
        G --> H[Response Normalizer]
        H --> I[Scheme Data Structure]
    end
    
    I --> J[Application]
```

### Configuration Data Flow

```mermaid
graph TD
    A[providers.scm] --> B[Config Loader]
    B --> C[Environment Variables]
    C --> D[Config Merger]
    B --> D
    D --> E[Validated Config]
    E --> F[Provider Registry]
    F --> G[Runtime Provider]
```

## Sequence Diagrams

### Basic Completion Request

```mermaid
sequenceDiagram
    participant App as Application
    participant Core as LLM Core
    participant Provider as Provider
    participant HTTP as HTTP Client
    participant API as LLM API
    
    App->>Core: llm-complete(provider, prompt)
    Core->>Provider: validate-config()
    Provider-->>Core: config-valid
    Core->>Provider: complete(prompt, options)
    Provider->>HTTP: POST /api/generate
    HTTP->>API: HTTP Request
    API-->>HTTP: HTTP Response
    HTTP-->>Provider: JSON Response
    Provider-->>Core: Normalized Response
    Core-->>App: Response String
```

### Streaming Request

```mermaid
sequenceDiagram
    participant App as Application
    participant Core as LLM Core
    participant Provider as Provider
    participant Stream as Stream Handler
    participant API as LLM API
    
    App->>Core: llm-stream(provider, prompt, callbacks)
    Core->>Provider: stream(prompt, options)
    Provider->>Stream: open-stream()
    Stream->>API: Streaming Request
    
    loop For each token
        API-->>Stream: Token Chunk
        Stream-->>Provider: Parse Token
        Provider-->>Core: Token Event
        Core-->>App: on-token callback
    end
    
    API-->>Stream: End of Stream
    Stream-->>Provider: Stream Complete
    Provider-->>Core: Complete Event
    Core-->>App: on-complete callback
```

### Multi-Provider Request

```mermaid
sequenceDiagram
    participant App as Application
    participant Core as LLM Core
    participant Ollama as Ollama Provider
    participant OpenAI as OpenAI Provider
    participant Registry as Provider Registry
    
    App->>Core: llm-complete(ollama, prompt1)
    Core->>Registry: get-provider(ollama)
    Registry-->>Core: Ollama Instance
    Core->>Ollama: complete(prompt1)
    Ollama-->>Core: Response1
    Core-->>App: Response1
    
    App->>Core: llm-complete(openai, prompt2)
    Core->>Registry: get-provider(openai)
    Registry-->>Core: OpenAI Instance
    Core->>OpenAI: complete(prompt2)
    OpenAI-->>Core: Response2
    Core-->>App: Response2
```

## Module Structure

### Dependency Graph

```mermaid
graph TD
    subgraph "Core Modules"
        A[llm/core/provider.scm]
    end
    
    subgraph "Provider Modules"
        B[llm/providers/ollama.scm]
        C[llm/providers/ollama-provider.scm]
    end
    
    subgraph "Utility Modules"
        D[llm/utils/http.scm]
        E[llm/utils/json.scm]
    end
    
    subgraph "System Modules"
        F[ice-9 match]
        G[web client]
        H[json]
    end
    
    A --> D
    A --> E
    B --> D
    B --> E
    C --> A
    C --> B
    D --> G
    E --> H
    A --> F
```

### Module Interfaces

```mermaid
classDiagram
    class CoreProvider {
        +make-provider(type, config)
        +provider-complete(provider, prompt, options)
        +provider-stream(provider, prompt, options)
        +provider-name(provider)
    }
    
    class HTTPUtils {
        +http-post(url, body, headers)
        +http-get(url, headers)
        +parse-response(response)
    }
    
    class JSONUtils {
        +scheme->json(data)
        +json->scheme(string)
        +validate-json(string)
    }
    
    class OllamaModule {
        +make-ollama-provider(config)
        +ollama-generate(url, model, prompt)
        +ollama-stream(url, model, prompt)
    }
    
    CoreProvider ..> HTTPUtils : uses
    CoreProvider ..> JSONUtils : uses
    OllamaModule ..> HTTPUtils : uses
    OllamaModule ..> JSONUtils : uses
    OllamaModule --|> CoreProvider : implements
```

## Design Principles

1. **Functional Composition**: All operations are pure functions that can be composed
2. **Provider Agnostic**: Core logic is independent of specific LLM providers
3. **Type Safety**: Strong typing through Scheme's type system and contracts
4. **Streaming First**: Designed for real-time streaming with fallback to batch
5. **Error Recovery**: Comprehensive error handling with retry mechanisms
6. **Configuration Driven**: External configuration for all provider settings

## Future Architecture Considerations

1. **Caching Layer**: Add response caching for identical prompts
2. **Load Balancing**: Distribute requests across multiple provider instances
3. **Metrics Collection**: Built-in telemetry for monitoring and debugging
4. **Plugin System**: Dynamic provider loading without core modifications
5. **Vector Store Integration**: Support for RAG (Retrieval Augmented Generation)

---

This architecture ensures the Scheme LLM Toolkit remains flexible, maintainable, and extensible while providing a clean functional interface for Scheme applications.