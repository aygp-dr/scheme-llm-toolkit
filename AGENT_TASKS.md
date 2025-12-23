# Agent Tasks for Scheme LLM Toolkit

Tasks designed for AI agents (Claude, Gemini, GPT, etc.) to implement.
Each task has clear inputs, outputs, and success criteria.

## Task Format

```yaml
id: experiment-XXX
name: Task Name
difficulty: easy|medium|hard
estimated_tokens: ~N
inputs:
  - existing files/modules to read
outputs:
  - files to create/modify
success_criteria:
  - testable conditions
```

---

## Provider Implementations

### 013: Google Gemini Provider
```yaml
id: experiment-013-gemini
name: Add Google Gemini API Provider
difficulty: medium
estimated_tokens: ~8000
inputs:
  - src/llm/providers/openai.scm (reference implementation)
  - src/llm/providers/anthropic.scm (reference implementation)
  - src/llm/core/provider.scm (provider interface)
  - https://ai.google.dev/api/rest (API docs)
outputs:
  - src/llm/providers/gemini.scm
  - src/llm/providers/gemini-provider.scm
  - experiments/013-gemini-test/test-gemini.scm
success_criteria:
  - Module loads without errors
  - Implements provider interface (complete, chat, list-models)
  - Handles API key from GEMINI_API_KEY env var
  - Test passes with mock or live API
```

### 014: Hugging Face Inference Provider
```yaml
id: experiment-014-huggingface
name: Add Hugging Face Inference API Provider
difficulty: medium
estimated_tokens: ~8000
inputs:
  - src/llm/providers/openai.scm
  - src/llm/core/provider.scm
  - https://huggingface.co/docs/api-inference/
outputs:
  - src/llm/providers/huggingface.scm
  - src/llm/providers/huggingface-provider.scm
  - experiments/014-huggingface-test/test-hf.scm
success_criteria:
  - Supports text-generation models
  - Supports embedding models
  - Handles HF_TOKEN env var
  - Test demonstrates model listing and completion
```

### 015: Cohere Provider
```yaml
id: experiment-015-cohere
name: Add Cohere API Provider
difficulty: medium
estimated_tokens: ~6000
inputs:
  - src/llm/providers/openai.scm
  - https://docs.cohere.com/reference/
outputs:
  - src/llm/providers/cohere.scm
  - experiments/015-cohere-test/test-cohere.scm
success_criteria:
  - Implements chat and embed endpoints
  - Handles COHERE_API_KEY
  - Test passes
```

---

## Prompt Engineering Experiments

### 016: Chain-of-Thought Prompting
```yaml
id: experiment-016-cot
name: Chain-of-Thought Prompt Patterns
difficulty: easy
estimated_tokens: ~4000
inputs:
  - src/llm/core/prompts.scm
outputs:
  - src/llm/prompts/chain-of-thought.scm
  - experiments/016-cot/test-cot.scm
  - experiments/016-cot/examples.scm
success_criteria:
  - Define cot-prompt macro/function
  - Include step-by-step reasoning template
  - Test with math/logic problems
  - Compare accuracy with/without CoT
```

### 017: Few-Shot Learning Templates
```yaml
id: experiment-017-few-shot
name: Few-Shot Learning Prompt Builder
difficulty: easy
estimated_tokens: ~3000
inputs:
  - src/llm/core/prompts.scm
outputs:
  - src/llm/prompts/few-shot.scm
  - experiments/017-few-shot/test-few-shot.scm
success_criteria:
  - make-few-shot-prompt function
  - Support for (example input output) pairs
  - Auto-formatting for different providers
  - Test with classification task
```

### 018: Self-Consistency Prompting
```yaml
id: experiment-018-self-consistency
name: Self-Consistency with Multiple Samples
difficulty: medium
estimated_tokens: ~5000
inputs:
  - src/llm/core/prompts.scm
  - src/llm/core/provider.scm
outputs:
  - src/llm/prompts/self-consistency.scm
  - experiments/018-self-consistency/test-sc.scm
success_criteria:
  - Generate N completions for same prompt
  - Aggregate answers (majority vote)
  - Return confidence score
  - Test with ambiguous questions
```

---

## Evaluation Experiments

### 019: Response Quality Metrics
```yaml
id: experiment-019-quality-metrics
name: LLM Response Quality Evaluation
difficulty: medium
estimated_tokens: ~6000
inputs:
  - src/llm/core/contracts.scm
outputs:
  - src/llm/eval/quality.scm
  - experiments/019-quality/test-quality.scm
success_criteria:
  - Implement metrics: coherence, relevance, fluency
  - Use contracts for metric bounds
  - Score normalization (0-1)
  - Test with good/bad response pairs
```

### 020: Toxicity Detection
```yaml
id: experiment-020-toxicity
name: Content Safety Evaluation
difficulty: medium
estimated_tokens: ~5000
inputs:
  - src/llm/core/contracts.scm
outputs:
  - src/llm/eval/safety.scm
  - experiments/020-toxicity/test-safety.scm
  - experiments/020-toxicity/test-cases.scm
success_criteria:
  - Define toxicity-check contract
  - Keyword-based detection (baseline)
  - LLM-based detection (optional)
  - Test with curated examples
```

### 021: Factuality Grounding
```yaml
id: experiment-021-factuality
name: Response Factuality Checker
difficulty: hard
estimated_tokens: ~8000
inputs:
  - src/llm/core/structured.scm
outputs:
  - src/llm/eval/factuality.scm
  - experiments/021-factuality/test-factuality.scm
success_criteria:
  - Extract claims from response
  - Structure claims as checkable statements
  - Compare against provided context
  - Return grounding score
```

---

## RAG & Retrieval Experiments

### 022: Embedding Similarity Search
```yaml
id: experiment-022-embeddings
name: Vector Similarity Search Utilities
difficulty: medium
estimated_tokens: ~6000
inputs:
  - src/llm/utils/json.scm
outputs:
  - src/llm/rag/embeddings.scm
  - src/llm/rag/similarity.scm
  - experiments/022-embeddings/test-embeddings.scm
success_criteria:
  - cosine-similarity function
  - euclidean-distance function
  - top-k retrieval
  - Test with sample document set
```

### 023: Simple Document Chunking
```yaml
id: experiment-023-chunking
name: Text Chunking for RAG
difficulty: easy
estimated_tokens: ~4000
inputs:
  - (none - standalone)
outputs:
  - src/llm/rag/chunking.scm
  - experiments/023-chunking/test-chunking.scm
success_criteria:
  - chunk-by-tokens function
  - chunk-by-sentences function
  - chunk-with-overlap function
  - Test preserves content integrity
```

### 024: Context Window Management
```yaml
id: experiment-024-context
name: Context Window Packing
difficulty: medium
estimated_tokens: ~5000
inputs:
  - src/llm/core/compaction.scm
  - src/llm/rag/chunking.scm
outputs:
  - src/llm/rag/context.scm
  - experiments/024-context/test-context.scm
success_criteria:
  - pack-context function (fit chunks to limit)
  - Prioritize by relevance score
  - Handle overflow gracefully
  - Test with various chunk sizes
```

---

## Multi-Agent Experiments

### 025: Debate Pattern
```yaml
id: experiment-025-debate
name: Multi-Agent Debate Implementation
difficulty: hard
estimated_tokens: ~10000
inputs:
  - src/llm/core/prompts.scm
  - src/llm/core/provider.scm
outputs:
  - src/llm/agents/debate.scm
  - experiments/025-debate/test-debate.scm
  - experiments/025-debate/debate-transcript.scm
success_criteria:
  - Define debater-agent
  - Run N rounds of argument/counter-argument
  - Summarize conclusions
  - Test on ethical dilemma topic
```

### 026: Critic Pattern
```yaml
id: experiment-026-critic
name: Generator-Critic Agent Pattern
difficulty: medium
estimated_tokens: ~6000
inputs:
  - src/llm/core/prompts.scm
outputs:
  - src/llm/agents/critic.scm
  - experiments/026-critic/test-critic.scm
success_criteria:
  - Generator produces initial output
  - Critic evaluates and suggests improvements
  - Generator refines based on feedback
  - Test with code generation task
```

### 027: Ensemble Voting
```yaml
id: experiment-027-ensemble
name: Multi-Model Ensemble Voting
difficulty: medium
estimated_tokens: ~5000
inputs:
  - src/llm/core/provider.scm
outputs:
  - src/llm/agents/ensemble.scm
  - experiments/027-ensemble/test-ensemble.scm
success_criteria:
  - Query multiple providers
  - Aggregate responses (vote/merge)
  - Return consensus answer
  - Test with classification task
```

---

## Format & Parsing Experiments

### 028: Markdown Extraction
```yaml
id: experiment-028-markdown
name: Extract Structured Data from Markdown
difficulty: easy
estimated_tokens: ~3000
inputs:
  - (none)
outputs:
  - src/llm/formats/markdown.scm
  - experiments/028-markdown/test-markdown.scm
success_criteria:
  - Extract code blocks by language
  - Extract headers as outline
  - Extract lists as Scheme lists
  - Test with LLM-generated markdown
```

### 029: JSON Repair
```yaml
id: experiment-029-json-repair
name: Repair Malformed JSON from LLMs
difficulty: medium
estimated_tokens: ~5000
inputs:
  - src/llm/utils/json.scm
outputs:
  - src/llm/formats/json-repair.scm
  - experiments/029-json-repair/test-repair.scm
success_criteria:
  - Fix unclosed brackets
  - Fix trailing commas
  - Fix unquoted keys
  - Test with common LLM JSON errors
```

### 030: XML/HTML Parsing
```yaml
id: experiment-030-xml
name: Simple XML/HTML Response Parser
difficulty: medium
estimated_tokens: ~4000
inputs:
  - (none)
outputs:
  - src/llm/formats/xml.scm
  - experiments/030-xml/test-xml.scm
success_criteria:
  - Parse tag structure
  - Extract by tag name
  - Handle attributes
  - Test with tool-use XML format
```

---

## Performance Experiments

### 031: Request Batching
```yaml
id: experiment-031-batching
name: Batch Multiple Requests
difficulty: medium
estimated_tokens: ~5000
inputs:
  - src/llm/utils/http.scm
  - src/llm/core/provider.scm
outputs:
  - src/llm/perf/batching.scm
  - experiments/031-batching/test-batching.scm
success_criteria:
  - Batch N requests into single call (where supported)
  - Fallback to sequential for unsupported
  - Return results in order
  - Test latency improvement
```

### 032: Response Caching
```yaml
id: experiment-032-caching
name: Deterministic Response Cache
difficulty: easy
estimated_tokens: ~4000
inputs:
  - src/llm/utils/json.scm
outputs:
  - src/llm/perf/cache.scm
  - experiments/032-caching/test-cache.scm
success_criteria:
  - Hash prompt+params for cache key
  - File-based cache storage
  - TTL expiration
  - Test cache hit/miss
```

### 033: Token Counting
```yaml
id: experiment-033-tokens
name: Accurate Token Counting
difficulty: medium
estimated_tokens: ~5000
inputs:
  - src/llm/core/compaction.scm (has estimate)
outputs:
  - src/llm/perf/tokenizer.scm
  - experiments/033-tokens/test-tokens.scm
success_criteria:
  - BPE tokenizer basics
  - cl100k_base approximation
  - Compare with API token counts
  - Test accuracy on various texts
```

---

## Formal Methods Experiments

### 034: Contract Generators
```yaml
id: experiment-034-generators
name: Property-Based Testing with Contracts
difficulty: hard
estimated_tokens: ~8000
inputs:
  - src/llm/core/contracts.scm
outputs:
  - src/llm/contracts/generators.scm
  - experiments/034-generators/test-generators.scm
success_criteria:
  - Generate random valid inputs from contracts
  - Shrinking for counterexamples
  - QuickCheck-style testing
  - Test provider contracts
```

### 035: TLA+ Spec Generation
```yaml
id: experiment-035-tlaplus
name: Generate TLA+ from Contracts
difficulty: hard
estimated_tokens: ~10000
inputs:
  - src/llm/core/contracts.scm
  - docs/formal-specification.md
outputs:
  - src/llm/formal/tlaplus.scm
  - experiments/035-tlaplus/provider.tla
  - experiments/035-tlaplus/test-tlaplus.scm
success_criteria:
  - Convert flat contracts to predicates
  - Convert function contracts to actions
  - Generate valid TLA+ syntax
  - Test with provider interface
```

---

## How to Work on a Task

1. **Read inputs** - Study the referenced files
2. **Create output files** - Follow naming conventions
3. **Implement** - Write idiomatic Guile Scheme
4. **Test** - Ensure success criteria are met
5. **Document** - Add comments explaining approach

### Code Style

```scheme
;;; module-name.scm --- Brief description

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm category module-name)
  #:use-module (required modules)
  #:export (public-functions))

;;; Commentary:
;;; Extended description of module purpose.

;;; Code:

(define (public-function args)
  "Docstring explaining function."
  implementation)

;;; module-name.scm ends here
```

### Running Tests

```bash
# From project root
guile3 -L src experiments/XXX-name/test-name.scm
```

---

## Task Assignment

Tasks can be assigned to different AI systems based on their strengths:

| System | Recommended Tasks |
|--------|-------------------|
| Claude | Complex reasoning (025, 034, 035), Code review |
| Gemini | Provider impl (013), Multi-modal tasks |
| GPT-4 | General implementation, Documentation |
| Ollama/Local | Testing, Simple utilities (023, 028, 032) |
| Codex/Copilot | Code completion, Boilerplate |

---

## Contributing

When completing a task:
1. Create a PR with your implementation
2. Include test output showing success criteria met
3. Document any deviations from spec
4. Note which AI system was used
