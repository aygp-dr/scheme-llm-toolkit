# Formal Specification: Scheme LLM Toolkit

This document provides formal specifications for the toolkit's core abstractions,
bridging Racket-style contracts with formal methods (TLA+, Lean4, OpenAPI).

## 1. Philosophical Foundation

### Racket Contracts

Our contract system follows Racket's philosophy:

1. **Contracts as Agreements** - A contract specifies obligations between parties
2. **Blame Assignment** - When violated, identify WHO broke the agreement
3. **Higher-Order Contracts** - Functions themselves can have contracts
4. **Compositional** - Complex contracts built from simple ones

### Mapping to Formal Methods

| Racket Concept | TLA+ Equivalent | Lean4 Equivalent | OpenAPI |
|----------------|-----------------|------------------|---------|
| Flat contract | State predicate | Prop | type |
| Function contract | Action spec | Function type | operation |
| Dependent contract | ENABLED pred | Dependent type | - |
| Blame | Error trace | Error monad | 4xx/5xx |

## 2. Provider Interface Specification

### 2.1 TLA+ Style Specification

```tla
---------------------------- MODULE Provider ----------------------------
EXTENDS Naturals, Sequences

CONSTANTS Providers, Models, Messages, Responses

VARIABLES
    providerState,    \* Provider -> State
    requestQueue,     \* Seq of requests
    responseCache     \* Request -> Response

TypeInvariant ==
    /\ providerState \in [Providers -> {"ready", "busy", "error"}]
    /\ requestQueue \in Seq([provider: Providers, prompt: STRING])

\* Provider must be ready to accept requests
CanAcceptRequest(p) ==
    providerState[p] = "ready"

\* Complete action
Complete(p, prompt, response) ==
    /\ CanAcceptRequest(p)
    /\ providerState' = [providerState EXCEPT ![p] = "busy"]
    /\ response \in Responses
    /\ providerState' = [providerState EXCEPT ![p] = "ready"]

\* Safety: Never return response when in error state
Safety ==
    \A p \in Providers:
        providerState[p] = "error" =>
            ~(\E r \in Responses: Complete(p, _, r))

\* Liveness: Every request eventually gets a response (with fairness)
Liveness ==
    \A req \in requestQueue:
        <>(req \notin requestQueue)
=========================================================================
```

### 2.2 Lean4 Style Specification

```lean4
-- Provider type with proofs
structure Provider where
  name : String
  complete : String → IO (Except Error Response)
  chat : List Message → IO (Except Error Response)

  -- Proof obligations
  complete_deterministic : ∀ p, complete p = complete p
  chat_preserves_order : ∀ msgs,
    (chat msgs).map (·.messages) = some msgs ++ _

-- Message type with role constraint
inductive Role where
  | user | assistant | system | tool
  deriving DecidableEq

structure Message where
  role : Role
  content : String

-- Response is either content or tool calls
inductive Response where
  | text (content : String)
  | toolCalls (calls : List ToolCall)

-- Contract: chat with empty messages returns error
theorem chat_nonempty (p : Provider) :
  p.chat [] = IO.pure (Except.error EmptyMessages) := by
  sorry  -- proof here

-- Contract: tool calls require tools in request
theorem tool_calls_require_tools (p : Provider) (msgs : List Message) :
  (p.chat msgs).bind (·.isToolCalls) → msgs.any (·.hasTools) := by
  sorry
```

### 2.3 Scheme Contract Specification

```scheme
;;; Provider Interface Contract

(define provider-complete/c
  "Contract for provider-complete function.

   PRECONDITIONS:
   - provider satisfies provider?
   - prompt is non-empty string

   POSTCONDITIONS:
   - Returns string OR structured response with tool_calls
   - Never throws for valid input (may return error response)

   BLAME:
   - Invalid provider → caller's fault (negative party)
   - Invalid prompt → caller's fault (negative party)
   - Invalid response → provider's fault (positive party)"

  (->i/c (provider provider?/c)
         (prompt (and/c string/c (lambda (s) (> (string-length s) 0))))
         (result response?/c)))

(define provider-chat/c
  "Contract for provider-chat function.

   PRECONDITIONS:
   - messages is non-empty list of valid messages
   - Each message has role and content
   - Messages alternate user/assistant (optional constraint)

   POSTCONDITIONS:
   - Returns response with assistant role
   - If tools provided and invoked, returns tool_calls"

  (->i/c (provider provider?/c)
         (messages (and/c messages/c
                        (lambda (m) (> (length m) 0))))
         (result response?/c)))
```

## 3. Tool/Function Calling Specification

### 3.1 TLA+ Specification

```tla
---------------------------- MODULE ToolUse ----------------------------
EXTENDS Naturals, Sequences, TLC

CONSTANTS Tools, Arguments, Results

VARIABLES
    toolState,       \* Tool -> State
    pendingCalls,    \* Seq of tool calls
    executedResults  \* Call ID -> Result

\* Tool call lifecycle
ToolCallStates == {"pending", "executing", "completed", "failed"}

\* A tool call must have valid arguments per schema
ValidToolCall(tool, args) ==
    /\ tool \in Tools
    /\ args \in Arguments
    /\ SchemaValidates(tool.schema, args)

\* Tool execution is atomic
ExecuteTool(callId, tool, args) ==
    /\ toolState[tool] = "ready"
    /\ ValidToolCall(tool, args)
    /\ toolState' = [toolState EXCEPT ![tool] = "executing"]
    /\ \E result \in Results:
        /\ executedResults' = [executedResults EXCEPT ![callId] = result]
        /\ toolState' = [toolState EXCEPT ![tool] = "ready"]

\* Safety: No orphan tool calls
NoOrphanCalls ==
    \A c \in pendingCalls: <>(c.id \in DOMAIN executedResults)
=========================================================================
```

### 3.2 Scheme Contract

```scheme
;;; Tool Definition Contract

(define tool-definition/c
  "Contract for tool definitions.

   A tool must have:
   - name: unique identifier (string, no spaces)
   - description: human-readable purpose
   - parameters: JSON Schema for arguments
   - handler: optional execution function

   INVARIANTS:
   - If handler provided, arity matches parameter count
   - Parameters schema is valid JSON Schema"

  (alist/c
   #:required
   `((name . ,(and/c string/c
                    (lambda (s) (not (string-index s #\space)))))
     (description . ,string/c)
     (parameters . ,json-schema/c))
   #:optional
   `((handler . ,procedure/c))))

(define tool-call/c
  "Contract for tool call (from LLM response)."
  (alist/c
   #:required
   `((id . ,string/c)
     (name . ,string/c))
   #:optional
   `((arguments . ,(or/c string/c list/c)))))

(define tool-result/c
  "Contract for tool execution result."
  (alist/c
   #:required
   `((id . ,string/c)
     (content . ,string/c))
   #:optional
   `((error . ,boolean/c))))
```

## 4. OpenAPI Schema Mapping

### 4.1 Provider API (OpenAPI 3.0)

```yaml
openapi: 3.0.0
info:
  title: Scheme LLM Toolkit Provider API
  version: 0.1.0

paths:
  /complete:
    post:
      operationId: providerComplete
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/CompletionRequest'
      responses:
        '200':
          description: Successful completion
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/CompletionResponse'
        '400':
          description: Contract violation (caller blame)
        '500':
          description: Provider error (callee blame)

components:
  schemas:
    Message:
      type: object
      required: [role, content]
      properties:
        role:
          type: string
          enum: [user, assistant, system, tool]
        content:
          type: string

    CompletionRequest:
      type: object
      required: [prompt]
      properties:
        prompt:
          type: string
          minLength: 1
        model:
          type: string
        temperature:
          type: number
          minimum: 0
          maximum: 2
        tools:
          type: array
          items:
            $ref: '#/components/schemas/Tool'

    Tool:
      type: object
      required: [name, description]
      properties:
        name:
          type: string
          pattern: '^[a-z_][a-z0-9_]*$'
        description:
          type: string
        parameters:
          $ref: '#/components/schemas/JSONSchema'

    CompletionResponse:
      oneOf:
        - type: string
        - type: object
          properties:
            role:
              type: string
            content:
              type: string
            tool_calls:
              type: array
              items:
                $ref: '#/components/schemas/ToolCall'
```

### 4.2 Schema to Contract Translation

```scheme
;;; Automatic contract generation from OpenAPI

(define (openapi->contract spec)
  "Convert OpenAPI schema to Scheme contract."
  (let ((schemas (assoc-ref spec 'components 'schemas)))
    (map (lambda (schema-def)
           (cons (car schema-def)
                 (json-schema/c (cdr schema-def))))
         schemas)))

;; Example usage:
(define message/c
  (schema->contract
   '((type . "object")
     (required . ("role" "content"))
     (properties
      . ((role . ((type . "string")
                  (enum . ("user" "assistant" "system" "tool"))))
         (content . ((type . "string"))))))))
```

## 5. Verification Strategy

### 5.1 Runtime Verification (Contracts)

```scheme
;; All public APIs use contracts
(define/contract (provider-complete provider prompt)
  provider-complete/c
  ...)

;; Contract violations produce blame
;; → "Contract violation: string?
;;    Expected: non-empty string
;;    Given: ""
;;    Blame: caller (negative party)"
```

### 5.2 Property-Based Testing

```scheme
;; Generate test cases from contracts
(define (contract-generate-tests contract n)
  "Generate n random test cases satisfying contract."
  (map (lambda (_) ((contract-generate contract)))
       (iota n)))

;; Test provider roundtrip
(check-property
 (for-all ((msg (contract-generate message/c)))
   (let ((response (provider-chat provider (list msg))))
     (contract-check response?/c response))))
```

### 5.3 Formal Verification Path

For critical invariants, translate to Lean4/TLA+:

1. **Scheme Contract** → Express intent
2. **Property Tests** → Build confidence
3. **Lean4 Proof** → Full verification (for critical paths)

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│ Scheme Contract │ --> │ Property Tests  │ --> │  Lean4 Proof    │
│   (Runtime)     │     │ (QuickCheck)    │     │  (Full Verify)  │
└─────────────────┘     └─────────────────┘     └─────────────────┘
        │                       │                       │
        v                       v                       v
   Catch bugs             Find edge cases         Prove correct
   in development         automatically           mathematically
```

## 6. Future Work

1. **Gradual Typing Integration** - Connect contracts to type inference
2. **Contract Monitoring** - Production telemetry on contract checks
3. **Lean4 Extraction** - Generate Lean proofs from contracts
4. **TLA+ Model Checking** - Verify concurrent provider usage
