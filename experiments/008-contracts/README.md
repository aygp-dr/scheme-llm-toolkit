# Experiment 008: Contract System

This experiment implements a design-by-contract system for the Scheme LLM Toolkit,
inspired by Racket's contract library and integrated with formal methods concepts.

## Philosophy

Following Racket's approach:

1. **Contracts as Agreements** - Specify what each party promises
2. **Blame Assignment** - When violated, identify who broke the contract
3. **Higher-Order Contracts** - Functions can have contracts too
4. **Compositional** - Build complex contracts from simple ones

## Integration with Formal Methods

| Approach | Purpose | When to Use |
|----------|---------|-------------|
| **Scheme Contracts** | Runtime checking | Development, testing |
| **Property Tests** | Find edge cases | Before release |
| **TLA+** | Model checking | Concurrent protocols |
| **Lean4** | Full proofs | Critical invariants |
| **OpenAPI** | API documentation | External interfaces |

## Usage

### Flat Contracts (Predicates)

```scheme
(use-modules (llm core contracts))

;; Create from predicate
(define positive? (flat-contract (lambda (x) (> x 0))
                                #:name "positive?"))

(contract-check positive? 5)   ; => #t
(contract-check positive? -1)  ; => #f
```

### Contract Combinators

```scheme
;; Logical AND
(define non-empty-string?
  (and/c (flat-contract string?)
         (flat-contract (lambda (s) (> (string-length s) 0)))))

;; Logical OR
(define string-or-number?
  (or/c (flat-contract string?)
        (flat-contract number?)))

;; Negation
(define not-null?
  (not/c (flat-contract null?)))
```

### Parametric Contracts

```scheme
;; List of numbers
(define numbers-list/c (listof/c (flat-contract number?)))

;; Association list
(define message/c
  (alist/c
   #:required `((role . ,(flat-contract string?))
                (content . ,(flat-contract string?)))
   #:optional `((name . ,(flat-contract string?)))))
```

### Function Contracts

```scheme
;; Function from string to number
(define string->number/c
  (->/c (flat-contract string?) (flat-contract number?)))

;; Apply contract to function
(define safe-length
  (let* ((c (->/c (flat-contract string?) (flat-contract number?)))
         (proj (contract-projection c)))
    (proj string-length 'string-length 'caller)))

(safe-length "hello")  ; => 5
(safe-length 42)       ; => CONTRACT VIOLATION, blame: caller
```

### define/contract

```scheme
(define/contract (add-positive a b)
  (->/c (flat-contract positive?) (flat-contract positive?) (flat-contract number?))
  (+ a b))

(add-positive 3 4)   ; => 7
(add-positive -1 4)  ; => CONTRACT VIOLATION
```

### JSON Schema Integration

```scheme
;; Convert JSON Schema to contract
(define user/c
  (json-schema/c
   '((type . "object")
     (required . ("name" "email"))
     (properties
      . ((name . ((type . "string")))
         (email . ((type . "string")))
         (age . ((type . "number"))))))))

(contract-check user/c '((name . "Alice") (email . "a@b.com")))  ; => #t
(contract-check user/c '((name . "Bob")))  ; => #f (missing email)
```

## Blame Assignment

When a contract is violated, the system identifies who is responsible:

- **Positive party** (callee/provider): Promised to return valid value
- **Negative party** (caller/client): Promised to provide valid arguments

```scheme
(with-contract-region my-module
  ;; Contract violations in here include 'my-module in context
  ...)
```

## LLM Domain Contracts

Pre-defined contracts for LLM types:

```scheme
;; Message must have role and content
message?/c

;; List of messages
messages/c

;; Response: string or structured with tool calls
response?/c

;; Provider interface
provider?/c
```

## Running Tests

```bash
chmod +x test-contracts.scm
./test-contracts.scm
```

## See Also

- `docs/formal-specification.md` - TLA+/Lean4 specifications
- Racket Contracts: https://docs.racket-lang.org/reference/contracts.html
