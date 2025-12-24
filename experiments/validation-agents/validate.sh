#!/usr/bin/env bash
# validate.sh - Validate scheme-llm-toolkit components
# Works around Guile HTTP segfaults by using curl directly

set -e

OLLAMA_URL="${OLLAMA_URL:-http://localhost:11434}"
MODEL="${MODEL:-llama3.2:1b}"
TIMEOUT=120

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

pass() { echo -e "  [${GREEN}PASS${NC}] $1"; }
fail() { echo -e "  [${RED}FAIL${NC}] $1"; }
warn() { echo -e "  [${YELLOW}WARN${NC}] $1"; }
header() { echo -e "\n=== $1 ==="; }

# Check if Ollama is running
check_ollama() {
    header "Checking Ollama"
    if curl -s --max-time 5 "$OLLAMA_URL/api/tags" > /dev/null 2>&1; then
        pass "Ollama responding at $OLLAMA_URL"
        return 0
    else
        fail "Ollama not responding"
        echo "  Run: ollama serve"
        return 1
    fi
}

# List models
list_models() {
    header "Available Models"
    local models=$(curl -s "$OLLAMA_URL/api/tags" | jq -r '.models[].name' 2>/dev/null)
    if [ -n "$models" ]; then
        echo "$models" | while read m; do echo "  - $m"; done
        pass "Models listed"
    else
        fail "No models found"
    fi
}

# Test Guile modules (non-HTTP)
test_guile_modules() {
    header "Testing Guile Modules"

    # JSON module
    if guile3 -L src -c '(use-modules (llm utils json)) (display "ok")' 2>/dev/null | grep -q ok; then
        pass "JSON module loads"
    else
        fail "JSON module failed"
    fi

    # Prompts module
    if guile3 -L src -c '(use-modules (llm core prompts)) (display "ok")' 2>/dev/null | grep -q ok; then
        pass "Prompts module loads"
    else
        fail "Prompts module failed"
    fi

    # Compaction module
    if guile3 -L src -c '(use-modules (llm core compaction)) (display "ok")' 2>/dev/null | grep -q ok; then
        pass "Compaction module loads"
    else
        fail "Compaction module failed"
    fi

    # Contracts module
    if guile3 -L src -c '(use-modules (llm core contracts)) (display "ok")' 2>/dev/null | grep -q ok; then
        pass "Contracts module loads"
    else
        fail "Contracts module failed"
    fi

    # Structured module
    if guile3 -L src -c '(use-modules (llm core structured)) (display "ok")' 2>/dev/null | grep -q ok; then
        pass "Structured output module loads"
    else
        fail "Structured module failed"
    fi
}

# Test prompt DSL
test_prompt_dsl() {
    header "Testing Prompt DSL"

    local result=$(guile3 -L src -c '
(use-modules (llm core prompts))
(define p (prompt
            (system-msg "You are helpful.")
            (user-msg (template "Hello " (var (quote name)) "!"))))
(define filled (interpolate p (quote ((name . "World")))))
(define msgs (prompt->messages filled))
(format #t "~a messages, content: ~a"
        (length msgs)
        (assoc-ref (cadr msgs) (quote content)))
' 2>&1)

    if echo "$result" | grep -q "2 messages.*Hello World"; then
        pass "Prompt template interpolation works"
        echo "    $result"
    else
        fail "Prompt DSL failed: $result"
    fi
}

# Test compaction
test_compaction() {
    header "Testing Compaction"

    local result=$(guile3 -L src -c '
(use-modules (llm core compaction))
(define msgs
  (list
    (list (cons (quote role) "system") (cons (quote content) "Be helpful."))
    (list (cons (quote role) "user") (cons (quote content) "Question 1"))
    (list (cons (quote role) "assistant") (cons (quote content) "Answer 1"))
    (list (cons (quote role) "user") (cons (quote content) "Question 2"))
    (list (cons (quote role) "assistant") (cons (quote content) "Answer 2"))
    (list (cons (quote role) "user") (cons (quote content) "Question 3"))))
(define config (make-compaction-config #:strategy (quote sliding-window) #:max-tokens 30))
(define compacted (compact-conversation msgs #:config config))
(format #t "~a -> ~a messages" (length msgs) (length compacted))
' 2>&1)

    if echo "$result" | grep -q "6 -> [0-5] messages"; then
        pass "Compaction reduces messages"
        echo "    $result"
    else
        fail "Compaction failed: $result"
    fi
}

# Test retry logic
test_retry() {
    header "Testing Retry Logic"

    local result=$(guile3 -L src -c '
(use-modules (llm utils http))
(define config (make-retry-config #:max-retries 3 #:initial-delay 0.01))
(define call-count 0)
(define result
  (with-retry
   (lambda ()
     (set! call-count (+ call-count 1))
     (if (< call-count 3)
         (throw (quote http-error) "503 retry")
         (quote success)))
   #:config config))
(format #t "result: ~a, calls: ~a" result call-count)
' 2>&1)

    if echo "$result" | grep -q "result: success, calls: 3"; then
        pass "Retry logic works"
        echo "    $result"
    else
        fail "Retry failed: $result"
    fi
}

# Test structured output
test_structured() {
    header "Testing Structured Output"

    local result=$(guile3 -L src -c '
(use-modules (llm core structured) (llm core contracts))
(define schema (object-schema
                (list (cons (quote name) (string-schema))
                      (cons (quote age) (number-schema)))))
(define contract (json-schema/c schema))
(define valid (quote ((name . "Alice") (age . 30))))
(format #t "schema-type: ~a, valid-check: ~a"
        (assoc-ref schema (quote type))
        (contract-check contract valid))
' 2>&1)

    if echo "$result" | grep -q "schema-type: object, valid-check: #t"; then
        pass "Structured output/contracts work"
        echo "    $result"
    else
        fail "Structured output failed: $result"
    fi
}

# Test LLM call (if Ollama responsive)
test_llm_call() {
    header "Testing LLM Call (Ollama)"

    # Quick responsiveness check
    local response=$(timeout 10 curl -s "$OLLAMA_URL/api/generate" \
        -d "{\"model\":\"$MODEL\",\"prompt\":\"1+1=\",\"stream\":false,\"options\":{\"num_predict\":3}}" 2>&1)

    if [ $? -eq 0 ] && echo "$response" | grep -q "response"; then
        local answer=$(echo "$response" | jq -r '.response' 2>/dev/null | head -1)
        pass "LLM responded: $answer"
    else
        warn "LLM call timed out (Ollama may be slow)"
        echo "    Try: ollama run $MODEL \"1+1=\""
    fi
}

# Main
main() {
    echo "=============================================="
    echo "  SCHEME LLM TOOLKIT - VALIDATION SUITE"
    echo "=============================================="
    echo "Model: $MODEL"
    echo "Ollama: $OLLAMA_URL"

    check_ollama || exit 1
    list_models
    test_guile_modules
    test_prompt_dsl
    test_compaction
    test_retry
    test_structured
    test_llm_call

    header "SUMMARY"
    echo "Validation complete. Check results above."
    echo ""
}

cd "$(dirname "$0")/../.."
main
