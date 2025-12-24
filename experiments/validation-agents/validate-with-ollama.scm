#!/usr/bin/env -S guile3 -L ../../src -s
!#
;;; validate-with-ollama.scm --- Validate implementations with live Ollama calls

(use-modules (llm providers ollama)
             (llm core provider)
             (llm core prompts)
             (llm core compaction)
             (llm core structured)
             (llm utils http)
             (llm utils json)
             (ice-9 format)
             (ice-9 match)
             (srfi srfi-1))

;;; --------------------------------------------------------------------
;;; Test Configuration
;;; --------------------------------------------------------------------

(define *ollama-url* "http://localhost:11434")
(define *model* "llama3.2:1b")
(define *verbose* #t)

(define (log msg . args)
  (when *verbose*
    (apply format #t msg args)
    (newline)))

(define (test-header name)
  (format #t "\n~a\n~a\n" (make-string 60 #\=) name)
  (format #t "~a\n" (make-string 60 #\=)))

(define (test-result name passed? details)
  (format #t "  [~a] ~a\n" (if passed? "PASS" "FAIL") name)
  (when (and (not passed?) details)
    (format #t "       ~a\n" details)))

;;; --------------------------------------------------------------------
;;; Test 1: Basic Ollama Provider
;;; --------------------------------------------------------------------

(define (test-ollama-provider)
  (test-header "TEST 1: Ollama Provider")

  ;; Test connection
  (log "Checking Ollama connection...")
  (let ((accessible? (check-url-accessible (string-append *ollama-url* "/api/tags"))))
    (test-result "Ollama accessible" accessible? "Cannot connect to Ollama")
    (unless accessible?
      (format #t "  Run: ollama serve\n")
      (exit 1)))

  ;; Test list models
  (log "Listing models...")
  (let ((models (ollama-list-models *ollama-url*)))
    (test-result "List models" (and (list? models) (> (length models) 0))
                 (format #f "Got: ~a" models))
    (log "  Available: ~{~a~^, ~}" (map (lambda (m) (assoc-ref m 'name)) models)))

  ;; Test simple completion
  (log "Testing completion with ~a..." *model*)
  (let ((response (ollama-complete *ollama-url* *model* "Say 'test passed' and nothing else.")))
    (let ((content (or (assoc-ref response 'response)
                       (assoc-ref response 'message))))
      (test-result "Simple completion"
                   (and content (string? content) (> (string-length content) 0))
                   (format #f "Response: ~a" response))
      (when content
        (log "  Response: ~a" (if (> (string-length content) 50)
                                  (string-append (substring content 0 50) "...")
                                  content))))))

;;; --------------------------------------------------------------------
;;; Test 2: Prompt DSL with LLM
;;; --------------------------------------------------------------------

(define (test-prompt-dsl)
  (test-header "TEST 2: Prompt DSL with LLM")

  ;; Test template interpolation
  (log "Testing prompt template...")
  (let* ((tmpl (template "What is the capital of " (var 'country) "? Answer in one word."))
         (msg (user-msg tmpl))
         (filled (interpolate msg '((country . "France"))))
         (content (assoc-ref filled 'content)))
    (test-result "Template interpolation"
                 (string-contains content "France")
                 (format #f "Got: ~a" content)))

  ;; Test prompt with actual LLM
  (log "Testing prompt with LLM...")
  (let* ((p (prompt
             (system-msg "You are a helpful assistant. Be very concise.")
             (user-msg (template "What is 2 + " (var 'num) "? Just the number."))))
         (filled (interpolate p '((num . "3"))))
         (messages (prompt->messages filled))
         (response (ollama-chat *ollama-url* *model* messages)))
    (let ((answer (assoc-ref (assoc-ref response 'message) 'content)))
      (test-result "Prompt with LLM"
                   (and answer (string-contains answer "5"))
                   (format #f "Expected 5, got: ~a" answer))
      (when answer
        (log "  Answer: ~a" answer))))

  ;; Test provider formatting
  (log "Testing provider format conversion...")
  (let* ((p (prompt
             (system-msg "System message")
             (user-msg "User message")))
         (ollama-fmt (format-for-provider p #:provider 'ollama))
         (anthropic-fmt (format-for-provider p #:provider 'anthropic)))
    (test-result "Ollama format" (assoc-ref ollama-fmt 'messages) "Missing messages")
    (test-result "Anthropic format" (assoc-ref anthropic-fmt 'system) "Missing system")))

;;; --------------------------------------------------------------------
;;; Test 3: Structured Output
;;; --------------------------------------------------------------------

(define (test-structured-output)
  (test-header "TEST 3: Structured Output")

  ;; Test schema creation
  (log "Testing schema creation...")
  (let ((schema (object-schema
                 `((name . ,(string-schema))
                   (age . ,(number-schema #:minimum 0 #:maximum 150))
                   (active . ,(boolean-schema))))))
    (test-result "Object schema" (equal? (assoc-ref schema 'type) "object") ""))

  ;; Test JSON parsing from LLM
  (log "Testing JSON extraction from LLM...")
  (let* ((prompt-text "Return ONLY valid JSON with no markdown: {\"name\": \"Alice\", \"age\": 30}")
         (response (ollama-complete *ollama-url* *model* prompt-text))
         (content (or (assoc-ref response 'response) "")))
    (log "  Raw response: ~a" content)
    (let ((result (parse-json-response content)))
      (test-result "JSON parsing"
                   (eq? (car result) 'ok)
                   (format #f "Parse failed: ~a" (cdr result)))
      (when (eq? (car result) 'ok)
        (log "  Parsed: ~a" (cdr result)))))

  ;; Test schema validation
  (log "Testing schema validation...")
  (let* ((schema (object-schema
                  `((color . ,(enum-schema '("red" "green" "blue"))))))
         (valid-data '((color . "red")))
         (invalid-data '((color . "purple"))))
    (test-result "Valid data passes"
                 (contract-check (json-schema/c schema) valid-data)
                 "Should have passed")))

;;; --------------------------------------------------------------------
;;; Test 4: Compaction with LLM
;;; --------------------------------------------------------------------

(define (test-compaction)
  (test-header "TEST 4: Compaction")

  ;; Test basic compaction
  (log "Testing sliding-window compaction...")
  (let* ((messages (list
                    '((role . "system") (content . "You are helpful."))
                    '((role . "user") (content . "What is Python?"))
                    '((role . "assistant") (content . "Python is a programming language."))
                    '((role . "user") (content . "What about JavaScript?"))
                    '((role . "assistant") (content . "JavaScript runs in browsers."))
                    '((role . "user") (content . "Which is better for web?"))))
         (config (make-compaction-config
                  #:strategy 'sliding-window
                  #:max-tokens 50
                  #:preserve-recent 2))
         (compacted (compact-conversation messages #:config config)))
    (test-result "Compaction reduces messages"
                 (< (length compacted) (length messages))
                 (format #f "~a -> ~a" (length messages) (length compacted)))
    (log "  Messages: ~a -> ~a" (length messages) (length compacted))
    (log "  Tokens: ~a -> ~a"
         (messages-token-count messages)
         (messages-token-count compacted)))

  ;; Test compacted conversation with LLM
  (log "Testing compacted conversation with LLM...")
  (let* ((messages (list
                    '((role . "system") (content . "You are a math tutor. Be very brief."))
                    '((role . "user") (content . "What is 2+2?"))
                    '((role . "assistant") (content . "4"))
                    '((role . "user") (content . "What is 3+3?"))
                    '((role . "assistant") (content . "6"))
                    '((role . "user") (content . "What is the pattern?"))))
         (config (make-compaction-config
                  #:strategy 'sliding-window
                  #:max-tokens 100))
         (compacted (compact-conversation messages #:config config))
         (response (ollama-chat *ollama-url* *model* compacted)))
    (let ((answer (assoc-ref (assoc-ref response 'message) 'content)))
      (test-result "LLM responds to compacted"
                   (and answer (> (string-length answer) 0))
                   "No response")
      (when answer
        (log "  Response: ~a" (if (> (string-length answer) 60)
                                  (string-append (substring answer 0 60) "...")
                                  answer))))))

;;; --------------------------------------------------------------------
;;; Test 5: Retry Logic
;;; --------------------------------------------------------------------

(define (test-retry-logic)
  (test-header "TEST 5: Retry Logic")

  ;; Test retry config
  (log "Testing retry configuration...")
  (let ((config (make-retry-config
                 #:max-retries 3
                 #:initial-delay 0.1
                 #:jitter? #f)))
    (test-result "Config creation"
                 (= (assoc-ref config 'max-retries) 3)
                 ""))

  ;; Test retryable error detection
  (log "Testing retryable error detection...")
  (test-result "429 is retryable" (retryable-error? 429) "")
  (test-result "500 is retryable" (retryable-error? 500) "")
  (test-result "404 is not retryable" (not (retryable-error? 404)) "")
  (test-result "rate limit string" (retryable-error? "rate limit exceeded") "")

  ;; Test successful retry
  (log "Testing with-retry success...")
  (let ((call-count 0))
    (define result
      (with-retry
       (lambda ()
         (set! call-count (+ call-count 1))
         'success)
       #:config (make-retry-config #:max-retries 3)))
    (test-result "Success on first try"
                 (and (eq? result 'success) (= call-count 1))
                 (format #f "Calls: ~a" call-count)))

  ;; Test retry on transient failure
  (log "Testing retry on transient failure...")
  (let ((call-count 0))
    (define result
      (with-retry
       (lambda ()
         (set! call-count (+ call-count 1))
         (if (< call-count 3)
             (throw 'http-error "503 Service Unavailable")
             'success-after-retry))
       #:config (make-retry-config
                 #:max-retries 5
                 #:initial-delay 0.01
                 #:jitter? #f)))
    (test-result "Success after retries"
                 (and (eq? result 'success-after-retry) (= call-count 3))
                 (format #f "Expected 3 calls, got ~a" call-count))))

;;; --------------------------------------------------------------------
;;; Test 6: End-to-End Validation
;;; --------------------------------------------------------------------

(define (test-end-to-end)
  (test-header "TEST 6: End-to-End Validation")

  ;; Build a full conversation, compact it, get structured response
  (log "Testing full pipeline...")

  (let* (;; 1. Build prompt with DSL
         (p (prompt
             (system-msg "You are a code review assistant. Respond in JSON format with keys: issue, severity (low/medium/high), fix")
             (user-msg "Review: SELECT * FROM users WHERE id = '" )
             (user-msg (template "id: " (var 'user-input)))))

         ;; 2. Interpolate variables
         (filled (interpolate p '((user-input . "1; DROP TABLE users;--"))))

         ;; 3. Convert to messages
         (messages (prompt->messages filled))

         ;; 4. Send to LLM
         (response (ollama-chat *ollama-url* *model* messages))
         (content (assoc-ref (assoc-ref response 'message) 'content)))

    (log "  Prompt built and sent")
    (log "  Response: ~a" (if (and content (> (string-length content) 80))
                              (string-append (substring content 0 80) "...")
                              content))

    (test-result "End-to-end pipeline"
                 (and content (> (string-length content) 0))
                 "No response from LLM")))

;;; --------------------------------------------------------------------
;;; Main
;;; --------------------------------------------------------------------

(define (main)
  (format #t "\n")
  (format #t "╔══════════════════════════════════════════════════════════╗\n")
  (format #t "║     SCHEME LLM TOOLKIT - OLLAMA VALIDATION SUITE         ║\n")
  (format #t "╚══════════════════════════════════════════════════════════╝\n")
  (format #t "\nModel: ~a\n" *model*)
  (format #t "URL: ~a\n" *ollama-url*)

  (test-ollama-provider)
  (test-prompt-dsl)
  (test-structured-output)
  (test-compaction)
  (test-retry-logic)
  (test-end-to-end)

  (format #t "\n~a\n" (make-string 60 #\=))
  (format #t "VALIDATION COMPLETE\n")
  (format #t "~a\n\n" (make-string 60 #\=)))

(main)
