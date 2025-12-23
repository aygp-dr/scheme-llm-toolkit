#!/usr/bin/env -S guile3 -L ../../src -s
!#
;;; test-structured.scm --- Test structured output support

(use-modules (llm core structured)
             (llm core contracts)
             (llm utils json)
             (ice-9 format))

;; Simple assert macro
(define-syntax assert
  (syntax-rules ()
    ((assert expr)
     (unless expr
       (error "Assertion failed" 'expr)))
    ((assert expr msg)
     (unless expr
       (error msg 'expr)))))

(define (test-response-formats)
  (display "Testing response formats...\n")

  ;; Test JSON mode
  (let ((mode (json-mode)))
    (assert (equal? (assoc-ref mode 'type) "json_object"))
    (display "  json-mode: PASS\n"))

  ;; Test structured output
  (let* ((schema (object-schema
                  `((name . ,(string-schema))
                    (age . ,(number-schema)))))
         (format (structured-output schema #:name "person")))
    (assert (equal? (assoc-ref format 'type) "json_schema"))
    (assert (assoc-ref format 'json_schema))
    (display "  structured-output: PASS\n")))

(define (test-schema-helpers)
  (display "\nTesting schema helpers...\n")

  ;; String schema
  (let ((s (string-schema)))
    (assert (equal? (assoc-ref s 'type) "string"))
    (display "  string-schema: PASS\n"))

  ;; Number schema with bounds
  (let ((s (number-schema #:minimum 0 #:maximum 100)))
    (assert (equal? (assoc-ref s 'type) "number"))
    (assert (= (assoc-ref s 'minimum) 0))
    (assert (= (assoc-ref s 'maximum) 100))
    (display "  number-schema with bounds: PASS\n"))

  ;; Boolean schema
  (let ((s (boolean-schema)))
    (assert (equal? (assoc-ref s 'type) "boolean"))
    (display "  boolean-schema: PASS\n"))

  ;; Array schema
  (let ((s (array-schema (string-schema) #:min-items 1)))
    (assert (equal? (assoc-ref s 'type) "array"))
    (assert (= (assoc-ref s 'minItems) 1))
    (display "  array-schema: PASS\n"))

  ;; Object schema
  (let ((s (object-schema
            `((name . ,(string-schema))
              (email . ,(string-schema)))
            #:required '("name"))))
    (assert (equal? (assoc-ref s 'type) "object"))
    (assert (assoc-ref s 'properties))
    (display "  object-schema: PASS\n"))

  ;; Enum schema
  (let ((s (enum-schema '("red" "green" "blue"))))
    (assert (assoc-ref s 'enum))
    (display "  enum-schema: PASS\n")))

(define (test-provider-format)
  (display "\nTesting provider format conversion...\n")

  (let ((mode (json-mode)))
    ;; OpenAI format
    (let ((openai (response-format->provider mode #:provider 'openai)))
      (assert (assoc-ref openai 'response_format))
      (display "  OpenAI format: PASS\n"))

    ;; Ollama format
    (let ((ollama (response-format->provider mode #:provider 'ollama)))
      (assert (equal? (assoc-ref ollama 'format) "json"))
      (display "  Ollama format: PASS\n"))))

(define (test-json-parsing)
  (display "\nTesting JSON response parsing...\n")

  ;; Valid JSON without schema
  (let ((result (parse-json-response "{\"name\": \"Alice\", \"age\": 30}")))
    (assert (eq? (car result) 'ok))
    (assert (equal? (assoc-ref (cdr result) 'name) "Alice"))
    (display "  parse valid JSON: PASS\n"))

  ;; Invalid JSON
  (let ((result (parse-json-response "not valid json {")))
    (assert (eq? (car result) 'error))
    (display "  parse invalid JSON: PASS\n"))

  ;; Valid JSON with matching schema
  (let* ((schema (object-schema
                  `((name . ,(string-schema))
                    (age . ,(number-schema)))))
         (result (parse-json-response "{\"name\": \"Bob\", \"age\": 25}"
                                      #:schema schema)))
    (assert (eq? (car result) 'ok))
    (display "  parse with matching schema: PASS\n")))

(define (test-schema-to-contract)
  (display "\nTesting schema to contract conversion...\n")

  ;; String contract
  (let ((contract (schema->contract (string-schema))))
    (assert (contract-check contract "hello"))
    (display "  string contract: PASS\n"))

  ;; Number contract
  (let ((contract (schema->contract (number-schema))))
    (assert (contract-check contract 42))
    (assert (contract-check contract 3.14))
    (display "  number contract: PASS\n"))

  ;; Number with bounds
  (let ((contract (schema->contract (number-schema #:minimum 0 #:maximum 10))))
    (assert (contract-check contract 5))
    (display "  bounded number contract: PASS\n"))

  ;; Enum contract
  (let ((contract (schema->contract (enum-schema '("a" "b" "c")))))
    (assert (contract-check contract "a"))
    (display "  enum contract: PASS\n")))

(define (test-record-generation)
  (display "\nTesting record type generation...\n")

  (let* ((schema (object-schema
                  `((name . ,(string-schema))
                    (age . ,(number-schema))
                    (active . ,(boolean-schema)))))
         (record-code (schema->record-type schema "person")))
    (assert (string-contains record-code "define-record-type"))
    (assert (string-contains record-code "<person>"))
    (assert (string-contains record-code "make-person"))
    (display "  record type generation: PASS\n")
    (format #t "  Generated code:\n~a\n" record-code)))

;; Run all tests
(display "=== Structured Output Tests ===\n\n")

(test-response-formats)
(test-schema-helpers)
(test-provider-format)
(test-json-parsing)
(test-schema-to-contract)
(test-record-generation)

(display "\n=== All tests passed! ===\n")
