#!/usr/bin/env -S guile -L ../../src -e main -s
!#
;;; test-tools.scm --- Test suite for function calling / tool use

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(use-modules (llm core tools)
             (llm providers openai)
             (llm providers anthropic)
             (ice-9 format)
             (srfi srfi-1))

;;; Test utilities

(define (test-result name passed? message)
  (format #t "~a ~a: ~a~%"
          (if passed? "PASS" "FAIL")
          name
          message))

(define (run-test name thunk)
  (catch #t
    (lambda ()
      (let ((result (thunk)))
        (test-result name result (if result "OK" "Failed"))
        result))
    (lambda (key . args)
      (test-result name #f (format #f "Error: ~a ~a" key args))
      #f)))

;;; Tool definition tests

(define (test-make-tool)
  "Test basic tool creation"
  (let ((tool (make-tool
               #:name "get_weather"
               #:description "Get current weather for a city"
               #:parameters `((type . "object")
                             (properties . ((city . ((type . "string")
                                                    (description . "City name")))))
                             (required . ("city"))))))
    (and (tool? tool)
         (string=? (tool-name tool) "get_weather")
         (string=? (tool-description tool) "Get current weather for a city"))))

(define (test-tool-with-handler)
  "Test tool with handler function"
  (let ((tool (make-tool
               #:name "add_numbers"
               #:description "Add two numbers"
               #:parameters `((type . "object")
                             (properties . ((a . ((type . "number")))
                                           (b . ((type . "number")))))
                             (required . ("a" "b")))
               #:handler (lambda (a b) (+ a b)))))
    (and (tool? tool)
         (procedure? (tool-handler tool)))))

(define (test-tool-execution)
  "Test executing a tool"
  (let* ((tool (make-tool
                #:name "multiply"
                #:description "Multiply two numbers"
                #:parameters '()
                #:handler (lambda (a b) (* a b))))
         (result (execute-tool tool '((a . 3) (b . 4)))))
    (and (tool-result? result)
         (not (tool-result-error? result))
         (string=? (tool-result-content result) "12"))))

(define (test-openai-schema)
  "Test conversion to OpenAI format"
  (let* ((tool (make-tool
                #:name "search"
                #:description "Search the web"
                #:parameters `((type . "object")
                              (properties . ((query . ((type . "string"))))))))
         (schema (tool->openai-schema tool)))
    (and (string=? (assoc-ref schema 'type) "function")
         (let ((func (assoc-ref schema 'function)))
           (and (string=? (assoc-ref func 'name) "search")
                (string=? (assoc-ref func 'description) "Search the web"))))))

(define (test-anthropic-schema)
  "Test conversion to Anthropic format"
  (let* ((tool (make-tool
                #:name "calculate"
                #:description "Perform calculation"
                #:parameters `((type . "object")
                              (properties . ((expression . ((type . "string"))))))))
         (schema (tool->anthropic-schema tool)))
    (and (string=? (assoc-ref schema 'name) "calculate")
         (string=? (assoc-ref schema 'description) "Perform calculation")
         (assoc-ref schema 'input_schema))))

(define (test-tool-call-creation)
  "Test creating tool call records"
  (let ((call (make-tool-call
               #:id "call_abc123"
               #:name "get_weather"
               #:arguments '((city . "Tokyo")))))
    (and (tool-call? call)
         (string=? (tool-call-id call) "call_abc123")
         (string=? (tool-call-name call) "get_weather")
         (assoc-ref (tool-call-arguments call) 'city))))

(define (test-execute-tool-calls)
  "Test executing multiple tool calls"
  (let* ((add-tool (make-tool
                    #:name "add"
                    #:description "Add numbers"
                    #:parameters '()
                    #:handler (lambda (a b) (+ a b))))
         (sub-tool (make-tool
                    #:name "subtract"
                    #:description "Subtract numbers"
                    #:parameters '()
                    #:handler (lambda (a b) (- a b))))
         (calls (list (make-tool-call #:id "1" #:name "add" #:arguments '((a . 5) (b . 3)))
                      (make-tool-call #:id "2" #:name "subtract" #:arguments '((a . 10) (b . 4)))))
         (results (execute-tool-calls (list add-tool sub-tool) calls)))
    (and (= (length results) 2)
         (string=? (tool-result-content (car results)) "8")
         (string=? (tool-result-content (cadr results)) "6"))))

;;; Provider integration tests (require API keys)

(define (test-openai-function-call api-key)
  "Test function calling with OpenAI"
  (let* ((provider (make-openai-provider #:api-key api-key
                                        #:model "gpt-4o-mini"))
         (weather-tool (make-tool
                        #:name "get_weather"
                        #:description "Get current weather for a location"
                        #:parameters `((type . "object")
                                      (properties . ((location . ((type . "string")
                                                                 (description . "City and country")))))
                                      (required . ("location")))))
         (tools (tools->openai-format (list weather-tool)))
         (response (openai-chat provider
                               '(((role . "user")
                                  (content . "What's the weather in Paris?")))
                               #:tools tools)))
    ;; Response should contain tool_calls
    (and response
         (list? response)
         (assoc-ref response 'tool_calls))))

(define (test-anthropic-function-call api-key)
  "Test function calling with Anthropic"
  (let* ((provider (make-anthropic-provider #:api-key api-key
                                           #:model "claude-3-haiku-20240307"))
         (weather-tool (make-tool
                        #:name "get_weather"
                        #:description "Get current weather for a location"
                        #:parameters `((type . "object")
                                      (properties . ((location . ((type . "string")
                                                                 (description . "City and country")))))
                                      (required . ("location")))))
         (tools (tools->anthropic-format (list weather-tool)))
         (response (anthropic-chat provider
                                  '(((role . "user")
                                     (content . "What's the weather in Paris?")))
                                  #:tools tools)))
    ;; Response should contain tool_use
    (and response
         (list? response)
         (assoc-ref response 'tool_use))))

;;; Main entry point

(define (main args)
  (format #t "~%=== Function Calling / Tool Use Tests ===~%~%")

  ;; Unit tests (no API needed)
  (format #t "--- Unit Tests ---~%")
  (let ((unit-results
         (list
          (run-test "make-tool" test-make-tool)
          (run-test "tool-with-handler" test-tool-with-handler)
          (run-test "tool-execution" test-tool-execution)
          (run-test "openai-schema" test-openai-schema)
          (run-test "anthropic-schema" test-anthropic-schema)
          (run-test "tool-call-creation" test-tool-call-creation)
          (run-test "execute-tool-calls" test-execute-tool-calls))))
    (format #t "~%Unit tests: ~a/~a passed~%"
            (count identity unit-results)
            (length unit-results)))

  ;; Integration tests (require API keys)
  (let ((openai-key (getenv "OPENAI_API_KEY"))
        (anthropic-key (getenv "ANTHROPIC_API_KEY")))

    (when openai-key
      (format #t "~%--- OpenAI Integration Tests ---~%")
      (run-test "openai-function-call"
               (lambda () (test-openai-function-call openai-key))))

    (when anthropic-key
      (format #t "~%--- Anthropic Integration Tests ---~%")
      (run-test "anthropic-function-call"
               (lambda () (test-anthropic-function-call anthropic-key))))

    (unless (or openai-key anthropic-key)
      (format #t "~%Set OPENAI_API_KEY or ANTHROPIC_API_KEY for integration tests.~%")))

  (format #t "~%=== Tests Complete ===~%"))
