#!/usr/bin/env guile
!#

;;; test-openai.scm --- Test OpenAI provider implementation

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

;; Add source directories to load path
(add-to-load-path (string-append (dirname (current-filename)) "/../../src"))

(use-modules (llm providers openai)
             (llm providers openai-provider)
             (llm core provider)
             (ice-9 format)
             (srfi srfi-1))

;;; Commentary:
;;;
;;; This script tests the OpenAI provider implementation.
;;; It requires an OpenAI API key to be set in the environment
;;; variable OPENAI_API_KEY.
;;;
;;; Code:

(define (test-api-key-validation)
  "Test that API key validation works correctly."
  (display "Testing API key validation...\n")
  (catch #t
    (lambda ()
      (make-openai-provider)
      (display "ERROR: Should have failed without API key\n"))
    (lambda (key . args)
      (display "PASS: Correctly requires API key\n"))))

(define (test-provider-creation)
  "Test provider creation with API key."
  (display "Testing provider creation...\n")
  (let ((api-key (getenv "OPENAI_API_KEY")))
    (if api-key
        (let ((provider (make-openai-provider #:api-key api-key)))
          (if provider
              (display "PASS: Provider created successfully\n")
              (display "FAIL: Provider creation failed\n")))
        (display "SKIP: No OPENAI_API_KEY environment variable\n"))))

(define (test-provider-interface)
  "Test that the provider implements the standard interface."
  (display "Testing provider interface...\n")
  (let ((api-key (getenv "OPENAI_API_KEY")))
    (if api-key
        (let ((provider (make-openai-llm-provider #:api-key api-key)))
          (display (format #f "Provider name: ~a\n" (provider-name provider)))
          (display (format #f "Capabilities: ~a\n" (provider-capabilities provider)))
          (display (format #f "Info: ~a\n" (provider-info provider)))
          (display "PASS: Provider interface working\n"))
        (display "SKIP: No OPENAI_API_KEY environment variable\n"))))

(define (test-provider-registration)
  "Test provider registration in global registry."
  (display "Testing provider registration...\n")
  (let ((api-key (getenv "OPENAI_API_KEY")))
    (if api-key
        (begin
          (register-openai-provider! #:api-key api-key)
          (let ((registered (get-provider 'openai)))
            (if registered
                (begin
                  (display "PASS: Provider registered successfully\n")
                  (display (format #f "Registered providers: ~a\n" (list-providers))))
                (display "FAIL: Provider registration failed\n"))))
        (display "SKIP: No OPENAI_API_KEY environment variable\n"))))

(define (test-list-models)
  "Test listing available models."
  (display "Testing model listing...\n")
  (let ((api-key (getenv "OPENAI_API_KEY")))
    (if api-key
        (catch #t
          (lambda ()
            (let ((provider (make-openai-provider #:api-key api-key)))
              (display "Checking API connectivity...\n")
              (if (check-openai-api provider)
                  (begin
                    (display "API accessible, listing models...\n")
                    (let ((models (openai-list-models provider)))
                      (if models
                          (begin
                            (display (format #f "Found ~a models\n" (length models)))
                            (for-each (lambda (model)
                                        (display (format #f "  - ~a\n" 
                                                        (assoc-ref model 'id))))
                                      (take models (min 5 (length models))))
                            (display "PASS: Model listing successful\n"))
                          (display "FAIL: No models returned\n"))))
                  (display "SKIP: API not accessible (check API key)\n"))))
          (lambda (key . args)
            (display (format #f "ERROR: ~a ~a\n" key args))))
        (display "SKIP: No OPENAI_API_KEY environment variable\n"))))

(define (test-simple-completion)
  "Test a simple completion request."
  (display "Testing simple completion...\n")
  (let ((api-key (getenv "OPENAI_API_KEY")))
    (if api-key
        (catch #t
          (lambda ()
            (let ((provider (make-openai-provider #:api-key api-key)))
              (if (check-openai-api provider)
                  (begin
                    (display "Making completion request...\n")
                    (let ((response (openai-complete provider 
                                                    "Say 'Hello from OpenAI' in exactly those words."
                                                    #:model "gpt-3.5-turbo")))
                      (if response
                          (begin
                            (display (format #f "Response: ~a\n" response))
                            (display "PASS: Completion successful\n"))
                          (display "FAIL: No response received\n"))))
                  (display "SKIP: API not accessible\n"))))
          (lambda (key . args)
            (display (format #f "ERROR: ~a ~a\n" key args))))
        (display "SKIP: No OPENAI_API_KEY environment variable\n"))))

(define (test-chat-completion)
  "Test a chat completion request."
  (display "Testing chat completion...\n")
  (let ((api-key (getenv "OPENAI_API_KEY")))
    (if api-key
        (catch #t
          (lambda ()
            (let ((provider (make-openai-provider #:api-key api-key)))
              (if (check-openai-api provider)
                  (let ((messages '(((role . "system") 
                                    (content . "You are a helpful assistant."))
                                   ((role . "user") 
                                    (content . "What is 2+2? Answer with just the number.")))))
                    (display "Making chat request...\n")
                    (let ((response (openai-chat provider messages
                                                #:model "gpt-3.5-turbo")))
                      (if response
                          (begin
                            (display (format #f "Chat response: ~a\n" response))
                            (display "PASS: Chat completion successful\n"))
                          (display "FAIL: No chat response received\n"))))
                  (display "SKIP: API not accessible\n"))))
          (lambda (key . args)
            (display (format #f "ERROR: ~a ~a\n" key args))))
        (display "SKIP: No OPENAI_API_KEY environment variable\n"))))

(define (run-all-tests)
  "Run all OpenAI provider tests."
  (display "=== OpenAI Provider Test Suite ===\n\n")
  
  (test-api-key-validation)
  (display "\n")
  
  (test-provider-creation)
  (display "\n")
  
  (test-provider-interface)
  (display "\n")
  
  (test-provider-registration)
  (display "\n")
  
  (test-list-models)
  (display "\n")
  
  (test-simple-completion)
  (display "\n")
  
  (test-chat-completion)
  (display "\n")
  
  (display "=== Test Suite Complete ===\n"))

;; Run tests if this script is executed directly
(when (and (defined? 'current-filename)
           (string=? (basename (car (command-line)))
                     (basename (current-filename))))
  (run-all-tests))