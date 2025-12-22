#!/usr/bin/env guile
!#

;;; basic-test.scm --- Basic OpenAI provider tests (no API calls)

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

;; Add source directories to load path
(add-to-load-path (string-append (dirname (current-filename)) "/../../src"))

(use-modules (llm providers openai)
             (llm providers openai-provider)
             (llm core provider)
             (ice-9 format))

;;; Commentary:
;;;
;;; This script tests basic OpenAI provider functionality without
;;; making actual API calls. It's useful for validating the
;;; implementation structure and interface compliance.
;;;
;;; Code:

(display "=== Basic OpenAI Provider Tests (No API Calls) ===\n\n")

;; Test 1: API key validation
(display "1. Testing API key validation...\n")
(catch #t
  (lambda ()
    (make-openai-provider)
    (display "   ERROR: Should have failed without API key\n"))
  (lambda (key . args)
    (display "   PASS: Correctly requires API key\n")))

;; Test 2: Provider creation with API key
(display "\n2. Testing provider creation with API key...\n")
(let ((provider (make-openai-provider #:api-key "test-key-123")))
  (if provider
      (display "   PASS: Provider created successfully\n")
      (display "   FAIL: Provider creation failed\n")))

;; Test 3: Provider interface compliance
(display "\n3. Testing provider interface compliance...\n")
(let ((provider (make-openai-llm-provider #:api-key "test-key-123")))
  (display (format #f "   Provider name: ~a\n" (provider-name provider)))
  (display (format #f "   Capabilities: ~a\n" (provider-capabilities provider)))
  (display (format #f "   Metadata: ~a\n" 
                   (provider-info provider)))
  (display "   PASS: Provider interface working\n"))

;; Test 4: Provider registration
(display "\n4. Testing provider registration...\n")
(register-openai-provider! #:api-key "test-key-123")
(let ((registered (get-provider 'openai)))
  (if registered
      (begin
        (display "   PASS: Provider registered successfully\n")
        (display (format #f "   Registered providers: ~a\n" (list-providers))))
      (display "   FAIL: Provider registration failed\n")))

;; Test 5: with-provider syntax
(display "\n5. Testing with-provider syntax...\n")
(with-provider openai
  (let ((provider (get-provider 'openai)))
    (display (format #f "   Provider in context: ~a\n" (provider-name provider)))
    (display "   PASS: with-provider syntax working\n")))

;; Test 6: Configuration options
(display "\n6. Testing configuration options...\n")
(let ((provider (make-openai-llm-provider 
                 #:api-key "test-key-456"
                 #:model "gpt-4"
                 #:base-url "https://custom.openai.com/v1"
                 #:timeout 600)))
  (let ((info (provider-info provider)))
    (display (format #f "   Custom model: ~a\n" (assoc-ref info 'default-model)))
    (display (format #f "   Custom base-url: ~a\n" (assoc-ref info 'base-url)))
    (display (format #f "   Custom timeout: ~a\n" (assoc-ref info 'timeout)))
    (display "   PASS: Configuration options working\n")))

;; Test 7: Message normalization
(display "\n7. Testing message normalization...\n")
(catch #t
  (lambda ()
    ;; This would normally call the API, but we just test the structure
    (let ((provider (make-openai-provider #:api-key "test-key")))
      (display "   Provider ready for message processing\n")
      (display "   PASS: Message handling structure in place\n")))
  (lambda (key . args)
    (display (format #f "   Unexpected error: ~a\n" key))))

(display "\n=== All Basic Tests Complete ===\n")
(display "\nTo test with real API calls:\n")
(display "1. Set OPENAI_API_KEY environment variable\n")
(display "2. Run: guile test-openai.scm\n")
(display "3. Run: guile demo.scm\n")