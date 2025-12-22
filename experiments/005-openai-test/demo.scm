#!/usr/bin/env guile
!#

;;; demo.scm --- OpenAI provider demonstration

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

;; Add source directories to load path
(add-to-load-path (string-append (dirname (current-filename)) "/../../src"))

(use-modules (llm providers openai-provider)
             (llm core provider)
             (ice-9 format))

;;; Commentary:
;;;
;;; This script demonstrates how to use the OpenAI provider.
;;; Set your OpenAI API key in the OPENAI_API_KEY environment variable.
;;;
;;; Usage:
;;;   export OPENAI_API_KEY="your-api-key-here"
;;;   guile demo.scm
;;;
;;; Code:

(define (demo-basic-usage)
  "Demonstrate basic OpenAI provider usage."
  (let ((api-key (getenv "OPENAI_API_KEY")))
    (unless api-key
      (display "Please set OPENAI_API_KEY environment variable\n")
      (exit 1))
    
    (display "=== OpenAI Provider Demo ===\n\n")
    
    ;; Register the provider
    (display "1. Registering OpenAI provider...\n")
    (register-openai-provider! #:api-key api-key #:model "gpt-3.5-turbo")
    
    ;; Get the provider from registry
    (let ((provider (get-provider 'openai)))
      (display (format #f "   Provider registered: ~a\n" (provider-name provider)))
      (display (format #f "   Capabilities: ~a\n" (provider-capabilities provider)))
      (display "\n")
      
      ;; Simple completion
      (display "2. Simple completion example:\n")
      (display "   Prompt: \"Explain what Scheme is in one sentence.\"\n")
      (let ((response (provider-complete provider 
                                        "Explain what Scheme is in one sentence.")))
        (display (format #f "   Response: ~a\n\n" response)))
      
      ;; Chat example
      (display "3. Chat completion example:\n")
      (let ((messages '(((role . "system") 
                        (content . "You are a helpful programming tutor."))
                       ((role . "user") 
                        (content . "What's the difference between functional and imperative programming?")))))
        (display "   Messages: System + User question about programming paradigms\n")
        (let ((response (provider-chat provider messages)))
          (display (format #f "   Response: ~a\n\n" response))))
      
      ;; List some models
      (display "4. Available models (first 5):\n")
      (catch #t
        (lambda ()
          (let ((models (provider-list-models provider)))
            (for-each (lambda (model)
                        (display (format #f "   - ~a\n" model)))
                      (take models (min 5 (length models))))))
        (lambda (key . args)
          (display "   Error listing models (may require different permissions)\n")))
      
      (display "\n=== Demo Complete ===\n"))))

(define (demo-streaming)
  "Demonstrate streaming completion."
  (let ((api-key (getenv "OPENAI_API_KEY")))
    (unless api-key
      (display "Please set OPENAI_API_KEY environment variable\n")
      (exit 1))
    
    (display "\n=== Streaming Demo ===\n")
    (register-openai-provider! #:api-key api-key)
    
    (let ((provider (get-provider 'openai)))
      (display "Streaming response for: \"Count from 1 to 10\"\n")
      (display "Response: ")
      
      (provider-complete-streaming provider 
                                  "Count from 1 to 10, putting each number on a new line."
                                  (lambda (token)
                                    (display token)
                                    (force-output)))
      (display "\n\n=== Streaming Complete ===\n"))))

(define (demo-with-provider-syntax)
  "Demonstrate using the with-provider syntax."
  (let ((api-key (getenv "OPENAI_API_KEY")))
    (unless api-key
      (display "Please set OPENAI_API_KEY environment variable\n")
      (exit 1))
    
    (display "\n=== with-provider Syntax Demo ===\n")
    (register-openai-provider! #:api-key api-key)
    
    (with-provider openai
      (let ((response (provider-complete provider 
                                        "What is the capital of France? Answer in one word.")))
        (display (format #f "Capital of France: ~a\n" response))))
    
    (display "=== with-provider Demo Complete ===\n")))

;; Main demo function
(define (run-demo)
  "Run the complete OpenAI provider demonstration."
  (demo-basic-usage)
  (demo-with-provider-syntax)
  ;; Uncomment to test streaming (may be verbose)
  ;; (demo-streaming)
  )

;; Run demo if this script is executed directly
(when (and (defined? 'current-filename)
           (string=? (basename (car (command-line)))
                     (basename (current-filename))))
  (run-demo))