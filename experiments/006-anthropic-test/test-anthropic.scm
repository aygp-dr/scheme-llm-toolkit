#!/usr/bin/env -S guile -L ../../src -e main -s
!#
;;; test-anthropic.scm --- Test suite for Anthropic Claude provider

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(use-modules (llm providers anthropic)
             (llm providers anthropic-provider)
             (llm core provider)
             (ice-9 format))

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

;;; Tests

(define (test-api-key-required)
  "Test that API key is required"
  (catch 'misc-error
    (lambda ()
      (make-anthropic-provider)
      #f)  ; Should have thrown
    (lambda (key . args)
      #t)))  ; Expected error

(define (test-provider-creation api-key)
  "Test creating an Anthropic provider"
  (let ((provider (make-anthropic-provider #:api-key api-key)))
    (anthropic-provider? provider)))

(define (test-llm-provider-interface api-key)
  "Test the LLM provider interface"
  (let ((provider (make-anthropic-llm-provider #:api-key api-key)))
    (and (provider? provider)
         (eq? (provider-name provider) 'anthropic))))

(define (test-provider-registration api-key)
  "Test provider registration"
  (register-anthropic-provider! #:api-key api-key)
  (let ((provider (get-provider 'anthropic)))
    (and provider (provider? provider))))

(define (test-list-models api-key)
  "Test listing models (static list)"
  (let ((provider (make-anthropic-provider #:api-key api-key)))
    (let ((models (anthropic-list-models provider)))
      (and (list? models)
           (> (length models) 0)
           (assoc-ref (car models) 'id)))))

(define (test-simple-completion api-key)
  "Test a simple completion"
  (let ((provider (make-anthropic-provider #:api-key api-key
                                          #:model "claude-3-haiku-20240307")))
    (let ((response (anthropic-complete provider "Say hello in one word.")))
      (and response
           (string? response)
           (> (string-length response) 0)))))

(define (test-chat-completion api-key)
  "Test chat completion"
  (let ((provider (make-anthropic-provider #:api-key api-key
                                          #:model "claude-3-haiku-20240307")))
    (let ((response (anthropic-chat provider
                                    '(((role . "user")
                                       (content . "What is 2+2? Reply with just the number."))))))
      (and response
           (string? response)
           (string-contains response "4")))))

(define (test-system-prompt api-key)
  "Test system prompt usage"
  (let ((provider (make-anthropic-provider #:api-key api-key
                                          #:model "claude-3-haiku-20240307")))
    (let ((response (anthropic-generate provider "What are you?"
                                        #:system "You are a helpful pirate. Always say 'Arrr!'"
                                        #:max-tokens 50)))
      (and response
           (string? response)
           (> (string-length response) 0)))))

;;; Main entry point

(define (main args)
  (format #t "~%=== Anthropic Claude Provider Tests ===~%~%")

  (let ((api-key (getenv "ANTHROPIC_API_KEY")))
    (if (not api-key)
        (begin
          (format #t "WARNING: ANTHROPIC_API_KEY not set. Running limited tests.~%~%")
          ;; Run tests that don't need API access
          (run-test "api-key-required" test-api-key-required)
          (format #t "~%Set ANTHROPIC_API_KEY to run full test suite.~%"))
        (begin
          (format #t "Running tests with API key...~%~%")
          (let ((results
                 (list
                  (run-test "api-key-required" test-api-key-required)
                  (run-test "provider-creation"
                           (lambda () (test-provider-creation api-key)))
                  (run-test "llm-provider-interface"
                           (lambda () (test-llm-provider-interface api-key)))
                  (run-test "provider-registration"
                           (lambda () (test-provider-registration api-key)))
                  (run-test "list-models"
                           (lambda () (test-list-models api-key)))
                  (run-test "simple-completion"
                           (lambda () (test-simple-completion api-key)))
                  (run-test "chat-completion"
                           (lambda () (test-chat-completion api-key)))
                  (run-test "system-prompt"
                           (lambda () (test-system-prompt api-key))))))
            (format #t "~%=== Results: ~a/~a passed ===~%"
                    (count identity results)
                    (length results)))))))
