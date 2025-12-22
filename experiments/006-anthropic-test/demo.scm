#!/usr/bin/env -S guile -L ../../src -e main -s
!#
;;; demo.scm --- Anthropic Claude provider demonstration

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(use-modules (llm providers anthropic)
             (llm providers anthropic-provider)
             (llm core provider)
             (ice-9 format))

(define (demo-basic-completion provider)
  "Demonstrate basic text completion"
  (format #t "~%=== Basic Completion ===~%")
  (let ((response (anthropic-complete provider
                                      "Explain recursion in one sentence.")))
    (format #t "Prompt: Explain recursion in one sentence.~%")
    (format #t "Response: ~a~%~%" response)))

(define (demo-chat provider)
  "Demonstrate multi-turn chat"
  (format #t "~%=== Multi-turn Chat ===~%")
  (let ((response (anthropic-chat provider
                                  '(((role . "user")
                                     (content . "What is the capital of France?"))
                                    ((role . "assistant")
                                     (content . "The capital of France is Paris."))
                                    ((role . "user")
                                     (content . "What's a famous landmark there?"))))))
    (format #t "Chat history:~%")
    (format #t "  User: What is the capital of France?~%")
    (format #t "  Assistant: The capital of France is Paris.~%")
    (format #t "  User: What's a famous landmark there?~%")
    (format #t "Response: ~a~%~%" response)))

(define (demo-system-prompt provider)
  "Demonstrate system prompt usage"
  (format #t "~%=== System Prompt ===~%")
  (let ((response (anthropic-generate provider
                                      "Tell me about yourself."
                                      #:system "You are a helpful robot from the year 3000. Respond briefly."
                                      #:max-tokens 100)))
    (format #t "System: You are a helpful robot from the year 3000.~%")
    (format #t "User: Tell me about yourself.~%")
    (format #t "Response: ~a~%~%" response)))

(define (demo-provider-interface api-key)
  "Demonstrate using the provider abstraction"
  (format #t "~%=== Provider Abstraction ===~%")
  (register-anthropic-provider! #:api-key api-key
                               #:model "claude-3-haiku-20240307")
  (let ((provider (get-provider 'anthropic)))
    (format #t "Registered provider: ~a~%"
            (provider-name provider))
    (format #t "Capabilities: ~a~%"
            (provider-capabilities provider))
    (format #t "Available models: ~a~%~%"
            (provider-list-models provider))))

(define (main args)
  (format #t "~%========================================~%")
  (format #t "  Anthropic Claude Provider Demo~%")
  (format #t "========================================~%")

  (let ((api-key (getenv "ANTHROPIC_API_KEY")))
    (if (not api-key)
        (begin
          (format #t "~%ERROR: ANTHROPIC_API_KEY environment variable not set.~%")
          (format #t "Please set your API key:~%")
          (format #t "  export ANTHROPIC_API_KEY=\"your-key-here\"~%~%"))
        (let ((provider (make-anthropic-provider
                        #:api-key api-key
                        #:model "claude-3-haiku-20240307")))
          (demo-basic-completion provider)
          (demo-chat provider)
          (demo-system-prompt provider)
          (demo-provider-interface api-key)
          (format #t "~%Demo complete!~%")))))
