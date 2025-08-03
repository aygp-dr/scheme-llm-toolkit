#!/usr/bin/env guile
!#

;;; demo.scm --- Demonstrate Ollama integration

(add-to-load-path "../../src")
(use-modules (llm providers ollama)
             (llm providers ollama-provider)
             (llm core provider)
             (ice-9 format))

(define (demo-ollama)
  "Demonstrate Ollama functionality."
  
  (display "=== Ollama LLM Toolkit Demo ===\n\n")
  
  ;; Create provider
  (let ((ollama (make-ollama-provider)))
    
    ;; Check connection
    (display "1. Checking Ollama server...\n")
    (if (check-ollama-server ollama)
        (display "   ✓ Connected to Ollama\n\n")
        (begin
          (display "   ✗ Cannot connect to Ollama\n")
          (exit 1)))
    
    ;; List models
    (display "2. Available models:\n")
    (let ((models (ollama-list-models ollama)))
      (cond
        ((not models)
         (display "   Error listing models\n"))
        ((null? models)
         (begin
           (display "   No models found. Let's pull a small model...\n\n")
           (display "3. Pulling tinyllama model (this may take a minute)...\n")
           (catch #t
             (lambda ()
               (ollama-pull-model ollama "tinyllama" #:stream #t))
             (lambda (key . args)
               (format #t "   Error pulling model: ~a\n" key)))
           (display "\n")))
        (else
         (for-each (lambda (model)
                     (format #t "   - ~a\n" (assoc-ref model 'name)))
                   models))))
    
    ;; Simple generation
    (display "\n4. Testing simple generation...\n")
    (display "   Prompt: \"What is 2+2? Answer with just the number.\"\n")
    (let ((response (ollama-generate ollama 
                                    "What is 2+2? Answer with just the number."
                                    #:model "tinyllama"
                                    #:temperature 0.1
                                    #:max-tokens 10)))
      (format #t "   Response: ~a\n" response))
    
    ;; Chat example
    (display "\n5. Testing chat API...\n")
    (let* ((messages '(((role . "user") 
                        (content . "Hi! I'm learning Scheme."))
                       ((role . "assistant") 
                        (content . "That's great! Scheme is a beautiful language."))
                       ((role . "user") 
                        (content . "What makes it beautiful?"))))
           (response (ollama-chat ollama messages 
                                 #:model "tinyllama"
                                 #:temperature 0.7
                                 #:max-tokens 100)))
      (format #t "   Response: ~a\n" response))
    
    ;; Streaming example
    (display "\n6. Testing streaming...\n")
    (display "   Streaming response: ")
    (flush-all-ports)
    (ollama-generate ollama 
                    "Count from 1 to 5, one number per line."
                    #:model "tinyllama"
                    #:stream #t
                    #:temperature 0.1
                    #:max-tokens 50)
    (newline)
    
    ;; Provider abstraction
    (display "\n7. Testing provider abstraction...\n")
    (register-ollama-provider! #:model "tinyllama")
    (let ((provider (get-provider 'ollama)))
      (when provider
        (let ((response (provider-complete provider 
                                          "What is Scheme? (one sentence)"
                                          #:temperature 0.5
                                          #:max-tokens 50)))
          (format #t "   Response: ~a\n" response))))
    
    (display "\n=== Demo completed! ===\n")))

;; Run demo
(demo-ollama)