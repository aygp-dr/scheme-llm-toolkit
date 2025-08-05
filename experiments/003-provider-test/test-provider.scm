;;; test-provider.scm --- Test provider abstraction

(add-to-load-path "../../src")
(use-modules (llm core provider)
             (llm providers ollama-provider)
             (ice-9 format))

(define (test-provider-abstraction)
  "Test the provider abstraction layer."
  
  (display "=== Provider Abstraction Test ===\n\n")
  
  ;; Register Ollama provider
  (display "1. Registering Ollama provider...\n")
  (register-ollama-provider!)
  
  ;; List providers
  (display "\n2. Available providers:\n")
  (for-each (lambda (name)
              (format #t "   - ~a\n" name))
            (list-providers))
  
  ;; Get provider info
  (display "\n3. Provider information:\n")
  (let ((provider (get-provider 'ollama)))
    (if provider
        (catch #t
          (lambda ()
            (let ((info (provider-info provider)))
              (format #t "   Name: ~a\n" (assoc-ref info 'name))
              (format #t "   Capabilities: ~a\n" (assoc-ref info 'capabilities))
              (format #t "   Base URL: ~a\n" (assoc-ref info 'base-url))
              (format #t "   Default Model: ~a\n" (assoc-ref info 'default-model))))
          (lambda (key . args)
            (display "   [Info unavailable - Ollama not running]\n")))
        (display "   Provider not found!\n")))
  
  ;; Test completion (if Ollama is running)
  (display "\n4. Testing completion...\n")
  (let ((provider (get-provider 'ollama)))
    (if provider
        (catch #t
          (lambda ()
            (let ((response (provider-complete provider 
                                              "What is 2+2? Answer in one number only."
                                              #:temperature 0.1
                                              #:max-tokens 5)))
              (format #t "   Response: ~a\n" response)))
          (lambda (key . args)
            (display "   [Skipped - Ollama not running]\n")))
        (display "   Provider not found!\n")))
  
  ;; Test streaming (demonstration only)
  (display "\n5. Streaming interface example:\n")
  (display "   (provider-complete-streaming provider prompt display)\n")
  (display "   This would stream tokens to the display function\n")
  
  ;; Create a mock provider for testing
  (display "\n6. Creating mock provider...\n")
  (let ((mock-provider
         (make-provider
          #:name 'mock
          #:complete (lambda (provider prompt . options)
                      (format #f "Mock response to: ~a" prompt))
          #:capabilities '(streaming chat)
          #:metadata '((type . "test")))))
    
    (register-provider! 'mock mock-provider)
    
    ;; Test mock provider
    (let ((response (provider-complete mock-provider "Hello!")))
      (format #t "   Mock response: ~a\n" response)))
  
  (display "\n=== Test completed ===\n"))

;; Run test
(test-provider-abstraction)