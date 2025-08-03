#!/usr/bin/env guile
!#

;;; test-ollama.scm --- Test Ollama provider implementation

(add-to-load-path "../../src")
(use-modules (llm providers ollama)
             (llm utils json)
             (ice-9 format))

(define (test-ollama-connection)
  "Test basic Ollama server connection."
  (display "Testing Ollama connection...\n")
  
  (let ((provider (make-ollama-provider)))
    (if (check-ollama-server provider)
        (display "✓ Ollama server is accessible\n")
        (display "✗ Cannot connect to Ollama server\n"))
    
    ;; List available models
    (display "\nAvailable models:\n")
    (catch #t
      (lambda ()
        (let ((models (ollama-list-models provider)))
          (for-each (lambda (model)
                      (format #t "  - ~a (~a)\n" 
                              (assoc-ref model 'name)
                              (assoc-ref model 'size)))
                    models)))
      (lambda (key . args)
        (display "  Error listing models\n")))))

(define (test-simple-generation)
  "Test simple text generation."
  (display "\n\nTesting simple generation...\n")
  
  (let ((provider (make-ollama-provider #:model "llama2")))
    (catch #t
      (lambda ()
        (let ((response (ollama-generate provider 
                                        "What is 2+2? Answer in one word."
                                        #:temperature 0.1
                                        #:max-tokens 10)))
          (format #t "Response: ~a\n" response)))
      (lambda (key . args)
        (format #t "Error: ~a\n" args)))))

(define (test-chat-api)
  "Test chat API."
  (display "\n\nTesting chat API...\n")
  
  (let ((provider (make-ollama-provider #:model "llama2"))
        (messages '(((role . "user") 
                     (content . "Hello! What's your name?"))
                    ((role . "assistant") 
                     (content . "I'm Claude, an AI assistant. How can I help you?"))
                    ((role . "user") 
                     (content . "What did you just say your name was?")))))
    
    (catch #t
      (lambda ()
        (let ((response (ollama-chat provider messages 
                                    #:temperature 0.3
                                    #:max-tokens 50)))
          (format #t "Chat response: ~a\n" response)))
      (lambda (key . args)
        (format #t "Error: ~a\n" args)))))

(define (test-streaming)
  "Test streaming responses."
  (display "\n\nTesting streaming...\n")
  
  (let ((provider (make-ollama-provider #:model "llama2")))
    (catch #t
      (lambda ()
        (display "Streaming response: ")
        (ollama-generate provider 
                        "Count from 1 to 5."
                        #:stream #t
                        #:temperature 0.1
                        #:max-tokens 50)
        (newline))
      (lambda (key . args)
        (format #t "\nError: ~a\n" args)))))

(define (test-json-structure)
  "Test structured JSON output."
  (display "\n\nTesting structured output...\n")
  
  (let ((provider (make-ollama-provider #:model "llama2"))
        (prompt "Generate a JSON object with fields: name (string), age (number), active (boolean). Use this exact format: {\"name\": \"...\", \"age\": ..., \"active\": ...}"))
    
    (catch #t
      (lambda ()
        (let* ((response (ollama-generate provider prompt
                                         #:temperature 0.1
                                         #:max-tokens 100))
               ;; Try to extract JSON from response
               (json-match (string-match "\\{[^}]+\\}" response)))
          
          (if json-match
              (let* ((json-str (match:substring json-match))
                     (parsed (catch #t
                              (lambda () (json->scm json-str))
                              (lambda _ #f))))
                (format #t "Generated JSON: ~a\n" json-str)
                (if parsed
                    (format #t "Parsed successfully: ~a\n" parsed)
                    (format #t "Failed to parse JSON\n")))
              (format #t "No JSON found in response: ~a\n" response))))
      (lambda (key . args)
        (format #t "Error: ~a\n" args)))))

(define (test-embeddings)
  "Test embeddings generation."
  (display "\n\nTesting embeddings...\n")
  
  (let ((provider (make-ollama-provider #:model "llama2")))
    (catch #t
      (lambda ()
        (let ((embedding (ollama-embeddings provider "Hello, world!")))
          (format #t "Embedding vector length: ~a\n" 
                  (if embedding (length embedding) "N/A"))
          (when embedding
            (format #t "First 5 values: ~a\n" 
                    (take embedding 5)))))
      (lambda (key . args)
        (format #t "Error: ~a\n" args)))))

(define (main)
  (display "=== Ollama Provider Test Suite ===\n")
  
  (test-ollama-connection)
  
  (display "\nPress Enter to continue with generation tests...")
  (read-line)
  
  (test-simple-generation)
  (test-chat-api)
  (test-streaming)
  (test-json-structure)
  (test-embeddings)
  
  (display "\n=== Tests completed ===\n"))

;; Run tests
(main)