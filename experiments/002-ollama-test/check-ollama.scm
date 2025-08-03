#!/usr/bin/env guile
!#

;;; check-ollama.scm --- Check if Ollama server is running

(add-to-load-path "../../src")
(use-modules (llm utils http))

(define (check-ollama)
  "Check if Ollama server is running."
  (display "Checking Ollama server at http://localhost:11434...\n")
  
  (let ((accessible (check-url-accessible "http://localhost:11434/api/tags")))
    (if accessible
        (begin
          (display "✓ Ollama server is running!\n")
          (display "\nTrying to get model list...\n")
          (catch #t
            (lambda ()
              (let ((response (http-get "http://localhost:11434/api/tags" 'json? #t)))
                (format #t "Found ~a models\n" 
                        (length (or (assoc-ref response 'models) '())))))
            (lambda (key . args)
              (display "Could not retrieve model list\n"))))
        (begin
          (display "✗ Ollama server is not accessible\n")
          (display "\nTo start Ollama:\n")
          (display "  1. Install Ollama: curl -fsSL https://ollama.ai/install.sh | sh\n")
          (display "  2. Start server: ollama serve\n")
          (display "  3. Pull a model: ollama pull llama2\n")))))

(check-ollama)