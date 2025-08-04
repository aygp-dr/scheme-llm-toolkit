#!/usr/bin/env guile3
!#

;;; test-json.scm --- Test JSON parsing and generation

(add-to-load-path "../../src")
(use-modules (llm utils json))

(define (test-json-generation)
  (display "Testing JSON generation...\n")
  
  ;; Test basic types
  (format #t "null: ~a\n" (scm->json-string 'null))
  (format #t "true: ~a\n" (scm->json-string #t))
  (format #t "false: ~a\n" (scm->json-string #f))
  (format #t "number: ~a\n" (scm->json-string 42))
  (format #t "float: ~a\n" (scm->json-string 3.14))
  (format #t "string: ~a\n" (scm->json-string "hello world"))
  (format #t "escaped: ~a\n" (scm->json-string "quote: \" and newline: \n"))
  
  ;; Test arrays
  (format #t "array: ~a\n" (scm->json-string '(1 2 3 "four")))
  (format #t "nested: ~a\n" (scm->json-string '(1 (2 3) "four")))
  
  ;; Test objects
  (format #t "object: ~a\n" 
          (scm->json-string '((name . "John") (age . 30) (active . #t))))
  
  ;; Test complex structure
  (let ((complex '((model . "llama2")
                   (prompt . "Hello, world!")
                   (options . ((temperature . 0.7)
                              (max_tokens . 100)))
                   (stream . #f))))
    (format #t "complex: ~a\n" (scm->json-string complex)))
  
  (newline))

(define (test-json-parsing)
  (display "Testing JSON parsing...\n")
  
  ;; Test basic types
  (format #t "null: ~a\n" (json->scm "null"))
  (format #t "true: ~a\n" (json->scm "true"))
  (format #t "false: ~a\n" (json->scm "false"))
  (format #t "number: ~a\n" (json->scm "42"))
  (format #t "float: ~a\n" (json->scm "3.14"))
  (format #t "string: ~a\n" (json->scm "\"hello world\""))
  
  ;; Test arrays
  (format #t "array: ~a\n" (json->scm "[1, 2, 3, \"four\"]"))
  
  ;; Test objects
  (let ((obj (json->scm "{\"name\": \"John\", \"age\": 30, \"active\": true}")))
    (format #t "object: ~a\n" obj)
    (format #t "  name: ~a\n" (assoc-ref obj 'name))
    (format #t "  age: ~a\n" (assoc-ref obj 'age))
    (format #t "  active: ~a\n" (assoc-ref obj 'active)))
  
  ;; Test complex Ollama response
  (let* ((ollama-response "{\"model\":\"llama2\",\"created_at\":\"2025-08-02T12:00:00Z\",\"response\":\"Hello!\",\"done\":true}")
         (parsed (json->scm ollama-response)))
    (format #t "ollama response: ~a\n" parsed)
    (format #t "  model: ~a\n" (assoc-ref parsed 'model))
    (format #t "  response: ~a\n" (assoc-ref parsed 'response))
    (format #t "  done: ~a\n" (assoc-ref parsed 'done)))
  
  (newline))

(define (test-roundtrip)
  (display "Testing roundtrip conversion...\n")
  
  (let* ((original '((prompt . "What is Scheme?")
                     (model . "llama2")
                     (options . ((temperature . 0.8)
                                (seed . 42)))
                     (messages . (((role . "user")
                                   (content . "Hello"))
                                 ((role . "assistant")
                                   (content . "Hi there!"))))))
         (json-str (scm->json-string original))
         (parsed (json->scm json-str)))
    
    (format #t "Original: ~a\n" original)
    (format #t "JSON: ~a\n" json-str)
    (format #t "Parsed: ~a\n" parsed)
    (format #t "Equal? ~a\n" (equal? (stringify-keys original) parsed)))
  
  (newline))

(define (stringify-keys alist)
  "Convert symbol keys to strings for comparison."
  (map (lambda (pair)
         (cons (symbol->string (car pair))
               (if (list? (cdr pair))
                   (if (and (pair? (cdr pair))
                           (list? (cadr pair))
                           (pair? (caadr pair)))
                       (map stringify-keys (cdr pair))
                       (cdr pair))
                   (cdr pair))))
       alist))

(define (main)
  (test-json-generation)
  (test-json-parsing)
  (test-roundtrip)
  
  ;; Test with jq if available
  (let ((jq-result (parse-json-with-jq "{\"test\": 42}" ".test")))
    (when jq-result
      (format #t "jq test: ~a\n" jq-result))))

(main)