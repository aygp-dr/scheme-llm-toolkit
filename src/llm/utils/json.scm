;;; json.scm --- JSON parsing and generation utilities for LLM toolkit

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm utils json)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (json-escape-string
            scm->json-string
            json-string->alist
            parse-json-with-jq
            json->scm
            json-null?
            make-json-null))

;;; Commentary:
;;;
;;; This module provides JSON parsing and generation utilities optimized
;;; for LLM API interactions. It uses a multi-strategy approach:
;;; 1. jq for parsing when available (most reliable)
;;; 2. Custom parser as fallback
;;; 3. Robust string escaping for generation
;;;
;;; Code:

;;;; JSON Generation

(define (json-escape-string str)
  "Properly escape a string for JSON output."
  (let ((chars (string->list str)))
    (list->string
     (fold (lambda (ch acc)
             (append acc
                     (case ch
                       ((#\") '(#\\ #\"))
                       ((#\\) '(#\\ #\\))
                       ((#\newline) '(#\\ #\n))
                       ((#\return) '(#\\ #\r))
                       ((#\tab) '(#\\ #\t))
                       ((#\backspace) '(#\\ #\b))
                       ((#\page) '(#\\ #\f))
                       (else (list ch)))))
           '()
           chars))))

(define (scm->json-string data)
  "Convert Scheme data structure to JSON string.
   
   Supported types:
   - null -> null
   - boolean -> true/false
   - number -> number
   - string -> \"string\"
   - symbol -> \"symbol\"
   - list -> array or object (if alist)
   - vector -> array"
  (cond
    ((eq? data 'null) "null")
    ((null? data) "null")
    ((boolean? data) (if data "true" "false"))
    ((number? data) 
     (if (and (exact? data) (integer? data))
         (number->string data)
         (number->string (exact->inexact data))))
    ((string? data) 
     (format #f "\"~a\"" (json-escape-string data)))
    ((symbol? data) 
     (format #f "\"~a\"" (json-escape-string (symbol->string data))))
    ((vector? data)
     ;; Convert vector to array
     (string-append "["
                   (string-join (map scm->json-string (vector->list data)) ",")
                   "]"))
    ((list? data)
     (if (alist? data)
         ;; Object (association list)
         (string-append "{"
                       (string-join
                        (map (lambda (pair)
                               (format #f "\"~a\":~a"
                                      (cond
                                        ((string? (car pair)) (json-escape-string (car pair)))
                                        ((symbol? (car pair)) (json-escape-string (symbol->string (car pair))))
                                        (else (error "Invalid object key type" (car pair))))
                                      (scm->json-string (cdr pair))))
                             data)
                        ",")
                       "}")
         ;; Array
         (string-append "["
                       (string-join (map scm->json-string data) ",")
                       "]")))
    (else "null")))

(define (alist? lst)
  "Check if a list is an association list (all elements are pairs)."
  (and (list? lst)
       (every pair? lst)
       (every (lambda (pair)
                (or (symbol? (car pair))
                    (string? (car pair))))
              lst)))

;;;; JSON Parsing - Multi-strategy approach

(define (parse-json-with-jq json-string query)
  "Parse JSON using jq command if available.
   Returns #f if jq is not available or parsing fails."
  (catch #t
    (lambda ()
      (let* ((temp-file (format #f "/tmp/json-parse-~a.json" (getpid))))
        (dynamic-wind
          (lambda () #f)
          (lambda ()
            ;; Write JSON to temp file
            (call-with-output-file temp-file
              (lambda (port)
                (display json-string port)))
            
            ;; Run jq
            (let* ((cmd (format #f "jq -r '~a' < ~a 2>/dev/null" 
                               (or query ".") temp-file))
                   (port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd))
                   (result (get-string-all port))
                   (status (close-pipe port)))
              (if (zero? (status:exit-val status))
                  (string-trim-right result #\newline)
                  #f)))
          (lambda ()
            ;; Clean up
            (when (file-exists? temp-file)
              (delete-file temp-file))))))
    (lambda (key . args)
      #f)))

;;;; Custom JSON Parser (Fallback)

(define json-null (cons 'json-null '()))

(define (make-json-null) json-null)

(define (json-null? obj)
  "Check if object is JSON null."
  (eq? obj json-null))

(define (json->scm str)
  "Parse JSON string to Scheme data structure."
  (call-with-input-string str json-read))

(define (json-string->alist str)
  "Parse JSON string to association list (convenience function)."
  (let ((result (json->scm str)))
    (if (list? result)
        result
        (error "JSON does not represent an object" str))))

(define (json-read port)
  "Read JSON from port."
  (json-skip-whitespace port)
  (let ((ch (peek-char port)))
    (cond
     ((eof-object? ch) ch)
     ((char=? ch #\{) (json-read-object port))
     ((char=? ch #\[) (json-read-array port))
     ((char=? ch #\") (json-read-string port))
     ((char=? ch #\t) (json-read-true port))
     ((char=? ch #\f) (json-read-false port))
     ((char=? ch #\n) (json-read-null port))
     ((or (char=? ch #\-) (char-numeric? ch)) (json-read-number port))
     (else (error "Invalid JSON character" ch)))))

(define (json-skip-whitespace port)
  "Skip whitespace characters."
  (let loop ()
    (let ((ch (peek-char port)))
      (when (and (not (eof-object? ch))
                 (char-whitespace? ch))
        (read-char port)
        (loop)))))

(define (json-read-object port)
  "Read JSON object as association list."
  (read-char port) ; consume {
  (json-skip-whitespace port)
  
  (if (char=? (peek-char port) #\})
      (begin
        (read-char port) ; consume }
        '())
      (let loop ((pairs '()))
        (json-skip-whitespace port)
        
        ;; Read key
        (let ((key (if (char=? (peek-char port) #\")
                      (string->symbol (json-read-string port))
                      (error "Object key must be string"))))
          
          (json-skip-whitespace port)
          
          ;; Expect :
          (if (char=? (peek-char port) #\:)
              (read-char port)
              (error "Expected : after object key"))
          
          (json-skip-whitespace port)
          
          ;; Read value
          (let ((value (json-read port)))
            (json-skip-whitespace port)
            
            (let ((next-char (peek-char port)))
              (cond
               ((char=? next-char #\,)
                (read-char port) ; consume ,
                (loop (cons (cons key value) pairs)))
               ((char=? next-char #\})
                (read-char port) ; consume }
                (reverse (cons (cons key value) pairs)))
               (else
                (error "Expected , or } in object")))))))))

(define (json-read-array port)
  "Read JSON array as list."
  (read-char port) ; consume [
  (json-skip-whitespace port)
  
  (if (char=? (peek-char port) #\])
      (begin
        (read-char port) ; consume ]
        '())
      (let loop ((elements '()))
        (let ((element (json-read port)))
          (json-skip-whitespace port)
          
          (let ((next-char (peek-char port)))
            (cond
             ((char=? next-char #\,)
              (read-char port) ; consume ,
              (json-skip-whitespace port)
              (loop (cons element elements)))
             ((char=? next-char #\])
              (read-char port) ; consume ]
              (reverse (cons element elements)))
             (else
              (error "Expected , or ] in array"))))))))

(define (json-read-string port)
  "Read JSON string."
  (read-char port) ; consume opening "
  
  (let loop ((chars '()))
    (let ((ch (read-char port)))
      (cond
       ((eof-object? ch)
        (error "Unexpected EOF in string"))
       ((char=? ch #\")
        (list->string (reverse chars)))
       ((char=? ch #\\)
        ;; Handle escape sequences
        (let ((escaped (read-char port)))
          (if (eof-object? escaped)
              (error "Unexpected EOF after backslash")
              (let ((replacement
                     (case escaped
                       ((#\") #\")
                       ((#\\) #\\)
                       ((#\/) #\/)
                       ((#\b) #\backspace)
                       ((#\f) #\page)
                       ((#\n) #\newline)
                       ((#\r) #\return)
                       ((#\t) #\tab)
                       ((#\u) (json-read-unicode-escape port))
                       (else (error "Invalid escape sequence" escaped)))))
                (loop (cons replacement chars))))))
       (else
        (loop (cons ch chars)))))))

(define (json-read-unicode-escape port)
  "Read \\uXXXX escape sequence."
  (let* ((hex-chars (list (read-char port)
                         (read-char port)
                         (read-char port)
                         (read-char port)))
         (hex-string (list->string hex-chars)))
    (integer->char (string->number hex-string 16))))

(define (json-read-number port)
  "Read JSON number."
  (let loop ((chars '()) (has-dot #f) (has-exp #f))
    (let ((ch (peek-char port)))
      (cond
       ((eof-object? ch)
        (string->number (list->string (reverse chars))))
       ((char-numeric? ch)
        (loop (cons (read-char port) chars) has-dot has-exp))
       ((and (char=? ch #\.) (not has-dot) (not has-exp))
        (loop (cons (read-char port) chars) #t has-exp))
       ((and (or (char=? ch #\e) (char=? ch #\E)) (not has-exp))
        (read-char port)
        (let ((next (peek-char port)))
          (when (or (char=? next #\+) (char=? next #\-))
            (read-char port)
            (set! chars (cons next chars)))
          (loop (cons ch chars) has-dot #t)))
       ((and (char=? ch #\-) (null? chars))
        (loop (cons (read-char port) chars) has-dot has-exp))
       (else
        (string->number (list->string (reverse chars))))))))

(define (json-read-true port)
  "Read JSON true."
  (if (string=? (read-n-chars port 4) "true")
      #t
      (error "Invalid true literal")))

(define (json-read-false port)
  "Read JSON false."
  (if (string=? (read-n-chars port 5) "false")
      #f
      (error "Invalid false literal")))

(define (json-read-null port)
  "Read JSON null."
  (if (string=? (read-n-chars port 4) "null")
      json-null
      (error "Invalid null literal")))

(define (read-n-chars port n)
  "Read exactly n characters from port."
  (let loop ((n n) (chars '()))
    (if (zero? n)
        (list->string (reverse chars))
        (let ((ch (read-char port)))
          (if (eof-object? ch)
              (error "Unexpected EOF")
              (loop (- n 1) (cons ch chars)))))))

;;; json.scm ends here