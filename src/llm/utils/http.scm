;;; http.scm --- HTTP client utilities for LLM toolkit

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm utils http)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (llm utils json)
  #:export (http-post
            http-get
            http-stream
            check-url-accessible
            with-http-error-handling
            make-temp-file
            cleanup-temp-file))

;;; Commentary:
;;;
;;; This module provides HTTP client utilities using curl subprocess.
;;; The implementation uses temporary files to avoid shell escaping issues
;;; and provides robust error handling.
;;;
;;; Code:

(define (make-temp-file prefix)
  "Create a temporary file with given prefix."
  (format #f "/tmp/~a-~a-~a.tmp" 
          prefix 
          (getpid)
          (random 100000)))

(define (cleanup-temp-file file)
  "Safely remove temporary file if it exists."
  (catch #t
    (lambda ()
      (when (file-exists? file)
        (delete-file file)))
    (lambda (key . args) #f)))

(define (build-curl-command method url . options)
  "Build curl command with common options."
  (let* ((headers (or (assoc-ref options 'headers) '()))
         (data-file (assoc-ref options 'data-file))
         (output-file (assoc-ref options 'output-file))
         (follow-redirects (assoc-ref options 'follow-redirects))
         (timeout (or (assoc-ref options 'timeout) 30))
         (silent (if (assoc-ref options 'verbose) "" "-s"))
         (show-errors (if (assoc-ref options 'show-errors) "-S" ""))
         (streaming (assoc-ref options 'streaming)))
    
    (string-join
     (filter identity
             (list "curl"
                   silent
                   show-errors
                   "-X" method
                   (if follow-redirects "-L" #f)
                   (format #f "--max-time ~a" timeout)
                   (if streaming "-N" #f) ; No buffering for streaming
                   ;; Add headers
                   (string-join
                    (map (lambda (header)
                           (format #f "-H '~a: ~a'" 
                                   (car header) 
                                   (cdr header)))
                         headers)
                    " ")
                   ;; Add data file if POST/PUT
                   (if (and data-file 
                            (member method '("POST" "PUT" "PATCH")))
                       (format #f "-d @~a" data-file)
                       #f)
                   ;; Output file
                   (if output-file
                       (format #f "-o ~a" output-file)
                       #f)
                   ;; URL must be last
                   (format #f "'~a'" url)
                   ;; Redirect stderr
                   "2>/dev/null"))
     " ")))

(define (execute-curl-command cmd)
  "Execute curl command and return response."
  (let* ((port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd))
         (response (get-string-all port))
         (status (close-pipe port)))
    (if (zero? (status:exit-val status))
        response
        (throw 'http-error 
               (format #f "HTTP request failed with status ~a" 
                       (status:exit-val status))))))

(define (http-post url data . options)
  "Make HTTP POST request with JSON data.
   
   Options:
   - headers: association list of headers
   - timeout: request timeout in seconds
   - json?: whether to parse response as JSON (default: #t)"
  
  (let* ((json? (if (assoc-ref options 'json?) 
                    (assoc-ref options 'json?)
                    #t))
         (headers (or (assoc-ref options 'headers) '()))
         (headers-with-content-type 
          (if (assoc-ref headers "Content-Type")
              headers
              (cons '("Content-Type" . "application/json") headers)))
         (temp-file (make-temp-file "http-post"))
         (json-data (if (string? data)
                       data
                       (scm->json-string data))))
    
    (dynamic-wind
      (lambda ()
        ;; Write data to temp file
        (call-with-output-file temp-file
          (lambda (port)
            (display json-data port))))
      
      (lambda ()
        (let* ((cmd (build-curl-command 
                     "POST" url
                     'headers headers-with-content-type
                     'data-file temp-file
                     'timeout (or (assoc-ref options 'timeout) 30)))
               (response (execute-curl-command cmd)))
          
          (if (and json? (not (string-null? response)))
              (json->scm response)
              response)))
      
      (lambda ()
        (cleanup-temp-file temp-file)))))

(define (http-get url . options)
  "Make HTTP GET request.
   
   Options:
   - headers: association list of headers
   - timeout: request timeout in seconds
   - json?: whether to parse response as JSON"
  
  (let* ((json? (assoc-ref options 'json?))
         (headers (or (assoc-ref options 'headers) '()))
         (cmd (build-curl-command 
               "GET" url
               'headers headers
               'timeout (or (assoc-ref options 'timeout) 30)
               'follow-redirects #t)))
    
    (let ((response (execute-curl-command cmd)))
      (if (and json? (not (string-null? response)))
          (json->scm response)
          response))))

(define* (http-stream url data on-chunk #:key 
                     (headers '())
                     (method "POST")
                     (timeout 300))
  "Stream HTTP response, calling on-chunk for each line.
   
   The on-chunk procedure receives each line of the response."
  
  (let ((temp-file (make-temp-file "http-stream"))
        (json-data (if (string? data)
                      data
                      (scm->json-string data))))
    
    (dynamic-wind
      (lambda ()
        ;; Write data to temp file
        (when json-data
          (call-with-output-file temp-file
            (lambda (port)
              (display json-data port)))))
      
      (lambda ()
        (let* ((headers-with-content-type 
                (if (and (string=? method "POST")
                         (not (assoc-ref headers "Content-Type")))
                    (cons '("Content-Type" . "application/json") headers)
                    headers))
               (cmd (build-curl-command 
                     method url
                     'headers headers-with-content-type
                     'data-file (if json-data temp-file #f)
                     'timeout timeout
                     'streaming #t))
               (port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd)))
          
          ;; Read streaming response line by line
          (let loop ()
            (let ((line (read-line port)))
              (unless (eof-object? line)
                (catch #t
                  (lambda ()
                    (unless (string-null? line)
                      (on-chunk line)))
                  (lambda (key . args)
                    ;; Log error but continue streaming
                    (format (current-error-port) 
                            "Error processing chunk: ~a\n" key)))
                (loop))))
          
          (let ((status (close-pipe port)))
            (unless (zero? (status:exit-val status))
              (throw 'http-error 
                     (format #f "Stream failed with status ~a" 
                             (status:exit-val status)))))))
      
      (lambda ()
        (cleanup-temp-file temp-file)))))

(define (check-url-accessible url)
  "Check if a URL is accessible."
  (catch #t
    (lambda ()
      (let* ((cmd (format #f "curl -s -o /dev/null -w '%{http_code}' '~a' 2>/dev/null" url))
             (port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd))
             (response (read-line port))
             (status (close-pipe port)))
        (and (zero? (status:exit-val status))
             (string? response)
             (string=? (string-take response 1) "2"))))
    (lambda (key . args) #f)))

(define-syntax with-http-error-handling
  (syntax-rules ()
    ((with-http-error-handling body ...)
     (catch 'http-error
       (lambda () body ...)
       (lambda (key message)
         (format (current-error-port) "HTTP Error: ~a\n" message)
         #f)))))

;;; Utility functions for response parsing

(define (parse-http-response response)
  "Parse HTTP response into headers and body."
  (let* ((lines (string-split response #\newline))
         (header-end (list-index (lambda (line) (string=? line "")) lines)))
    (if header-end
        (let ((headers (take lines header-end))
              (body (string-join (drop lines (+ header-end 1)) "\n")))
          (list (cons 'headers (parse-headers headers))
                (cons 'body body)))
        (list (cons 'headers '())
              (cons 'body response)))))

(define (parse-headers header-lines)
  "Parse HTTP header lines into association list."
  (filter-map
   (lambda (line)
     (let ((colon-pos (string-index line #\:)))
       (if colon-pos
           (cons (string-trim-both (substring line 0 colon-pos))
                 (string-trim-both (substring line (+ colon-pos 1))))
           #f)))
   header-lines))

;;; http.scm ends here