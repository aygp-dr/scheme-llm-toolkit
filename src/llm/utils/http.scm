;;; http.scm --- HTTP client utilities for LLM toolkit

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

;;; Commentary:
;;;
;;; This module provides HTTP client utilities using curl subprocess.
;;;
;;; On FreeBSD 14 with Guile 3.0.10, the ice-9 popen module causes
;;; segfaults due to bug #79494 (incompatible gnulib posix_spawn with
;;; FreeBSD's posix_spawn_file_actions_addclosefrom_np).
;;;
;;; This module uses (llm utils subprocess) which works around the bug
;;; by using primitive-fork + execl directly.
;;;
;;; See: https://codeberg.org/guile/guile/pulls/17
;;;
;;; Code:

(define-module (llm utils http)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (llm utils json)
  #:use-module (llm utils subprocess)
  #:export (http-post
            http-get
            http-stream
            check-url-accessible
            with-http-error-handling
            with-retry
            make-retry-config
            retryable-error?
            make-temp-file
            cleanup-temp-file))

;;; --------------------------------------------------------------------
;;; Retry Logic with Exponential Backoff
;;; --------------------------------------------------------------------

(define* (make-retry-config #:key
                            (max-retries 3)
                            (initial-delay 1.0)
                            (max-delay 30.0)
                            (backoff-multiplier 2.0)
                            (jitter? #t))
  "Create a retry configuration.

   Parameters:
   - max-retries: Maximum number of retry attempts (default: 3)
   - initial-delay: Initial delay in seconds (default: 1.0)
   - max-delay: Maximum delay cap in seconds (default: 30.0)
   - backoff-multiplier: Multiplier for exponential growth (default: 2.0)
   - jitter?: Add random jitter to prevent thundering herd (default: #t)"
  (list (cons 'max-retries max-retries)
        (cons 'initial-delay initial-delay)
        (cons 'max-delay max-delay)
        (cons 'backoff-multiplier backoff-multiplier)
        (cons 'jitter? jitter?)))

(define *default-retry-config*
  (make-retry-config))

(define (calculate-delay config attempt)
  "Calculate delay for given attempt number using exponential backoff.

   Formula: min(max-delay, initial-delay * (multiplier ^ attempt))
   With optional jitter: delay * (0.5 + random(0.5))"
  (let* ((initial (assoc-ref config 'initial-delay))
         (max-d (assoc-ref config 'max-delay))
         (multiplier (assoc-ref config 'backoff-multiplier))
         (jitter? (assoc-ref config 'jitter?))
         (base-delay (min max-d
                         (* initial (expt multiplier attempt))))
         (jitter-factor (if jitter?
                           (+ 0.5 (* 0.5 (/ (random 1000) 1000.0)))
                           1.0)))
    (* base-delay jitter-factor)))

(define (retryable-error? error-info)
  "Check if an error is retryable based on HTTP status code.

   Retryable errors:
   - 429 Too Many Requests (rate limiting)
   - 500 Internal Server Error
   - 502 Bad Gateway
   - 503 Service Unavailable
   - 504 Gateway Timeout"
  (cond
   ((number? error-info)
    (or (= error-info 429)
        (>= error-info 500)))
   ((string? error-info)
    (or (string-contains error-info "429")
        (string-contains error-info "500")
        (string-contains error-info "502")
        (string-contains error-info "503")
        (string-contains error-info "504")
        (string-contains error-info "rate limit")
        (string-contains error-info "timeout")
        (string-contains error-info "temporarily unavailable")))
   (else #f)))

(define (sleep-seconds seconds)
  "Sleep for the specified number of seconds (supports fractional)."
  (usleep (inexact->exact (floor (* seconds 1000000)))))

(define* (with-retry thunk #:key
                     (config #f)
                     (on-retry #f)
                     (should-retry? retryable-error?))
  "Execute thunk with retry logic on transient failures.

   Parameters:
   - thunk: Zero-argument procedure to execute
   - config: Retry configuration (default: *default-retry-config*)
   - on-retry: Optional callback (lambda (attempt delay error) ...)
   - should-retry?: Predicate to determine if error is retryable

   Returns the result of thunk on success.
   Throws the last error after all retries exhausted."
  (let* ((cfg (or config *default-retry-config*))
         (max-retries (assoc-ref cfg 'max-retries)))

    (let retry-loop ((attempt 0)
                     (last-error #f))
      (catch #t
        (lambda ()
          (thunk))
        (lambda (key . args)
          (let* ((error-msg (if (pair? args)
                               (format #f "~a" (car args))
                               (format #f "~a" key)))
                 (is-retryable (should-retry? error-msg)))

            (if (and is-retryable (< attempt max-retries))
                ;; Retry
                (let ((delay (calculate-delay cfg attempt)))
                  (when on-retry
                    (on-retry attempt delay error-msg))
                  (sleep-seconds delay)
                  (retry-loop (+ attempt 1) (cons key args)))
                ;; Give up - re-throw last error
                (apply throw key args))))))))

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
  "Execute curl command and return response.
   Uses safe-pipe-command to avoid FreeBSD 14 popen segfault."
  (let ((response (safe-pipe-command cmd)))
    ;; safe-pipe-command returns empty string on failure
    (if (string-null? response)
        (throw 'http-error "HTTP request failed (empty response)")
        response)))

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

   The on-chunk procedure receives each line of the response.
   Note: On FreeBSD, response is buffered then processed line-by-line
   to avoid popen segfaults."

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
               ;; Use safe-pipe-command instead of open-pipe*
               (response (safe-pipe-command cmd)))

          ;; Process response line by line
          (for-each
           (lambda (line)
             (catch #t
               (lambda ()
                 (unless (string-null? line)
                   (on-chunk line)))
               (lambda (key . args)
                 ;; Log error but continue
                 (format (current-error-port)
                         "Error processing chunk: ~a\n" key))))
           (string-split response #\newline))))

      (lambda ()
        (cleanup-temp-file temp-file)))))

(define (check-url-accessible url)
  "Check if a URL is accessible.
   Uses safe-pipe-command to avoid FreeBSD popen segfaults."
  (catch #t
    (lambda ()
      (let* ((cmd (format #f "curl -s -o /dev/null -w '%%{http_code}' '~a' 2>/dev/null" url))
             (response (string-trim-both (safe-pipe-command cmd))))
        (and (not (string-null? response))
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