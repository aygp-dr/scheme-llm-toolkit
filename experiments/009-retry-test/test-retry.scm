#!/usr/bin/env -S guile3 -L ../../src -e main -s
!#
;;; test-retry.scm --- Test exponential backoff retry logic

(use-modules (llm utils http)
             (srfi srfi-64))

(define (main args)
  (test-begin "retry-logic")

  ;; Test retry config creation
  (test-group "make-retry-config"
    (let ((config (make-retry-config)))
      (test-equal "default max-retries" 3 (assoc-ref config 'max-retries))
      (test-equal "default initial-delay" 1.0 (assoc-ref config 'initial-delay))
      (test-equal "default max-delay" 30.0 (assoc-ref config 'max-delay)))

    (let ((custom (make-retry-config #:max-retries 5
                                     #:initial-delay 0.5
                                     #:max-delay 10.0)))
      (test-equal "custom max-retries" 5 (assoc-ref custom 'max-retries))
      (test-equal "custom initial-delay" 0.5 (assoc-ref custom 'initial-delay))
      (test-equal "custom max-delay" 10.0 (assoc-ref custom 'max-delay))))

  ;; Test retryable error detection
  (test-group "retryable-error?"
    (test-assert "429 is retryable" (retryable-error? 429))
    (test-assert "500 is retryable" (retryable-error? 500))
    (test-assert "502 is retryable" (retryable-error? 502))
    (test-assert "503 is retryable" (retryable-error? 503))
    (test-assert "504 is retryable" (retryable-error? 504))
    (test-assert "400 is not retryable" (not (retryable-error? 400)))
    (test-assert "401 is not retryable" (not (retryable-error? 401)))
    (test-assert "404 is not retryable" (not (retryable-error? 404)))

    ;; String-based detection
    (test-assert "rate limit string" (retryable-error? "rate limit exceeded"))
    (test-assert "timeout string" (retryable-error? "connection timeout"))
    (test-assert "502 in string" (retryable-error? "HTTP 502 Bad Gateway")))

  ;; Test successful execution (no retry needed)
  (test-group "with-retry success"
    (let ((call-count 0))
      (define result
        (with-retry
         (lambda ()
           (set! call-count (+ call-count 1))
           'success)
         #:config (make-retry-config #:max-retries 3)))
      (test-equal "returns result" 'success result)
      (test-equal "called once" 1 call-count)))

  ;; Test retry on transient failure
  (test-group "with-retry transient failure"
    (let ((call-count 0))
      (define result
        (with-retry
         (lambda ()
           (set! call-count (+ call-count 1))
           (if (< call-count 3)
               (throw 'http-error "503 Service Unavailable")
               'success-after-retry))
         #:config (make-retry-config #:max-retries 5
                                     #:initial-delay 0.01
                                     #:jitter? #f)))
      (test-equal "returns result after retries" 'success-after-retry result)
      (test-equal "called 3 times" 3 call-count)))

  ;; Test non-retryable error
  (test-group "with-retry non-retryable"
    (let ((call-count 0))
      (test-error "throws on 404"
        (with-retry
         (lambda ()
           (set! call-count (+ call-count 1))
           (throw 'http-error "404 Not Found"))
         #:config (make-retry-config #:max-retries 3
                                     #:initial-delay 0.01)))
      (test-equal "called once (no retry)" 1 call-count)))

  ;; Test on-retry callback
  (test-group "with-retry callback"
    (let ((retry-log '()))
      (catch #t
        (lambda ()
          (with-retry
           (lambda ()
             (throw 'http-error "429 rate limit"))
           #:config (make-retry-config #:max-retries 2
                                       #:initial-delay 0.01
                                       #:jitter? #f)
           #:on-retry (lambda (attempt delay error)
                       (set! retry-log
                             (cons (list attempt delay error) retry-log)))))
        (lambda (key . args) #f))
      (test-equal "on-retry called twice" 2 (length retry-log))))

  (test-end "retry-logic")

  ;; Print results summary
  (let ((runner (test-runner-current)))
    (format #t "\n~a tests passed, ~a failed\n"
            (test-runner-pass-count runner)
            (test-runner-fail-count runner))
    (exit (if (zero? (test-runner-fail-count runner)) 0 1))))
