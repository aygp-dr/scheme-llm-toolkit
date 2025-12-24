#!/usr/bin/env -S guile3 -L ../../src -s
!#
;;; test-practical.scm --- Test compaction with practical dev conversations

(use-modules (llm core compaction)
             (ice-9 format)
             (srfi srfi-1))

;; Load practical conversations
(load "practical-conversations.scm")

;;; --------------------------------------------------------------------
;;; Test Framework
;;; --------------------------------------------------------------------

(define (print-header title)
  (format #t "\n~a\n~a\n" title (make-string (string-length title) #\-)))

(define (check-retention compacted expected-terms)
  "Check which expected terms are retained."
  (let* ((text (string-downcase
                (apply string-append
                       (map (lambda (m) (or (assoc-ref m 'content) ""))
                            compacted))))
         (found (filter (lambda (term)
                         (string-contains text (string-downcase term)))
                       expected-terms)))
    (values found (length found) (length expected-terms))))

(define (test-conversation name msgs expected description)
  "Test compaction on a conversation."
  (print-header (format #f "~a" name))
  (format #t "~a\n" description)
  (format #t "Original: ~a messages, ~a tokens\n"
          (length msgs) (messages-token-count msgs))
  (format #t "Must retain: ~{~a~^, ~}\n\n" expected)

  (let ((results '()))
    (for-each
     (lambda (strategy)
       (let* ((config (make-compaction-config
                       #:strategy strategy
                       #:max-tokens 200
                       #:preserve-recent 2
                       #:importance-threshold 0.3))
              (compacted (compact-conversation msgs #:config config))
              (orig-tokens (messages-token-count msgs))
              (comp-tokens (messages-token-count compacted))
              (compression (* 100 (- 1 (/ comp-tokens orig-tokens)))))
         (call-with-values
             (lambda () (check-retention compacted expected))
           (lambda (found found-count total-count)
             (let ((retention (* 100 (/ found-count total-count))))
               (set! results (cons (cons strategy retention) results))
               (format #t "~a: ~a msgs, ~,0f% compressed, ~,0f% retained\n"
                       strategy
                       (length compacted)
                       compression
                       retention)
               (when (< retention 50)
                 (format #t "  WARNING: Lost critical info: ~{~a~^, ~}\n"
                         (lset-difference equal? expected found))))))))
     '(sliding-window summarize-older importance-weighted))

    ;; Best strategy
    (let ((best (fold (lambda (r best)
                       (if (> (cdr r) (cdr best)) r best))
                     (car results)
                     (cdr results))))
      (format #t "Best: ~a (~,0f%)\n" (car best) (cdr best)))))

;;; --------------------------------------------------------------------
;;; Main
;;; --------------------------------------------------------------------

(define (main)
  (format #t "~a\n" (make-string 60 #\=))
  (format #t "COMPACTION EVAL - Practical Developer Conversations\n")
  (format #t "~a\n" (make-string 60 #\=))

  (test-conversation
   "TECHNICAL SUPPORT"
   support-conversation
   support-expected
   "Debugging file upload 500 errors (nginx/express config)")

  (test-conversation
   "CODE REVIEW"
   code-review-conversation
   code-review-expected
   "Finding and fixing SQL injection vulnerability")

  (test-conversation
   "API INTEGRATION"
   api-conversation
   api-expected
   "Troubleshooting auth, rate limits, pagination")

  (test-conversation
   "INCIDENT RESPONSE"
   incident-conversation
   incident-expected
   "Production outage: OOMKilled pods, rollback")

  (test-conversation
   "ONBOARDING"
   onboarding-conversation
   onboarding-expected
   "New developer setup and deployment workflow")

  (print-header "RECOMMENDATIONS")
  (format #t "
For technical conversations with specific solutions:
  -> Use sliding-window (preserves recent fix details)

For troubleshooting with multiple steps:
  -> Use importance-weighted (keeps high-signal messages)

For onboarding/documentation:
  -> Use summarize-older (compress history, keep recent)

Key insight: Compaction should preserve ACTIONABLE info:
  - Error codes, commands, config values
  - Security fixes, version numbers
  - Specific steps to reproduce/fix
")
  (format #t "\n~a\n" (make-string 60 #\=)))

(main)
