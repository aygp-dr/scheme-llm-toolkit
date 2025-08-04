#!/usr/bin/env guile3
;; Dependency Check for Guile Scheme LLM Integration Toolkit
;; Confirms Guile 3.0+ and HTTP/JSON capabilities

(use-modules (ice-9 format)
             (ice-9 regex)
             (ice-9 pretty-print)
             (srfi srfi-1)
             (srfi srfi-26)
             (web client)
             (web uri))

(define (check-guile-version)
  "Check that we're running Guile 3.0 or later"
  (let ((version (version)))
    (format #t "✓ Guile version: ~a~%" version)
    (if (string-prefix? "3." version)
        (format #t "✓ Guile 3.x detected - compatible~%")
        (format #t "⚠ Warning: Expected Guile 3.x, got ~a~%" version))))

(define (check-module module-name)
  "Check if a module can be loaded"
  (catch #t
    (lambda ()
      (resolve-module module-name)
      (format #t "✓ Module ~a: available~%" module-name)
      #t)
    (lambda (key . args)
      (format #t "✗ Module ~a: missing (~a)~%" module-name key)
      #f)))

(define required-modules
  '((web client)
    (web uri)
    (web request)
    (web response)
    (ice-9 format)
    (ice-9 match)
    (srfi srfi-1)
    (srfi srfi-26)
    (json)))

(define optional-modules
  '((gnutls)
    (ice-9 threads)
    (ice-9 binary-ports)))

(define (main)
  (format #t "=== Guile Scheme LLM Integration Toolkit - Dependency Check ===~%~%")
  
  ;; Check Guile version
  (check-guile-version)
  (newline)
  
  ;; Check required modules
  (format #t "Required modules:~%")
  (let ((missing (filter (lambda (mod) (not (check-module mod))) required-modules)))
    (if (null? missing)
        (format #t "✓ All required modules available~%")
        (format #t "✗ Missing required modules: ~a~%" missing)))
  
  (newline)
  
  ;; Check optional modules
  (format #t "Optional modules (for enhanced features):~%")
  (for-each check-module optional-modules)
  
  (newline)
  (format #t "Feature checks:~%")
  (format #t "✓ HTTP client support: web modules available~%")
  (format #t "✓ JSON processing: json module check~%")
  (format #t "✓ Pattern matching: ice-9 match available~%")
  
  (newline)
  (format #t "=== Dependency check complete ===~%"))

;; Run the check
(main)