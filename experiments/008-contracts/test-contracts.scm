#!/usr/bin/env -S guile -L ../../src -e main -s
!#
;;; test-contracts.scm --- Test suite for contract system

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(use-modules (llm core contracts)
             (ice-9 format)
             (srfi srfi-1))

;;; Test utilities

(define (test-result name passed? message)
  (format #t "~a ~a: ~a~%"
          (if passed? "PASS" "FAIL")
          name
          message))

(define (run-test name thunk)
  (catch #t
    (lambda ()
      (let ((result (thunk)))
        (test-result name result (if result "OK" "Failed"))
        result))
    (lambda (key . args)
      (test-result name #f (format #f "Error: ~a" key))
      #f)))

(define (expect-violation thunk)
  "Expect a contract violation to be raised."
  (catch 'contract-violation
    (lambda ()
      (thunk)
      #f)  ; Should have raised
    (lambda (key . args)
      #t)))  ; Expected violation

;;; Flat Contract Tests

(define (test-flat-contract-string)
  "Test flat contract for strings"
  (let ((c (flat-contract string? #:name "string?")))
    (and (contract? c)
         (contract-check c "hello")
         (not (contract-check c 42))
         (not (contract-check c '())))))

(define (test-flat-contract-predicate)
  "Test custom predicate contract"
  (let ((positive? (flat-contract (lambda (x) (and (number? x) (> x 0)))
                                 #:name "positive?")))
    (and (contract-check positive? 5)
         (contract-check positive? 0.1)
         (not (contract-check positive? 0))
         (not (contract-check positive? -3)))))

;;; Combinator Tests

(define (test-and-contract)
  "Test and/c combinator"
  (let* ((string? (flat-contract string? #:name "string?"))
         (non-empty? (flat-contract (lambda (s) (> (string-length s) 0))
                                   #:name "non-empty?"))
         (c (and/c string? non-empty?)))
    (and (contract-check c "hello")
         (not (contract-check c ""))
         (not (contract-check c 42)))))

(define (test-or-contract)
  "Test or/c combinator"
  (let* ((string? (flat-contract string? #:name "string?"))
         (number? (flat-contract number? #:name "number?"))
         (c (or/c string? number?)))
    (and (contract-check c "hello")
         (contract-check c 42)
         (not (contract-check c '())))))

(define (test-not-contract)
  "Test not/c combinator"
  (let* ((null? (flat-contract null? #:name "null?"))
         (c (not/c null?)))
    (and (contract-check c "hello")
         (contract-check c 42)
         (not (contract-check c '())))))

;;; Parametric Contract Tests

(define (test-listof-contract)
  "Test listof/c contract"
  (let* ((number? (flat-contract number? #:name "number?"))
         (c (listof/c number?)))
    (and (contract-check c '())
         (contract-check c '(1 2 3))
         (not (contract-check c '(1 "two" 3)))
         (not (contract-check c "not a list")))))

(define (test-cons-contract)
  "Test cons/c contract"
  (let* ((symbol? (flat-contract symbol? #:name "symbol?"))
         (string? (flat-contract string? #:name "string?"))
         (c (cons/c symbol? string?)))
    (and (contract-check c '(name . "value"))
         (not (contract-check c '("name" . "value")))
         (not (contract-check c '(name . 42))))))

;;; Alist Contract Tests

(define (test-alist-contract)
  "Test alist/c with required and optional fields"
  (let* ((string? (flat-contract string? #:name "string?"))
         (number? (flat-contract number? #:name "number?"))
         (c (alist/c
             #:required `((name . ,string?)
                         (age . ,number?))
             #:optional `((email . ,string?)))))
    (and (contract-check c '((name . "Alice") (age . 30)))
         (contract-check c '((name . "Bob") (age . 25) (email . "bob@example.com")))
         (not (contract-check c '((name . "Charlie"))))  ; missing age
         (not (contract-check c '((name . 123) (age . 30))))))) ; wrong type

;;; Function Contract Tests

(define (test-function-contract-basic)
  "Test basic function contract"
  (let* ((string? (flat-contract string? #:name "string?"))
         (number? (flat-contract number? #:name "number?"))
         (c (->/c string? number?)))
    ;; First-order check: is it a procedure?
    (and (contract-check c string-length)
         (not (contract-check c "not a function")))))

(define (test-function-contract-wrapped)
  "Test that wrapped functions check contracts"
  (let* ((string? (flat-contract string? #:name "string?"))
         (number? (flat-contract number? #:name "number?"))
         (c (->/c string? number?))
         (wrapped ((contract-projection c) string-length 'callee 'caller)))
    ;; Valid call
    (and (= (wrapped "hello") 5)
         ;; Invalid call should raise (but we catch it)
         (catch 'contract-violation
           (lambda () (wrapped 42) #f)
           (lambda _ #t)))))

;;; JSON Schema Contract Tests

(define (test-json-schema-string)
  "Test JSON schema for string type"
  (let ((c (json-schema/c '((type . "string")))))
    (and (contract-check c "hello")
         (not (contract-check c 42)))))

(define (test-json-schema-object)
  "Test JSON schema for object with properties"
  (let ((c (json-schema/c
            '((type . "object")
              (required . ("name"))
              (properties
               . ((name . ((type . "string")))
                  (count . ((type . "number")))))))))
    (and (contract-check c '((name . "test")))
         (contract-check c '((name . "test") (count . 5)))
         (not (contract-check c '((count . 5))))))) ; missing required

;;; LLM Domain Contract Tests

(define (test-message-contract)
  "Test message contract"
  (and (contract-check message?/c '((role . "user") (content . "Hello")))
       (contract-check message?/c '((role . assistant) (content . "Hi there")))
       (not (contract-check message?/c '((content . "no role"))))))

(define (test-response-contract)
  "Test response contract"
  (and (contract-check response?/c "Simple string response")
       (contract-check response?/c '((role . "assistant")
                                    (content . "text")
                                    (tool_calls . ())))
       (contract-check response?/c '((role . "assistant")
                                    (content . #f)
                                    (tool_use . ())))))

;;; Blame Tests

(define (test-blame-context)
  "Test blame context tracking"
  (with-contract-region outer
    (with-contract-region inner
      (catch 'contract-violation
        (lambda ()
          (let* ((c (flat-contract (lambda (_) #f) #:name "always-fail"))
                 (proj (lambda (v p n) (contract-violation c v p n))))
            (proj "value" 'provider 'caller)
            #f))
        (lambda (key . args)
          ;; Violation was raised with context
          #t)))))

;;; Main

(define (main args)
  (format #t "~%=== Contract System Tests ===~%~%")

  (format #t "--- Flat Contracts ---~%")
  (let ((results
         (list
          (run-test "flat-contract-string" test-flat-contract-string)
          (run-test "flat-contract-predicate" test-flat-contract-predicate))))
    (format #t "Flat contracts: ~a/~a~%" (count identity results) (length results)))

  (format #t "~%--- Combinators ---~%")
  (let ((results
         (list
          (run-test "and/c" test-and-contract)
          (run-test "or/c" test-or-contract)
          (run-test "not/c" test-not-contract))))
    (format #t "Combinators: ~a/~a~%" (count identity results) (length results)))

  (format #t "~%--- Parametric Contracts ---~%")
  (let ((results
         (list
          (run-test "listof/c" test-listof-contract)
          (run-test "cons/c" test-cons-contract)
          (run-test "alist/c" test-alist-contract))))
    (format #t "Parametric: ~a/~a~%" (count identity results) (length results)))

  (format #t "~%--- Function Contracts ---~%")
  (let ((results
         (list
          (run-test "function-basic" test-function-contract-basic)
          (run-test "function-wrapped" test-function-contract-wrapped))))
    (format #t "Functions: ~a/~a~%" (count identity results) (length results)))

  (format #t "~%--- JSON Schema ---~%")
  (let ((results
         (list
          (run-test "json-schema-string" test-json-schema-string)
          (run-test "json-schema-object" test-json-schema-object))))
    (format #t "JSON Schema: ~a/~a~%" (count identity results) (length results)))

  (format #t "~%--- LLM Domain ---~%")
  (let ((results
         (list
          (run-test "message-contract" test-message-contract)
          (run-test "response-contract" test-response-contract))))
    (format #t "LLM Domain: ~a/~a~%" (count identity results) (length results)))

  (format #t "~%--- Blame ---~%")
  (let ((results
         (list
          (run-test "blame-context" test-blame-context))))
    (format #t "Blame: ~a/~a~%" (count identity results) (length results)))

  (format #t "~%=== Tests Complete ===~%"))
