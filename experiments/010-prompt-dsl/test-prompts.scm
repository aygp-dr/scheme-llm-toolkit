#!/usr/bin/env -S guile3 -L ../../src -s
!#
;;; test-prompts.scm --- Test S-expression prompt DSL

(use-modules (llm core prompts)
             (ice-9 format))

;; Simple assert macro
(define-syntax assert
  (syntax-rules ()
    ((assert expr)
     (unless expr
       (error "Assertion failed" 'expr)))))

(define (test-basic-messages)
  (display "Testing basic message creation...\n")

  ;; Test system message
  (let ((msg (system-msg "You are helpful.")))
    (assert (equal? (assoc-ref msg 'role) 'system))
    (assert (equal? (assoc-ref msg 'content) "You are helpful."))
    (display "  system message: PASS\n"))

  ;; Test user message
  (let ((msg (user-msg "Hello!")))
    (assert (equal? (assoc-ref msg 'role) 'user))
    (assert (equal? (assoc-ref msg 'content) "Hello!"))
    (display "  user message: PASS\n"))

  ;; Test assistant message
  (let ((msg (assistant-msg "Hi there!")))
    (assert (equal? (assoc-ref msg 'role) 'assistant))
    (assert (equal? (assoc-ref msg 'content) "Hi there!"))
    (display "  assistant message: PASS\n")))

(define (test-prompt-composition)
  (display "\nTesting prompt composition...\n")

  ;; Test basic prompt
  (let ((p (prompt
            (system-msg "You are a helpful assistant.")
            (user-msg "What is 2+2?"))))
    (let ((msgs (prompt->messages p)))
      (assert (= (length msgs) 2))
      (assert (equal? (assoc-ref (car msgs) 'role) "system"))
      (assert (equal? (assoc-ref (cadr msgs) 'role) "user"))
      (display "  basic prompt: PASS\n")))

  ;; Test prompt append
  (let* ((base (prompt (system-msg "Be helpful.")))
         (extended (prompt-append base (user-msg "Hello"))))
    (let ((msgs (prompt->messages extended)))
      (assert (= (length msgs) 2))
      (display "  prompt-append: PASS\n"))))

(define (test-templates)
  (display "\nTesting template system...\n")

  ;; Test simple template
  (let ((tmpl (template "Hello, " (var 'name) "!")))
    (let ((result (interpolate (user-msg tmpl) '((name . "World")))))
      (assert (equal? (assoc-ref result 'content) "Hello, World!"))
      (display "  simple template: PASS\n")))

  ;; Test multi-variable template
  (let ((tmpl (template "Review this " (var 'language) " code:\n" (var 'code))))
    (let* ((msg (user-msg tmpl))
           (result (interpolate msg '((language . "Python")
                                      (code . "print('hello')")))))
      (assert (string-contains (assoc-ref result 'content) "Python"))
      (assert (string-contains (assoc-ref result 'content) "print"))
      (display "  multi-variable template: PASS\n"))))

(define (test-prompt-interpolation)
  (display "\nTesting full prompt interpolation...\n")

  (let ((p (prompt
            (system-msg (template "You are a " (var 'role) " expert."))
            (user-msg (template "Explain " (var 'topic) " to me.")))))
    (let* ((filled (interpolate p '((role . "Python")
                                    (topic . "decorators"))))
           (msgs (prompt->messages filled)))
      (assert (string-contains (assoc-ref (car msgs) 'content) "Python"))
      (assert (string-contains (assoc-ref (cadr msgs) 'content) "decorators"))
      (display "  full prompt interpolation: PASS\n"))))

(define (test-provider-formatting)
  (display "\nTesting provider formatting...\n")

  (let ((p (prompt
            (system-msg "Be concise.")
            (user-msg "Hello"))))

    ;; OpenAI format
    (let ((openai-fmt (format-for-provider p #:provider 'openai)))
      (assert (assoc-ref openai-fmt 'messages))
      (display "  OpenAI format: PASS\n"))

    ;; Anthropic format (system separate)
    (let ((anthropic-fmt (format-for-provider p #:provider 'anthropic)))
      (assert (assoc-ref anthropic-fmt 'system))
      (assert (assoc-ref anthropic-fmt 'messages))
      ;; Anthropic messages should not include system
      (let ((msgs (assoc-ref anthropic-fmt 'messages)))
        (assert (= (length msgs) 1))
        (assert (equal? (assoc-ref (car msgs) 'role) "user")))
      (display "  Anthropic format: PASS\n"))))

(define (test-define-prompt-macro)
  (display "\nTesting define-prompt macro...\n")

  ;; Define a reusable prompt template
  (define-prompt code-review
    (system-msg "You are an expert code reviewer.")
    (user-msg (template "Review this code:\n```\n" (var 'code) "\n```")))

  ;; Use the defined prompt
  (let* ((result (code-review '((code . "def hello(): pass"))))
         (msgs (prompt->messages result)))
    (assert (= (length msgs) 2))
    (assert (string-contains (assoc-ref (cadr msgs) 'content) "def hello"))
    (display "  define-prompt macro: PASS\n")))

;; Run all tests
(display "=== S-expression Prompt DSL Tests ===\n\n")

(test-basic-messages)
(test-prompt-composition)
(test-templates)
(test-prompt-interpolation)
(test-provider-formatting)
(test-define-prompt-macro)

(display "\n=== All tests passed! ===\n")
