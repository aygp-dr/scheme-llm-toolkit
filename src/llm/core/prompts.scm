;;; prompts.scm --- S-expression prompt DSL for LLM toolkit

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm core prompts)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (;; Message constructors
            system-msg
            user-msg
            assistant-msg
            ;; Aliases (use with rename to avoid core binding conflicts)
            (system-msg . system)
            (user-msg . user)
            (assistant-msg . assistant)

            ;; Prompt composition
            prompt
            prompt-append
            prompt-prepend

            ;; Templates
            template
            var
            interpolate

            ;; Prompt definition macro
            define-prompt

            ;; Utilities
            messages->alist
            prompt->messages
            format-for-provider))

;;; Commentary:
;;;
;;; This module provides an S-expression DSL for constructing LLM prompts.
;;; It leverages Scheme's homoiconicity to create composable, type-safe
;;; prompt templates.
;;;
;;; Example usage:
;;;
;;;   (define review-prompt
;;;     (prompt
;;;       (system "You are an expert code reviewer.")
;;;       (user (template "Review this code:\n" (var 'code)))))
;;;
;;;   (interpolate review-prompt `((code . "def hello(): pass")))
;;;
;;; Code:

;;; --------------------------------------------------------------------
;;; Message Types
;;; --------------------------------------------------------------------

(define (make-message role content)
  "Create a message with role and content."
  (list (cons 'role role)
        (cons 'content content)))

(define (system-msg content)
  "Create a system message."
  (make-message 'system content))

(define (user-msg content)
  "Create a user message."
  (make-message 'user content))

(define (assistant-msg content)
  "Create an assistant message."
  (make-message 'assistant content))

(define (message-role msg)
  "Get the role of a message."
  (assoc-ref msg 'role))

(define (message-content msg)
  "Get the content of a message."
  (assoc-ref msg 'content))

;;; --------------------------------------------------------------------
;;; Template System
;;; --------------------------------------------------------------------

(define (var name)
  "Create a variable reference for template interpolation.

   Usage: (var 'code) => placeholder for 'code variable"
  (list 'var name))

(define (var? x)
  "Check if x is a variable reference."
  (and (list? x)
       (= (length x) 2)
       (eq? (car x) 'var)))

(define (var-name v)
  "Get the name of a variable reference."
  (cadr v))

(define (template . parts)
  "Create a template from parts (strings and variable references).

   Usage: (template \"Hello, \" (var 'name) \"!\")
   Returns a template structure that can be interpolated."
  (list 'template parts))

(define (template? x)
  "Check if x is a template."
  (and (list? x)
       (>= (length x) 1)
       (eq? (car x) 'template)))

(define (template-parts t)
  "Get the parts of a template."
  (cadr t))

(define (interpolate-template tmpl bindings)
  "Interpolate a template with variable bindings.

   bindings: association list of (name . value) pairs"
  (let ((parts (template-parts tmpl)))
    (apply string-append
           (map (lambda (part)
                  (cond
                   ((string? part) part)
                   ((var? part)
                    (let ((value (assoc-ref bindings (var-name part))))
                      (if value
                          (if (string? value)
                              value
                              (format #f "~a" value))
                          (error "Unbound variable in template" (var-name part)))))
                   ((template? part)
                    (interpolate-template part bindings))
                   (else
                    (format #f "~a" part))))
                parts))))

;;; --------------------------------------------------------------------
;;; Prompt Composition
;;; --------------------------------------------------------------------

(define (prompt . messages)
  "Create a prompt from a sequence of messages.

   Usage:
     (prompt
       (system \"You are helpful.\")
       (user \"Hello!\"))"
  (list 'prompt messages))

(define (prompt? x)
  "Check if x is a prompt."
  (and (list? x)
       (>= (length x) 1)
       (eq? (car x) 'prompt)))

(define (prompt-messages p)
  "Get the messages from a prompt."
  (cadr p))

(define (prompt-append base . messages)
  "Append messages to a prompt."
  (if (prompt? base)
      (apply prompt (append (prompt-messages base) messages))
      (apply prompt (cons base messages))))

(define (prompt-prepend base . messages)
  "Prepend messages to a prompt."
  (if (prompt? base)
      (apply prompt (append messages (prompt-messages base)))
      (apply prompt (append messages (list base)))))

;;; --------------------------------------------------------------------
;;; Interpolation
;;; --------------------------------------------------------------------

(define (interpolate-message msg bindings)
  "Interpolate variables in a message's content."
  (let ((role (message-role msg))
        (content (message-content msg)))
    (make-message
     role
     (cond
      ((string? content) content)
      ((template? content) (interpolate-template content bindings))
      ((var? content)
       (let ((value (assoc-ref bindings (var-name content))))
         (or value (error "Unbound variable" (var-name content)))))
      (else (format #f "~a" content))))))

(define (interpolate p bindings)
  "Interpolate all variables in a prompt with bindings.

   bindings: association list of (name . value) pairs

   Usage:
     (interpolate my-prompt '((code . \"...\") (language . \"python\")))"
  (cond
   ((prompt? p)
    (list 'prompt
          (map (lambda (msg) (interpolate-message msg bindings))
               (prompt-messages p))))
   ((list? p)
    ;; Assume it's a single message
    (interpolate-message p bindings))
   (else p)))

;;; --------------------------------------------------------------------
;;; Prompt Definition Macro
;;; --------------------------------------------------------------------

(define-syntax define-prompt
  (syntax-rules ()
    "Define a named prompt template.

     Usage:
       (define-prompt code-review
         (system \"You are a code reviewer.\")
         (user (template \"Review: \" (var 'code))))

       (code-review '((code . \"...\")))"
    ((define-prompt name messages ...)
     (define (name . args)
       (let ((bindings (if (null? args) '() (car args))))
         (interpolate (prompt messages ...) bindings))))))

;;; --------------------------------------------------------------------
;;; Conversion Utilities
;;; --------------------------------------------------------------------

(define (messages->alist msgs)
  "Convert messages to a list of association lists.
   Suitable for JSON serialization."
  (map (lambda (msg)
         `((role . ,(symbol->string (message-role msg)))
           (content . ,(message-content msg))))
       msgs))

(define (prompt->messages p)
  "Extract messages from a prompt as an alist list."
  (if (prompt? p)
      (messages->alist (prompt-messages p))
      (if (list? p)
          (messages->alist (list p))
          (error "Not a prompt" p))))

(define* (format-for-provider p #:key (provider 'openai))
  "Format a prompt for a specific provider's API format.

   provider: 'openai, 'anthropic, or 'ollama

   Returns an alist suitable for JSON encoding."
  (let ((messages (if (prompt? p)
                      (prompt-messages p)
                      (list p))))
    (case provider
      ((openai ollama)
       ;; OpenAI/Ollama format: {messages: [{role, content}, ...]}
       `((messages . ,(messages->alist messages))))

      ((anthropic)
       ;; Anthropic format: separate system, then messages array
       (let* ((system-msgs (filter (lambda (m) (eq? (message-role m) 'system))
                                   messages))
              (other-msgs (filter (lambda (m) (not (eq? (message-role m) 'system)))
                                  messages))
              (system-text (if (null? system-msgs)
                              #f
                              (string-join
                               (map message-content system-msgs)
                               "\n"))))
         (if system-text
             `((system . ,system-text)
               (messages . ,(messages->alist other-msgs)))
             `((messages . ,(messages->alist other-msgs))))))

      (else
       (error "Unknown provider" provider)))))

;;; prompts.scm ends here
