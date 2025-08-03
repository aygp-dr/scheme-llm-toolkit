;;; provider.scm --- Provider abstraction for LLM toolkit

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm core provider)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:export (<provider>
            make-provider
            provider?
            provider-name
            provider-complete
            provider-complete-streaming
            provider-chat
            provider-chat-streaming
            provider-embeddings
            provider-list-models
            provider-capabilities
            
            ;; Provider registry
            register-provider!
            get-provider
            list-providers
            
            ;; Utilities
            with-provider
            provider-info))

;;; Commentary:
;;;
;;; This module defines the provider abstraction layer for the LLM toolkit.
;;; All LLM providers (Ollama, OpenAI, Anthropic, etc.) must implement
;;; this interface to ensure consistent behavior across the toolkit.
;;;
;;; Code:

;;;; Provider Type Definition

(define-record-type <provider>
  (make-provider-internal name complete-fn chat-fn embeddings-fn 
                         list-models-fn capabilities metadata)
  provider?
  (name provider-name)
  (complete-fn provider-complete-fn)
  (chat-fn provider-chat-fn)
  (embeddings-fn provider-embeddings-fn)
  (list-models-fn provider-list-models-fn)
  (capabilities provider-capabilities)
  (metadata provider-metadata))

;;;; Provider Registry

(define *provider-registry* '())

(define (register-provider! name provider)
  "Register a provider in the global registry."
  (set! *provider-registry*
        (assoc-set! *provider-registry* name provider))
  provider)

(define (get-provider name)
  "Get a provider from the registry by name."
  (assoc-ref *provider-registry* name))

(define (list-providers)
  "List all registered provider names."
  (map car *provider-registry*))

;;;; Provider Constructor

(define* (make-provider #:key
                       name
                       complete
                       chat
                       embeddings
                       list-models
                       (capabilities '())
                       (metadata '()))
  "Create a new provider with the given implementation functions.
   
   Required parameters:
   - name: Symbol identifying the provider
   - complete: Function (provider prompt . options) -> response
   
   Optional parameters:
   - chat: Function (provider messages . options) -> response
   - embeddings: Function (provider text . options) -> vector
   - list-models: Function (provider) -> list of models
   - capabilities: List of supported features
   - metadata: Additional provider information"
  
  (unless name
    (error "Provider name is required"))
  
  (unless complete
    (error "Complete function is required"))
  
  ;; Default implementations
  (let ((chat-fn (or chat
                     (lambda (provider messages . options)
                       ;; Convert chat to completion
                       (let ((prompt (messages->prompt messages)))
                         (apply complete provider prompt options)))))
        
        (embeddings-fn (or embeddings
                          (lambda args
                            (error "Embeddings not supported by this provider"))))
        
        (list-models-fn (or list-models
                           (lambda (provider)
                             '()))))
    
    (make-provider-internal name complete chat-fn embeddings-fn 
                           list-models-fn capabilities metadata)))

;;;; Provider Interface Functions

(define (provider-complete provider prompt . options)
  "Complete a prompt using the provider."
  (apply (provider-complete-fn provider) provider prompt options))

(define (provider-complete-streaming provider prompt on-token . options)
  "Complete a prompt with streaming response."
  (apply (provider-complete-fn provider) provider prompt 
         (append options `(#:stream #t #:on-token ,on-token))))

(define (provider-chat provider messages . options)
  "Chat completion using the provider."
  (apply (provider-chat-fn provider) provider messages options))

(define (provider-chat-streaming provider messages on-token . options)
  "Chat completion with streaming response."
  (apply (provider-chat-fn provider) provider messages
         (append options `(#:stream #t #:on-token ,on-token))))

(define (provider-embeddings provider text . options)
  "Generate embeddings for text."
  (apply (provider-embeddings-fn provider) provider text options))

(define (provider-list-models provider)
  "List available models for this provider."
  ((provider-list-models-fn provider) provider))

(define (provider-info provider)
  "Get information about the provider."
  `((name . ,(provider-name provider))
    (capabilities . ,(provider-capabilities provider))
    (models . ,(catch #t
                 (lambda () (provider-list-models provider))
                 (lambda _ '())))
    ,@(provider-metadata provider)))

;;;; Utility Functions

(define-syntax-rule (with-provider provider-name body ...)
  (let ((provider (get-provider 'provider-name)))
    (unless provider
      (error "Provider not found" 'provider-name))
    body ...))

(define (messages->prompt messages)
  "Convert chat messages to a single prompt string.
   This is a fallback for providers that don't support chat natively."
  (string-join
   (map (lambda (msg)
          (let ((role (assoc-ref msg 'role))
                (content (assoc-ref msg 'content)))
            (format #f "~a: ~a" 
                    (string-capitalize (symbol->string role))
                    content)))
        messages)
   "\n"))

;;;; Standard Capabilities

;; These are standard capabilities that providers can declare support for
(define *standard-capabilities*
  '(streaming          ; Streaming responses
    chat              ; Native chat API
    embeddings        ; Text embeddings
    function-calling  ; Function/tool calling
    vision           ; Image understanding
    audio            ; Audio processing
    system-prompts   ; System message support
    context-window   ; Large context windows
    json-mode        ; Structured JSON output
    fine-tuning      ; Model fine-tuning
    rate-limits      ; Rate limiting info
    usage-tracking)) ; Token usage tracking

(define (validate-capabilities capabilities)
  "Validate that capabilities are recognized."
  (for-each
   (lambda (cap)
     (unless (member cap *standard-capabilities*)
       (format (current-error-port) 
               "Warning: Unknown capability '~a'\n" cap)))
   capabilities))

;;; provider.scm ends here