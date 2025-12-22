;;; anthropic-provider.scm --- Anthropic provider using the provider abstraction

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm providers anthropic-provider)
  #:use-module (llm core provider)
  #:use-module (llm providers anthropic)
  #:use-module (srfi srfi-1)
  #:export (make-anthropic-llm-provider
            register-anthropic-provider!))

;;; Commentary:
;;;
;;; This module bridges the Anthropic implementation with the provider
;;; abstraction layer, making Claude available through the standard
;;; provider interface.
;;;
;;; Code:

(define* (make-anthropic-llm-provider #:key
                                     (base-url "https://api.anthropic.com/v1")
                                     (api-key #f)
                                     (model "claude-sonnet-4-20250514")
                                     (timeout 300)
                                     (api-version "2023-06-01"))
  "Create an Anthropic provider that implements the standard provider interface.

   Required:
   - api-key: Anthropic API key for authentication

   Optional:
   - base-url: API base URL (default: https://api.anthropic.com/v1)
   - model: Default model to use (default: claude-sonnet-4-20250514)
   - timeout: Request timeout in seconds (default: 300)
   - api-version: Anthropic API version (default: 2023-06-01)"

  (unless api-key
    (error "Anthropic API key is required"))

  (let ((anthropic (make-anthropic-provider
                    #:base-url base-url
                    #:api-key api-key
                    #:model model
                    #:timeout timeout
                    #:api-version api-version)))

    (make-provider
     #:name 'anthropic

     #:complete
     (lambda (provider prompt . options)
       (apply anthropic-generate anthropic prompt options))

     #:chat
     (lambda (provider messages . options)
       (apply anthropic-chat anthropic messages options))

     #:embeddings
     (lambda (provider text . options)
       ;; Anthropic doesn't have a public embeddings API
       (error "Anthropic does not provide an embeddings API"))

     #:list-models
     (lambda (provider)
       (map (lambda (model-info)
              (assoc-ref model-info 'id))
            (anthropic-list-models anthropic)))

     #:capabilities
     '(streaming chat system-prompts context-window
       function-calling usage-tracking rate-limits)

     #:metadata
     `((base-url . ,base-url)
       (default-model . ,model)
       (timeout . ,timeout)
       (api-version . ,api-version)
       (implementation . anthropic)
       (requires-api-key . #t)))))

(define* (register-anthropic-provider! #:key
                                      (base-url "https://api.anthropic.com/v1")
                                      (api-key #f)
                                      (model "claude-sonnet-4-20250514")
                                      (timeout 300)
                                      (api-version "2023-06-01"))
  "Register Anthropic as a provider in the global registry.

   Required:
   - api-key: Anthropic API key for authentication

   Optional:
   - base-url: API base URL (default: https://api.anthropic.com/v1)
   - model: Default model to use (default: claude-sonnet-4-20250514)
   - timeout: Request timeout in seconds (default: 300)
   - api-version: Anthropic API version (default: 2023-06-01)"

  (unless api-key
    (error "Anthropic API key is required"))

  (let ((provider (make-anthropic-llm-provider
                   #:base-url base-url
                   #:api-key api-key
                   #:model model
                   #:timeout timeout
                   #:api-version api-version)))
    (register-provider! 'anthropic provider)))

;;; anthropic-provider.scm ends here
