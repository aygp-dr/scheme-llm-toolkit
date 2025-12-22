;;; openai-provider.scm --- OpenAI provider using the provider abstraction

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm providers openai-provider)
  #:use-module (llm core provider)
  #:use-module (llm providers openai)
  #:use-module (srfi srfi-1)
  #:export (make-openai-llm-provider
            register-openai-provider!))

;;; Commentary:
;;;
;;; This module bridges the OpenAI implementation with the provider
;;; abstraction layer, making OpenAI available through the standard
;;; provider interface.
;;;
;;; Code:

(define* (make-openai-llm-provider #:key
                                  (base-url "https://api.openai.com/v1")
                                  (api-key #f)
                                  (model "gpt-3.5-turbo")
                                  (timeout 300))
  "Create an OpenAI provider that implements the standard provider interface.
   
   Required:
   - api-key: OpenAI API key for authentication
   
   Optional:
   - base-url: API base URL (default: https://api.openai.com/v1)
   - model: Default model to use (default: gpt-3.5-turbo)
   - timeout: Request timeout in seconds (default: 300)"
  
  (unless api-key
    (error "OpenAI API key is required"))
  
  (let ((openai (make-openai-provider 
                 #:base-url base-url
                 #:api-key api-key
                 #:model model
                 #:timeout timeout)))
    
    (make-provider
     #:name 'openai
     
     #:complete
     (lambda (provider prompt . options)
       (apply openai-generate openai prompt options))
     
     #:chat
     (lambda (provider messages . options)
       (apply openai-chat openai messages options))
     
     #:embeddings
     (lambda (provider text . options)
       (apply openai-embeddings openai text options))
     
     #:list-models
     (lambda (provider)
       (map (lambda (model-info)
              (assoc-ref model-info 'id))
            (openai-list-models openai)))
     
     #:capabilities
     '(streaming chat embeddings system-prompts context-window 
       json-mode function-calling usage-tracking rate-limits)
     
     #:metadata
     `((base-url . ,base-url)
       (default-model . ,model)
       (timeout . ,timeout)
       (implementation . openai)
       (requires-api-key . #t)))))

(define* (register-openai-provider! #:key
                                   (base-url "https://api.openai.com/v1")
                                   (api-key #f)
                                   (model "gpt-3.5-turbo")
                                   (timeout 300))
  "Register OpenAI as a provider in the global registry.
   
   Required:
   - api-key: OpenAI API key for authentication
   
   Optional:
   - base-url: API base URL (default: https://api.openai.com/v1)
   - model: Default model to use (default: gpt-3.5-turbo)
   - timeout: Request timeout in seconds (default: 300)"
  
  (unless api-key
    (error "OpenAI API key is required"))
  
  (let ((provider (make-openai-llm-provider 
                   #:base-url base-url
                   #:api-key api-key
                   #:model model
                   #:timeout timeout)))
    (register-provider! 'openai provider)))

;;; openai-provider.scm ends here