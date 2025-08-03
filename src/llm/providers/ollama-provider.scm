;;; ollama-provider.scm --- Ollama provider using the provider abstraction

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm providers ollama-provider)
  #:use-module (llm core provider)
  #:use-module (llm providers ollama)
  #:use-module (srfi srfi-1)
  #:export (make-ollama-llm-provider
            register-ollama-provider!))

;;; Commentary:
;;;
;;; This module bridges the Ollama implementation with the provider
;;; abstraction layer, making Ollama available through the standard
;;; provider interface.
;;;
;;; Code:

(define* (make-ollama-llm-provider #:key
                                  (base-url "http://localhost:11434")
                                  (model "llama2")
                                  (timeout 300))
  "Create an Ollama provider that implements the standard provider interface."
  
  (let ((ollama (make-ollama-provider 
                 #:base-url base-url
                 #:model model
                 #:timeout timeout)))
    
    (make-provider
     #:name 'ollama
     
     #:complete
     (lambda (provider prompt . options)
       (apply ollama-generate ollama prompt options))
     
     #:chat
     (lambda (provider messages . options)
       (apply ollama-chat ollama messages options))
     
     #:embeddings
     (lambda (provider text . options)
       (apply ollama-embeddings ollama text options))
     
     #:list-models
     (lambda (provider)
       (map (lambda (model-info)
              (assoc-ref model-info 'name))
            (ollama-list-models ollama)))
     
     #:capabilities
     '(streaming chat embeddings json-mode)
     
     #:metadata
     `((base-url . ,base-url)
       (default-model . ,model)
       (timeout . ,timeout)
       (implementation . ollama)))))

(define* (register-ollama-provider! #:key
                                   (base-url "http://localhost:11434")
                                   (model "llama2")
                                   (timeout 300))
  "Register Ollama as a provider in the global registry."
  (let ((provider (make-ollama-llm-provider 
                   #:base-url base-url
                   #:model model
                   #:timeout timeout)))
    (register-provider! 'ollama provider)))

;;; ollama-provider.scm ends here