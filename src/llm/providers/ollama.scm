;;; ollama.scm --- Ollama provider implementation

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm providers ollama)
  #:use-module (llm utils http)
  #:use-module (llm utils json)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-ollama-provider
            ollama-generate
            ollama-chat
            ollama-embeddings
            ollama-list-models
            ollama-model-info
            ollama-pull-model
            check-ollama-server
            *default-ollama-url*))

;;; Commentary:
;;;
;;; This module provides a complete Ollama API client implementation.
;;; It supports all major Ollama endpoints including generation, chat,
;;; embeddings, and model management.
;;;
;;; Code:

(define *default-ollama-url* "http://localhost:11434")

(define-record-type <ollama-provider>
  (make-ollama-provider-internal base-url default-model timeout)
  ollama-provider?
  (base-url ollama-provider-base-url)
  (default-model ollama-provider-default-model set-ollama-provider-default-model!)
  (timeout ollama-provider-timeout))

(define* (make-ollama-provider #:key 
                              (base-url *default-ollama-url*)
                              (model "llama2")
                              (timeout 300))
  "Create an Ollama provider instance."
  (make-ollama-provider-internal base-url model timeout))

(define (check-ollama-server provider)
  "Check if Ollama server is accessible."
  (let ((url (format #f "~a/api/tags" 
                     (ollama-provider-base-url provider))))
    (check-url-accessible url)))

;;;; Core API Functions

(define* (ollama-generate provider prompt #:key
                         (model #f)
                         (temperature 0.7)
                         (max-tokens #f)
                         (stream #f)
                         (system #f)
                         (template #f)
                         (context #f)
                         (options '()))
  "Generate a response using Ollama's generate endpoint."
  
  (let* ((url (format #f "~a/api/generate" 
                      (ollama-provider-base-url provider)))
         (request-data `((model . ,(or model 
                                       (ollama-provider-default-model provider)))
                        (prompt . ,prompt)
                        ,@(if system `((system . ,system)) '())
                        ,@(if template `((template . ,template)) '())
                        ,@(if context `((context . ,context)) '())
                        (stream . ,stream)
                        (options . ,(append
                                    `((temperature . ,temperature))
                                    (if max-tokens 
                                        `((num_predict . ,max-tokens))
                                        '())
                                    options)))))
    
    (if stream
        ;; Streaming response
        (let ((chunks '()))
          (http-stream url request-data
                      (lambda (line)
                        (let ((chunk (json->scm line)))
                          (set! chunks (cons chunk chunks))
                          (when (assoc-ref chunk 'response)
                            (display (assoc-ref chunk 'response)))))
                      #:timeout (ollama-provider-timeout provider))
          (reverse chunks))
        ;; Regular response
        (let ((response (http-post url request-data
                                  'timeout (ollama-provider-timeout provider))))
          (assoc-ref response 'response)))))

(define* (ollama-chat provider messages #:key
                     (model #f)
                     (temperature 0.7)
                     (max-tokens #f)
                     (stream #f)
                     (options '()))
  "Chat with Ollama using the chat endpoint."
  
  (let* ((url (format #f "~a/api/chat" 
                      (ollama-provider-base-url provider)))
         (request-data `((model . ,(or model 
                                       (ollama-provider-default-model provider)))
                        (messages . ,(map normalize-message messages))
                        (stream . ,stream)
                        (options . ,(append
                                    `((temperature . ,temperature))
                                    (if max-tokens 
                                        `((num_predict . ,max-tokens))
                                        '())
                                    options)))))
    
    (if stream
        ;; Streaming chat response
        (let ((chunks '())
              (full-response ""))
          (http-stream url request-data
                      (lambda (line)
                        (let ((chunk (json->scm line)))
                          (set! chunks (cons chunk chunks))
                          (let ((content (and=> (assoc-ref chunk 'message)
                                               (lambda (msg)
                                                 (assoc-ref msg 'content)))))
                            (when content
                              (set! full-response 
                                    (string-append full-response content))
                              (display content)))))
                      #:timeout (ollama-provider-timeout provider))
          (list (cons 'response full-response)
                (cons 'chunks (reverse chunks))))
        ;; Regular chat response
        (let ((response (http-post url request-data
                                  'timeout (ollama-provider-timeout provider))))
          (and=> (assoc-ref response 'message)
                 (lambda (msg) (assoc-ref msg 'content)))))))

(define (normalize-message msg)
  "Normalize message format for Ollama API."
  (cond
   ;; Already in correct format
   ((and (list? msg) (assoc-ref msg 'role))
    msg)
   ;; Simple (role . content) pair
   ((and (pair? msg) (symbol? (car msg)) (string? (cdr msg)))
    `((role . ,(symbol->string (car msg)))
      (content . ,(cdr msg))))
   ;; List format (role content)
   ((and (list? msg) (= (length msg) 2))
    `((role . ,(symbol->string (car msg)))
      (content . ,(cadr msg))))
   (else
    (error "Invalid message format" msg))))

(define* (ollama-embeddings provider text #:key
                           (model #f))
  "Generate embeddings for text using Ollama."
  
  (let* ((url (format #f "~a/api/embeddings" 
                      (ollama-provider-base-url provider)))
         (request-data `((model . ,(or model 
                                       (ollama-provider-default-model provider)))
                        (prompt . ,text)))
         (response (http-post url request-data
                             'timeout (ollama-provider-timeout provider))))
    (assoc-ref response 'embedding)))

;;;; Model Management

(define (ollama-list-models provider)
  "List available models on the Ollama server."
  
  (let* ((url (format #f "~a/api/tags" 
                      (ollama-provider-base-url provider)))
         (response (http-get url 'json? #t)))
    (assoc-ref response 'models)))

(define (ollama-model-info provider model)
  "Get detailed information about a specific model."
  
  (let* ((url (format #f "~a/api/show" 
                      (ollama-provider-base-url provider)))
         (request-data `((name . ,model)))
         (response (http-post url request-data)))
    response))

(define* (ollama-pull-model provider model #:key
                           (stream #t))
  "Pull a model from the Ollama registry."
  
  (let* ((url (format #f "~a/api/pull" 
                      (ollama-provider-base-url provider)))
         (request-data `((name . ,model)
                        (stream . ,stream))))
    
    (if stream
        (http-stream url request-data
                    (lambda (line)
                      (let ((chunk (json->scm line)))
                        (when (assoc-ref chunk 'status)
                          (format #t "~a\n" (assoc-ref chunk 'status)))))
                    #:timeout 3600) ; 1 hour timeout for model downloads
        (http-post url request-data 'timeout 3600))))

;;;; Convenience Functions

(define* (ollama-complete provider prompt #:key (model #f))
  "Simple completion function for quick use."
  (ollama-generate provider prompt #:model model #:stream #f))

(define (ollama-stream-complete provider prompt on-token)
  "Stream completion with custom token handler."
  (http-stream 
   (format #f "~a/api/generate" (ollama-provider-base-url provider))
   `((model . ,(ollama-provider-default-model provider))
     (prompt . ,prompt)
     (stream . #t))
   (lambda (line)
     (let ((chunk (json->scm line)))
       (when (assoc-ref chunk 'response)
         (on-token (assoc-ref chunk 'response)))))))

;;; ollama.scm ends here