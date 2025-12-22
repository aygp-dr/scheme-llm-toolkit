;;; openai.scm --- OpenAI API provider implementation

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm providers openai)
  #:use-module (llm utils http)
  #:use-module (llm utils json)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-openai-provider
            openai-generate
            openai-chat
            openai-embeddings
            openai-list-models
            openai-model-info
            check-openai-api
            *default-openai-url*))

;;; Commentary:
;;;
;;; This module provides a complete OpenAI API client implementation.
;;; It supports the OpenAI chat completions endpoint, embeddings,
;;; and model management with proper authentication.
;;;
;;; Code:

(define *default-openai-url* "https://api.openai.com/v1")

(define-record-type <openai-provider>
  (make-openai-provider-internal base-url api-key default-model timeout)
  openai-provider?
  (base-url openai-provider-base-url)
  (api-key openai-provider-api-key)
  (default-model openai-provider-default-model set-openai-provider-default-model!)
  (timeout openai-provider-timeout))

(define* (make-openai-provider #:key 
                              (base-url *default-openai-url*)
                              (api-key #f)
                              (model "gpt-3.5-turbo")
                              (timeout 300))
  "Create an OpenAI provider instance.
   
   Required:
   - api-key: OpenAI API key for authentication
   
   Optional:
   - base-url: API base URL (default: https://api.openai.com/v1)
   - model: Default model to use (default: gpt-3.5-turbo)
   - timeout: Request timeout in seconds (default: 300)"
  
  (unless api-key
    (error "OpenAI API key is required"))
  
  (make-openai-provider-internal base-url api-key model timeout))

(define (check-openai-api provider)
  "Check if OpenAI API is accessible with the given credentials."
  (let ((url (format #f "~a/models" 
                     (openai-provider-base-url provider))))
    (catch #t
      (lambda ()
        (let ((response (http-get url 
                                 'headers `(("Authorization" . ,(format #f "Bearer ~a" 
                                                                       (openai-provider-api-key provider))))
                                 'json? #t
                                 'timeout 10)))
          (and response (assoc-ref response 'data))))
      (lambda (key . args) #f))))

(define (make-auth-headers provider)
  "Create authentication headers for OpenAI API requests."
  `(("Authorization" . ,(format #f "Bearer ~a" (openai-provider-api-key provider)))
    ("Content-Type" . "application/json")))

;;;; Core API Functions

(define* (openai-generate provider prompt #:key
                         (model #f)
                         (temperature 0.7)
                         (max-tokens #f)
                         (stream #f)
                         (system #f)
                         (top-p #f)
                         (frequency-penalty #f)
                         (presence-penalty #f)
                         (stop #f))
  "Generate a response using OpenAI's chat completions endpoint.
   This converts a prompt to chat format for compatibility."
  
  (let ((messages (if system
                      `(((role . "system") (content . ,system))
                        ((role . "user") (content . ,prompt)))
                      `(((role . "user") (content . ,prompt))))))
    
    (openai-chat provider messages
                 #:model model
                 #:temperature temperature
                 #:max-tokens max-tokens
                 #:stream stream
                 #:top-p top-p
                 #:frequency-penalty frequency-penalty
                 #:presence-penalty presence-penalty
                 #:stop stop)))

(define* (openai-chat provider messages #:key
                     (model #f)
                     (temperature 0.7)
                     (max-tokens #f)
                     (stream #f)
                     (top-p #f)
                     (frequency-penalty #f)
                     (presence-penalty #f)
                     (stop #f)
                     (tools #f)
                     (tool-choice #f))
  "Chat with OpenAI using the chat completions endpoint.

   Optional tools parameter accepts a list of tool schemas in OpenAI format.
   Use tools->openai-format from (llm core tools) to convert tool definitions."

  (let* ((url (format #f "~a/chat/completions"
                      (openai-provider-base-url provider)))
         (request-data `((model . ,(or model
                                       (openai-provider-default-model provider)))
                        (messages . ,(map normalize-openai-message messages))
                        (stream . ,stream)
                        ,@(if temperature `((temperature . ,temperature)) '())
                        ,@(if max-tokens `((max_tokens . ,max-tokens)) '())
                        ,@(if top-p `((top_p . ,top-p)) '())
                        ,@(if frequency-penalty `((frequency_penalty . ,frequency-penalty)) '())
                        ,@(if presence-penalty `((presence_penalty . ,presence-penalty)) '())
                        ,@(if stop `((stop . ,stop)) '())
                        ,@(if tools `((tools . ,tools)) '())
                        ,@(if tool-choice `((tool_choice . ,tool-choice)) '()))))
    
    (if stream
        ;; Streaming chat response
        (let ((chunks '())
              (full-response ""))
          (http-stream url request-data
                      (lambda (line)
                        (when (and (not (string-null? line))
                                  (string-prefix? "data: " line))
                          (let ((json-data (substring line 6)))
                            (unless (string=? json-data "[DONE]")
                              (catch #t
                                (lambda ()
                                  (let* ((chunk (json->scm json-data))
                                         (choices (assoc-ref chunk 'choices)))
                                    (set! chunks (cons chunk chunks))
                                    (when (and choices (not (null? choices)))
                                      (let* ((choice (car choices))
                                             (delta (assoc-ref choice 'delta))
                                             (content (and delta (assoc-ref delta 'content))))
                                        (when content
                                          (set! full-response 
                                                (string-append full-response content))
                                          (display content))))))
                                (lambda (key . args)
                                  ;; Ignore parsing errors for streaming
                                  #f))))))
                      #:headers (make-auth-headers provider)
                      #:timeout (openai-provider-timeout provider))
          (list (cons 'response full-response)
                (cons 'chunks (reverse chunks))))
        ;; Regular chat response
        (let ((response (http-post url request-data
                                  'headers (make-auth-headers provider)
                                  'timeout (openai-provider-timeout provider))))
          (let* ((choices (assoc-ref response 'choices))
                 (first-choice (and choices (not (null? choices)) (car choices)))
                 (message (and first-choice (assoc-ref first-choice 'message)))
                 (tool-calls (and message (assoc-ref message 'tool_calls))))
            ;; If tool calls present, return full message structure
            (if tool-calls
                `((role . "assistant")
                  (content . ,(assoc-ref message 'content))
                  (tool_calls . ,tool-calls))
                ;; Otherwise just return content string
                (and message (assoc-ref message 'content))))))))

(define (normalize-openai-message msg)
  "Normalize message format for OpenAI API."
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

(define* (openai-embeddings provider text #:key
                           (model #f))
  "Generate embeddings for text using OpenAI."
  
  (let* ((url (format #f "~a/embeddings" 
                      (openai-provider-base-url provider)))
         (embedding-model (or model "text-embedding-ada-002"))
         (request-data `((model . ,embedding-model)
                        (input . ,text)))
         (response (http-post url request-data
                             'headers (make-auth-headers provider)
                             'timeout (openai-provider-timeout provider))))
    (let* ((data (assoc-ref response 'data))
           (first-embedding (and data (not (null? data)) (car data))))
      (and first-embedding (assoc-ref first-embedding 'embedding)))))

;;;; Model Management

(define (openai-list-models provider)
  "List available models from the OpenAI API."
  
  (let* ((url (format #f "~a/models" 
                      (openai-provider-base-url provider)))
         (response (http-get url 
                            'headers (make-auth-headers provider)
                            'json? #t
                            'timeout (openai-provider-timeout provider))))
    (assoc-ref response 'data)))

(define (openai-model-info provider model)
  "Get detailed information about a specific model."
  
  (let* ((url (format #f "~a/models/~a" 
                      (openai-provider-base-url provider)
                      model))
         (response (http-get url
                            'headers (make-auth-headers provider)
                            'json? #t
                            'timeout (openai-provider-timeout provider))))
    response))

;;;; Convenience Functions

(define* (openai-complete provider prompt #:key (model #f))
  "Simple completion function for quick use."
  (openai-generate provider prompt #:model model #:stream #f))

(define (openai-stream-complete provider prompt on-token)
  "Stream completion with custom token handler."
  (http-stream 
   (format #f "~a/chat/completions" (openai-provider-base-url provider))
   `((model . ,(openai-provider-default-model provider))
     (messages . (((role . "user") (content . ,prompt))))
     (stream . #t))
   (lambda (line)
     (when (and (not (string-null? line))
               (string-prefix? "data: " line))
       (let ((json-data (substring line 6)))
         (unless (string=? json-data "[DONE]")
           (catch #t
             (lambda ()
               (let* ((chunk (json->scm json-data))
                      (choices (assoc-ref chunk 'choices)))
                 (when (and choices (not (null? choices)))
                   (let* ((choice (car choices))
                          (delta (assoc-ref choice 'delta))
                          (content (and delta (assoc-ref delta 'content))))
                     (when content
                       (on-token content))))))
             (lambda (key . args) #f))))))
   #:headers (make-auth-headers provider)))

;;; openai.scm ends here