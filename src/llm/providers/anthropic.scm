;;; anthropic.scm --- Anthropic Claude API provider implementation

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm providers anthropic)
  #:use-module (llm utils http)
  #:use-module (llm utils json)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-anthropic-provider
            anthropic-generate
            anthropic-chat
            anthropic-list-models
            check-anthropic-api
            *default-anthropic-url*
            *anthropic-api-version*))

;;; Commentary:
;;;
;;; This module provides a complete Anthropic Claude API client implementation.
;;; It supports the Anthropic Messages API for chat completions with proper
;;; authentication and streaming support.
;;;
;;; Code:

(define *default-anthropic-url* "https://api.anthropic.com/v1")
(define *anthropic-api-version* "2023-06-01")

(define-record-type <anthropic-provider>
  (make-anthropic-provider-internal base-url api-key default-model timeout api-version)
  anthropic-provider?
  (base-url anthropic-provider-base-url)
  (api-key anthropic-provider-api-key)
  (default-model anthropic-provider-default-model set-anthropic-provider-default-model!)
  (timeout anthropic-provider-timeout)
  (api-version anthropic-provider-api-version))

(define* (make-anthropic-provider #:key
                                 (base-url *default-anthropic-url*)
                                 (api-key #f)
                                 (model "claude-sonnet-4-20250514")
                                 (timeout 300)
                                 (api-version *anthropic-api-version*))
  "Create an Anthropic provider instance.

   Required:
   - api-key: Anthropic API key for authentication

   Optional:
   - base-url: API base URL (default: https://api.anthropic.com/v1)
   - model: Default model to use (default: claude-sonnet-4-20250514)
   - timeout: Request timeout in seconds (default: 300)
   - api-version: Anthropic API version (default: 2023-06-01)"

  (unless api-key
    (error "Anthropic API key is required"))

  (make-anthropic-provider-internal base-url api-key model timeout api-version))

(define (check-anthropic-api provider)
  "Check if Anthropic API is accessible with the given credentials.
   Makes a minimal request to verify authentication."
  (let ((url (format #f "~a/messages"
                     (anthropic-provider-base-url provider))))
    (catch #t
      (lambda ()
        ;; Anthropic doesn't have a /models endpoint, so we make a minimal request
        (let* ((request-data `((model . ,(anthropic-provider-default-model provider))
                              (max_tokens . 1)
                              (messages . (((role . "user") (content . "Hi"))))))
               (response (http-post url request-data
                                   'headers (make-auth-headers provider)
                                   'timeout 10)))
          (and response (assoc-ref response 'content))))
      (lambda (key . args) #f))))

(define (make-auth-headers provider)
  "Create authentication headers for Anthropic API requests."
  `(("x-api-key" . ,(anthropic-provider-api-key provider))
    ("anthropic-version" . ,(anthropic-provider-api-version provider))
    ("Content-Type" . "application/json")))

;;;; Core API Functions

(define* (anthropic-generate provider prompt #:key
                            (model #f)
                            (temperature 1.0)
                            (max-tokens 1024)
                            (stream #f)
                            (system #f)
                            (top-p #f)
                            (top-k #f)
                            (stop-sequences #f))
  "Generate a response using Anthropic's Messages endpoint.
   This converts a prompt to chat format for compatibility."

  (let ((messages `(((role . "user") (content . ,prompt)))))
    (anthropic-chat provider messages
                   #:model model
                   #:temperature temperature
                   #:max-tokens max-tokens
                   #:stream stream
                   #:system system
                   #:top-p top-p
                   #:top-k top-k
                   #:stop-sequences stop-sequences)))

(define* (anthropic-chat provider messages #:key
                        (model #f)
                        (temperature 1.0)
                        (max-tokens 1024)
                        (stream #f)
                        (system #f)
                        (top-p #f)
                        (top-k #f)
                        (stop-sequences #f))
  "Chat with Anthropic Claude using the Messages endpoint."

  (let* ((url (format #f "~a/messages"
                      (anthropic-provider-base-url provider)))
         (request-data `((model . ,(or model
                                       (anthropic-provider-default-model provider)))
                        (messages . ,(map normalize-anthropic-message messages))
                        (max_tokens . ,max-tokens)
                        (stream . ,stream)
                        ,@(if system `((system . ,system)) '())
                        ,@(if temperature `((temperature . ,temperature)) '())
                        ,@(if top-p `((top_p . ,top-p)) '())
                        ,@(if top-k `((top_k . ,top-k)) '())
                        ,@(if stop-sequences `((stop_sequences . ,stop-sequences)) '()))))

    (if stream
        ;; Streaming chat response
        (let ((chunks '())
              (full-response ""))
          (http-stream url request-data
                      (lambda (line)
                        (when (and (not (string-null? line))
                                  (string-prefix? "data: " line))
                          (let ((json-data (substring line 6)))
                            (catch #t
                              (lambda ()
                                (let* ((event (json->scm json-data))
                                       (event-type (assoc-ref event 'type)))
                                  (set! chunks (cons event chunks))
                                  (cond
                                   ;; Content block delta - extract text
                                   ((string=? event-type "content_block_delta")
                                    (let* ((delta (assoc-ref event 'delta))
                                           (text (and delta (assoc-ref delta 'text))))
                                      (when text
                                        (set! full-response
                                              (string-append full-response text))
                                        (display text))))
                                   ;; Message stop - done
                                   ((string=? event-type "message_stop")
                                    #t))))
                              (lambda (key . args)
                                ;; Ignore parsing errors for streaming
                                #f)))))
                      #:headers (make-auth-headers provider)
                      #:timeout (anthropic-provider-timeout provider))
          (list (cons 'response full-response)
                (cons 'chunks (reverse chunks))))
        ;; Regular chat response
        (let ((response (http-post url request-data
                                  'headers (make-auth-headers provider)
                                  'timeout (anthropic-provider-timeout provider))))
          (let* ((content (assoc-ref response 'content))
                 (first-block (and content (not (null? content)) (car content))))
            (and first-block (assoc-ref first-block 'text)))))))

(define (normalize-anthropic-message msg)
  "Normalize message format for Anthropic API.
   Anthropic only supports 'user' and 'assistant' roles in messages.
   System messages should be passed separately via the system parameter."
  (cond
   ;; Already in correct format with role
   ((and (list? msg) (assoc-ref msg 'role))
    (let ((role (assoc-ref msg 'role))
          (content (assoc-ref msg 'content)))
      ;; Skip system messages (should be passed via #:system)
      (if (or (string=? role "system")
              (and (symbol? role) (eq? role 'system)))
          #f
          `((role . ,(if (symbol? role) (symbol->string role) role))
            (content . ,content)))))
   ;; Simple (role . content) pair
   ((and (pair? msg) (symbol? (car msg)) (string? (cdr msg)))
    (if (eq? (car msg) 'system)
        #f  ; Skip system messages
        `((role . ,(symbol->string (car msg)))
          (content . ,(cdr msg)))))
   ;; List format (role content)
   ((and (list? msg) (= (length msg) 2))
    (if (eq? (car msg) 'system)
        #f  ; Skip system messages
        `((role . ,(symbol->string (car msg)))
          (content . ,(cadr msg)))))
   (else
    (error "Invalid message format" msg))))

;;;; Model Management

(define *anthropic-models*
  '("claude-opus-4-20250514"
    "claude-sonnet-4-20250514"
    "claude-3-5-sonnet-20241022"
    "claude-3-5-haiku-20241022"
    "claude-3-opus-20240229"
    "claude-3-sonnet-20240229"
    "claude-3-haiku-20240307"))

(define (anthropic-list-models provider)
  "List available Anthropic models.
   Note: Anthropic doesn't have a public models endpoint,
   so we return a static list of known models."
  (map (lambda (model)
         `((id . ,model)))
       *anthropic-models*))

;;;; Convenience Functions

(define* (anthropic-complete provider prompt #:key (model #f) (system #f))
  "Simple completion function for quick use."
  (anthropic-generate provider prompt #:model model #:system system #:stream #f))

(define (anthropic-stream-complete provider prompt on-token)
  "Stream completion with custom token handler."
  (http-stream
   (format #f "~a/messages" (anthropic-provider-base-url provider))
   `((model . ,(anthropic-provider-default-model provider))
     (max_tokens . 1024)
     (messages . (((role . "user") (content . ,prompt))))
     (stream . #t))
   (lambda (line)
     (when (and (not (string-null? line))
               (string-prefix? "data: " line))
       (let ((json-data (substring line 6)))
         (catch #t
           (lambda ()
             (let* ((event (json->scm json-data))
                    (event-type (assoc-ref event 'type)))
               (when (string=? event-type "content_block_delta")
                 (let* ((delta (assoc-ref event 'delta))
                        (text (and delta (assoc-ref delta 'text))))
                   (when text
                     (on-token text))))))
           (lambda (key . args) #f)))))
   #:headers (make-auth-headers provider)))

;;; anthropic.scm ends here
