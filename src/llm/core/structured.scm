;;; structured.scm --- Structured output and JSON mode support

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm core structured)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (llm utils json)
  #:use-module (llm core contracts)
  #:export (;; Response format
            json-mode
            structured-output
            response-format->provider

            ;; Schema definition
            define-schema

            ;; Validation
            validate-response
            parse-json-response

            ;; Record generation
            schema->record-type

            ;; Common schemas
            string-schema
            number-schema
            boolean-schema
            array-schema
            object-schema
            enum-schema))

;; Re-export from contracts for convenience
(define schema->contract (@ (llm core contracts) json-schema/c))

;;; Commentary:
;;;
;;; This module provides structured output support for LLM responses.
;;; It enables reliable JSON responses with schema validation and
;;; automatic Scheme record generation.
;;;
;;; OpenAI-style response_format is supported:
;;;   - {"type": "json_object"} for basic JSON mode
;;;   - {"type": "json_schema", "json_schema": {...}} for strict schemas
;;;
;;; Code:

;;; --------------------------------------------------------------------
;;; Response Format Types
;;; --------------------------------------------------------------------

(define (json-mode)
  "Create a basic JSON mode response format.

   This instructs the LLM to output valid JSON but without
   schema constraints."
  '((type . "json_object")))

(define* (structured-output schema #:key (name "response") (strict #t))
  "Create a structured output response format with JSON schema.

   schema: A JSON schema specification
   name: Name for the schema (default: \"response\")
   strict: Whether to enforce strict mode (default: #t)"
  `((type . "json_schema")
    (json_schema . ((name . ,name)
                    (strict . ,strict)
                    (schema . ,schema)))))

(define* (response-format->provider format #:key (provider 'openai))
  "Convert response format to provider-specific format.

   Currently OpenAI and compatible APIs are supported."
  (case provider
    ((openai)
     `((response_format . ,format)))
    ((anthropic)
     ;; Anthropic doesn't have native JSON mode, but we can instruct
     ;; via system prompt and parse the response
     (if (equal? (assoc-ref format 'type) "json_object")
         '()  ; No special format, handle via prompting
         '()))
    ((ollama)
     ;; Ollama supports format: "json"
     (if (equal? (assoc-ref format 'type) "json_object")
         '((format . "json"))
         '()))
    (else format)))

;;; --------------------------------------------------------------------
;;; Schema Definition Helpers
;;; --------------------------------------------------------------------

(define (string-schema)
  "Create a string schema."
  '((type . "string")))

(define* (number-schema #:key minimum maximum)
  "Create a number schema with optional bounds."
  (let ((base '((type . "number"))))
    (cond
     ((and minimum maximum)
      (append base `((minimum . ,minimum) (maximum . ,maximum))))
     (minimum (append base `((minimum . ,minimum))))
     (maximum (append base `((maximum . ,maximum))))
     (else base))))

(define (boolean-schema)
  "Create a boolean schema."
  '((type . "boolean")))

(define* (array-schema items #:key min-items max-items)
  "Create an array schema with item type."
  (let ((base `((type . "array")
                (items . ,items))))
    (cond
     ((and min-items max-items)
      (append base `((minItems . ,min-items) (maxItems . ,max-items))))
     (min-items (append base `((minItems . ,min-items))))
     (max-items (append base `((maxItems . ,max-items))))
     (else base))))

(define* (object-schema properties #:key (required #f) (additional-properties #f))
  "Create an object schema with properties.

   properties: alist of (name . schema) pairs
   required: list of required property names (default: all properties)
   additional-properties: allow unlisted properties (default: #f)"
  (let* ((prop-names (map car properties))
         (req (or required prop-names)))
    `((type . "object")
      (properties . ,properties)
      (required . ,(list->vector req))
      (additionalProperties . ,additional-properties))))

(define (enum-schema values)
  "Create an enum schema from a list of allowed values."
  `((enum . ,values)))

;;; --------------------------------------------------------------------
;;; Schema Macro
;;; --------------------------------------------------------------------

(define-syntax define-schema
  (syntax-rules (: string number boolean array object enum optional)
    "Define a named JSON schema.

     Usage:
       (define-schema person
         (object
           (name : string)
           (age : number)
           (active : boolean optional)))

     Generates a function that returns the schema."

    ;; Base case - object with fields
    ((define-schema name (object fields ...))
     (define (name)
       (parse-object-fields (list fields ...))))

    ;; Simple schema binding
    ((define-schema name schema-expr)
     (define (name) schema-expr))))

(define (parse-object-fields fields)
  "Parse field definitions into an object schema."
  (let loop ((fields fields)
             (properties '())
             (required '()))
    (if (null? fields)
        (object-schema (reverse properties)
                      #:required (reverse required))
        (let ((field (car fields)))
          (match field
            ((fname ': ftype)
             (loop (cdr fields)
                   (cons (cons fname (type->schema ftype)) properties)
                   (cons (symbol->string fname) required)))
            ((fname ': ftype 'optional)
             (loop (cdr fields)
                   (cons (cons fname (type->schema ftype)) properties)
                   required))
            (_
             (error "Invalid field definition" field)))))))

(define (type->schema type)
  "Convert type symbol to schema."
  (case type
    ((string) (string-schema))
    ((number) (number-schema))
    ((boolean) (boolean-schema))
    (else (error "Unknown type" type))))

;;; --------------------------------------------------------------------
;;; Schema to Contract Conversion
;;; --------------------------------------------------------------------

;; Note: schema->contract is defined at module top as an alias to
;; json-schema/c from (llm core contracts)

;;; --------------------------------------------------------------------
;;; Response Validation
;;; --------------------------------------------------------------------

(define* (parse-json-response response #:key (schema #f))
  "Parse a JSON response string and optionally validate against schema.

   Returns: (ok . parsed-data) or (error . message)"
  (catch #t
    (lambda ()
      (let ((parsed (json->scm response)))
        (if (and schema (not (validate-against-schema parsed schema)))
            (cons 'error "Response does not match schema")
            (cons 'ok parsed))))
    (lambda (key . args)
      (cons 'error (format #f "JSON parse error: ~a" key)))))

(define (validate-against-schema data schema)
  "Validate data against a JSON schema. Returns #t if valid."
  (let ((contract (schema->contract schema)))
    (catch 'contract-violation
      (lambda ()
        (contract-check contract data)
        #t)
      (lambda (key . args)
        #f))))

(define* (validate-response response schema #:key (on-error 'throw))
  "Validate an LLM response against a schema.

   on-error: 'throw (default), 'return-error, or a handler procedure

   Returns the parsed response if valid."
  (let ((result (parse-json-response response #:schema schema)))
    (case (car result)
      ((ok) (cdr result))
      ((error)
       (case on-error
         ((throw) (throw 'validation-error (cdr result)))
         ((return-error) result)
         (else
          (if (procedure? on-error)
              (on-error (cdr result))
              (throw 'validation-error (cdr result)))))))))

;;; --------------------------------------------------------------------
;;; Record Type Generation
;;; --------------------------------------------------------------------

(define (schema->record-type schema name)
  "Generate a record type definition string from a schema.

   This returns Scheme code as a string that can be eval'd or
   written to a file."
  (let* ((props (assoc-ref schema 'properties))
         (field-names (if props (map car props) '())))
    (format #f "(define-record-type <~a>
  (make-~a ~{~a~^ ~})
  ~a?
~{  (~a ~a)~^~%~})"
            name
            name
            field-names
            name
            (apply append (map (lambda (f) (list f f)) field-names)))))

;;; structured.scm ends here
