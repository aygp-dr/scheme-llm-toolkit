;;; tools.scm --- Function calling / tool use abstraction

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm core tools)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (<tool>
            make-tool
            tool?
            tool-name
            tool-description
            tool-parameters
            tool-handler

            <tool-call>
            make-tool-call
            tool-call?
            tool-call-id
            tool-call-name
            tool-call-arguments

            <tool-result>
            make-tool-result
            tool-result?
            tool-result-id
            tool-result-content
            tool-result-error?

            ;; Conversion functions
            tool->openai-schema
            tool->anthropic-schema
            tools->openai-format
            tools->anthropic-format

            ;; Tool execution
            execute-tool
            execute-tool-calls

            ;; Convenience macro
            define-tool))

;;; Commentary:
;;;
;;; This module provides abstractions for LLM function calling / tool use.
;;; It allows defining tools that can be sent to LLM providers and executed
;;; when the LLM requests them.
;;;
;;; Code:

;;;; Tool Definition

(define-record-type <tool>
  (make-tool-internal name description parameters handler)
  tool?
  (name tool-name)
  (description tool-description)
  (parameters tool-parameters)      ; JSON schema for parameters
  (handler tool-handler))           ; Scheme function to execute

(define* (make-tool #:key name description parameters (handler #f))
  "Create a tool definition.

   Required:
   - name: Tool name (string or symbol)
   - description: Description of what the tool does
   - parameters: JSON schema describing tool parameters

   Optional:
   - handler: Scheme function to execute when tool is called"
  (unless name
    (error "Tool name is required"))
  (unless description
    (error "Tool description is required"))

  (make-tool-internal
   (if (symbol? name) (symbol->string name) name)
   description
   (or parameters '())
   handler))

;;;; Tool Call Representation

(define-record-type <tool-call>
  (make-tool-call-internal id name arguments)
  tool-call?
  (id tool-call-id)
  (name tool-call-name)
  (arguments tool-call-arguments))

(define* (make-tool-call #:key id name arguments)
  "Create a tool call representation (from LLM response).

   - id: Unique identifier for this call
   - name: Name of the tool being called
   - arguments: Parsed arguments (alist)"
  (make-tool-call-internal id name (or arguments '())))

;;;; Tool Result Representation

(define-record-type <tool-result>
  (make-tool-result-internal id content error?)
  tool-result?
  (id tool-result-id)
  (content tool-result-content)
  (error? tool-result-error?))

(define* (make-tool-result #:key id content (error? #f))
  "Create a tool execution result.

   - id: The tool call ID this result corresponds to
   - content: Result content (string or structured data)
   - error?: Whether this represents an error"
  (make-tool-result-internal id content error?))

;;;; Provider-Specific Schema Conversion

(define (tool->openai-schema tool)
  "Convert a tool to OpenAI function calling format."
  `((type . "function")
    (function . ((name . ,(tool-name tool))
                 (description . ,(tool-description tool))
                 (parameters . ,(if (null? (tool-parameters tool))
                                   `((type . "object")
                                     (properties . ()))
                                   (tool-parameters tool)))))))

(define (tool->anthropic-schema tool)
  "Convert a tool to Anthropic tool use format."
  `((name . ,(tool-name tool))
    (description . ,(tool-description tool))
    (input_schema . ,(if (null? (tool-parameters tool))
                        `((type . "object")
                          (properties . ()))
                        (tool-parameters tool)))))

(define (tools->openai-format tools)
  "Convert a list of tools to OpenAI format."
  (map tool->openai-schema tools))

(define (tools->anthropic-format tools)
  "Convert a list of tools to Anthropic format."
  (map tool->anthropic-schema tools))

;;;; Tool Execution

(define (execute-tool tool arguments)
  "Execute a tool with the given arguments.
   Returns a tool-result."
  (let ((handler (tool-handler tool)))
    (if handler
        (catch #t
          (lambda ()
            (let ((result (apply handler
                                (map (lambda (pair)
                                       (cdr pair))
                                     arguments))))
              (make-tool-result
               #:id #f
               #:content (if (string? result)
                            result
                            (format #f "~a" result)))))
          (lambda (key . args)
            (make-tool-result
             #:id #f
             #:content (format #f "Error executing tool: ~a ~a" key args)
             #:error? #t)))
        (make-tool-result
         #:id #f
         #:content "No handler defined for this tool"
         #:error? #t))))

(define (execute-tool-calls tools tool-calls)
  "Execute a list of tool calls and return results.

   - tools: List of tool definitions
   - tool-calls: List of tool-call records

   Returns: List of tool-result records"
  (map (lambda (call)
         (let ((tool (find (lambda (t)
                            (string=? (tool-name t)
                                     (tool-call-name call)))
                          tools)))
           (if tool
               (let ((result (execute-tool tool (tool-call-arguments call))))
                 (make-tool-result
                  #:id (tool-call-id call)
                  #:content (tool-result-content result)
                  #:error? (tool-result-error? result)))
               (make-tool-result
                #:id (tool-call-id call)
                #:content (format #f "Unknown tool: ~a" (tool-call-name call))
                #:error? #t))))
       tool-calls))

;;;; Convenience Macro

(define-syntax define-tool
  (syntax-rules ()
    "Define a tool with a handler function.

     Usage:
     (define-tool tool-name
       \"Description of the tool\"
       ((param1 . type1) (param2 . type2))
       (lambda (param1 param2)
         ...body...))

     Or without parameters:
     (define-tool tool-name
       \"Description\"
       ()
       (lambda () ...body...))"
    ((_ name description params handler)
     (define name
       (make-tool
        #:name 'name
        #:description description
        #:parameters (params->json-schema 'params)
        #:handler handler)))))

(define (params->json-schema params)
  "Convert simple parameter list to JSON schema."
  (if (null? params)
      `((type . "object")
        (properties . ())
        (required . ()))
      `((type . "object")
        (properties . ,(map (lambda (param)
                             (let ((pname (car param))
                                   (ptype (cdr param)))
                               (cons pname
                                     `((type . ,(type->json-type ptype))
                                       (description . ,(format #f "The ~a parameter" pname))))))
                           params))
        (required . ,(map (lambda (p) (symbol->string (car p))) params)))))

(define (type->json-type type)
  "Convert Scheme type symbol to JSON schema type."
  (case type
    ((string str) "string")
    ((number num int integer) "number")
    ((boolean bool) "boolean")
    ((array list) "array")
    ((object alist) "object")
    (else "string")))

;;; tools.scm ends here
