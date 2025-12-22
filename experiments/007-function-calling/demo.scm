#!/usr/bin/env -S guile -L ../../src -e main -s
!#
;;; demo.scm --- Function calling demonstration

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(use-modules (llm core tools)
             (llm providers openai)
             (ice-9 format))

;;; Define some example tools

(define get-weather
  (make-tool
   #:name "get_weather"
   #:description "Get the current weather for a specified location"
   #:parameters `((type . "object")
                 (properties . ((location . ((type . "string")
                                            (description . "City and country, e.g. Paris, France")))
                               (unit . ((type . "string")
                                       (enum . ("celsius" "fahrenheit"))
                                       (description . "Temperature unit")))))
                 (required . ("location")))
   #:handler (lambda (location unit)
              (format #f "Weather in ~a: 22°~a, sunny with light clouds"
                      location
                      (if (string=? unit "fahrenheit") "F" "C")))))

(define get-time
  (make-tool
   #:name "get_current_time"
   #:description "Get the current time in a specified timezone"
   #:parameters `((type . "object")
                 (properties . ((timezone . ((type . "string")
                                            (description . "IANA timezone, e.g. Europe/London")))))
                 (required . ("timezone")))
   #:handler (lambda (timezone)
              (format #f "Current time in ~a: 14:30" timezone))))

(define calculate
  (make-tool
   #:name "calculate"
   #:description "Perform a mathematical calculation"
   #:parameters `((type . "object")
                 (properties . ((expression . ((type . "string")
                                              (description . "Mathematical expression to evaluate")))))
                 (required . ("expression")))
   #:handler (lambda (expression)
              ;; Simple evaluation (in real code, use a proper parser)
              (format #f "Result: ~a = 42" expression))))

(define all-tools (list get-weather get-time calculate))

;;; Demo function

(define (demo-function-calling api-key)
  "Demonstrate function calling with OpenAI"

  (let ((provider (make-openai-provider #:api-key api-key
                                       #:model "gpt-4o-mini")))

    (format #t "~%=== Function Calling Demo ===~%~%")

    ;; Show available tools
    (format #t "Available tools:~%")
    (for-each (lambda (tool)
                (format #t "  - ~a: ~a~%"
                        (tool-name tool)
                        (tool-description tool)))
              all-tools)

    ;; Make a request with tools
    (format #t "~%User: What's the weather in Tokyo and what time is it there?~%~%")

    (let* ((tools (tools->openai-format all-tools))
           (response (openai-chat provider
                                 '(((role . "user")
                                    (content . "What's the weather in Tokyo and what time is it there?")))
                                 #:tools tools)))

      (format #t "Response type: ~a~%"
              (if (list? response) "tool_calls" "text"))

      (when (and (list? response) (assoc-ref response 'tool_calls))
        (let ((tool-calls (assoc-ref response 'tool_calls)))
          (format #t "~%Tool calls requested (~a):~%" (length tool-calls))

          ;; Parse and execute each tool call
          (for-each
           (lambda (tc)
             (let* ((tc-id (assoc-ref tc 'id))
                    (func (assoc-ref tc 'function))
                    (name (assoc-ref func 'name))
                    (args-str (assoc-ref func 'arguments)))
               (format #t "~%  Tool: ~a~%" name)
               (format #t "  Arguments: ~a~%" args-str)

               ;; Find and execute the tool
               (let ((tool (find (lambda (t) (string=? (tool-name t) name))
                                all-tools)))
                 (when tool
                   ;; Note: In real code, parse args-str as JSON
                   (format #t "  [Would execute tool handler here]~%")))))
           tool-calls))))))

;;; Main entry point

(define (main args)
  (let ((api-key (getenv "OPENAI_API_KEY")))
    (if api-key
        (demo-function-calling api-key)
        (begin
          (format #t "~%ERROR: OPENAI_API_KEY not set.~%")
          (format #t "Please set your API key:~%")
          (format #t "  export OPENAI_API_KEY=\"your-key-here\"~%~%")))))
