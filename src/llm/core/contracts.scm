;;; contracts.scm --- Design-by-contract system inspired by Racket

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm core contracts)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (;; Contract types
            <contract>
            contract?
            contract-name
            contract-check
            contract-generate
            contract-first-order?

            ;; Flat contracts (predicates)
            flat-contract
            flat-contract-predicate
            and/c or/c not/c

            ;; Function contracts
            ->/c
            ->i/c
            case->/c

            ;; Parametric contracts
            listof/c
            vectorof/c
            hash/c
            cons/c

            ;; Structural contracts
            struct/c
            alist/c

            ;; Blame and enforcement
            contract-violation
            blame
            with-contract-region

            ;; Contract application
            contract-out
            define/contract
            λ/contract

            ;; Standard predicates for LLM domain
            provider?/c
            message?/c
            tool?/c
            response?/c

            ;; OpenAPI-style schema contracts
            schema->contract
            json-schema/c))

;;; Commentary:
;;;
;;; This module implements a contract system inspired by Racket's contract
;;; library, adapted for Guile Scheme and the LLM toolkit domain.
;;;
;;; Philosophy (from Racket):
;;; 1. Contracts are AGREEMENTS between parties (caller/callee)
;;; 2. BLAME identifies who violated the agreement
;;; 3. Contracts can be HIGHER-ORDER (for functions)
;;; 4. Contracts should be COMPOSITIONAL
;;;
;;; Integration with formal methods:
;;; - OpenAPI: Schema definitions map to structural contracts
;;; - Lean4/TLA+: Pre/post conditions as function contracts
;;;
;;; Code:

;;;; ============================================================
;;;; Contract Record Type
;;;; ============================================================

(define-record-type <contract>
  (make-contract-internal name check-proc generate-proc first-order? projection)
  contract?
  (name contract-name)              ; Human-readable name
  (check-proc contract-check-proc)  ; (value -> boolean)
  (generate-proc contract-generate) ; (-> random-value) for testing
  (first-order? contract-first-order?) ; Can be checked immediately?
  (projection contract-projection)) ; For higher-order contracts

(define (contract-check contract value)
  "Check if value satisfies contract."
  ((contract-check-proc contract) value))

;;;; ============================================================
;;;; Blame Tracking
;;;; ============================================================

(define-record-type <blame>
  (make-blame positive negative contract value context)
  blame?
  (positive blame-positive)   ; Who promised (callee/server)
  (negative blame-negative)   ; Who relied (caller/client)
  (contract blame-contract)   ; Which contract was violated
  (value blame-value)         ; What value violated it
  (context blame-context))    ; Stack of contract applications

(define *current-blame-context* (make-parameter '()))

(define-syntax with-contract-region
  (syntax-rules ()
    ((_ name body ...)
     (parameterize ((*current-blame-context*
                     (cons 'name (*current-blame-context*))))
       body ...))))

(define (contract-violation contract value positive negative)
  "Signal a contract violation with blame assignment."
  (let ((blame-info (make-blame positive negative contract value
                                (*current-blame-context*))))
    (error 'contract-violation
           (format #f "Contract violation: ~a~%  Expected: ~a~%  Given: ~a~%  Blame: ~a (promised by ~a)~%  Context: ~a"
                   (contract-name contract)
                   (contract-name contract)
                   (truncate-value value)
                   negative
                   positive
                   (reverse (*current-blame-context*))))))

(define (truncate-value v)
  "Truncate value for error display."
  (let ((s (format #f "~s" v)))
    (if (> (string-length s) 80)
        (string-append (substring s 0 77) "...")
        s)))

;;;; ============================================================
;;;; Flat Contracts (First-Order Predicates)
;;;; ============================================================

(define (flat-contract pred #:name (name #f))
  "Create a flat contract from a predicate."
  (make-contract-internal
   (or name (format #f "~a" pred))
   pred
   (lambda () (error "No generator for flat contract"))
   #t    ; first-order
   #f))  ; no projection needed

(define (flat-contract-predicate contract)
  "Extract predicate from flat contract."
  (contract-check-proc contract))

;;; Contract Combinators

(define (and/c . contracts)
  "Conjunction of contracts - value must satisfy ALL."
  (make-contract-internal
   (format #f "(and/c ~{~a~^ ~})" (map contract-name contracts))
   (lambda (v)
     (every (lambda (c) (contract-check c v)) contracts))
   (lambda ()
     ;; Generate from first contract, hope it satisfies rest
     ((contract-generate (car contracts))))
   (every contract-first-order? contracts)
   #f))

(define (or/c . contracts)
  "Disjunction of contracts - value must satisfy ANY."
  (make-contract-internal
   (format #f "(or/c ~{~a~^ ~})" (map contract-name contracts))
   (lambda (v)
     (any (lambda (c) (contract-check c v)) contracts))
   (lambda ()
     ((contract-generate (list-ref contracts (random (length contracts))))))
   (every contract-first-order? contracts)
   #f))

(define (not/c contract)
  "Negation of a contract."
  (make-contract-internal
   (format #f "(not/c ~a)" (contract-name contract))
   (lambda (v) (not (contract-check contract v)))
   (lambda () (error "Cannot generate for not/c"))
   (contract-first-order? contract)
   #f))

;;;; ============================================================
;;;; Function Contracts (Higher-Order)
;;;; ============================================================

(define (->/c . contracts)
  "Function contract: (-> domain ... range).
   Last contract is for return value, rest are for arguments."
  (when (null? contracts)
    (error "-> requires at least a range contract"))
  (let* ((range-contract (last contracts))
         (domain-contracts (drop-right contracts 1))
         (arity (length domain-contracts)))
    (make-contract-internal
     (format #f "(-> ~{~a ~}~a)"
             (map contract-name domain-contracts)
             (contract-name range-contract))
     (lambda (v)
       ;; First-order check: is it a procedure?
       (procedure? v))
     (lambda () (error "Cannot generate functions"))
     #f  ; NOT first-order - requires wrapping
     ;; Projection wraps the function
     (lambda (f positive negative)
       (lambda args
         ;; Check arity
         (unless (= (length args) arity)
           (error 'contract-violation
                  (format #f "Arity mismatch: expected ~a arguments, got ~a"
                          arity (length args))))
         ;; Check domain contracts (blame NEGATIVE - caller)
         (for-each
          (lambda (arg contract idx)
            (unless (contract-check contract arg)
              (contract-violation contract arg negative positive)))
          args domain-contracts (iota arity))
         ;; Call function
         (let ((result (apply f args)))
           ;; Check range contract (blame POSITIVE - callee)
           (unless (contract-check range-contract result)
             (contract-violation range-contract result positive negative))
           result))))))

(define (->i/c . specs)
  "Dependent function contract - arguments can depend on each other.
   Each spec is (name contract) or (name (deps ...) contract-expr)."
  ;; Simplified implementation - full version would eval contract-expr
  (apply ->/c (map (lambda (spec)
                    (if (pair? (cdr spec))
                        (if (list? (cadr spec))
                            (caddr spec)  ; (name (deps) contract)
                            (cadr spec))  ; (name contract)
                        (cdr spec)))
                  specs)))

(define (case->/c . cases)
  "Case function contract - different contracts for different arities."
  (make-contract-internal
   (format #f "(case-> ~{~a~^ ~})" (map contract-name cases))
   (lambda (v) (procedure? v))
   (lambda () (error "Cannot generate case-> functions"))
   #f
   (lambda (f positive negative)
     (lambda args
       ;; Find matching case by arity
       (let ((matching (find (lambda (c)
                              ;; Parse arity from contract name
                              #t) ; Simplified
                            cases)))
         (if matching
             (apply ((contract-projection matching) f positive negative) args)
             (error "No matching case for arity")))))))

;;;; ============================================================
;;;; Parametric Contracts
;;;; ============================================================

(define (listof/c elem-contract)
  "Contract for homogeneous lists."
  (make-contract-internal
   (format #f "(listof ~a)" (contract-name elem-contract))
   (lambda (v)
     (and (list? v)
          (every (lambda (e) (contract-check elem-contract e)) v)))
   (lambda ()
     (map (lambda (_) ((contract-generate elem-contract)))
          (iota (random 10))))
   (contract-first-order? elem-contract)
   #f))

(define (vectorof/c elem-contract)
  "Contract for homogeneous vectors."
  (make-contract-internal
   (format #f "(vectorof ~a)" (contract-name elem-contract))
   (lambda (v)
     (and (vector? v)
          (every (lambda (e) (contract-check elem-contract e))
                 (vector->list v))))
   (lambda ()
     (list->vector ((contract-generate (listof/c elem-contract)))))
   (contract-first-order? elem-contract)
   #f))

(define (hash/c key-contract val-contract)
  "Contract for hash tables."
  (make-contract-internal
   (format #f "(hash/c ~a ~a)"
           (contract-name key-contract)
           (contract-name val-contract))
   (lambda (v)
     (and (hash-table? v)
          ;; Check all entries
          (call/cc
           (lambda (return)
             (hash-for-each
              (lambda (k v)
                (unless (and (contract-check key-contract k)
                            (contract-check val-contract v))
                  (return #f)))
              v)
             #t))))
   (lambda () (make-hash-table))
   #t
   #f))

(define (cons/c car-contract cdr-contract)
  "Contract for pairs."
  (make-contract-internal
   (format #f "(cons/c ~a ~a)"
           (contract-name car-contract)
           (contract-name cdr-contract))
   (lambda (v)
     (and (pair? v)
          (contract-check car-contract (car v))
          (contract-check cdr-contract (cdr v))))
   (lambda ()
     (cons ((contract-generate car-contract))
           ((contract-generate cdr-contract))))
   (and (contract-first-order? car-contract)
        (contract-first-order? cdr-contract))
   #f))

;;;; ============================================================
;;;; Structural Contracts (for alists/records)
;;;; ============================================================

(define* (alist/c #:key (required '()) (optional '()))
  "Contract for association lists with specified fields.

   required: ((field-name . contract) ...)
   optional: ((field-name . contract) ...)"
  (make-contract-internal
   (format #f "(alist/c ~a)"
           (map car (append required optional)))
   (lambda (v)
     (and (list? v)
          ;; All required fields present and valid
          (every (lambda (spec)
                  (let ((field (car spec))
                        (contract (cdr spec)))
                    (let ((entry (assoc field v)))
                      (and entry
                           (contract-check contract (cdr entry))))))
                required)
          ;; Optional fields, if present, are valid
          (every (lambda (spec)
                  (let ((field (car spec))
                        (contract (cdr spec)))
                    (let ((entry (assoc field v)))
                      (or (not entry)
                          (contract-check contract (cdr entry))))))
                optional)))
   (lambda ()
     (append
      (map (lambda (spec)
             (cons (car spec) ((contract-generate (cdr spec)))))
           required)
      (filter identity
              (map (lambda (spec)
                     (if (zero? (random 2))
                         (cons (car spec) ((contract-generate (cdr spec))))
                         #f))
                   optional))))
   #t
   #f))

(define (struct/c type-pred . field-contracts)
  "Contract for SRFI-9 records."
  (make-contract-internal
   (format #f "(struct/c ~a)" type-pred)
   (lambda (v)
     (and (type-pred v)
          ;; Would need accessor functions to check fields
          #t))
   (lambda () (error "Cannot generate structs"))
   #t
   #f))

;;;; ============================================================
;;;; Contract Application Macros
;;;; ============================================================

(define-syntax define/contract
  (syntax-rules ()
    "Define a function with a contract."
    ((_ (name args ...) contract-expr body ...)
     (define name
       (let* ((contract contract-expr)
              (raw-fn (lambda (args ...) body ...))
              (projection (contract-projection contract)))
         (if projection
             (projection raw-fn 'name 'caller)
             raw-fn))))
    ((_ name contract-expr value)
     (define name
       (let ((contract contract-expr))
         (unless (contract-check contract value)
           (contract-violation contract value 'name 'definition))
         value)))))

(define-syntax λ/contract
  (syntax-rules ()
    "Lambda with contract."
    ((_ contract-expr (args ...) body ...)
     (let* ((contract contract-expr)
            (raw-fn (lambda (args ...) body ...))
            (projection (contract-projection contract)))
       (if projection
           (projection raw-fn 'lambda 'caller)
           raw-fn)))))

(define-syntax contract-out
  (syntax-rules ()
    "Export with contract (for module boundaries)."
    ((_ (name contract) ...)
     (begin
       (let* ((c contract)
              (v name))
         (unless (or (not (contract-first-order? c))
                    (contract-check c v))
           (contract-violation c v 'module 'export)))
       ...))))

;;;; ============================================================
;;;; LLM Domain Contracts
;;;; ============================================================

;;; Basic type contracts
(define string/c (flat-contract string? #:name "string?"))
(define symbol/c (flat-contract symbol? #:name "symbol?"))
(define number/c (flat-contract number? #:name "number?"))
(define boolean/c (flat-contract boolean? #:name "boolean?"))
(define procedure/c (flat-contract procedure? #:name "procedure?"))
(define list/c (flat-contract list? #:name "list?"))
(define any/c (flat-contract (lambda (_) #t) #:name "any/c"))

;;; LLM-specific contracts

(define provider?/c
  (flat-contract
   (lambda (v)
     ;; Check if it has provider structure
     (and (procedure? v) #t))  ; Simplified - real check uses provider?
   #:name "provider?"))

(define role/c
  (or/c (flat-contract (lambda (v) (member v '("user" "assistant" "system" "tool")))
                       #:name "role-string")
        (flat-contract (lambda (v) (member v '(user assistant system tool)))
                       #:name "role-symbol")))

(define message?/c
  (alist/c
   #:required `((role . ,role/c)
                (content . ,(or/c string/c (flat-contract not #:name "null"))))))

(define messages/c
  (listof/c message?/c))

(define tool?/c
  (alist/c
   #:required `((name . ,string/c)
                (description . ,string/c))
   #:optional `((parameters . ,list/c)
                (handler . ,procedure/c))))

(define response?/c
  (or/c string/c  ; Simple text response
        (alist/c  ; Structured response with tool calls
         #:required `((role . ,role/c))
         #:optional `((content . ,(or/c string/c any/c))
                     (tool_calls . ,list/c)
                     (tool_use . ,list/c)))))

;;;; ============================================================
;;;; OpenAPI / JSON Schema Integration
;;;; ============================================================

(define (json-schema/c schema)
  "Create a contract from a JSON Schema definition."
  (match schema
    ;; Type constraints
    (('type . "string")
     string/c)
    (('type . "number")
     number/c)
    (('type . "integer")
     (flat-contract integer? #:name "integer?"))
    (('type . "boolean")
     boolean/c)
    (('type . "array")
     (let ((items (assoc-ref schema 'items)))
       (if items
           (listof/c (json-schema/c items))
           list/c)))
    (('type . "object")
     (let ((props (or (assoc-ref schema 'properties) '()))
           (required (or (assoc-ref schema 'required) '())))
       (alist/c
        #:required (map (lambda (name)
                         (let ((prop-schema (assoc-ref props (string->symbol name))))
                           (cons (string->symbol name)
                                 (if prop-schema
                                     (json-schema/c prop-schema)
                                     any/c))))
                       required)
        #:optional (filter-map
                    (lambda (prop)
                      (let ((name (car prop)))
                        (if (member (symbol->string name) required)
                            #f
                            (cons name (json-schema/c (cdr prop))))))
                    props))))
    ;; Enum constraint
    ((? (lambda (s) (assoc-ref s 'enum)))
     (let ((values (assoc-ref schema 'enum)))
       (flat-contract
        (lambda (v) (member v values))
        #:name (format #f "(enum ~a)" values))))
    ;; Default
    (_ any/c)))

(define (schema->contract schema-alist)
  "Convert an OpenAPI-style schema to a contract."
  (json-schema/c schema-alist))

;;;; ============================================================
;;;; Provider Interface Contract (Formal Specification)
;;;; ============================================================

;;; This defines the INTERFACE CONTRACT that all providers must satisfy.
;;; Inspired by TLA+ specification of interfaces.

(define provider-interface/c
  "Contract specifying the provider interface.

   INVARIANTS (TLA+ style):
   - provider-name always returns a symbol
   - provider-complete returns string or structured response
   - provider-chat accepts message list, returns response
   - provider-capabilities returns list of known capability symbols"

  (alist/c
   #:required
   `((name . ,symbol/c)
     (complete . ,(->/c any/c string/c response?/c))
     (chat . ,(->/c any/c messages/c response?/c)))
   #:optional
   `((embeddings . ,(->/c any/c string/c (listof/c number/c)))
     (list-models . ,(->/c any/c (listof/c string/c)))
     (capabilities . ,(listof/c symbol/c)))))

;;; contracts.scm ends here
