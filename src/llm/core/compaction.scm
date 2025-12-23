;;; compaction.scm --- Conversation compaction and summarization

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

(define-module (llm core compaction)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (llm core prompts)
  #:export (;; Compaction strategies
            make-compaction-config
            compact-conversation
            compact-messages

            ;; Strategies
            strategy:sliding-window
            strategy:summarize-older
            strategy:hierarchical
            strategy:importance-weighted

            ;; Token estimation
            estimate-tokens
            messages-token-count

            ;; Evaluation
            make-compaction-eval
            eval-compaction
            compaction-metrics

            ;; Utilities
            split-at-token-limit
            merge-adjacent-messages
            extract-key-points))

;;; Commentary:
;;;
;;; This module provides conversation compaction strategies for managing
;;; long-running LLM conversations within token limits.
;;;
;;; Compaction Strategies:
;;; - sliding-window: Keep only the N most recent messages
;;; - summarize-older: Summarize older messages, keep recent verbatim
;;; - hierarchical: Multi-level summarization with decay
;;; - importance-weighted: Preserve messages based on importance scores
;;;
;;; Evaluation Metrics:
;;; - Information retention: Key facts preserved
;;; - Semantic similarity: Meaning preservation
;;; - Token efficiency: Compression ratio
;;; - Coherence: Logical flow maintained
;;;
;;; Code:

;;; --------------------------------------------------------------------
;;; Token Estimation
;;; --------------------------------------------------------------------

(define *chars-per-token* 4)  ; Approximate for English text

(define (estimate-tokens text)
  "Estimate token count for text (rough approximation).
   Uses ~4 characters per token heuristic."
  (if (string? text)
      (ceiling (/ (string-length text) *chars-per-token*))
      0))

(define (message-tokens msg)
  "Estimate tokens in a single message."
  (let ((content (assoc-ref msg 'content)))
    (+ 4  ; Role and formatting overhead
       (estimate-tokens (if (string? content) content "")))))

(define (messages-token-count messages)
  "Estimate total tokens in a list of messages."
  (fold + 0 (map message-tokens messages)))

;;; --------------------------------------------------------------------
;;; Compaction Configuration
;;; --------------------------------------------------------------------

(define* (make-compaction-config #:key
                                  (strategy 'sliding-window)
                                  (max-tokens 4096)
                                  (preserve-system #t)
                                  (preserve-recent 5)
                                  (summary-ratio 0.3)
                                  (importance-threshold 0.5))
  "Create a compaction configuration.

   Parameters:
   - strategy: Compaction strategy to use
   - max-tokens: Maximum tokens after compaction
   - preserve-system: Always keep system messages
   - preserve-recent: Number of recent messages to keep verbatim
   - summary-ratio: Target ratio for summarized content
   - importance-threshold: Minimum importance to preserve"
  `((strategy . ,strategy)
    (max-tokens . ,max-tokens)
    (preserve-system . ,preserve-system)
    (preserve-recent . ,preserve-recent)
    (summary-ratio . ,summary-ratio)
    (importance-threshold . ,importance-threshold)))

;;; --------------------------------------------------------------------
;;; Compaction Strategies
;;; --------------------------------------------------------------------

(define (strategy:sliding-window messages config)
  "Keep only the most recent messages within token limit.

   Simple but effective for conversations where recent context
   is most relevant."
  (let* ((max-tokens (assoc-ref config 'max-tokens))
         (preserve-system (assoc-ref config 'preserve-system))
         (system-msgs (if preserve-system
                         (filter (lambda (m)
                                  (equal? (assoc-ref m 'role) "system"))
                                 messages)
                         '()))
         (other-msgs (filter (lambda (m)
                              (not (equal? (assoc-ref m 'role) "system")))
                            messages))
         (system-tokens (messages-token-count system-msgs))
         (available-tokens (- max-tokens system-tokens)))

    ;; Take messages from end until we hit limit
    (let loop ((remaining (reverse other-msgs))
               (selected '())
               (tokens 0))
      (if (or (null? remaining)
              (> (+ tokens (message-tokens (car remaining))) available-tokens))
          (append system-msgs selected)
          (loop (cdr remaining)
                (cons (car remaining) selected)
                (+ tokens (message-tokens (car remaining))))))))

(define (strategy:summarize-older messages config)
  "Summarize older messages, keep recent ones verbatim.

   Returns a structure with summary placeholder for older content."
  (let* ((preserve-recent (assoc-ref config 'preserve-recent))
         (preserve-system (assoc-ref config 'preserve-system))
         (system-msgs (if preserve-system
                         (filter (lambda (m)
                                  (equal? (assoc-ref m 'role) "system"))
                                 messages)
                         '()))
         (other-msgs (filter (lambda (m)
                              (not (equal? (assoc-ref m 'role) "system")))
                            messages))
         (msg-count (length other-msgs))
         (split-point (max 0 (- msg-count preserve-recent)))
         (older-msgs (take other-msgs split-point))
         (recent-msgs (drop other-msgs split-point)))

    (if (null? older-msgs)
        messages
        (let ((summary (create-summary-message older-msgs)))
          (append system-msgs (list summary) recent-msgs)))))

(define (create-summary-message messages)
  "Create a summary message from a list of messages.

   This creates a placeholder - actual summarization requires LLM call."
  (let* ((msg-count (length messages))
         (speakers (delete-duplicates
                    (map (lambda (m) (assoc-ref m 'role)) messages)))
         (total-chars (fold + 0
                           (map (lambda (m)
                                 (string-length
                                  (or (assoc-ref m 'content) "")))
                                messages))))
    `((role . "system")
      (content . ,(format #f "[Summary of ~a previous messages (~a characters) between: ~{~a~^, ~}]"
                         msg-count total-chars speakers))
      (is-summary . #t)
      (summarized-count . ,msg-count))))

(define (strategy:hierarchical messages config)
  "Multi-level summarization with temporal decay.

   More recent messages get finer granularity, older ones
   are progressively more compressed."
  (let* ((preserve-recent (assoc-ref config 'preserve-recent))
         (max-tokens (assoc-ref config 'max-tokens))
         (levels '((0.8 . 1)    ; 80% of history: full compression
                   (0.5 . 2)    ; 50-80%: medium compression
                   (0.2 . 4)    ; 20-50%: light compression
                   (0.0 . 10))) ; 0-20%: minimal compression (keep ~10%)
         (msg-count (length messages)))

    ;; For now, use sliding window as fallback
    ;; Full implementation would do multi-level summarization
    (strategy:sliding-window messages config)))

(define (strategy:importance-weighted messages config)
  "Preserve messages based on importance scores.

   Importance can be based on:
   - Message length (longer = more important)
   - Question/answer patterns
   - Explicit markers
   - Semantic centrality"
  (let* ((threshold (assoc-ref config 'importance-threshold))
         (max-tokens (assoc-ref config 'max-tokens))
         ;; Add index to each message for later ordering
         (indexed (map (lambda (m i) (cons i m))
                      messages
                      (iota (length messages))))
         ;; Score each message
         (scored (map (lambda (pair)
                       (let ((idx (car pair))
                             (msg (cdr pair)))
                         (list (calculate-importance msg) idx msg)))
                     indexed))
         ;; Sort by importance (highest first)
         (sorted (sort scored (lambda (a b) (> (car a) (car b))))))

    ;; Select most important messages within token limit
    (let loop ((remaining sorted)
               (selected '())
               (tokens 0))
      (if (null? remaining)
          ;; Re-sort by original index and extract messages
          (map caddr
               (sort selected (lambda (a b) (< (cadr a) (cadr b)))))
          (let* ((entry (car remaining))
                 (importance (car entry))
                 (msg (caddr entry))
                 (msg-tokens (message-tokens msg))
                 (new-tokens (+ tokens msg-tokens)))
            (if (and (>= importance threshold)
                     (<= new-tokens max-tokens))
                (loop (cdr remaining)
                      (cons entry selected)
                      new-tokens)
                (loop (cdr remaining) selected tokens)))))))

(define (calculate-importance msg)
  "Calculate importance score for a message (0.0 - 1.0)."
  (let* ((content (or (assoc-ref msg 'content) ""))
         (role (assoc-ref msg 'role))
         (length-score (min 1.0 (/ (string-length content) 500.0)))
         (role-score (cond
                      ((equal? role "system") 1.0)
                      ((equal? role "user") 0.8)
                      (else 0.6)))
         ;; Questions are important
         (question-score (if (string-contains content "?") 0.2 0.0))
         ;; Code blocks are important
         (code-score (if (string-contains content "```") 0.2 0.0)))
    (min 1.0 (+ (* 0.4 length-score)
                (* 0.3 role-score)
                question-score
                code-score))))

;;; --------------------------------------------------------------------
;;; Main Compaction Interface
;;; --------------------------------------------------------------------

(define* (compact-conversation conversation #:key (config #f))
  "Compact a conversation using the configured strategy.

   conversation: List of messages or prompt structure
   config: Compaction configuration (uses defaults if not provided)"
  (let* ((cfg (or config (make-compaction-config)))
         (messages (if (list? conversation)
                      conversation
                      (prompt->messages conversation)))
         (strategy (assoc-ref cfg 'strategy)))

    (case strategy
      ((sliding-window) (strategy:sliding-window messages cfg))
      ((summarize-older) (strategy:summarize-older messages cfg))
      ((hierarchical) (strategy:hierarchical messages cfg))
      ((importance-weighted) (strategy:importance-weighted messages cfg))
      (else (strategy:sliding-window messages cfg)))))

(define (compact-messages messages max-tokens)
  "Simple interface to compact messages to fit within token limit."
  (compact-conversation messages
                       #:config (make-compaction-config
                                #:max-tokens max-tokens)))

;;; --------------------------------------------------------------------
;;; Utilities
;;; --------------------------------------------------------------------

(define (split-at-token-limit messages max-tokens)
  "Split messages into (within-limit . excess) pair."
  (let loop ((remaining messages)
             (within '())
             (tokens 0))
    (if (null? remaining)
        (cons (reverse within) '())
        (let ((msg-tokens (message-tokens (car remaining))))
          (if (> (+ tokens msg-tokens) max-tokens)
              (cons (reverse within) remaining)
              (loop (cdr remaining)
                    (cons (car remaining) within)
                    (+ tokens msg-tokens)))))))

(define (merge-adjacent-messages messages)
  "Merge adjacent messages from the same role."
  (if (null? messages)
      '()
      (let loop ((remaining (cdr messages))
                 (current (car messages))
                 (result '()))
        (if (null? remaining)
            (reverse (cons current result))
            (let ((next (car remaining)))
              (if (equal? (assoc-ref current 'role)
                         (assoc-ref next 'role))
                  ;; Merge
                  (loop (cdr remaining)
                        `((role . ,(assoc-ref current 'role))
                          (content . ,(string-append
                                       (assoc-ref current 'content)
                                       "\n"
                                       (assoc-ref next 'content))))
                        result)
                  ;; Don't merge
                  (loop (cdr remaining)
                        next
                        (cons current result))))))))

(define (extract-key-points messages)
  "Extract key points from messages for summarization.

   Returns a list of extracted points."
  (filter-map
   (lambda (msg)
     (let ((content (or (assoc-ref msg 'content) "")))
       ;; Simple heuristic: sentences with key indicators
       (let ((indicators '("important" "key" "must" "critical"
                          "remember" "note" "conclusion")))
         (if (any (lambda (ind) (string-contains content ind))
                 indicators)
             `((role . ,(assoc-ref msg 'role))
               (key-point . ,content))
             #f))))
   messages))

;;; --------------------------------------------------------------------
;;; Evaluation Framework
;;; --------------------------------------------------------------------

(define* (make-compaction-eval #:key
                                (original-messages '())
                                (compacted-messages '())
                                (expected-retention '())
                                (expected-tokens #f))
  "Create a compaction evaluation specification.

   Parameters:
   - original-messages: The full conversation before compaction
   - compacted-messages: The result after compaction
   - expected-retention: Key facts/phrases that must be preserved
   - expected-tokens: Expected token count (optional)"
  `((original . ,original-messages)
    (compacted . ,compacted-messages)
    (expected-retention . ,expected-retention)
    (expected-tokens . ,expected-tokens)))

(define (eval-compaction eval-spec)
  "Evaluate compaction quality.

   Returns an evaluation result with metrics."
  (let* ((original (assoc-ref eval-spec 'original))
         (compacted (assoc-ref eval-spec 'compacted))
         (expected (assoc-ref eval-spec 'expected-retention))
         (original-tokens (messages-token-count original))
         (compacted-tokens (messages-token-count compacted))
         (compression-ratio (if (> original-tokens 0)
                               (/ compacted-tokens original-tokens)
                               1.0))
         (retention-score (calculate-retention compacted expected)))

    `((original-tokens . ,original-tokens)
      (compacted-tokens . ,compacted-tokens)
      (compression-ratio . ,compression-ratio)
      (retention-score . ,retention-score)
      (message-count-original . ,(length original))
      (message-count-compacted . ,(length compacted))
      (passed . ,(and (> retention-score 0.7)
                      (< compression-ratio 0.8))))))

(define (calculate-retention compacted-messages expected-items)
  "Calculate what fraction of expected items are retained."
  (if (null? expected-items)
      1.0
      (let* ((compacted-text (string-join
                              (map (lambda (m)
                                    (or (assoc-ref m 'content) ""))
                                   compacted-messages)
                              " "))
             (found (filter (lambda (item)
                             (string-contains compacted-text item))
                           expected-items)))
        (/ (length found) (length expected-items)))))

(define (compaction-metrics original compacted)
  "Calculate comprehensive compaction metrics."
  (let* ((orig-tokens (messages-token-count original))
         (comp-tokens (messages-token-count compacted))
         (orig-chars (fold + 0
                          (map (lambda (m)
                                (string-length (or (assoc-ref m 'content) "")))
                               original)))
         (comp-chars (fold + 0
                          (map (lambda (m)
                                (string-length (or (assoc-ref m 'content) "")))
                               compacted))))
    `((token-ratio . ,(if (> orig-tokens 0) (/ comp-tokens orig-tokens) 0))
      (char-ratio . ,(if (> orig-chars 0) (/ comp-chars orig-chars) 0))
      (message-ratio . ,(if (> (length original) 0)
                           (/ (length compacted) (length original))
                           0))
      (tokens-saved . ,(- orig-tokens comp-tokens))
      (chars-saved . ,(- orig-chars comp-chars)))))

;;; compaction.scm ends here
