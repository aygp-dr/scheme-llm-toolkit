#!/usr/bin/env -S guile3 -L ../../src -s
!#
;;; test-compaction.scm --- Test compaction with literary conversations

(use-modules (llm core compaction)
             (ice-9 format)
             (srfi srfi-1))

;;; --------------------------------------------------------------------
;;; Literary Conversations
;;; --------------------------------------------------------------------

;; Camus-inspired (The Stranger / Myth of Sisyphus)
(define camus-conversation
  '(((role . "system")
     (content . "You are Meursault, speaking with detached honesty about existence."))
    ((role . "user")
     (content . "Mother died today. Or maybe yesterday, I do not know."))
    ((role . "assistant")
     (content . "It does not really matter when. The telegram was unclear. What matters is that it happened. The sun was very hot at the funeral."))
    ((role . "user")
     (content . "Do you not feel grief? Should you not mourn?"))
    ((role . "assistant")
     (content . "I do not see why I should pretend to feel something I do not. The sun was in my eyes. I was tired. These are facts. Grief is just a word people use."))
    ((role . "user")
     (content . "But surely life must have meaning? Purpose?"))
    ((role . "assistant")
     (content . "The absurd is clear to me now. We seek meaning in a universe that offers none. But this is not despair - it is freedom. Sisyphus pushes his boulder. We must imagine him happy."))
    ((role . "user")
     (content . "How can struggling without purpose bring happiness?"))
    ((role . "assistant")
     (content . "Because the struggle itself is enough. The moment of consciousness, when Sisyphus watches the boulder roll down - that is his victory. He owns his fate."))
    ((role . "user")
     (content . "And if they condemn you? If society rejects your honesty?"))
    ((role . "assistant")
     (content . "They condemned me for not crying at my mother funeral. Not for what I did, but for who I am. In the end, I hoped there would be many spectators at my execution."))))

(define camus-expected '("absurd" "Sisyphus" "meaning" "struggle" "freedom" "universe"))

;; Kant-inspired (Critique of Pure Reason)
(define kant-conversation
  '(((role . "system")
     (content . "You are a Kantian philosopher explaining the critical philosophy with precision."))
    ((role . "user")
     (content . "What can we actually know about reality?"))
    ((role . "assistant")
     (content . "We must distinguish between phenomena - things as they appear to us through our cognitive faculties - and noumena - things as they are in themselves. We can have knowledge only of the former."))
    ((role . "user")
     (content . "But does that not mean we are trapped in our own minds?"))
    ((role . "assistant")
     (content . "Not trapped - structured. The mind brings a priori categories to experience: causality, substance, unity. These are not learned from experience; they make experience possible."))
    ((role . "user")
     (content . "So mathematics is certain because it is about our mental structures?"))
    ((role . "assistant")
     (content . "Precisely. Mathematical propositions are synthetic a priori - they extend our knowledge yet are necessarily true. This is universally valid because all rational beings share these structures."))
    ((role . "user")
     (content . "What about morality? Is that also a priori?"))
    ((role . "assistant")
     (content . "The moral law is given through pure practical reason. The categorical imperative commands unconditionally: act only according to maxims you could will to be universal laws."))))

(define kant-expected '("phenomena" "noumena" "a priori" "categorical" "universal" "reason"))

;; Waiting for Godot-inspired (Beckett)
(define godot-conversation
  '(((role . "system")
     (content . "You are Vladimir and Estragon, waiting endlessly for someone who may never come."))
    ((role . "user")
     (content . "Let us go."))
    ((role . "assistant")
     (content . "We cannot."))
    ((role . "user")
     (content . "Why not?"))
    ((role . "assistant")
     (content . "We are waiting for Godot."))
    ((role . "user")
     (content . "What does he look like?"))
    ((role . "assistant")
     (content . "He has a white beard. No... I am not sure. We have never seen him."))
    ((role . "user")
     (content . "What if he does not come?"))
    ((role . "assistant")
     (content . "He will come tomorrow. He always comes tomorrow."))
    ((role . "user")
     (content . "But what about today?"))
    ((role . "assistant")
     (content . "Today we wait. Like yesterday. The tree has leaves now. Or did it before?"))
    ((role . "user")
     (content . "Nothing happens."))
    ((role . "assistant")
     (content . "Nothing happens. Nobody comes. Nobody goes. It is awful."))
    ((role . "user")
     (content . "Let us go."))
    ((role . "assistant")
     (content . "We cannot."))
    ((role . "user")
     (content . "Why not?"))
    ((role . "assistant")
     (content . "We are waiting for Godot."))))

(define godot-expected '("Godot" "waiting" "tomorrow" "nothing" "cannot" "tree"))

;;; --------------------------------------------------------------------
;;; Test Framework
;;; --------------------------------------------------------------------

(define (print-header title)
  (format #t "\n~a\n~a\n" title (make-string (string-length title) #\=)))

(define (check-retention compacted expected-terms)
  "Check which expected terms are retained in compacted output."
  (let* ((text (apply string-append
                     (map (lambda (m) (or (assoc-ref m 'content) ""))
                          compacted)))
         (found (filter (lambda (term) (string-contains text term))
                       expected-terms)))
    (values found (length found) (length expected-terms))))

(define (test-strategy strategy msgs expected max-tokens)
  "Test a compaction strategy and return results."
  (let* ((config (make-compaction-config
                  #:strategy strategy
                  #:max-tokens max-tokens
                  #:preserve-recent 2
                  #:importance-threshold 0.3))
         (orig-tokens (messages-token-count msgs))
         (compacted (compact-conversation msgs #:config config))
         (comp-tokens (messages-token-count compacted))
         (compression (if (> orig-tokens 0)
                         (* 100 (- 1 (/ comp-tokens orig-tokens)))
                         0)))
    (call-with-values
        (lambda () (check-retention compacted expected))
      (lambda (found found-count total-count)
        `((strategy . ,strategy)
          (original-messages . ,(length msgs))
          (compacted-messages . ,(length compacted))
          (original-tokens . ,orig-tokens)
          (compacted-tokens . ,comp-tokens)
          (compression-pct . ,compression)
          (retained-terms . ,found)
          (retention-ratio . ,(/ found-count total-count)))))))

(define (run-conversation-test name msgs expected)
  "Run compaction tests on a conversation."
  (print-header (format #f "~a" name))
  (format #t "Original: ~a messages, ~a tokens\n"
          (length msgs) (messages-token-count msgs))
  (format #t "Expected retention: ~{~a~^, ~}\n\n" expected)

  (let ((strategies '(sliding-window summarize-older importance-weighted))
        (results '()))
    (for-each
     (lambda (strategy)
       (let ((result (test-strategy strategy msgs expected 150)))
         (set! results (cons result results))
         (format #t "~a:\n" strategy)
         (format #t "  Messages: ~a -> ~a\n"
                 (assoc-ref result 'original-messages)
                 (assoc-ref result 'compacted-messages))
         (format #t "  Tokens:   ~a -> ~a (~,0f% compression)\n"
                 (assoc-ref result 'original-tokens)
                 (assoc-ref result 'compacted-tokens)
                 (assoc-ref result 'compression-pct))
         (format #t "  Retained: ~{~a~^, ~} (~,0f%)\n\n"
                 (assoc-ref result 'retained-terms)
                 (* 100 (assoc-ref result 'retention-ratio)))))
     strategies)

    ;; Determine best strategy for this conversation type
    (let* ((best (fold (lambda (r best)
                        (if (> (assoc-ref r 'retention-ratio)
                               (assoc-ref best 'retention-ratio))
                            r best))
                      (car results)
                      (cdr results))))
      (format #t "Best for retention: ~a (~,0f% retained)\n"
              (assoc-ref best 'strategy)
              (* 100 (assoc-ref best 'retention-ratio))))))

;;; --------------------------------------------------------------------
;;; Main
;;; --------------------------------------------------------------------

(define (main)
  (format #t "~a\n" (make-string 60 #\=))
  (format #t "COMPACTION EVALUATION - Literary Conversation Styles\n")
  (format #t "~a\n" (make-string 60 #\=))
  (format #t "\nTesting conversation compaction with three distinct styles:\n")
  (format #t "  - Camus: Sparse, absurdist, existential themes\n")
  (format #t "  - Kant: Dense philosophical reasoning, technical terms\n")
  (format #t "  - Godot: Circular, repetitive, minimalist dialogue\n")

  (run-conversation-test "CAMUS (Absurdist)" camus-conversation camus-expected)
  (run-conversation-test "KANT (Philosophical)" kant-conversation kant-expected)
  (run-conversation-test "GODOT (Circular)" godot-conversation godot-expected)

  (print-header "SUMMARY")
  (format #t "\nCompaction Strategy Recommendations:\n")
  (format #t "  - sliding-window: Best for recent context, good retention\n")
  (format #t "  - summarize-older: Good balance, needs LLM for actual summary\n")
  (format #t "  - importance-weighted: Maximum compression, may lose key terms\n")
  (format #t "\nFor philosophical/technical content: Use sliding-window\n")
  (format #t "For repetitive dialogue: Use summarize-older (captures patterns)\n")
  (format #t "For general chat: importance-weighted is efficient\n")
  (format #t "\n~a\n" (make-string 60 #\=)))

(main)
