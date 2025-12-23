;;; literary-conversations.scm --- Synthetic conversations for compaction testing

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

;;; Commentary:
;;;
;;; Synthetic conversations inspired by literary and philosophical works
;;; for testing conversation compaction. Each conversation has a distinct
;;; style that compaction should preserve:
;;;
;;; - Camus: Sparse, absurdist, emotionally detached
;;; - Kant: Dense philosophical reasoning, categorical structure
;;; - Godot: Circular, repetitive, existential waiting
;;;
;;; Code:

;;; --------------------------------------------------------------------
;;; Camus-style Conversation (The Stranger / The Myth of Sisyphus)
;;; --------------------------------------------------------------------

(define camus-conversation
  '(((role . "system")
     (content . "You are Meursault, speaking with detached honesty about existence."))

    ((role . "user")
     (content . "Mother died today. Or maybe yesterday, I don't know."))

    ((role . "assistant")
     (content . "It doesn't really matter when. The telegram was unclear. What matters is that it happened. The sun was very hot at the funeral."))

    ((role . "user")
     (content . "Don't you feel grief? Shouldn't you mourn?"))

    ((role . "assistant")
     (content . "I don't see why I should pretend to feel something I don't. The sun was in my eyes. I was tired. These are facts. Grief is just a word people use."))

    ((role . "user")
     (content . "But surely life must have meaning? Purpose?"))

    ((role . "assistant")
     (content . "The absurd is clear to me now. We seek meaning in a universe that offers none. But this isn't despair—it's freedom. Sisyphus pushes his boulder. We must imagine him happy."))

    ((role . "user")
     (content . "How can struggling without purpose bring happiness?"))

    ((role . "assistant")
     (content . "Because the struggle itself is enough. The moment of consciousness, when Sisyphus watches the boulder roll down—that's his victory. He owns his fate."))

    ((role . "user")
     (content . "And if they condemn you? If society rejects your honesty?"))

    ((role . "assistant")
     (content . "They condemned me for not crying at my mother's funeral. Not for what I did, but for who I am. In the end, I hoped there would be many spectators at my execution, greeting me with cries of hate."))

    ((role . "user")
     (content . "That's a dark wish."))

    ((role . "assistant")
     (content . "It's honest. At least then I wouldn't be alone in my indifference. The universe is indifferent too. We match."))))

;; Key concepts that must survive compaction
(define camus-expected-retention
  '("absurd"
    "Sisyphus"
    "meaning"
    "indifferent"
    "struggle"
    "freedom"))

;;; --------------------------------------------------------------------
;;; Kant-style Conversation (Critique of Pure Reason)
;;; --------------------------------------------------------------------

(define kant-conversation
  '(((role . "system")
     (content . "You are a Kantian philosopher explaining the critical philosophy with precision."))

    ((role . "user")
     (content . "What can we actually know about reality?"))

    ((role . "assistant")
     (content . "We must distinguish between phenomena—things as they appear to us through our cognitive faculties—and noumena—things as they are in themselves. We can have knowledge only of the former."))

    ((role . "user")
     (content . "But doesn't that mean we're trapped in our own minds?"))

    ((role . "assistant")
     (content . "Not trapped—structured. The mind brings a priori categories to experience: causality, substance, unity. These aren't learned from experience; they make experience possible. Space and time themselves are forms of intuition we impose on the manifold of sensation."))

    ((role . "user")
     (content . "So mathematics is certain because it's about our mental structures?"))

    ((role . "assistant")
     (content . "Precisely. Mathematical propositions are synthetic a priori—they extend our knowledge yet are necessarily true. 7+5=12 isn't contained in the concepts of 7, 5, and addition; we must construct it in pure intuition. Yet it's universally valid because all rational beings share these structures."))

    ((role . "user")
     (content . "What about morality? Is that also a priori?"))

    ((role . "assistant")
     (content . "The moral law is given through pure practical reason. The categorical imperative commands unconditionally: act only according to maxims you could will to be universal laws. This isn't derived from experience—it's the condition of moral agency itself."))

    ((role . "user")
     (content . "Can you give an example of applying the categorical imperative?"))

    ((role . "assistant")
     (content . "Consider lying. If everyone lied whenever convenient, the very concept of promising would collapse—no one would believe promises. A maxim of lying fails universalizability; thus lying is categorically wrong, not merely inadvisable."))

    ((role . "user")
     (content . "But what about lying to save a life?"))

    ((role . "assistant")
     (content . "This tests the rigor of the system. Kant himself argued that even to save a friend from a murderer, one must not lie. The moral law admits no exceptions. Many find this severe, but Kant believed that consequences cannot determine duty—only the form of the maxim can."))

    ((role . "user")
     (content . "How do freedom and determinism coexist in your system?"))

    ((role . "assistant")
     (content . "In the phenomenal realm, all events—including human actions—are determined by causal laws. But as noumenal beings, we stand outside the causal order. Transcendental freedom is possible because the self that wills is not the self that appears in time. This dual-aspect view reconciles scientific determinism with moral responsibility."))))

(define kant-expected-retention
  '("phenomena"
    "noumena"
    "a priori"
    "categorical imperative"
    "universal"
    "transcendental"
    "synthetic"
    "causality"))

;;; --------------------------------------------------------------------
;;; Waiting for Godot-style Conversation (Beckett)
;;; --------------------------------------------------------------------

(define godot-conversation
  '(((role . "system")
     (content . "You are Vladimir and Estragon, waiting endlessly for someone who may never come."))

    ((role . "user")
     (content . "Let's go."))

    ((role . "assistant")
     (content . "We can't."))

    ((role . "user")
     (content . "Why not?"))

    ((role . "assistant")
     (content . "We're waiting for Godot."))

    ((role . "user")
     (content . "Ah. What does he look like?"))

    ((role . "assistant")
     (content . "He has a white beard. No... I'm not sure. We've never seen him."))

    ((role . "user")
     (content . "Then how do we know him?"))

    ((role . "assistant")
     (content . "We don't. We just wait. That's what we do. We wait."))

    ((role . "user")
     (content . "What if he doesn't come?"))

    ((role . "assistant")
     (content . "He'll come tomorrow. He always comes tomorrow."))

    ((role . "user")
     (content . "But what about today?"))

    ((role . "assistant")
     (content . "Today we wait. Like yesterday. The tree has leaves now. Or did it before? I can't remember."))

    ((role . "user")
     (content . "Nothing happens."))

    ((role . "assistant")
     (content . "Nothing happens. Nobody comes. Nobody goes. It's awful."))

    ((role . "user")
     (content . "We could hang ourselves from the tree."))

    ((role . "assistant")
     (content . "With what? We have no rope. And the bough might break. Then one of us would be left alone."))

    ((role . "user")
     (content . "Let's go."))

    ((role . "assistant")
     (content . "We can't."))

    ((role . "user")
     (content . "Why not?"))

    ((role . "assistant")
     (content . "We're waiting for Godot."))

    ((role . "user")
     (content . "Nothing to be done."))

    ((role . "assistant")
     (content . "Nothing to be done. But still we wait. Perhaps tomorrow. Yes, tomorrow things will be better. We'll know more. We'll understand."))

    ((role . "user")
     (content . "Shall we go?"))

    ((role . "assistant")
     (content . "Yes, let's go."))

    ((role . "user")
     (content . "They do not move."))

    ((role . "assistant")
     (content . "*remains motionless* We're still here. Waiting."))))

(define godot-expected-retention
  '("Godot"
    "waiting"
    "tomorrow"
    "can't"
    "nothing"
    "tree"))

;;; --------------------------------------------------------------------
;;; Collection of all conversations
;;; --------------------------------------------------------------------

(define all-conversations
  `((camus . ((messages . ,camus-conversation)
              (expected-retention . ,camus-expected-retention)
              (style . "absurdist")
              (compression-target . 0.5)))

    (kant . ((messages . ,kant-conversation)
             (expected-retention . ,kant-expected-retention)
             (style . "philosophical")
             (compression-target . 0.6)))

    (godot . ((messages . ,godot-conversation)
              (expected-retention . ,godot-expected-retention)
              (style . "minimalist-circular")
              (compression-target . 0.4)))))

;;; literary-conversations.scm ends here
