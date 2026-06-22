#!/usr/bin/env -S guile3 -L ../../src -e main -s
!#
;;; test-subprocess.scm --- Regression test for the FreeBSD spawn workaround
;;;
;;; Exercises (llm utils subprocess) WITHOUT touching system*/open-pipe, which
;;; segfault on FreeBSD 14 (bug #79494). See docs/research/guile-spawn-bug-79494.org.

(use-modules (llm utils subprocess)
             (srfi srfi-64))

(define (main args)
  (test-begin "subprocess-workaround")

  (test-group "detection"
    (test-assert "freebsd-spawn-workaround? returns a boolean"
      (boolean? (freebsd-spawn-workaround?))))

  (test-group "safe-command"
    (test-assert "echo exits 0"
      (zero? (status:exit-val (safe-command "/bin/echo" "ok"))))
    (test-assert "missing program does not exit 0"
      (not (zero? (status:exit-val
                   (safe-command "/nonexistent/program-xyz"))))))

  (test-group "safe-command-output"
    (test-equal "captures stdout"
      "hello\n" (safe-command-output "/bin/echo" "hello")))

  (test-group "safe-pipe-command"
    (test-equal "runs through sh -c"
      "piped\n" (safe-pipe-command "echo piped")))

  (test-group "safe-write-read-command"
    (test-equal "feeds stdin to a command"
      "data" (safe-write-read-command "cat" "data")))

  (test-group "command-available?"
    (test-assert "sh is available" (command-available? "sh"))
    (test-assert "bogus command is not available"
      (not (command-available? "definitely-not-a-real-command-xyz"))))

  (test-end "subprocess-workaround")
  (exit (if (zero? (test-runner-fail-count (test-runner-current))) 0 1)))
