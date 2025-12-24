;;; subprocess.scm --- FreeBSD-safe subprocess utilities
;;;
;;; Copyright (C) 2025 aygp-dr
;;; Licensed under MIT License

;;; Commentary:
;;;
;;; This module provides subprocess utilities that work around
;;; Guile 3.0.10 bug #79494 on FreeBSD 14, where system* and
;;; open-pipe* segfault due to incompatible gnulib posix_spawn
;;; with FreeBSD's posix_spawn_file_actions_addclosefrom_np.
;;;
;;; The workaround uses primitive-fork + execl directly, which
;;; work correctly on FreeBSD.
;;;
;;; See: https://codeberg.org/guile/guile/pulls/17
;;;
;;; Code:

(define-module (llm utils subprocess)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:export (safe-command
            safe-command-output
            safe-pipe-command
            safe-write-read-command
            command-available?
            freebsd-spawn-workaround?))

;;; Detect if we're on a system affected by the bug
(define (freebsd-spawn-workaround?)
  "Check if we're on FreeBSD where the spawn bug occurs."
  (let ((uname (catch #t
                 (lambda ()
                   (call-with-input-file "/proc/version"
                     (lambda (p) (read-line p))))
                 (lambda _ #f))))
    ;; If /proc/version doesn't exist or mentions FreeBSD
    (or (not uname)
        (and (string? uname)
             (string-contains uname "FreeBSD")))))

;;; Use primitive-fork + execl for safe subprocess execution
(define (safe-command program . args)
  "Execute a command safely using primitive-fork + execl.
   Returns the exit status."
  (let ((pid (primitive-fork)))
    (if (zero? pid)
        ;; Child process
        (begin
          (apply execl program program args)
          (primitive-exit 127))
        ;; Parent process
        (cdr (waitpid pid)))))

(define (safe-command-output program . args)
  "Execute a command and capture its stdout.
   Uses file-based IPC to avoid pipe-related segfaults."
  (let* ((tmp-file (format #f "/tmp/guile-subprocess-~a-~a.tmp"
                           (getpid) (random 100000)))
         (pid (primitive-fork)))
    (if (zero? pid)
        ;; Child: redirect stdout to file, then exec
        (let ((fd (open-fdes tmp-file (logior O_WRONLY O_CREAT O_TRUNC) #o644)))
          (dup2 fd 1)  ; stdout -> file
          (close-fdes fd)
          (apply execl program program args)
          (primitive-exit 127))
        ;; Parent: wait, read file, cleanup
        (begin
          (waitpid pid)
          (let ((output (if (file-exists? tmp-file)
                           (call-with-input-file tmp-file get-string-all)
                           "")))
            (catch #t
              (lambda () (delete-file tmp-file))
              (lambda _ #f))
            output)))))

(define (safe-pipe-command cmd)
  "Execute a shell command string and capture output.
   Uses sh -c for complex commands."
  (safe-command-output "/bin/sh" "-c" cmd))

(define (safe-write-read-command cmd input-data)
  "Execute a command with stdin input and capture stdout.

   Used for commands like: echo 'data' | curl -d @- url"
  (let* ((in-file (format #f "/tmp/guile-in-~a-~a.tmp"
                          (getpid) (random 100000)))
         (out-file (format #f "/tmp/guile-out-~a-~a.tmp"
                           (getpid) (random 100000))))
    (dynamic-wind
      (lambda ()
        ;; Write input data
        (call-with-output-file in-file
          (lambda (p) (display input-data p))))
      (lambda ()
        ;; Execute: sh -c "cmd < in-file > out-file"
        (let* ((full-cmd (format #f "~a < ~a > ~a 2>/dev/null"
                                 cmd in-file out-file))
               (pid (primitive-fork)))
          (if (zero? pid)
              ;; Child
              (begin
                (execl "/bin/sh" "sh" "-c" full-cmd)
                (primitive-exit 127))
              ;; Parent
              (begin
                (waitpid pid)
                (if (file-exists? out-file)
                    (call-with-input-file out-file get-string-all)
                    "")))))
      (lambda ()
        ;; Cleanup
        (catch #t (lambda () (delete-file in-file)) (lambda _ #f))
        (catch #t (lambda () (delete-file out-file)) (lambda _ #f))))))

(define (command-available? cmd)
  "Check if a command is available in PATH."
  (zero? (safe-command "/bin/sh" "-c"
                       (format #f "command -v ~a >/dev/null 2>&1" cmd))))

;;; subprocess.scm ends here
