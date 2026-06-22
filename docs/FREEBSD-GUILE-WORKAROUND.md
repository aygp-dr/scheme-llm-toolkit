# FreeBSD Guile 3.0.10 Segfault Workaround

## Problem

Guile 3.0.10 on FreeBSD 14 crashes with a segmentation fault when using:
- `system*`
- `open-pipe*` / `open-pipe` (from `ice-9 popen`)
- Any HTTP client libraries that use these functions

## Root Cause

**Bug #79494**: Incompatible gnulib `posix_spawn` with FreeBSD's
`posix_spawn_file_actions_addclosefrom_np`.

The issue is that gnulib provides a replacement `posix_spawn` implementation,
but it's incompatible with FreeBSD's native `posix_spawn_file_actions_addclosefrom_np()`.
When Guile 3.0.10 is built on FreeBSD 14, it uses gnulib's spawn with FreeBSD's
native file actions function, causing a segfault in `do_spawn` within `libguile/posix.c`.

## References

- [Guile Bug #79494](https://www.mail-archive.com/bug-guile@gnu.org/msg11737.html)
- [FreeBSD Bug #282534](https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=282534)
- [Fix PR on Codeberg](https://codeberg.org/guile/guile/pulls/17)

## Workaround

This toolkit includes `(llm utils subprocess)` which provides FreeBSD-safe
subprocess execution using `primitive-fork` + `execl` directly, bypassing
the broken `system*` and `popen` functions.

### What Works

```scheme
;; These work fine:
(primitive-fork)
(execl "/bin/echo" "echo" "hello")
(waitpid pid)
```

### What Crashes

```scheme
;; These cause segfaults on FreeBSD 14:
(system* "echo" "hello")                    ;; SEGFAULT
(open-pipe* OPEN_READ "echo" "hello")       ;; SEGFAULT
(open-input-pipe "echo hello")              ;; SEGFAULT
```

## Using the Workaround

```scheme
(use-modules (llm utils subprocess))

;; Run command, capture output
(safe-pipe-command "curl -s http://api.example.com/data")

;; Check command availability
(command-available? "curl")

;; Write input, read output
(safe-write-read-command "cat" "input data")

;; Check if workaround is active
(freebsd-spawn-workaround?)  ;; => #t on FreeBSD
```

## HTTP Module

The `(llm utils http)` module has been updated to use the safe subprocess
functions. All HTTP operations (GET, POST, streaming) work without segfaults.

## When Will This Be Fixed?

The fix has been merged upstream (Codeberg PR #17) but is not yet in a
released version. It should be available in Guile 3.0.11 or later.

**Status (2026-06-22): NOT fixed in FreeBSD 14.4.** After upgrading hydra to
`FreeBSD 14.4-RELEASE-p6` (and rebuilding `guile3-3.0.10`), the crash still
reproduces from a clean `system*`/`open-input-pipe` call (SIGSEGV, rc=139).
The faulting frame is `posix_spawn_file_actions_addclosefrom_np()` in
`libc.so.7`, called from `libguile-3.0.so.1`. The workaround in this repo
remains load-bearing. Full reproduction, backtrace, and the upstream-tracking
write-up live in [`research/guile-spawn-bug-79494.org`](research/guile-spawn-bug-79494.org).

## Debugging / Reproduction Notes

To reproduce and capture evidence on a FreeBSD box:

```bash
# 1. Trigger the crash (cores land in cwd; kern.corefile = %N.core)
guile3 -c '(system* "/bin/echo" "hi")'      # -> Segmentation fault (core dumped)

# 2. Pull a backtrace from the core
gdb -batch -q -ex bt /usr/local/bin/guile3 ./guile-3.0.core

# 3. For an interactive session, drive it under tmux so the REPL and gdb
#    share a pane layout and survive disconnects:
tmux new -s guilebug 'gdb /usr/local/bin/guile3'
#    then inside gdb:  run -c '(open-input-pipe "echo hi")'
```

Core files match `*.core` and are already gitignored — do not commit them
(they are ~33 MB and host-specific).

### Repo Boundaries

- **In scope for this repo:** the `(llm utils subprocess)` workaround, its
  tests, and documenting/reproducing the bug. We make the toolkit run
  correctly on the affected platform.
- **Out of scope:** patching Guile or FreeBSD `libc` themselves. The actual
  fix belongs upstream (Guile / FreeBSD ports). This repo only carries
  evidence and a submission-ready report to push that fix along — see the
  research doc above.

## Testing

Run the validation script:

```bash
./experiments/validation-agents/validate.sh
```

Or the subprocess regression test (asserts the safe path works without
touching the crashing primitives):

```bash
make test    # includes 013-subprocess-test
```

All core modules should load and pass tests without segfaults.
