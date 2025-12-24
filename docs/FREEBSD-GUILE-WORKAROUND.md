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

FreeBSD ports may also include the fix in a future package update.

## Testing

Run the validation script:

```bash
./experiments/validation-agents/validate.sh
```

All core modules should load and pass tests without segfaults.
