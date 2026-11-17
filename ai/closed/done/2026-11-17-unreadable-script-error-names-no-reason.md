# An unreadable script's error names no reason

`runMagix` (`src/Magix/Main.hs`) reads the script with
``readFile path `onException` (logE … >> exitFailure)``, which drops the
exception: a missing file, a permission error and a directory all log "Could
not read file: <path>" and exit 1. `onException` also fires on asynchronous
exceptions, so Ctrl-C during the read becomes exit 1 with that message. Found
while moving Main into the library on 2026-11-17.

Fix: catch `IOException` only (`try`), and log its message.

## Outcome

Fixed on 2026-11-17. `runMagix` catches `IOException` only (`try`), and logs
"Could not read file: <path>: <reason>"; asynchronous exceptions propagate.
The reason is the exception with its file name cleared, since not every
`IOException` carries one, e.g. "withBinaryFile: does not exist (No such file
or directory)". `MainSpec` captures the logged errors with a handler of its
own, and covers a missing script and a directory; both tests fail without the
fix.
