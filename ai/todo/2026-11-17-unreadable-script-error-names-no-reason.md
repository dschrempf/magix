# An unreadable script's error names no reason

`runMagix` (`src/Magix/Main.hs`) reads the script with
``readFile path `onException` (logE … >> exitFailure)``, which drops the
exception: a missing file, a permission error and a directory all log "Could
not read file: <path>" and exit 1. `onException` also fires on asynchronous
exceptions, so Ctrl-C during the read becomes exit 1 with that message. Found
while moving Main into the library on 2026-11-17.

Fix: catch `IOException` only (`try`), and log its message.
