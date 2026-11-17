# A script run by its store path keeps one build

A slot is keyed by the path a script is invoked by (`getSlotHash`,
`src/Magix/Hash.hs`). A script run by its store path, such as a systemd
`ExecStart` or a wrapper citing `${script}/bin/<name>`, is invoked by a new
path with every deploy, so it gets a new slot each time; `removeSuperseded`
only clears builds within one slot.

The old slot's result link keeps its build rooted until both:

1. the garbage collector removes the old script's store path (the slot's
   `path` link is no root), and
2. someone runs `clean-magix-cache --stale`, which nothing runs automatically.

Before the cache slots (`rebuild-replaces-the-superseded-build`), every deploy
of every script added a build this way; this is the part they leave.

A consumer does (2026-11-17): a system configuration installs a program of
nixpkgs as a magix script, and a wrapper of its own runs it by its store
path, which changes with an update of nixpkgs or magix. It took the system's way: a
daily user timer runs `clean-magix-cache --stale`, so an old slot goes once
the garbage collector took its script.

Open: whether magix cleans stale slots itself, so a consumer needs no
timer. A build could remove them after it succeeds, but cleaning holds the
cache lock exclusively, which a build holding it shared cannot take; it
would have to try the lock and skip when another build runs.
