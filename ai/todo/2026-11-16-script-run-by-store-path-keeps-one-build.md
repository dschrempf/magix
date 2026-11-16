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

Open:

- Does a consumer run a magix script by its store path? `~/nix/config` is
  unchecked.
- Fix in magix, or in the system: a timer running `clean-magix-cache --stale`
  after garbage collection needs no change here. In magix, a build could
  remove stale slots after it succeeds, but cleaning holds the cache lock
  exclusively, which a build holding it shared cannot take; it would have to
  try the lock and skip when another build runs.
