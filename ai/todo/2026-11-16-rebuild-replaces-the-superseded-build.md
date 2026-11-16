# A rebuild replaces the build it supersedes

A cache entry is `<hash>-<name>{,.lock,-script,-build,-result}`, and the
hash covers the script's contents (`src/Magix/Hash.hs`). A changed script
gets a new entry; the old entry's `-result` is a garbage-collector root, so
its build stays.

On one system on 2026-11-16, before the cache was cleaned, `~/.cache/magix`
held 535 result links for 30 script names, up to 97 builds of one script.
They rooted 37.5 GiB, 36.0 GiB of it outside the system's closure.

`cache-evicts-by-age-or-count` trims by age or count. An entry superseded by a newer
build of the same script is dead at once, and should go when the new one is
built:

- Key a script's slot by the path it is invoked by, not the canonical path.
  A script installed from a flake lives in the store, so its canonical path
  changes with every deploy; the path it is called by
  (`/etc/profiles/per-user/<user>/bin/<name>`, `~/bin/<name>`) does not.
- Keep the content hash inside the slot. A mismatch rebuilds and replaces
  the slot, so each script keeps one build.
- `clean-magix-cache --stale` then removes slots whose script no longer
  exists.

This comes before `flake-nix-path-entry-resolves-to-a-store-path`, which
would otherwise add a build per script with every nixpkgs update.
