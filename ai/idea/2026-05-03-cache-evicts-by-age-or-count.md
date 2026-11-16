# The cache evicts by age or count

Each slot holds one build (`rebuild-replaces-the-superseded-build`), and
`clean-magix-cache --stale` removes the slots of scripts that are gone, so the
cache is bounded by the scripts that exist. What it still holds is the build of
every script that exists but is rarely run: its result link is a
garbage-collector root, so `nix-collect-garbage` frees none of it. Today the
only way to free it is `clean-magix-cache` without flags, which removes every
build and makes every script rebuild on its next run.

Eviction would remove the slots of scripts not run recently, at the cost of a
rebuild on their next run. Nobody needs it now.

## Design

Record use: on every run, built or not, set the modification time of the
slot's `lock`, a regular file the run holds anyway. Not the result link:
`touch` and `setModificationTime` follow it into the read-only store.

New flags of `clean-magix-cache`, combinable with `--stale`:

| Flag              | Meaning                                       |
| ----------------- | --------------------------------------------- |
| `--older-than N`  | Remove slots not used for N days              |
| `--keep-latest N` | Keep the N most recently used slots           |
| `--dry-run`       | Print what would be removed, without removing |

Eviction removes whole slots inside `withCacheEntries` (`src/Magix/Cache.hs`),
holding the cache lock exclusively, like `--stale`; no slot lock is needed.

## Open

- Recording use costs a system call on every cache hit, against the 20 ms a
  cached run takes; opt-in, or always?
- A size cap (`--max-size`) instead of age or count: measuring a slot's
  closure needs `nix path-info`, and builds share store paths.
- Running eviction automatically, after a build or on a timer, meets the same
  lock problem as `script-run-by-store-path-keeps-one-build`.
