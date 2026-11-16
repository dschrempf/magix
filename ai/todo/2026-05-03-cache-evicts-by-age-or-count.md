# Spec: Smarter Cache Eviction

## Problem

`clean-magix-cache` removes the entire cache. Users who run many different
scripts accumulate stale build entries indefinitely, but wiping everything means
paying a full rebuild cost on the next run of every script.

## Goal

Add optional eviction policies so the cache can be trimmed automatically or
on-demand without discarding everything.

## Design

### Access-time tracking

On each cache hit (i.e. `HasBeenBuilt`), `touch` the result symlink to update
its mtime. This gives an LRU signal at zero extra cost.

### New `clean-magix-cache` flags

| Flag | Meaning |
|------|---------|
| `--older-than N` | Remove entries whose result link mtime is older than N days |
| `--keep-latest N` | Keep only the N most recently used entries, remove the rest |
| `--dry-run` | Print what would be removed, without removing |

Without any flags the command keeps its current behaviour (remove everything).

### Entry grouping

Each cache entry is a slot, one directory per invoked path holding one build
(`rebuild-replaces-the-superseded-build`). Eviction removes the whole
directory, taking its lock first.

### Scheduled eviction (optional / future)

A `--auto-evict-after N` option in a future global config could trigger
eviction automatically after each run, similar to how `nix-collect-garbage`
can be run on a timer.

## Open questions

- Should access-time tracking be opt-in (to avoid extra `syscall`s on every
  cache hit for users who never evict)?
- Is there a need for a total-size cap (`--max-size`) in addition to age/count
  limits? Each result symlink is a garbage-collector root, so it keeps its
  build in the Nix store: `nix-collect-garbage` frees nothing a cache entry
  holds. One user's cache rooted 36 GiB outside the system's closure
  (`rebuild-replaces-the-superseded-build`, which removes most of it at
  build time).
