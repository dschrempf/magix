# A flake build rebuilds on a changed flake

A flake build hashes the reference's text (`buildModeBytes`,
`src/Magix/Hash.hs`), and Nix resolves that text again at every build. Where
the text doesn't fix the flake's contents, a cached script keeps the build of
its first call:

- `nixpkgs`, the Readme's example: an update of the registry entry, such as a
  NixOS rebuild that bumps nixpkgs, doesn't rebuild. Channel mode had the same
  bug (`flake-nix-path-entry-resolves-to-a-store-path`).
- `github:NixOS/nixpkgs/nixos-unstable`: the branch moves on, the build stays.
- `path:/home/…/flake`: an edit of the flake, or of a library it holds, keeps
  the old build until `magix -f` (checked 2026-11-17 with a flake applying an
  overlay, the answer to overriding-packages-for-use-with-magix (#14)).

`FlakeBuild`'s comment asks for a pinned reference; nothing enforces it.

## Plan, by kind of reference

1. **Fixed by its text:** `github:<owner>/<repo>/<rev>`, a `rev=` or `narHash=`
   parameter, `path:/nix/store/…`. Hash the text, as now.
2. **A flake id** (`nixpkgs`, `flake:nixpkgs`): resolve it through the
   registries like channel mode (`resolveFlakeRef`,
   `src/Magix/FlakeRegistry.hs`), hash the store path, and build from
   `path:<store path>`, so the build is what was hashed. A file read, no
   subprocess. On NixOS the system registry holds the store path
   (`nix.registry.<id>.flake`).
3. **Mutable:** everything else, such as a branch, a `path:` outside the store,
   or `git+file:` without `rev`. Only Nix knows the contents: call
   `nix flake metadata --json`, hash the flake's `narHash` and those of its
   inputs (which a flake without a lock file resolves anew), and build from
   the locked reference it returns. It costs about 70 ms (measured
   2026-11-18, after the first fetch); a cached run costs 5.5 ms. Only a
   script naming a mutable reference pays it (decided 2026-11-18); refusing
   such a reference would rule out the answer to #14 for a library under
   development.

Telling 1 from 3 means classifying the reference's text, by the URL forms Nix
accepts.

## Where a `narHash` comes from

Nix computes it: the hash of the source tree's NAR serialization. A reference
carries it when pinned with `narHash=`, and a `flake.lock` or a registry entry
records it (NixOS writes `rev`, `narHash` and `lastModified` into
`/etc/nix/registry.json`). Cases 1 and 2 need none: a store path or a `rev`
already fixes the contents. Only case 3 has to compute it, which takes the
subprocess.

## Afterwards

- The draft reply to #14 tells the reader to run `magix -f` after an edit;
  drop that paragraph once case 3 follows the flake.
- `packages-from-an-overlay` cites the text-only hash as a blocker for a
  `#!nixpkgs` flake carrying an overlay.
