# A `flake:` entry of `NIX_PATH` resolves to a store path

Date: 2026-11-16

A flake-built NixOS sets `NIX_PATH=nixpkgs=flake:nixpkgs`
(`nixpkgs.flake.setNixPath`, on by default). magix's default build mode
takes the entry's text as the path (`resolveNixpkgs` in
`src/Magix/NixpkgsPath.hs`; only `--nixpkgs-path` is canonicalized), so the
cache hash holds the literal `channel:flake:nixpkgs`, while `nix-build`
reads whatever nixpkgs the registry names today. A nixpkgs update therefore
never rebuilds a cached script: it runs on the nixpkgs of its first build,
rooted, until the script changes. `BuildMode`'s comment ("the store path is
included in the cache hash") does not hold for such an entry.

Resolve a `flake:` entry to its store path before hashing. Either
`nix-instantiate --find-file nixpkgs`, a subprocess on every call to
measure against the 20 ms a cached run costs, or reading the registries
(`/etc/nix/registry.json`, then the user's). A system can sidestep it with
a `NIX_PATH` holding the store path (`nix.nixPath = [ "nixpkgs=${nixpkgs}" ]`).

Wait for `rebuild-replaces-the-superseded-build` first.
