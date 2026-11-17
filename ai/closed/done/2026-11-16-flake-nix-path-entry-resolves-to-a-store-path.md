# A `flake:` entry of `NIX_PATH` resolves to a store path

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

## Outcome

Done on 2026-11-17 (merge `4071669`). A subprocess was too slow: a cached
run took 5.5 ms, `nix-instantiate --find-file nixpkgs` 33 ms for a `flake:`
entry. So magix reads the user registry, then the system one, at the paths
Nix 2.35 uses (`NIX_CONFIG_HOME`, `XDG_CONFIG_HOME`, `NIX_CONF_DIR`), and
follows indirect entries to a `path` target. It fails with a message where
these registries don't give a path, and on `channel:` and URL entries.
`NIX_PATH` now splits like Nix, keeping `nixpkgs=flake:nixpkgs` one entry. An
entry whose path is missing is skipped, and paths are canonicalized, so a
channel symlink rebuilds on update too. `nix-build` gets
`-I nixpkgs=<hashed path>`, which also fixes `--nixpkgs-path`. A cached run
with a `flake:` entry costs the same as with a store path.
