# The flake template drops flake-utils

`src/Magix/FlakeTemplate.nix` takes `github:numtide/flake-utils`, unpinned,
and `nix build` runs with `--no-write-lock-file`: a network fetch on every
first build in flake mode, and an input nobody locks.
`nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed` needs no input.

## Outcome

Done on 2026-11-17: the template's only input is nixpkgs, and it builds
`packages.<system>.default` for every system in
`nixpkgs.lib.systems.flakeExposed` with `nixpkgs.lib.genAttrs`. A flake-mode
run of `test-scripts/bash/hello-flake` on an empty cache fetched nixpkgs
alone and built the same store path as before.
