# The flake template drops flake-utils

Date: 2026-11-16

`src/Magix/FlakeTemplate.nix` takes `github:numtide/flake-utils`, unpinned,
and `nix build` runs with `--no-write-lock-file`: a network fetch on every
first build in flake mode, and an input nobody locks.
`nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed` needs no input.
