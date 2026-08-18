# Spec: Nix Flakes Support

## Problem

All three language templates use `pkgs ? import <nixpkgs> {}`, which requires
`nix-build` and a nixpkgs channel registered in `NIX_PATH`. On flake-only NixOS
setups (i.e. no channels configured), this breaks entirely. Flake-based builds
are also more reproducible because the nixpkgs revision is pinned rather than
taken from a mutable channel pointer.

## Goal

Allow users to opt in to flake-based builds, where the generated Nix expression
is a flake and the build is driven by `nix build` instead of `nix-build`.

## Design

### New directive

Add an optional top-level directive `#!nixpkgs <flake-ref>` (e.g.
`#!nixpkgs github:NixOS/nixpkgs/nixos-unstable`) that selects flake mode and
pins the nixpkgs input. When absent, behaviour is unchanged (channel mode).

### Template variants

Each language gets a second template `FlakeTemplate.nix` (a `flake.nix`) with
the same placeholders plus `__NIXPKGS_REF__`. The existing `Template.nix` files
are untouched.

### Build path

`Config` gains a `buildMode :: BuildMode` field (`ChannelBuild | FlakeBuild`).
`build` dispatches to `nix-build` or `nix build --out-link` accordingly.
The nixpkgs ref is folded into the hash so channel and flake builds for the same
script get separate cache entries.

### Nixpkgs path option

The existing `--nixpkgs-path` flag keeps its current meaning (channel mode
only). In flake mode the ref comes from the directive; `--nixpkgs-path` is
ignored with a warning.

## Open questions

- Should a global config file (e.g. `~/.config/magix/config.toml`) be able to
  set a default flake ref, so users don't repeat it in every script?
- Should `nix build` be invoked with `--no-write-lock-file` to avoid writing
  `flake.lock` into the build directory?
