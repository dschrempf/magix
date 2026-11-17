# Unreleased

- Add `print-magix-expression`, printing the Nix expression of a script
  without building it
- Keep one build per script: the cache holds a slot per path a script is
  invoked by, and a build removes the build it supersedes. A deploy that moves
  an unchanged script (a new store path behind the same invoked path) no
  longer rebuilds it
- Add `clean-magix-cache --stale`, removing the slots whose script is gone and
  every entry of the previous cache layout; run it once after upgrading
- Execute the store path of the build, resolved while holding the lock
- `clean-magix-cache` no longer fails before the first build created the
  cache directory
- `clean-magix-cache` waits for running builds instead of removing their files
- Read the build output of Nix as UTF-8 under any locale; magix failed
  with `hGetContents: invalid argument` where `LANG` was unset or `C`
- The flake's `packages.magix` (and `default`) is the executable alone
  (`justStaticExecutables`); its closure drops from 3.1 GiB to 74 MiB
- Resolve `nixpkgs` from unprefixed directories in `$NIX_PATH` (like
  `builtins.findFile`); fixes <https://github.com/dschrempf/magix/issues/18>
- The flake a script builds with in flake mode no longer takes the unpinned
  `flake-utils` input, which `nix build` fetched on the first build of every
  script; it builds for every system in `nixpkgs.lib.systems.flakeExposed`
- Resolve a `flake:<id>` entry of `$NIX_PATH`, which a flake-built NixOS
  sets, to its store path through the user and the system flake registry, as
  Nix does: a Nixpkgs update now rebuilds a cached script. magix split such
  an entry in two and hashed its text. It fails on an id these registries do
  not map to a path, and on a `channel:` or URL entry
- Find Nixpkgs in `$NIX_PATH` like Nix: skip an entry whose path does not
  exist, and resolve symbolic links, so a channel update rebuilds a cached
  script. A script cached under a symlinked `$NIX_PATH` entry rebuilds once
  after upgrading
- Build against the Nixpkgs in the cache hash: `nix-build` used `<nixpkgs>`
  from `$NIX_PATH`, also when `--nixpkgs-path` named another
- A directive line must end after its values: magix read `#!magix bashful`
  as `bash`, and `#!packages jq` followed by an invalid character as `jq`,
  leaving the rest of the line to the script body. A directive line ending in
  CRLF fails with a message naming CRLF line endings

# Version 1.0.0.0

## User-facing

- Support Nix Flakes (`nix build`)
- Use `execve` to replace process for exact exit-code propagation

## Internal

- Directives now contain common and language-specific directives
- Collect environment variables in `Magix.Env` module
- Rename `Magix.Languages.*` to `Magix.Language.*`
- Fix package categories

# Version 0.6.0.0

- Add command to clean Magix cache (`clean-magix-cache`)
- Add script path to Magix hash (fixes
  <https://github.com/dschrempf/magix/issues/15>)

# Version 0.5.0.0

- Split modules so that implementing new languages becomes easier
- More tests
- Language data type
- Toolchain downgrade to GHC 9.8 (dropped into Nixpkgs, and so is easier to use)

# Version 0.4.0.1

- Update and improve GitHub workflows (use Nix and Cachix)
- Toolchain update to GHC 9.10
- Refactor "Expression" modules
- Simplify test suite

# Version 0.4.0.0

- Refactor large parts of "Directive" modules and test suite
- Improve logging framework

# Version 0.3.0.0

- Consistently use the word *package* across languages: `runtimeInputs` -\>
  `packages` .
