# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

Magix is a Haskell tool that builds, caches, and runs scripts with Nix-managed dependencies. Scripts declare their language and packages via special `#!` directives, and Magix generates a Nix expression, calls `nix-build`, caches the result, and executes the script.

Supported languages: **Bash**, **Haskell**, **Python**.

Example script:
```bash
#!/usr/bin/env magix
#!magix bash
#!packages jq

jq --help
```

## Development Environment

Enter the dev shell (provides `cabal`, `haskell-language-server`, etc.):
```sh
nix develop
```

## Common Commands

```sh
cabal build          # build
cabal test           # run unit tests
cabal run magix -- [args]   # run the executable

# Run a specific test group (hspec pattern matching)
cabal test --test-option='--match=/Magix.Build/'

# Full integration check (builds, tests, runs all test-scripts)
check        # on PATH inside nix develop shell

# Format the cabal file
cabal-fmt -i magix.cabal
```

CI runs `nix build` and then `nix develop .#check --command check`.

## Architecture

### Directive Parsing (`src/Magix/Directives.hs`, `src/Magix/Language/*/Directives.hs`)

Scripts are parsed with `megaparsec`. The top-level parser (`pDirectives`) reads the shebang, then dispatches to a language-specific parser based on the `#!magix <language>` directive. Each language module defines its own `Directives` type and parser (e.g. `BashDirectives`, `HaskellDirectives`). All language `Directives` are unified via the `Directives` sum type in `src/Magix/Language/Directives.hs`.

### Nix Expression Generation (`src/Magix/Expression.hs`, `src/Magix/Language/*/Expression.hs`)

Each language has a `Template.nix` file (declared as `data-files` in the cabal file) with placeholders like `__SCRIPT_NAME__` and `__PACKAGES__`. The expression modules produce `[Replacement]` (pairs of placeholder/value `Text`) that are applied to the template via `Data.Text.replace`.

### Build & Cache (`src/Magix/Build.hs`, `src/Magix/Cache.hs`, `src/Magix/Config.hs`, `src/Magix/Hash.hs`, `src/Magix/Paths.hs`)

The cache holds one slot per path a script is invoked by (absolute, symlinks unresolved), named by a hash of that path (`getSlotHash`). A slot holds a `lock`, a `path` symlink to the invoked path, and one build named by the content hash (`getMagixHash`): `<hash>-build/` and the `<hash>-result` symlink. `Config` holds these paths. `getBuildStatus` checks whether the result symlink exists. `build` writes the generated Nix expression into a pending build directory (`getPendingBuildDir`) and calls `nix-build` with the result symlink as out-link, which Nix replaces only on success; then the pending directory replaces `<hash>-build/`, and `build` removes the builds it supersedes (`removeSuperseded`). So a failed build, also a forced one, keeps the previous build. A slot's `lock` (`filelock`) serializes its builds; the result is resolved to its store path while holding it. Builds also hold the cache's `lock` (`getCacheLockPath`) shared, and `clean-magix-cache` holds it exclusively, so cleaning waits for running builds. `clean-magix-cache --stale` removes slots whose invoked path is gone, and every entry that is no slot.

### Adding a New Language

1. Add the constructor to `Language` in `src/Magix/Language/Language.hs`.
2. Create `src/Magix/Language/<Lang>/Directives.hs` with a `<Lang>Directives` type and parser.
3. Create `src/Magix/Language/<Lang>/Expression.hs` with replacements.
4. Create `src/Magix/Language/<Lang>/Template.nix`.
5. Register the template in `data-files` in `magix.cabal`.
6. Wire into `src/Magix/Language/Directives.hs` and `src/Magix/Language/Expression.hs`.
7. Add test scripts under `test-scripts/<lang>/`.

## Compiler Settings

All targets use `-Wall -Werror -Wunused-packages -Wredundant-constraints` with `GHC2024`. Default extensions: `DuplicateRecordFields`, `OverloadedRecordDot`, `OverloadedStrings`.

## Tests

Pull requests merge without review (see Branches), so the tests are the gate. A change
that can break behavior ships a test that fails without it; check that by
reverting the change and running the test. A tiny change, or one whose test
would only restate the code, needs none: judge which. A test that needs the
`nix` binary goes into the suite `test-magix-nix` (`test-nix/`); `cabal test`
runs both suites, the sandboxed `nix build` only `test-magix`. `check` prints
the per-module coverage of the library by both suites, as a guide to thin
spots and not a target.

## Commits

Conventional Commits: `type(scope): statement`, e.g. `fix(cache): …`, `docs(ai): …`. The scope names a module (`run`, `cache`, `directives`, a language) or `flake`, `ai`, `readme`, `changelog`. Older history mixes styles (`tooling: update nix flake`, bare sentences); new commits don't follow it.

## Branches

Changes reach `main` through a pull request, merged once CI
(`.github/workflows/haskell.yml`) passes on it; commit to `main` directly only
when asked. Keep branches short-lived. A pull request may carry several changes,
such as the branches of agents working in parallel.
