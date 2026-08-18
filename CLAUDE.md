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

### Directive Parsing (`src/Magix/Directives.hs`, `src/Magix/Languages/*/Directives.hs`)

Scripts are parsed with `megaparsec`. The top-level parser (`pDirectives`) reads the shebang, then dispatches to a language-specific parser based on the `#!magix <language>` directive. Each language module defines its own `Directives` type and parser (e.g. `BashDirectives`, `HaskellDirectives`). All language `Directives` are unified via the `Directives` sum type in `src/Magix/Languages/Directives.hs`.

### Nix Expression Generation (`src/Magix/Expression.hs`, `src/Magix/Languages/*/Expression.hs`)

Each language has a `Template.nix` file (declared as `data-files` in the cabal file) with placeholders like `__SCRIPT_NAME__` and `__PACKAGES__`. The expression modules produce `[Replacement]` (pairs of placeholder/value `Text`) that are applied to the template via `Data.Text.replace`.

### Build & Cache (`src/Magix/Build.hs`, `src/Magix/Config.hs`, `src/Magix/Paths.hs`)

`Config` holds all relevant paths (cache dir, build dir, result link, lock file). `getBuildStatus` checks whether a result symlink exists. `build` writes the generated Nix expression to disk and calls `nix-build`. A file lock (`filelock`) prevents concurrent builds for the same script.

### Adding a New Language

1. Add the constructor to `Language` in `src/Magix/Languages/Language.hs`.
2. Create `src/Magix/Languages/<Lang>/Directives.hs` with a `<Lang>Directives` type and parser.
3. Create `src/Magix/Languages/<Lang>/Expression.hs` with replacements.
4. Create `src/Magix/Languages/<Lang>/Template.nix`.
5. Register the template in `data-files` in `magix.cabal`.
6. Wire into `src/Magix/Languages/Directives.hs` and `src/Magix/Languages/Expression.hs`.
7. Add test scripts under `test-scripts/<lang>/`.

## Compiler Settings

All targets use `-Wall -Werror -Wunused-packages -Wredundant-constraints` with `GHC2021`. Default extensions: `DuplicateRecordFields`, `OverloadedRecordDot`, `OverloadedStrings`.
