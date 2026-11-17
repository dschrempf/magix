# Tests cover build, options and main

Work merges to `main` without review, so the test suite is the gate. Its
gaps (`cabal test --enable-coverage`, expressions used, on `3413364`):

- `Magix.Options` 0% (2/274), `Magix.Env` 0%, `Magix.Build` 17% (38/220),
  `Magix.Config` 57%, `Magix.Directives` 76%.
- `app/Main.hs` lies outside the library, so no unit test reaches it.
- Parsers, expressions, `Paths` and `Cache` are at 85–100%.

`scripts/check` runs the build and cache end to end, but it needs Nix and the
network: CI runs it, `cabal test` does not.

## Scope, by what a regression costs

1. **Hash stability.** Golden values of `getMagixHash`, `getSlotHash` and
   `buildModeBytes` for fixed inputs. A changed hash silently rebuilds every
   cached script or orphans its slot. Changing a golden value then becomes a
   deliberate act, and the Changelog says so.
2. **Build without Nix.** `build`, `withBuildLock`, `resolveResult`,
   `removeBuild` and the removal of superseded builds, driven by a stub
   `nix-build` and `nix` on `PATH` that writes the result link or fails. This
   covers what `check_rebuild_replaces_superseded` and
   `check_failed_build_keeps_previous` check, with no Nix.
3. **Options.** Run `rawCommandParser` through `execParserPure`: every flag;
   `--nixpkgs-path` and `--nixpkgs-ref` exclude each other;
   `MAGIX_NIXPKGS_REF` yields to `--nixpkgs-ref`; arguments after the script
   (`-h` included) go to the script; `clean-magix-cache` and `--stale`.
   `getCommand` mixes this with reading the environment; split out a pure core
   taking `RawCommand`, `MagixEnv` and `NixEnv`.
4. **Config.** Test the priority in `resolveBuildMode` (directive, then
   CLI/env, then `NIX_PATH`), and its failures: `NIX_PATH` unset, unparsable,
   or naming no nixpkgs.
5. **Main.** Move `runMagix` and `cleanCache` into a library module, so tests
   reach the reuse path, the rebuild flags and the parse-error path (the last
   meets `parse-error-shows-the-header-alone`).
6. **Templates.** Every language's generated expression passes
   `nix-instantiate --parse`, and evaluates against `<nixpkgs>`
   (`--eval` or `--dry-run`). This needs the `nix` binary.
7. **Properties.** QuickCheck or hedgehog properties for the `NIX_PATH` and
   directive parsers: whitespace, package names, entry order.

## Decided (2026-11-17)

- **The build tests stub `nix-build` and `nix` on `PATH`.** No change to
  production code, and the test exercises the real process call.
- **Tests that need the `nix` binary (6) form a second test suite,
  `test-magix-nix`.** Plain `cabal test` runs both suites, no flag; the
  sandboxed `nix build`, which has no `nix`, runs `test-magix` alone
  (`testTargets`).
- **`scripts/check` stays the end-to-end tier against real Nix.** (2) moves
  its cache logic into unit tests; nothing is ported to Haskell.
- **`check` prints the per-module coverage report, with no threshold.** It
  shows where tests are thin, and is no target.
- **The `Tests` rule in `CLAUDE.md`:** a change that can break behavior
  ships a test; a tiny one needs none, by judgment.
- **`withSystemTempDirectory` from `temporary` replaces
  `getRandomTempDir`,** which never removes its directories.
- **QuickCheck for (7).**

When the coverage report lands, add it to `Tests` in `CLAUDE.md`.

## Status

`flake-nix-path-entry-resolves-to-a-store-path` merged on 2026-11-17, so
(2)–(5), the `temporary` switch and the coverage report can start. (1), (6)
and (7) are on branch `test/hashes-templates-properties`.
