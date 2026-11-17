# The nixpkgs-path option accepts a missing path

`--nixpkgs-path /nonexistent` resolves to `ChannelBuild "/nonexistent"`:
`canonicalizePath` (`resolveBuildMode`, `src/Magix/Config.hs`) does not fail
on a missing path, so the error surfaces only in `nix-build`. A `NIX_PATH`
entry naming a missing path is skipped, and a `flake:` target fails early
with a clear message. Found by the config tests on 2026-11-17.

Related, dead code: `pNixPath` (`src/Magix/NixpkgsPath.hs`) accepts every
string (the QuickCheck properties in `test/Magix/NixpkgsPathPropertiesSpec.hs`
show it is total), so the `Left` branch of `getDefaultNixpkgsPathOrFail`, with
its `putStrLn` to stdout, and `NixpkgsPathError` never run.
`getDefaultNixpkgsPath` could return the entries directly.

## Outcome

Fixed on 2026-11-17 (`fix(config): fail early on a missing path of
--nixpkgs-path`). `resolveBuildMode` resolves the path of `--nixpkgs-path`
with `canonicalIfExists`, as for a `NIX_PATH` entry, and dies with a message
naming the path if it does not exist, also for a dangling symbolic link. A
`NIX_PATH` entry naming a missing path is still skipped. The config test
"fails if the Nixpkgs path of the options does not exist" fails without the
fix.

`getDefaultNixpkgsPath` returns the entries (`refactor(nixpkgs-path): return
the NIX_PATH entries directly`); `NixpkgsPathError` and the `Left` branch of
`getDefaultNixpkgsPathOrFail` are gone. A failure of `pNixPath`, which the
properties show cannot happen, calls `error`.
