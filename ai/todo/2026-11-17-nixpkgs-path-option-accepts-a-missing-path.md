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
