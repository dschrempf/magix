# `getConfig` has unit tests

No unit test covers `getConfig` (`src/Magix/Config.hs`). Breaking the cache
slot rules is caught only by `check_deploy_keeps_build` in `scripts/check`,
which needs Nix: once with the script path back in the content hash, and once
with the slot keyed by the canonical instead of the invoked path.

Test, for a script reached through a symbolic link and by a relative path:

- `invokedPath` is absolute, with symbolic links unresolved; `scriptPath`
  resolves them.
- `slotDir` and `scriptName` follow the invoked path, not the contents.
- `magixHash` follows the contents and the build mode, not the path: the same
  contents behind another canonical path keep the hash.

Pass a flake reference as the directive and a cache path in `Options`, so
the test reads neither `NIX_PATH` nor the XDG cache directory.
