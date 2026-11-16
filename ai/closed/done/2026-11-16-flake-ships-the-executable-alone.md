# The flake ships the executable alone

`packages.magix` (and `default`) is `callCabal2nix`'s library derivation.
Installed, it keeps GHC, its documentation and every Haskell dependency at
run time. In the closures of two NixOS systems on 2026-11-16, magix alone
held 2.7 GiB and 3.1 GiB, GHC 9.10.3 with its 753 MiB `-doc` the bulk of it.

magix runs `nix-build` and `nix build`; it needs no GHC of its own. Export
`haskell.lib.justStaticExecutables` of the package (or give it a separate
`bin` output). The dev shell keeps the library.

## Outcome

Done on 2026-11-16: `packages.magix` and `default` are
`justStaticExecutables` of the package; the overlay and the dev shells keep
the library. The closure dropped from 3.1 GiB to 74 MiB. The templates sit in
a separate `-data` output, which the executable references. All test scripts
ran with the new executable.
