# The flake ships the executable alone

Date: 2026-11-16

`packages.magix` (and `default`) is `callCabal2nix`'s library derivation.
Installed, it keeps GHC, its documentation and every Haskell dependency at
run time. In the closures of `~/nix/config` on 2026-11-16, magix alone held
2.7 GiB on schwarzbaer and 3.1 GiB on blaubaer, GHC 9.10.3 with its 753 MiB
`-doc` the bulk of it.

magix runs `nix-build` and `nix build`; it needs no GHC of its own. Export
`haskell.lib.justStaticExecutables` of the package (or give it a separate
`bin` output). The dev shell keeps the library.
