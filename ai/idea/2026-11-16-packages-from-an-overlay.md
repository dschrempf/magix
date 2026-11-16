# `#!packages` from an overlay

`#!packages` resolves against `import <nixpkgs> { }`, with no overlay and
no `config` (`allowUnfree`). A `#!nixpkgs` flake reference could name a
flake whose `legacyPackages` carries an overlay, but its cache hash holds
the reference's text alone, the problem of
`flake-nix-path-entry-resolves-to-a-store-path` again. Nothing needs it
yet; every package the known scripts need is in nixpkgs.
