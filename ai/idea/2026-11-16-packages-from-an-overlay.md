# `#!packages` from an overlay

`#!packages` resolves against `import <nixpkgs> { }`, with no overlay and
no `config` (`allowUnfree`). A `#!nixpkgs` flake reference could name a
flake whose `legacyPackages` carries an overlay, but its cache hash holds
the reference's text alone, the problem of
`flake-nix-path-entry-resolves-to-a-store-path` again.

Two uses wait on it (2026-11-17). overriding-packages-for-use-with-magix
(#14) asks to build a Haskell script against a modified library. A system
configuration wants mpv built against `ffmpeg-full` on first use, an
override no attribute of nixpkgs holds; it installs its other sporadic
programs as magix scripts already. A `#!packages` value stays a word, so the
override needs a place of its own: an overlay the script or the cache names,
or an expression directive.
