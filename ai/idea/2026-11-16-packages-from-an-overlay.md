# `#!packages` from an overlay

`#!packages` resolves against `import <nixpkgs> { }`, with no overlay and
no `config` (`allowUnfree`). A `#!nixpkgs` flake reference can name a
flake whose `legacyPackages` carries an overlay. Since
`flake-build-rebuilds-on-a-changed-flake`, an edit of that flake rebuilds
the script; an unpinned flake costs a `nix flake metadata` call per run.

Two uses wait on it (2026-11-17). overriding-packages-for-use-with-magix
(#14) asks to build a Haskell script against a modified library. A system
configuration wants mpv built against `ffmpeg-full` on first use, an
override no attribute of nixpkgs holds; it installs its other sporadic
programs as magix scripts already. A `#!packages` value stays a word, so the
override needs a place of its own: an overlay the script or the cache names,
or an expression directive.
