# Refuse an unpinned flake reference

Instead of locking a `#!nixpkgs` reference its text doesn't pin, magix could
refuse it, as `FlakeBuild`'s comment once asked for a pinned reference.

Dropped 2026-11-18 in a-cached-script-ignores-changes-to-its-nixpkgs-flake
(#27): it would rule out the answer to overriding-packages-for-use-with-magix
(#14), a local flake applying an overlay to a library under development.
magix locks such a reference with `nix flake metadata` instead.
