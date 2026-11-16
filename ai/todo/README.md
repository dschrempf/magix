# Order of the todos

From closure measurements of two NixOS systems that install magix
(2026-11-16), in order:

1. `rebuild-replaces-the-superseded-build`.
2. `flake-nix-path-entry-resolves-to-a-store-path`, after 1.
3. `script-builds-as-a-derivation-for-checks`.
4. `flake-template-drops-flake-utils`.

`cache-evicts-by-age-or-count`, `parse-error-shows-the-header-alone` and
`directive-tests-cover-every-language` have no rank yet;
`cache-evicts-by-age-or-count` overlaps with 1.
