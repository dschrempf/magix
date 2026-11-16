# Order of the todos

From the closure measurements of `~/nix/config` (2026-11-16), in order:

1. `flake-ships-the-executable-alone`, the largest gain and the smallest
   change.
2. `rebuild-replaces-the-superseded-build`.
3. `flake-nix-path-entry-resolves-to-a-store-path`, after 2.
4. `script-builds-as-a-derivation-for-checks`.
5. `flake-template-drops-flake-utils`.

The `spec-` entries have no rank yet; `spec-cache-eviction` overlaps with 2.
