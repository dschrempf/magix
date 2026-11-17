# Order of the todos

From closure measurements of two NixOS systems that install magix
(2026-11-16), in order:

1. `script-builds-as-a-derivation-for-checks`.

`failed-forced-rebuild-keeps-the-build` loses a working build; it is the
next fix the tests found (2026-11-17).

`parse-error-shows-the-header-alone`, `format-script-formats-all-code`,
`script-run-by-store-path-keeps-one-build`,
`slot-of-a-non-ascii-path-ignores-the-locale`,
`flag-before-clean-magix-cache-runs-a-script`,
`nixpkgs-path-option-accepts-a-missing-path` and
`unreadable-script-error-names-no-reason` have no rank yet.
