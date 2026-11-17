# Order of the todos

From closure measurements of two NixOS systems that install magix
(2026-11-16), in order:

1. `script-builds-as-a-derivation-for-checks`.

`tests-cover-build-options-and-main` comes first: work merges to `main`
without review, so the tests are the gate (2026-11-17).

`parse-error-shows-the-header-alone`, `directive-line-ignores-an-invalid-tail`,
`format-script-formats-all-code`,
`script-run-by-store-path-keeps-one-build` and
`slot-of-a-non-ascii-path-ignores-the-locale` have no rank yet.
