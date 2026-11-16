# `process` drops the non-ASCII bytes of arguments under the C locale

Under `LC_ALL=C`, `System.Process` drops the bytes of an argument that the
locale cannot encode. Unreported; `report.md` holds the report and how to file
it.

How it reaches magix:

- `cabal exec` passes its arguments on through `process`, so
  `check_stale_keeps_non_ascii_path` runs magix directly (`WTNG` marker in
  `scripts/check`).
- Magix passes the build directory and the result link to `nix-build`. With a
  non-ASCII cache path under the C locale, the build fails: `path '…/c/…-build'
  does not exist` for a cache at `…/cä`.
