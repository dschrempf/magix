# Posix: treat arguments using filesystem encoding (haskell/process#388)

Under `LC_ALL=C`, `System.Process` drops the bytes of an argument that the
locale cannot encode. The pull request restores the encoding of arguments that
"Posix: Do not treat arguments as `FilePath`" (haskell/process#320) replaced.

How it reaches magix:

- `cabal exec` passes its arguments on through `process`, so
  `check_stale_keeps_non_ascii_path` runs magix directly (`WTNG` marker in
  `scripts/check`).
- Magix passes the build directory and the result link to `nix-build`. With a
  non-ASCII cache path under the C locale, the build fails for a cache at
  `…/cä`: `path '…/c/…-build' does not exist`.
