# `process` drops the non-ASCII bytes of arguments under the C locale

Under `LC_ALL=C`, GHC 9.10.3 with `process` 1.6.26.1 drops the non-ASCII
bytes of an argument passed to a child process: `getArgs` reads `dä` back as
`d\56515\56484`, but `callProcess "printf" ["%s\n", "dä"]` prints `d`.
"unicode parts of env are lost when running createProcess"
(haskell/process#152) fixed the same loss for the environment, saying
arguments were already safe. Find the upstream issue, or report one in
`haskell/process` with this reproduction:

```haskell
import System.Environment (getArgs)
import System.Process (callProcess)

main :: IO ()
main = getArgs >>= callProcess "printf" . ("%s\n" :)
```

Run it as `LC_ALL=C ./t dä | od -c`. Then replace `(unreported)` in the
`WTNG` marker of `scripts/check` with the issue.

How it reaches magix:

- `cabal exec` passes its arguments on through `process`, so
  `check_stale_keeps_non_ascii_path` runs magix directly.
- Magix passes the build directory and the result link to `nix-build`. With a
  non-ASCII cache path under the C locale, the build fails: `path '…/c/…-build'
  does not exist` for a cache at `…/cä`. A home directory with a non-ASCII name
  and a service without `LANG` hit this.
