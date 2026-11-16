# Upstream report

File the issue below in `haskell/process`, then:

1. In `scripts/check`, replace `(unreported)` in the `WTNG` marker with
   `(haskell/process#<n>)`.
2. Move "How it reaches magix" from `README.md` to
   `ai/issue/haskell/process/<n>-<slug>.md`, and delete this todo.

---

**Title:** Arguments lose their non-ASCII bytes under a non-UTF-8 locale

Under `LC_ALL=C`, `createProcess` drops the bytes of an argument the locale
cannot encode, even when the argument comes from `getArgs` and would round-trip.

```haskell
import System.Environment (getArgs)
import System.Process (callProcess)

main :: IO ()
main = getArgs >>= callProcess "printf" . ("%s\n" :)
```

```console
$ LC_ALL=C ./t dä | od -c
0000000   d  \n
```

Expected `d 303 244 \n`: `getArgs` returns `"d\56515\56484"`, which the file
system encoding turns back into the original bytes.

`System/Process/Posix.hs` encodes the command and the working directory with
`withFilePath`, but the arguments with `withCString`, which uses the foreign
encoding. #152 fixed the same for the environment.

GHC 9.10.3, process 1.6.26.1, Linux; `master` encodes the arguments the same
way.
