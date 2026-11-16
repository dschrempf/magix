# A format script formats all code

Formatting is by hand, so it drifts: `app/Main.hs` departs from ormolu, and a
feature commit re-indented `scripts/check`.

Add `scripts/format`, on `PATH` in the dev shell like `check`, running ormolu
on the Haskell files (the code follows it; the dev shell also ships
fourmolu), `cabal-fmt` on `magix.cabal`, and `shfmt -i 4` on the shell
scripts. `check` runs it in check mode, so CI fails on unformatted code.
Then format `app/Main.hs` in a commit of its own.
