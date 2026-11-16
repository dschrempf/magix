# A format script formats all code

Formatting is by hand, so it drifts: `app/Main.hs` departs from ormolu, and a
feature commit re-indented `scripts/check`.

Add `scripts/format`, on `PATH` in the dev shell like `check`, formatting every
file type in the repository:

- Haskell with ormolu (the code follows it; the dev shell also ships fourmolu);
- `magix.cabal` with `cabal-fmt`;
- shell scripts with `shfmt -i 2`;
- Nix, Markdown and YAML, with formatters still to choose.

Run it before every commit; whether a Git hook or `CLAUDE.md` enforces that is
still to decide. `check` runs it in check mode, so CI fails on unformatted
code. Then format each file type that drifted in a commit of its own.

Open: the scripts in `test-scripts/` carry magix directives and no extension;
format them, or leave them out.
