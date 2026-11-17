# The templates lint the script body

The bash template (`src/Magix/Language/Bash/Template.nix`) builds a
`writeShellApplication` whose text only `source`s the script, under
`# shellcheck source=/dev/null`: shellcheck checks the two-line wrapper,
never the script. So neither a build at run time nor `lib.build` (a flake
check) reports a shellcheck finding in a bash script. Found while adding
`lib.build` on 2026-11-17 (`script-builds-as-a-derivation-for-checks`).

Lint the script body in each template, where a linter exists:

- bash: shellcheck the script itself, e.g. by putting its text into
  `writeShellApplication`, or a `shellcheck` call in a check phase;
- python and haskell: open whether a linter (e.g. `ruff`, `hlint`) fits;
  GHC's warnings already run.

Open:

- A script that fails the lint stops building, at run time too: a behavior
  change for every consumer. Check their scripts first; consider making the
  lint a check of `lib.build` only.
- A script body's `#!` lines (shebang, directives) must not trip the linter.
