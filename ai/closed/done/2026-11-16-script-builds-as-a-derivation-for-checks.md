# A script builds as a derivation, for checks

A flake that installs a magix script as text (its closure: the text alone)
cannot check it: magix cannot run inside the build sandbox, and the
template's `writeShellApplication` shellcheck runs only at the first call.
A Nix function from this flake, say `magix.lib.build { pkgs, script }`,
would build a script with the same template and directives magix uses at
run time, so a flake check builds every installed script.

A system configuration needs this before it turns its rarely used
programs, each with a check, into magix scripts.

## Outcome

Done on 2026-11-17 in the branch
`feat/script-builds-as-a-derivation-for-checks`:

- `magix print-magix-expression [-v] SCRIPT` prints the expression of
  `getNixExpression`, which now takes the script name instead of the
  `Config`; it reads neither `NIX_PATH` nor the cache, so it runs in the
  sandbox.
- `lib.build { pkgs, script, name ? … }` runs it in a derivation with the
  magix of this flake for the build platform, writes the layout of a build
  directory (`default.nix`, `script`) to `$out`, and imports it with
  `{ inherit pkgs; }`. Import from derivation works under pure evaluation:
  `nix flake check`, here and in a consumer flake with a `path:` input.
- `name` defaults to the base name of `script` without the hash of a store
  path; the executable drops its extension, as at run time. A `#!nixpkgs`
  directive is ignored: the build uses `pkgs`.
- The flake's `checks` build every script under `test-scripts/` (all three
  languages, seconds each once the dependencies are cached) and the naming;
  `scripts/check` runs `nix flake check`.

The check does not lint a Bash script: `writeShellApplication` shellchecks
only the wrapper, which sources the script under
`# shellcheck source=/dev/null`. It catches a failed parse, a missing package
and a Haskell compile error.
