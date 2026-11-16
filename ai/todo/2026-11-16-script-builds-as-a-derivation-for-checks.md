# A script builds as a derivation, for checks

A flake that installs a magix script as text (its closure: the text alone)
cannot check it: magix cannot run inside the build sandbox, and the
template's `writeShellApplication` shellcheck runs only at the first call.
A Nix function from this flake, say `magix.lib.build { pkgs, script }`,
would build a script with the same template and directives magix uses at
run time, so a flake check builds every installed script.

A system configuration needs this before it turns its rarely used
programs, each with a check, into magix scripts.
