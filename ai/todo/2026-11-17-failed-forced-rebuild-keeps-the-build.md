# A failed forced rebuild keeps the build

`build` (`src/Magix/Build.hs`) calls `removeBuild` before it runs Nix, so a
forced rebuild (`-f`) of unchanged contents that fails leaves the slot without
a result: the next run builds again, and the script is unusable until a build
succeeds. A failed build of changed contents keeps the previous build
(`removeSuperseded` runs only on success); the forced case should match.
Found by the stub Nix tests (`test/Magix/BuildSpec.hs`) on 2026-11-17.

Fix: build into a temporary out link, and replace the result link only on
success; then remove the build directory of the failed attempt.

Related, in `removeBuild`: it tests the result link with `doesDirectoryExist`,
which follows the link, so a link whose store path was garbage-collected is
never removed by it. A successful build overwrites it; a failed one leaves it,
and `removeSuperseded` keeps it, as its name is current.
