# A flag before clean-magix-cache runs a script

`magix -v clean-magix-cache --stale` runs a script named `clean-magix-cache`
with the argument `--stale` ("Could not read file: clean-magix-cache"): a
top-level flag commits `pRawCommand` (`src/Magix/Options.hs`) to the run
branch, so `-v` and `-c` work only after the subcommand. Found by the options
tests (`test/Magix/OptionsSpec.hs`) on 2026-11-17; they record the current
behavior and leave this case open.

Open: accept the shared flags before the subcommand, or reject them there with
a clear message. Either way, a script really named `clean-magix-cache` needs a
path (`./clean-magix-cache`), as it does today.
