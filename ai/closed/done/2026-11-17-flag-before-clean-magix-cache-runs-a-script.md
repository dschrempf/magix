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

## Outcome

Fixed on 2026-11-17 (`fix(options): accept -v and -c before clean-magix-cache`).
Decided: accept the shared flags before the subcommand. `pRawCommand`
(`src/Magix/Options.hs`) parses `-v` and `-c` outside the alternative of
`clean-magix-cache` and the run, so they no longer commit it to a run, and
applies them to either command. After `clean-magix-cache`, both are accepted as
well: `-v` in either position sets the verbosity, and the `-c` after the command
wins over the one before it. Run-only flags (`-f`, `-n`, `-r`) before
`clean-magix-cache` still run a script of that name, and a script named
`clean-magix-cache` still needs a path. `--help` lists `-v` and `-c` before
`COMMAND`; the Readme shows the new output. `OptionsSpec` covers the flags
before, after and on both sides of the command; the four cases with a flag
before it fail without the fix.
